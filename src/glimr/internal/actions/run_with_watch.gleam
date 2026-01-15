import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/io
import gleam/list
import gleam/string
import glimr/console/console
import glimr/internal/actions/run_hooks
import glimr/internal/config.{type Hooks}
import simplifile

/// Starts the application with file watching. Monitors the src
/// directory for changes and triggers hooks or restarts based
/// on which files changed.
///
pub fn run(hooks: Hooks) -> Nil {
  console.output()
  |> console.unpadded()
  |> console.blank_line(1)
  |> console.line_warning("Starting application with file watching... ✨")
  |> console.line_warning("Press Ctrl+C to stop")
  |> console.print()

  let initial_mtimes = get_gleam_file_mtimes("src")
  let port = start_gleam_run()
  start_output_reader(port)
  watch_loop(initial_mtimes, port, hooks)
}

/// Main watch loop that polls for file changes every second.
/// Triggers route-modified hooks or restarts the application
/// depending on which files changed.
///
fn watch_loop(last_mtimes: Dict(String, Int), port: Port, hooks: Hooks) -> Nil {
  process.sleep(1000)

  let current_mtimes = get_gleam_file_mtimes("src")
  let changed_files = find_changed_files(last_mtimes, current_mtimes)

  case changed_files {
    [] -> watch_loop(current_mtimes, port, hooks)
    files -> {
      let routes_source_changed =
        list.any(files, fn(f) {
          {
            string.contains(f, "src/routes/")
            && !string.contains(f, "src/bootstrap/gen/routes/")
          }
          || string.contains(f, "src/app/http/controllers/")
        })

      let only_compiled_routes =
        list.all(files, fn(f) { string.contains(f, "src/bootstrap/gen/routes/") })

      case routes_source_changed {
        True -> {
          // Route source files changed - run routes-modified hooks
          io.println("")
          io.println(console.warning("File changes detected:"))
          list.each(files, fn(f) { io.println("  " <> f) })

          case list.is_empty(hooks.run_reload_routes_modified) {
            True -> watch_loop(current_mtimes, port, hooks)
            False -> {
              io.println("")
              io.println(console.warning("Running routes-modified hooks..."))
              case run_hooks.run(hooks.run_reload_routes_modified) {
                Ok(_) -> watch_loop(current_mtimes, port, hooks)
                Error(msg) -> {
                  io.println("")
                  io.println(console.error(msg))
                  watch_loop(current_mtimes, port, hooks)
                }
              }
            }
          }
        }
        False -> {
          // Other files changed - run reload-run hooks and restart
          case only_compiled_routes {
            True -> Nil
            False -> {
              io.println("")
              io.println(console.warning("File changes detected:"))
              list.each(files, fn(f) { io.println("  " <> f) })
            }
          }

          case list.is_empty(hooks.run_reload_default) {
            True -> Nil
            False -> {
              io.println("")
              io.println(console.warning("Running reload hooks..."))
              case run_hooks.run(hooks.run_reload_default) {
                Ok(_) -> Nil
                Error(msg) -> {
                  io.println(console.error(msg))
                }
              }
            }
          }

          io.println("")
          io.println(console.warning("Restarting application... ✨"))
          stop_port(port)
          let new_port = start_gleam_run()
          start_output_reader(new_port)
          watch_loop(current_mtimes, new_port, hooks)
        }
      }
    }
  }
}

// ------------------------------------------------------------- Private Types

/// Opaque type representing an Erlang port. Used to communicate
/// with the spawned gleam run process for starting, stopping,
/// and reading output.
///
type Port

// ------------------------------------------------------------- FFI Bindings

/// Starts a new gleam run process via Erlang port. Returns a
/// port handle that can be used for stopping the process and
/// reading its output.
///
@external(erlang, "glimr_port_ffi", "start_gleam_run")
fn start_gleam_run() -> Port

/// Stops a running gleam process by closing its port. Used
/// when restarting the application after file changes are
/// detected.
///
@external(erlang, "glimr_port_ffi", "stop_port")
fn stop_port(port: Port) -> Nil

/// Spawns a process to read and print port output. Ensures
/// output from the gleam run process is displayed in the
/// console.
///
@external(erlang, "glimr_port_ffi", "start_output_reader")
fn start_output_reader(port: Port) -> Nil

/// Collects modification times for all .gleam files in a
/// directory. Returns a Dict mapping file paths to their
/// mtime in seconds.
///
fn get_gleam_file_mtimes(dir: String) -> Dict(String, Int) {
  case simplifile.get_files(dir) {
    Ok(files) -> {
      files
      |> list.filter(fn(f) { string.ends_with(f, ".gleam") })
      |> list.filter_map(fn(f) {
        case get_mtime(f) {
          Ok(mtime) -> Ok(#(f, mtime))
          Error(_) -> Error(Nil)
        }
      })
      |> dict.from_list
    }
    Error(_) -> dict.new()
  }
}

/// Gets the modification time for a single file. Returns the
/// mtime in seconds or Error if the file info cannot be
/// retrieved.
///
fn get_mtime(path: String) -> Result(Int, Nil) {
  case simplifile.file_info(path) {
    Ok(info) -> Ok(info.mtime_seconds)
    Error(_) -> Error(Nil)
  }
}

/// Compares two mtime dictionaries to find changed files.
/// Returns a list of file paths that have different mtimes
/// or are new in the current snapshot.
///
fn find_changed_files(
  old: Dict(String, Int),
  new: Dict(String, Int),
) -> List(String) {
  new
  |> dict.to_list
  |> list.filter_map(fn(entry) {
    let #(path, new_mtime) = entry
    case dict.get(old, path) {
      Ok(old_mtime) if old_mtime == new_mtime -> Error(Nil)
      _ -> Ok(path)
    }
  })
}
