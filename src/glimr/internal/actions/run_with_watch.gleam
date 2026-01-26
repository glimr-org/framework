//// Watch Runner
////
//// Runs the application with file watching for development.
//// Monitors source files for changes and triggers appropriate
//// hooks or restarts the application automatically.
////

import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/io
import gleam/list
import gleam/string
import glimr/console/console
import glimr/internal/actions/run_hooks
import glimr/internal/config.{type Hooks}
import glimr/internal/dev_proxy
import simplifile

/// Starts the application with file watching. Monitors the src
/// directory for changes and triggers hooks or restarts based
/// on which files changed.
///
pub fn run(hooks: Hooks) -> Nil {
  config.load_env()

  let app_port = config.app_port()
  let dev_proxy_port = config.dev_proxy_port()

  dev_proxy.start(app_port, dev_proxy_port)

  let initial_mtimes = get_watched_file_mtimes("src")
  let port = start_gleam_run()
  start_output_reader(port)
  watch_loop(initial_mtimes, port, hooks)
}

/// Main file watching loop. Polls for file changes every
/// second and triggers appropriate hooks or restarts based
/// on which files were modified.
///
fn watch_loop(last_mtimes: Dict(String, Int), port: Port, hooks: Hooks) -> Nil {
  process.sleep(1000)

  let current_mtimes = get_watched_file_mtimes("src")
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

      let loom_source_changed =
        list.any(files, fn(f) {
          string.contains(f, ".loom.html")
          || string.contains(f, "src/app/loom/")
        })

      let only_compiled_files =
        list.all(files, fn(f) {
          string.contains(f, "src/bootstrap/gen/routes/")
          || string.contains(f, "src/bootstrap/gen/loom/")
        })

      case routes_source_changed, loom_source_changed {
        True, _ -> {
          let route_files =
            list.filter(files, fn(f) {
              string.contains(f, "src/routes/")
              && !string.contains(f, "src/bootstrap/gen/routes/")
            })

          let controller_files =
            list.filter(files, fn(f) {
              string.contains(f, "src/app/http/controllers/")
            })

          let controller_changed = !list.is_empty(controller_files)

          io.println("")
          io.println(console.warning("File changes detected:"))
          list.each(route_files, fn(f) { io.println("  " <> f) })
          list.each(controller_files, fn(f) { io.println("  " <> f) })
          io.println("")

          // If controllers changed, recompile all routes
          let files_to_compile = case controller_changed {
            True -> get_all_route_files()
            False -> route_files
          }

          case list.is_empty(hooks.run_reload_route_modified) {
            True -> watch_loop(current_mtimes, port, hooks)
            False -> {
              case
                run_hooks.run_for_files(
                  hooks.run_reload_route_modified,
                  files_to_compile,
                )
              {
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
        _, True -> {
          let loom_files =
            list.filter(files, fn(f) {
              string.ends_with(f, ".loom.html")
              || string.contains(f, "src/app/loom/")
            })

          io.println("")
          io.println(console.warning("File changes detected:"))
          list.each(loom_files, fn(f) { io.println(f) })
          io.println("")

          case list.is_empty(hooks.run_reload_loom_modified) {
            True -> watch_loop(current_mtimes, port, hooks)
            False -> {
              case
                run_hooks.run_for_files(
                  hooks.run_reload_loom_modified,
                  loom_files,
                )
              {
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
        False, False -> {
          case only_compiled_files {
            True -> Nil
            False -> {
              io.println("")
              io.println(console.warning("File changes detected:"))
              list.each(files, fn(f) { io.println("  " <> f) })
              io.println("")
            }
          }

          case list.is_empty(hooks.run_reload_pre) {
            True -> Nil
            False -> {
              io.println("")
              io.println(console.warning("Running pre-reload hooks..."))
              case run_hooks.run(hooks.run_reload_pre) {
                Ok(_) -> Nil
                Error(msg) -> {
                  io.println(console.error(msg))
                }
              }
            }
          }

          case list.is_empty(hooks.run_reload_post_modified) {
            True -> Nil
            False -> {
              io.println("")
              io.println(console.warning("Running post-modified hooks..."))
              case run_hooks.run(hooks.run_reload_post_modified) {
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

/// Collects modification times for all watched files in a
/// directory. Watches .gleam and .loom.html files. Returns
/// a Dict mapping file paths to their mtime in seconds.
///
fn get_watched_file_mtimes(dir: String) -> Dict(String, Int) {
  case simplifile.get_files(dir) {
    Ok(files) -> {
      files
      |> list.filter(fn(f) {
        string.ends_with(f, ".gleam") || string.ends_with(f, ".loom.html")
      })
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

/// Gets all route files in the src/routes directory. Used
/// when controller files change to recompile all routes
/// since any route might reference the changed controller.
///
fn get_all_route_files() -> List(String) {
  case simplifile.get_files("src/routes") {
    Ok(files) -> list.filter(files, fn(f) { string.ends_with(f, ".gleam") })
    Error(_) -> []
  }
}

/// Compares two mtime dictionaries to find changed files.
/// Returns a list of file paths that have different mtimes,
/// are new in the current snapshot, or were deleted.
///
fn find_changed_files(
  old: Dict(String, Int),
  new: Dict(String, Int),
) -> List(String) {
  // Files that changed or are new
  let changed_or_new =
    new
    |> dict.to_list
    |> list.filter_map(fn(entry) {
      let #(path, new_mtime) = entry
      case dict.get(old, path) {
        Ok(old_mtime) if old_mtime == new_mtime -> Error(Nil)
        _ -> Ok(path)
      }
    })

  // Files that were deleted (in old but not in new)
  let deleted =
    old
    |> dict.keys
    |> list.filter(fn(path) { !dict.has_key(new, path) })

  list.append(changed_or_new, deleted)
}
