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
import glimr/internal/actions/compile_commands
import glimr/internal/actions/compile_loom
import glimr/internal/actions/compile_routes
import glimr/internal/actions/run_hooks
import glimr/internal/config.{type Config}
import glimr/internal/dev_proxy
import simplifile

// ------------------------------------------------------------- Private Types

/// Opaque type representing an Erlang port. Used to communicate
/// with the spawned gleam run process for starting, stopping,
/// and reading output.
///
type Port

// ------------------------------------------------------------- Public Functions

/// Starts the application with file watching. Monitors the src
/// directory for changes and triggers hooks or restarts based
/// on which files changed.
///
pub fn run(cfg: Config) -> Nil {
  config.load_env()

  let app_port = config.app_port()
  let dev_proxy_port = config.dev_proxy_port()

  dev_proxy.start(app_port, dev_proxy_port)

  let initial_mtimes = get_watched_file_mtimes("src")
  let port = start_gleam_run()
  start_output_reader(port)
  watch_loop(initial_mtimes, port, cfg, False)
}

// ------------------------------------------------------------- Private Functions

/// Main file watching loop. Polls for file changes every
/// second and triggers appropriate hooks or restarts based
/// on which files were modified. The had_compile_error flag
/// tracks if the previous iteration had a compilation failure.
///
fn watch_loop(
  last_mtimes: Dict(String, Int),
  port: Port,
  cfg: Config,
  had_compile_error: Bool,
) -> Nil {
  process.sleep(1000)

  let current_mtimes = get_watched_file_mtimes("src")
  let changed_files = find_changed_files(last_mtimes, current_mtimes)

  case changed_files {
    [] -> watch_loop(current_mtimes, port, cfg, False)
    files -> {
      let controller_changed =
        list.any(files, fn(f) {
          string.contains(f, "src/app/http/controllers/")
        })

      let validator_changed =
        list.any(files, fn(f) {
          string.contains(f, "src/app/http/validators/")
          && string.ends_with(f, ".gleam")
        })

      let middleware_changed =
        list.any(files, fn(f) {
          string.contains(f, "src/app/http/middleware/")
          && string.ends_with(f, ".gleam")
        })

      let loom_source_changed =
        list.any(files, fn(f) {
          string.ends_with(f, ".loom.html")
          || string.contains(f, "src/app/loom/")
        })

      let command_changed =
        list.any(files, fn(f) {
          string.contains(f, "src/app/console/commands/")
          && string.ends_with(f, ".gleam")
        })

      let only_compiled_files =
        list.all(files, fn(f) { string.contains(f, "src/compiled/") })

      let routes_trigger =
        controller_changed || validator_changed || middleware_changed

      case routes_trigger, loom_source_changed, command_changed {
        True, _, _ -> {
          case cfg.routes.auto_compile {
            True -> {
              let route_related_files =
                list.filter(files, fn(f) {
                  string.contains(f, "src/app/http/controllers/")
                  || string.contains(f, "src/app/http/validators/")
                  || string.contains(f, "src/app/http/middleware/")
                })

              io.println("")
              io.println(console.warning("Route-related changes detected:"))
              list.each(route_related_files, fn(f) { io.println("  " <> f) })
              io.println("")

              // Recompile all routes when controllers or validators change
              case compile_routes.run(False) {
                Ok(_) -> watch_loop(current_mtimes, port, cfg, False)
                Error(msg) -> {
                  io.println(console.error(msg))
                  watch_loop(current_mtimes, port, cfg, True)
                }
              }
            }
            False -> watch_loop(current_mtimes, port, cfg, False)
          }
        }
        _, True, _ -> {
          case cfg.loom.auto_compile {
            True -> {
              let loom_files =
                list.filter(files, fn(f) {
                  string.ends_with(f, ".loom.html")
                  || string.contains(f, "src/app/loom/")
                })

              io.println("")
              io.println(console.warning("Loom changes detected:"))
              list.each(loom_files, fn(f) { io.println("  " <> f) })
              io.println("")

              // Compile changed loom files
              let loom_had_error =
                list.any(loom_files, fn(path) {
                  case compile_loom.run_path(path, False) {
                    Ok(_) -> False
                    Error(msg) -> {
                      io.println(console.error(msg))
                      True
                    }
                  }
                })

              watch_loop(current_mtimes, port, cfg, loom_had_error)
            }
            False -> watch_loop(current_mtimes, port, cfg, False)
          }
        }
        _, _, True -> {
          case cfg.commands.auto_compile {
            True -> {
              let command_files =
                list.filter(files, fn(f) {
                  string.contains(f, "src/app/console/commands/")
                  && string.ends_with(f, ".gleam")
                })

              io.println("")
              io.println(console.warning("Command changes detected:"))
              list.each(command_files, fn(f) { io.println("  " <> f) })
              io.println("")

              // Regenerate command registry
              let cmd_had_error = case compile_commands.run(False) {
                Ok(_) -> False
                Error(msg) -> {
                  io.println(console.error(msg))
                  True
                }
              }
              watch_loop(current_mtimes, port, cfg, cmd_had_error)
            }
            False -> watch_loop(current_mtimes, port, cfg, False)
          }
        }
        False, False, False -> {
          // Skip restart if only compiled files changed after a compile error
          case only_compiled_files && had_compile_error {
            True -> watch_loop(current_mtimes, port, cfg, False)
            False -> {
              // Don't print "File changes detected" for compiled files
              case only_compiled_files {
                True -> Nil
                False -> {
                  io.println("")
                  io.println(console.warning("File changes detected:"))
                  list.each(files, fn(f) { io.println("  " <> f) })
                  io.println("")
                }
              }

              case list.is_empty(cfg.hooks.run_reload_pre) {
                True -> Nil
                False -> {
                  io.println("")
                  io.println(console.warning("Running pre-reload hooks..."))
                  case run_hooks.run(cfg.hooks.run_reload_pre) {
                    Ok(_) -> Nil
                    Error(msg) -> {
                      io.println(console.error(msg))
                    }
                  }
                }
              }

              case list.is_empty(cfg.hooks.run_reload_post_modified) {
                True -> Nil
                False -> {
                  io.println("")
                  io.println(console.warning("Running post-modified hooks..."))
                  case run_hooks.run(cfg.hooks.run_reload_post_modified) {
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
              watch_loop(current_mtimes, new_port, cfg, False)
            }
          }
        }
      }
    }
  }
}

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
