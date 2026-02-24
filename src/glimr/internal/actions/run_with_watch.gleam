//// Watch Runner
////
//// Hot-reloading during development requires detecting file
//// changes and deciding what to do — some changes (routes,
//// loom templates, commands) can be recompiled incrementally
//// without restarting the app, while others need a full
//// restart. Polling mtimes every second is simple, portable,
//// and avoids platform-specific filesystem notification APIs.

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

/// Wraps the Erlang port handle so Gleam's type system prevents
/// accidentally passing other values to the FFI functions that
/// expect a port.
///
type Port

// ------------------------------------------------------------- Public Functions

/// Starts the dev proxy before the app so that incoming
/// requests are buffered during restarts instead of failing
/// with connection refused.
///
pub fn run(cfg: Config) -> Nil {
  let app_port = config.app_port()
  let dev_proxy_port = config.dev_proxy_port()

  dev_proxy.start(app_port, dev_proxy_port)

  let initial_mtimes = get_watched_file_mtimes("src")
  let port = start_gleam_run()
  start_output_reader(port)
  watch_loop(initial_mtimes, port, cfg, False)
}

// ------------------------------------------------------------- Private Functions

/// Priority order matters: route/loom/command changes are
/// handled with targeted recompilation (cheaper than a full
/// restart), while other changes trigger a restart. The
/// had_compile_error flag prevents compiled output from
/// triggering a spurious restart on the next tick.
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

/// Spawning via Erlang port gives us a handle to stop the
/// process later — unlike os:cmd which blocks and offers no
/// way to terminate the child.
///
@external(erlang, "glimr_port_ffi", "start_gleam_run")
fn start_gleam_run() -> Port

/// Closing the port sends SIGHUP to the child process, which is
/// how we cleanly shut down the old app instance before
/// starting a new one.
///
@external(erlang, "glimr_port_ffi", "stop_port")
fn stop_port(port: Port) -> Nil

/// Reading port output in a separate process prevents the watch
/// loop from blocking on I/O — the app's stdout appears
/// immediately in the terminal.
///
@external(erlang, "glimr_port_ffi", "start_output_reader")
fn start_output_reader(port: Port) -> Nil

/// Filtering to .gleam and .loom.html avoids reacting to editor
/// swap files, .beam outputs, or other transient files that
/// shouldn't trigger recompilation.
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

/// Wraps simplifile.file_info to expose only the mtime field,
/// keeping the watch loop code focused on time comparison
/// rather than file metadata details.
///
fn get_mtime(path: String) -> Result(Int, Nil) {
  case simplifile.file_info(path) {
    Ok(info) -> Ok(info.mtime_seconds)
    Error(_) -> Error(Nil)
  }
}

/// Detecting deletions alongside modifications means the watch
/// loop can clean up generated files for removed templates in
/// the same tick that notices the deletion.
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
