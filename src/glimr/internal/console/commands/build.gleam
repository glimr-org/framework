import gleam/io
import gleam/list
import glimr/console/command.{type Command, type Args}
import glimr/console/console
import glimr/internal/actions/run_build
import glimr/internal/actions/run_hooks
import glimr/internal/config

/// The name of the console command.
const name = "build"

/// The console command description.
const description = "Build the application"

/// Define the console command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.name(name)
  |> command.description(description)
  |> command.handler(fn(args) { run(args) })
}

/// Execute the console command.
///
fn run(_args: Args) -> Nil {
  let cfg = config.load()

  case run_hooks.run(cfg.hooks.build_pre) {
    Ok(_) -> {
      case run_build.run() {
        Ok(_) -> {
          case list.is_empty(cfg.hooks.build_post) {
            True -> Nil
            False -> {
              case run_hooks.run(cfg.hooks.build_post) {
                Ok(_) -> Nil
                Error(msg) -> {
                  io.println(console.error(msg))
                  halt(1)
                }
              }
            }
          }
        }
        Error(_) -> halt(1)
      }
    }
    Error(msg) -> {
      io.println(console.error(msg))
      halt(1)
    }
  }
}

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> Nil
