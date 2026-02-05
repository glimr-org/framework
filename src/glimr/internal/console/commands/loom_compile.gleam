import gleam/io
import gleam/string
import glimr/console/command.{type Args, type Command, Flag, Option}
import glimr/console/console
import glimr/internal/actions/compile_loom

/// The console command description.
const description = "Compile loom templates to Gleam code"

/// Define the console command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.description(description)
  |> command.args([
    Option("path", "Path to a specific loom file to compile", ""),
  ])
  |> command.args([
    Flag(
      "verbose",
      "v",
      "Display detailed information about compiled templates",
    ),
  ])
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(args: Args) -> Nil {
  let path = command.get_option(args, "path")
  let verbose = command.has_flag(args, "verbose")

  case path {
    "" -> {
      case compile_loom.run(verbose) {
        Ok(_) -> Nil
        Error(msg) -> {
          io.println(console.error(msg))
          io.println("")
          io.println(console.error("Build failed"))
          halt(1)
        }
      }
    }
    _ -> {
      let is_views_path = string.starts_with(path, "src/resources/views/")
      let is_app_loom_path = string.starts_with(path, "src/app/loom/")

      case is_views_path || is_app_loom_path {
        False -> {
          io.println(console.error(
            "Not a loom file: path must be in src/resources/views/ or src/app/loom/",
          ))
          io.println("")
          io.println(console.error("Build failed"))
          halt(1)
        }
        True -> {
          case compile_loom.run_path(path, verbose) {
            Ok(_) -> Nil
            Error(msg) -> {
              io.println(console.error(msg))
              io.println("")
              io.println(console.error("Build failed"))
              halt(1)
            }
          }
        }
      }
    }
  }
}

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> Nil

/// Console command's entry point
///
pub fn main() {
  command.run(command())
}
