import gleam/io
import glimr/console/command.{type Args, type Command, Flag}
import glimr/console/console
import glimr/internal/actions/compile_routes

/// The console command description.
const description = "Compile controller routes to optimized pattern matching"

/// Define the console command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.description(description)
  |> command.args([
    Flag("verbose", "v", "Display information about compiled routes"),
  ])
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(args: Args) -> Nil {
  let verbose = command.has_flag(args, "verbose")

  case compile_routes.run(verbose) {
    Ok(_) -> Nil
    Error(msg) -> {
      io.println(console.error(msg))
      io.println("")
      io.println(console.error("Build failed"))
      halt(1)
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
