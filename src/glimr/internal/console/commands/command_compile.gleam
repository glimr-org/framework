import gleam/io
import glimr/console/command.{type Args, type Command, Flag}
import glimr/console/console
import glimr/internal/actions/compile_commands

/// The name of the console command.
const name = "command:compile"

/// The console command description.
const description = "Compile command registry from configured packages"

/// Define the console command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.name(name)
  |> command.description(description)
  |> command.args([
    Flag("verbose", "v", "Display information about compiled commands"),
  ])
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(args: Args) -> Nil {
  let verbose = command.has_flag(args, "verbose")

  case compile_commands.run(verbose) {
    Ok(_) -> Nil
    Error(msg) -> io.println(console.error(msg))
  }
}

/// Console command's entry point
///
pub fn main() {
  command.run(command())
}
