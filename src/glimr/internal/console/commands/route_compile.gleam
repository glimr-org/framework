import gleam/io
import glimr/console/command.{type Command, type ParsedArgs, Flag}
import glimr/console/console
import shellout

/// The name of the console command.
const name = "route:compile"

/// The console command description.
const description = "Compile controller routes to optimized pattern matching"

/// Define the console command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.name(name)
  |> command.description(description)
  |> command.args([
    Flag("verbose", "v", "Display information about compiled routes"),
  ])
  |> command.handler(run)
}

/// Execute the console command.
///
/// Note: This is handled by the CLI bash wrapper, so this 
/// code only runs if called programmatically from Gleam code.
///
fn run(args: ParsedArgs) -> Nil {
  let verbose = command.has_flag(args, "verbose")
  let cmd_args = case verbose {
    True -> ["route:compile", "-v"]
    False -> ["route:compile"]
  }

  case
    shellout.command("./glimr", cmd_args, in: ".", opt: [
      shellout.LetBeStdout,
      shellout.LetBeStderr,
    ])
  {
    Ok(_) -> Nil
    Error(#(_, msg)) -> io.println(console.error(msg))
  }
}
