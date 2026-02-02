import gleam/io
import glimr/console/command.{type Command, type ParsedArgs, Flag}
import glimr/console/console
import shellout

/// The name of the console command.
const name = "loom:compile"

/// The console command description.
const description = "Compile loom templates to Gleam code"

/// Define the console command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.name(name)
  |> command.description(description)
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
/// Note: This is handled by the CLI bash wrapper, so this
/// code only runs if called programmatically from Gleam code.
///
fn run(args: ParsedArgs) -> Nil {
  let verbose = command.has_flag(args, "verbose")
  let cmd_args = case verbose {
    True -> ["loom:compile", "-v"]
    False -> ["loom:compile"]
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
