import glimr/console/command.{type Command, type Args}
import glimr/console/console

/// The name of the console command.
const name = "glimr:greet"

/// The console command description.
const description = "A simple hello from Glimr, to you"

/// Define the console command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.name(name)
  |> command.description(description)
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(_args: Args) -> Nil {
  console.output()
  |> console.line("Hello! We hope you're enjoying Glimr")
  |> console.print()
}
