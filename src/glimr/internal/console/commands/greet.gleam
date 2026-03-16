import glimr/console/command.{type Args, type Command}
import glimr/console/console

/// The console command description.
const description = "A simple hello from Glimr, to you"

/// Define the console command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.description(description)
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(_args: Args) -> Nil {
  console.line("Hello! We hope you're enjoying Glimr")
}

/// Console command's entry point
///
pub fn main() {
  command.run(command())
}
