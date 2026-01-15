import gleam/io
import glimr/console/command.{type Command, type ParsedArgs}
import glimr/console/console
import glimr/internal/actions/compile_routes

/// The name of the console command.
const name = "route:compile"

/// The console command description.
const description = "Compile route definitions to optimized pattern matching"

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
fn run(_args: ParsedArgs) -> Nil {
  case compile_routes.run() {
    Ok(_) -> Nil
    Error(msg) -> {
      io.println(console.error(msg))
    }
  }
}
