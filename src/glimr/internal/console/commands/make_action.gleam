import gleam/string
import glimr/console/command.{type Command, type Args, Argument}
import glimr/console/console
import glimr/filesystem/filesystem

/// The name of the console command.
const name = "make:action"

/// The console command description.
const description = "Create a new action"

/// Define the Command and it's properties.
///
pub fn command() -> Command {
  command.new()
  |> command.name(name)
  |> command.description(description)
  |> command.args([
    Argument(name: "name", description: "The name of the action"),
  ])
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(args: Args) -> Nil {
  let name = command.get_arg(args, "name")

  let module_name = string.lowercase(name)
  let file_path = "src/app/actions/" <> module_name <> ".gleam"

  let assert Ok(file_exists) = filesystem.file_exists(file_path)

  case file_exists {
    True -> {
      console.output()
      |> console.line_error("Error: Action already exists!")
      |> console.line("[" <> file_path <> "]")
      |> console.print()
    }
    False -> {
      let assert Ok(_) = filesystem.write_from_stub("action.stub", file_path)

      console.output()
      |> console.line_success("Action created successfully!")
      |> console.line("[" <> file_path <> "]")
      |> console.print()
    }
  }
}
