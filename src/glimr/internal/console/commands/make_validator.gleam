import gleam/string
import glimr/console/command.{type Args, type Command, Argument}
import glimr/console/console
import glimr/filesystem/filesystem

/// The console command description.
const description = "Create a new form validator"

/// Define the Command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.description(description)
  |> command.args([
    Argument(name: "name", description: "The name of the form validator"),
  ])
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(args: Args) -> Nil {
  let name = command.get_arg(args, "name")

  let module_name = string.lowercase(name)
  let file_path = "src/app/http/validators/" <> module_name <> ".gleam"

  let assert Ok(file_exists) = filesystem.file_exists(file_path)

  case file_exists {
    True -> {
      console.output()
      |> console.line_error("Error: Validator already exists!")
      |> console.line("[" <> file_path <> "]")
      |> console.print()
    }
    False -> {
      let assert Ok(_) = {
        filesystem.write_from_stub("http/validator.stub", file_path)
      }

      console.output()
      |> console.line_success("Validator created successfully!")
      |> console.line("[" <> file_path <> "]")
      |> console.print()
    }
  }
}

/// Console command's entry point
///
pub fn main() {
  command.run(command())
}
