import gleam/string
import glimr/console/command.{type Args, type Command, Argument, Flag}
import glimr/console/console
import glimr/filesystem/filesystem

/// The console command description.
const description = "Create a new controller"

/// Define the Command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.description(description)
  |> command.args([
    Argument(name: "name", description: "The name of the controller"),
    Flag(
      name: "resource",
      short: "r",
      description: "Generate a resource controller",
    ),
  ])
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(args: Args) -> Nil {
  let name = command.get_arg(args, "name")
  let resource = command.has_flag(args, "resource")

  let module_name = string.lowercase(name)
  let file_path = "src/app/http/controllers/" <> module_name <> ".gleam"

  let stub_name = case resource {
    True -> "controller_resource.stub"
    False -> "controller.stub"
  }

  let assert Ok(file_exists) = filesystem.file_exists(file_path)

  case file_exists {
    True -> {
      console.output()
      |> console.line_error("Error: Controller already exists!")
      |> console.line("[" <> file_path <> "]")
      |> console.print()
    }
    False -> {
      let assert Ok(_) = {
        filesystem.write_from_stub("glimr", "http/" <> stub_name, file_path)
      }

      console.output()
      |> console.line_success("Controller created successfully!")
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
