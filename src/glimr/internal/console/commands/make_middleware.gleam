import gleam/string
import glimr/console/command.{type Args, type Command, Argument}
import glimr/console/console
import glimr/filesystem/filesystem

/// The console command description.
const description = "Create a new HTTP middleware"

/// Define the Command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.description(description)
  |> command.args([
    Argument(name: "name", description: "The name of the middleware"),
  ])
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(args: Args) -> Nil {
  let name = command.get_arg(args, "name")

  let module_name = string.lowercase(name)
  let file_path = "src/app/http/middleware/" <> module_name <> ".gleam"

  let assert Ok(file_exists) = filesystem.file_exists(file_path)

  case file_exists {
    True -> {
      console.output()
      |> console.line_error("Error: Middleware already exists!")
      |> console.line("[" <> file_path <> "]")
      |> console.print()
    }
    False -> {
      let assert Ok(_) = {
        filesystem.write_from_stub("http/middleware.stub", file_path)
      }

      console.output()
      |> console.line_success("Middleware created successfully!")
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
