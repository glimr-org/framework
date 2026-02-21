import gleam/string
import glimr/console/command.{type Args, type Command, Argument, Flag}
import glimr/console/console
import glimr/filesystem/filesystem

/// The console command description.
const description = "Create a new route file"

/// Define the Command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.description(description)
  |> command.args([
    Argument(name: "name", description: "The name of the route file"),
    Flag(
      name: "direct",
      short: "d",
      description: "Determines if this route file should not be in the compiled location",
    ),
  ])
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(args: Args) -> Nil {
  let name = command.get_arg(args, "name")
  let direct = command.has_flag(args, "direct")

  let module_name = string.lowercase(name)

  let file_path = case direct {
    True -> "src/routes/" <> module_name <> ".gleam"
    False -> "src/compiled/routes/" <> module_name <> ".gleam"
  }

  let assert Ok(file_exists) = filesystem.file_exists(file_path)

  case file_exists {
    True -> {
      console.output()
      |> console.line_error("Error: Route already exists!")
      |> console.line("[" <> file_path <> "]")
      |> console.print()
    }
    False -> {
      let assert Ok(_) =
        filesystem.write_from_stub("glimr", "route.stub", file_path)

      console.output()
      |> console.line_success("Route created successfully!")
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
