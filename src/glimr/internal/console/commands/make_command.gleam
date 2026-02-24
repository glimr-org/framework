import gleam/string
import glimr/console/command.{type Args, type Command, Argument, Flag}
import glimr/console/console
import glimr/filesystem/filesystem

/// The console command description.
const description = "Create a new command"

/// Define the Command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.description(description)
  |> command.args([
    Argument(name: "name", description: "The name of the command"),
    Flag(
      name: "db",
      short: "d",
      description: "Generate a command with database access",
    ),
  ])
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(args: Args) -> Nil {
  let name = command.get_arg(args, "name")
  let with_db = command.has_flag(args, "db")

  let module_name = string.lowercase(name)
  let file_path = "src/app/console/commands/" <> module_name <> ".gleam"

  let stub_name = case with_db {
    True -> "command_with_db.stub"
    False -> "command.stub"
  }

  let assert Ok(file_exists) = filesystem.file_exists(file_path)

  case file_exists {
    True -> {
      console.output()
      |> console.line_error("Error: Command already exists!")
      |> console.line("[" <> file_path <> "]")
      |> console.print()
    }
    False -> {
      let assert Ok(_) = {
        filesystem.write_from_stub_with_variables(
          package: "glimr",
          stub_path: "console/" <> stub_name,
          dest_path: file_path,
          variables: [#("command_name", module_name)],
        )
      }

      console.output()
      |> console.line_success("Command created successfully!")
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
