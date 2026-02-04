import gleam/string
import glimr/console/command.{type Command, type Args, Argument, Flag}
import glimr/console/console
import glimr/filesystem/filesystem

/// The name of the console command.
const name = "make:command"

/// The console command description.
const description = "Create a new command"

/// Define the Command and it's properties.
///
pub fn command() -> Command {
  command.new()
  |> command.name(name)
  |> command.description(description)
  |> command.args([
    Argument(name: "name", description: "The name of the command"),
    Flag(
      name: "sqlite",
      short: "s",
      description: "Generate a command with SQLite database access",
    ),
    Flag(
      name: "postgres",
      short: "p",
      description: "Generate a command with PostgreSQL database access",
    ),
  ])
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(args: Args) -> Nil {
  let name = command.get_arg(args, "name")
  let with_sqlite = command.has_flag(args, "sqlite")
  let with_postgres = command.has_flag(args, "postgres")

  let module_name = string.lowercase(name)
  let file_path = "src/app/console/commands/" <> module_name <> ".gleam"
  let hyphened_name = string.replace(name, each: "_", with: "-")

  let stub_name = case with_sqlite, with_postgres {
    True, True -> {
      console.output()
      |> console.line_error("Error: Cannot use both --sqlite and --postgres")
      |> console.print()
      ""
    }
    True, False -> "command_with_sqlite.stub"
    False, True -> "command_with_postgres.stub"
    False, False -> "command.stub"
  }

  case stub_name {
    "" -> Nil
    _ -> {
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
              stub_path: "console/" <> stub_name,
              dest_path: file_path,
              variables: [#("command_name", hyphened_name)],
            )
          }

          console.output()
          |> console.line_success("Command created successfully!")
          |> console.line("[" <> file_path <> "]")
          |> console.print()
        }
      }
    }
  }
}
