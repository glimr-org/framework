import gleam/list
import gleam/string
import glimr/console/command.{type Args, type Command, Argument, Option}
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
    Option(
      name: "db-postgres",
      description: "Generate with PostgreSQL pool",
      default: "",
    ),
    Option(
      name: "db-sqlite",
      description: "Generate with SQLite pool",
      default: "",
    ),
    Option(
      name: "cache-redis",
      description: "Generate with Redis cache",
      default: "",
    ),
    Option(
      name: "cache-file",
      description: "Generate with file cache",
      default: "",
    ),
    Option(
      name: "cache-postgres",
      description: "Generate with PostgreSQL cache",
      default: "",
    ),
    Option(
      name: "cache-sqlite",
      description: "Generate with SQLite cache",
      default: "",
    ),
  ])
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(args: Args) -> Nil {
  let name = command.get_arg(args, "name")
  let module_name = string.lowercase(name)
  let file_path = "src/app/console/commands/" <> module_name <> ".gleam"

  // Collect all non-empty driver options
  let options = [
    #("db-postgres", "connection_name", "command_with_db_postgres.stub"),
    #("db-sqlite", "connection_name", "command_with_db_sqlite.stub"),
    #("cache-redis", "store_name", "command_with_cache_redis.stub"),
    #("cache-file", "store_name", "command_with_cache_file.stub"),
    #("cache-postgres", "store_name", "command_with_cache_postgres.stub"),
    #("cache-sqlite", "store_name", "command_with_cache_sqlite.stub"),
  ]

  let selected =
    list.filter_map(options, fn(opt) {
      let #(option_name, variable_name, stub_name) = opt
      let value = command.get_option(args, option_name)
      case value {
        "" -> Error(Nil)
        _ -> Ok(#(value, variable_name, stub_name))
      }
    })

  case selected {
    // Multiple options — error
    [_, _, ..] -> {
      console.output()
      |> console.line_error(
        "Error: Only one driver option is allowed at a time.",
      )
      |> console.print()
    }

    // No options — plain command
    [] -> write_stub(file_path, module_name, "command.stub", [])

    // One option — use the corresponding stub
    [#(value, variable_name, stub_name)] ->
      write_stub(file_path, module_name, stub_name, [
        #(variable_name, value),
      ])
  }
}

/// Write a stub file to the destination path if it doesn't
/// already exist.
///
fn write_stub(
  file_path: String,
  module_name: String,
  stub_name: String,
  extra_variables: List(#(String, String)),
) -> Nil {
  let assert Ok(file_exists) = filesystem.file_exists(file_path)

  case file_exists {
    True -> {
      console.output()
      |> console.line_error("Error: Command already exists!")
      |> console.line("[" <> file_path <> "]")
      |> console.print()
    }
    False -> {
      let variables = [#("command_name", module_name), ..extra_variables]

      let assert Ok(_) = {
        filesystem.write_from_stub_with_variables(
          package: "glimr",
          stub_path: "console/" <> stub_name,
          dest_path: file_path,
          variables: variables,
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
