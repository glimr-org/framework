import glimr/config/database
import glimr/console/command.{type Args, type Command, Argument, Flag}
import glimr/console/console
import glimr/db/driver
import glimr/internal/actions/run_setup_db

/// The console command description.
const description = "Set up a new database directory in src/database"

/// Define the console command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.description(description)
  |> command.args([
    Argument(name: "name", description: "Database connection name"),
    Flag(
      name: "sqlite",
      short: "s",
      description: "Creates a data.db file to be used as the sqlite db",
    ),
  ])
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(args: Args) -> Nil {
  let name = command.get_arg(args, "name")
  let create_sqlite = command.has_flag(args, "sqlite")
  let connections = database.load()

  // Validate that the connection exists in config
  case driver.get_connection_safe(connections, name) {
    Error(_) -> {
      console.output()
      |> console.line_error(
        "Database connection \"" <> name <> "\" does not exist in your config.",
      )
      |> console.print()
    }
    Ok(_) -> run_setup_db.run(name, create_sqlite)
  }
}

/// Console command's entry point
///
pub fn main() {
  command.run(command())
}
