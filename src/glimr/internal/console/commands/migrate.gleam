import glimr/console/command.{type Args, type Command, Flag}
import glimr/db/db.{type DbPool}
import glimr/internal/actions/run_fresh
import glimr/internal/actions/run_migrate

/// The console command description.
const description = "Run pending database migrations"

/// Creates the migrate command.
///
pub fn command() -> Command {
  command.new()
  |> command.description(description)
  |> command.args([
    Flag(
      name: "fresh",
      short: "f",
      description: "Drop all tables and re-run all migrations",
    ),
    Flag(
      name: "status",
      short: "s",
      description: "Show migration status without running",
    ),
  ])
  |> command.db_handler(run)
}

/// Executes the migrate command.
///
fn run(args: Args, pool: DbPool) -> Nil {
  let database = command.get_option(args, "database")
  let fresh = command.has_flag(args, "fresh")
  let status = command.has_flag(args, "status")

  case status {
    True -> run_migrate.show_status(pool, database)
    False -> {
      case fresh {
        True -> run_fresh.run(pool, database)
        False -> run_migrate.run(pool, database)
      }
    }
  }
}

/// Console command's entry point
///
pub fn main() {
  command.run(command())
}
