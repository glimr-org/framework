//// Glimr Console Kernel
////
//// Registers Glimr's console commands and is the entry point
//// to run your app's custom console commands as well as
//// Glimr's default console commands.
////
//// NOTE: Database commands (migrate, gen) are now in driver packages:
//// - glimr_sqlite/console/kernel for sqlite:migrate, sqlite:gen
//// - glimr_postgres/console/kernel for postgres:migrate, postgres:gen

import gleam/io
import gleam/list
import glimr/console/command.{type Command}
import glimr/console/console
import glimr/db/driver.{type Connection}
import glimr/internal/console/commands/greet
import glimr/internal/console/commands/make_action
import glimr/internal/console/commands/make_command
import glimr/internal/console/commands/make_controller
import glimr/internal/console/commands/make_middleware
import glimr/internal/console/commands/make_model
import glimr/internal/console/commands/make_request
import glimr/internal/console/commands/make_rule
import glimr/internal/console/commands/setup_database

// ------------------------------------------------------------- Public Functions

/// Returns the list of internal Glimr framework commands.
/// Commands are generic over ctx so they can be merged
/// with user commands into a single unified list.
///
/// NOTE: Database commands (migrate, gen) are now in driver packages.
/// Add them via glimr_sqlite/console/kernel or glimr_postgres/console/kernel.
///
pub fn commands(connections: List(Connection)) -> List(Command) {
  [
    greet.command(),
    make_action.command(),
    make_controller.command(),
    make_middleware.command(),
    make_request.command(),
    make_rule.command(),
    make_command.command(),
    make_model.command(),
    setup_database.command(connections),
  ]
}

/// Entry point for running console commands. Merges internal
/// Glimr commands with user-defined commands into a single
/// list. Commands receive the user's context and can decode
/// GlimrContext from it if needed.
///
pub fn run(
  commands app_commands: List(Command),
  db_connections db_connections: List(Connection),
) {
  let commands = list.append(commands(db_connections), app_commands)
  let args = command.get_args()

  case args {
    [] -> command.print_help(commands)
    ["-V"] | ["--version"] -> command.print_glimr_version()
    [name, ..rest] -> {
      case command.find_and_run(commands, db_connections, name, rest) {
        True -> Nil
        False -> io.println(console.error("Command not found: " <> name))
      }
    }
  }
}
