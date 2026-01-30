//// Glimr Console Kernel
////
//// Registers Glimr's console commands and is the entry point
//// to run your app's custom console commands as well as
//// Glimr's default console commands.

import gleam/list
import glimr/cache/driver.{type CacheStore} as _cache_driver
import glimr/console/command.{type Command}
import glimr/console/console
import glimr/db/db
import glimr/db/driver.{type Connection}
import glimr/internal/console/commands/build
import glimr/internal/console/commands/glimr_greet
import glimr/internal/console/commands/loom_compile
import glimr/internal/console/commands/make_action
import glimr/internal/console/commands/make_command
import glimr/internal/console/commands/make_controller
import glimr/internal/console/commands/make_middleware
import glimr/internal/console/commands/make_model
import glimr/internal/console/commands/make_route_file
import glimr/internal/console/commands/make_rule
import glimr/internal/console/commands/make_validator
import glimr/internal/console/commands/route_compile
import glimr/internal/console/commands/run
import glimr/internal/console/commands/setup_database
import glimr/routing/router.{type RouteGroupConfig}

// ------------------------------------------------------------- Public Functions

/// Returns the list of internal Glimr framework commands.
/// Commands are generic over ctx so they can be merged
/// with user commands into a single unified list.
///
pub fn commands(connections: List(Connection)) -> List(Command) {
  [
    build.command(),
    run.command(),
    route_compile.command(),
    loom_compile.command(),
    glimr_greet.command(),
    make_action.command(),
    make_controller.command(),
    make_middleware.command(),
    make_validator.command(),
    make_rule.command(),
    make_command.command(),
    make_model.command(),
    make_route_file.command(),
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
  cache_stores cache_stores: List(CacheStore),
  route_groups _route_groups: List(RouteGroupConfig),
) {
  db.validate_connections(db_connections)

  let commands = list.append(commands(db_connections), app_commands)
  command.store_commands(commands)

  let args = command.get_args()

  case args {
    [] -> command.print_help(commands)
    ["-V"] | ["--version"] -> command.print_glimr_version()
    [name, ..rest] -> {
      case
        command.find_and_run(commands, db_connections, cache_stores, name, rest)
      {
        True -> Nil
        False -> {
          console.output()
          |> console.line_error("Command not found: " <> name)
          |> console.blank_line(1)
          |> console.print()
        }
      }
    }
  }
}
