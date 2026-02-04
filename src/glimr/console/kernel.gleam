//// Glimr Console Kernel
////
//// Registers Glimr's console commands and is the entry point
//// to run your app's custom console commands as well as
//// Glimr's default console commands.

import gleam/list
import glimr/cache/driver.{type CacheStore} as _cache_driver
import glimr/console/command.{type Command}
import glimr/console/console
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

// ------------------------------------------------------------- Public Functions

/// Returns the list of internal Glimr framework commands.
///
pub fn commands() -> List(Command) {
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
    setup_database.command(),
  ]
}

/// Entry point for running console commands. Merges internal
/// Glimr commands with user-defined commands into a single
/// list. Commands receive the user's context and can decode
/// GlimrContext from it if needed.
///
pub fn run(
  commands app_commands: List(Command),
  cache_stores cache_stores: List(CacheStore),
) {
  let commands = list.append(commands(), app_commands)
  command.store_commands(commands)

  let args = command.get_args()

  case args {
    [] -> command.print_help(commands)
    ["-V"] | ["--version"] -> command.print_glimr_version()
    [name, ..rest] -> {
      case command.find_and_run(commands, cache_stores, name, rest) {
        True -> Nil
        False -> {
          console.output()
          |> console.line_error("Command not found: " <> name)
          |> console.print()
        }
      }
    }
  }
}
