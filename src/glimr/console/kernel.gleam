//// Console Kernel
////
//// Entry point for CLI execution so application code only needs
//// to call run() with custom commands. Handles argument parsing,
//// command dispatch, and merging framework commands with app
//// commands into a unified interface.
////

import gleam/list
import glimr/console/command.{type Command}
import glimr/console/console
import glimr/internal/console/commands/build
import glimr/internal/console/commands/command_compile
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

/// Provides built-in commands so apps get build, run, make:*,
/// etc. out of the box. Returned as a list so apps can filter
/// or extend before passing to run().
///
pub fn commands() -> List(Command) {
  [
    build.command(),
    run.command(),
    route_compile.command(),
    loom_compile.command(),
    command_compile.command(),
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

/// Single entry point keeps app console setup to one function
/// call. Framework commands come first so apps can override
/// them by registering commands with the same name.
///
pub fn run(commands app_commands: List(Command)) {
  let commands = list.append(commands(), app_commands)
  command.store_commands(commands)

  let args = command.get_args()

  case args {
    [] -> command.print_help(commands)
    ["-V"] | ["--version"] -> command.print_glimr_version()
    [name, ..] -> {
      case command.find_and_run(commands, name) {
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
