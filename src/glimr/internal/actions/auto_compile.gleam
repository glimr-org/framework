//// Auto-Compile Action
////
//// Checks each auto_compile / auto_gen config flag and runs
//// the corresponding compile action. Called by build and run
//// before their respective hooks so that compilation happens
//// out-of-the-box without manual hook configuration.

import gleam/list
import gleam/result
import glimr/internal/actions/compile_commands
import glimr/internal/actions/compile_database
import glimr/internal/actions/compile_loom
import glimr/internal/actions/compile_routes
import glimr/internal/config.{type Config}
import simplifile

// ------------------------------------------------------------- Public Functions

/// Routes, templates, commands, and database models all need to
/// be compiled before the app starts. Running them here means
/// developers don't have to remember to run each one manually —
/// save a file, and the next build/run picks everything up.
/// Stops on the first error so you fix one thing at a time
/// instead of scrolling through a wall of failures.
///
pub fn run(cfg: Config) -> Result(Nil, String) {
  use _ <- result.try(case cfg.routes.auto_compile {
    True -> compile_routes.run(False)
    False -> Ok(Nil)
  })

  use _ <- result.try(case cfg.loom.auto_compile {
    True -> compile_loom.run(False)
    False -> Ok(Nil)
  })

  use _ <- result.try(case cfg.commands.auto_compile {
    True -> compile_commands.run(False)
    False -> Ok(Nil)
  })

  case cfg.database.auto_gen {
    True -> compile_database_all()
    False -> Ok(Nil)
  }
}

// ------------------------------------------------------------ Private Functions

/// A project can have multiple database connections (e.g.
/// "primary" and "analytics"). Rather than requiring the
/// developer to list them, we scan src/database/ and regenerate
/// each one — so adding a new connection just means creating
/// the directory.
///
fn compile_database_all() -> Result(Nil, String) {
  case simplifile.read_directory("src/database") {
    Ok(entries) -> {
      let connections =
        list.filter(entries, fn(entry) {
          case simplifile.is_directory("src/database/" <> entry) {
            Ok(True) -> True
            _ -> False
          }
        })
      list.try_each(connections, fn(name) { compile_database.run(name) })
    }
    Error(_) -> Ok(Nil)
  }
}
