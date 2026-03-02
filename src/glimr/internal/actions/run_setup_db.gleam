//// Database Setup
////
//// New database connections need a conventional directory
//// layout so that the migration runner, model generator, and
//// code-gen tools can all find files without per-project
//// configuration. This action scaffolds that layout in one
//// step so developers don't have to remember the structure.

import gleam/io
import glimr/console/console
import simplifile

// ------------------------------------------------------------- Public Functions

/// Scaffolds the directory tree for a named database
/// connection. Checking for an existing directory first
/// prevents accidentally overwriting a connection that was
/// already configured.
///
pub fn run(name: String, create_sqlite: Bool) -> Nil {
  let base_path = "src/database/" <> name

  // Check if directory already exists
  case simplifile.is_directory(base_path) {
    Ok(True) -> {
      console.output()
      |> console.line_error(
        "Database directory \"" <> base_path <> "\" already exists.",
      )
      |> console.print()
    }
    _ -> do_setup(base_path, create_sqlite)
  }
}

// ------------------------------------------------------------- Private Functions

/// Creates each subdirectory individually rather than in a
/// batch so that partial failures still produce as much of the
/// layout as possible and the user sees exactly which step
/// failed.
///
fn do_setup(base_path: String, create_sqlite: Bool) -> Nil {
  io.println("")

  // Create base directory
  case simplifile.create_directory_all(base_path) {
    Ok(_) -> io.println(console.warning("Created: ") <> base_path)
    Error(_) -> {
      io.println("  " <> console.error("Error: Could not create " <> base_path))
    }
  }

  // Create _migrations directory
  let migrations_path = base_path <> "/_migrations"
  case simplifile.create_directory_all(migrations_path) {
    Ok(_) -> io.println(console.warning("Created: ") <> migrations_path)
    Error(_) -> {
      io.println(
        "  " <> console.error("Error: Could not create " <> migrations_path),
      )
    }
  }

  // Create models directory
  let models_path = base_path <> "/models"
  case simplifile.create_directory_all(models_path) {
    Ok(_) -> io.println(console.warning("Created: ") <> models_path)
    Error(_) -> {
      io.println(
        "  " <> console.error("Error: Could not create " <> models_path),
      )
    }
  }

  // Create data.db file if --sqlite flag is provided
  case create_sqlite {
    True -> {
      let db_path = base_path <> "/data.db"
      case simplifile.write(db_path, "") {
        Ok(_) -> io.println(console.warning("Created: ") <> db_path)
        Error(_) -> {
          io.println(
            "  " <> console.error("Error: Could not create " <> db_path),
          )
        }
      }
    }
    False -> Nil
  }

  console.output()
  |> console.line_success("Database set up successfully!")
  |> console.print()
}
