import gleam/io
import glimr/console/console
import simplifile

/// Sets up the database directory structure for a named connection.
/// Creates base directory, _migrations, and models subdirectories.
/// Optionally creates a data.db file for SQLite databases.
///
pub fn run(name: String, create_sqlite: Bool) -> Nil {
  let base_path = "src/data/" <> name

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

/// Performs the actual directory and file creation. Creates
/// base directory, _migrations subdirectory, models subdirectory,
/// and optionally a data.db file for SQLite.
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
