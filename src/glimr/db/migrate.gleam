//// Database Migration Utilities
////
//// Provides shared types and functions for database migrations.
//// Used by driver packages to load and filter migrations from
//// the filesystem.

import gleam/list
import gleam/string
import simplifile

// ------------------------------------------------------------- Public Types

/// Represents a database migration loaded from a SQL file.
/// Contains the version timestamp, descriptive name, and
/// the raw SQL content to execute.
///
pub type Migration {
  Migration(version: String, name: String, sql: String)
}

// ------------------------------------------------------------- Public Functions

/// Loads all migration files from the migrations directory.
/// Reads SQL files from src/data/{connection_name}/_migrations,
/// parses version and name from filenames, and returns sorted
/// by version.
///
pub fn load_all_migrations(
  connection_name: String,
) -> Result(List(Migration), String) {
  let migrations_path = "src/data/" <> connection_name <> "/_migrations"

  let _ = simplifile.create_directory_all(migrations_path)

  case simplifile.read_directory(migrations_path) {
    Ok(files) -> {
      let migrations =
        files
        |> list.filter(fn(f) { string.ends_with(f, ".sql") })
        |> list.filter_map(fn(file) {
          let path = migrations_path <> "/" <> file
          case simplifile.read(path) {
            Ok(content) -> {
              let base = string.replace(file, ".sql", "")
              case string.split_once(base, "_") {
                Ok(#(version, name)) -> Ok(Migration(version, name, content))
                Error(_) -> Error(Nil)
              }
            }
            Error(_) -> Error(Nil)
          }
        })
        |> list.sort(by: fn(a, b) { string.compare(a.version, b.version) })

      Ok(migrations)
    }
    Error(_) -> Ok([])
  }
}

/// Filters migrations to only those that haven't been applied.
/// Compares against a list of already-applied version strings
/// and returns only the pending migrations.
///
pub fn get_pending_migrations(
  all: List(Migration),
  applied: List(String),
) -> List(Migration) {
  list.filter(all, fn(m) { !list.contains(applied, m.version) })
}

/// Removes SQL comments from migration content. Strips lines
/// starting with -- to clean up the SQL before execution.
/// Returns the trimmed SQL content.
///
pub fn extract_sql(sql: String) -> String {
  sql
  |> string.split("\n")
  |> list.filter(fn(line) {
    let trimmed = string.trim(line)
    !string.starts_with(trimmed, "--")
  })
  |> string.join("\n")
  |> string.trim()
}
