//// ------------------------------------------------------------
//// Migration Runner
//// ------------------------------------------------------------
////
//// Manages database schema migrations for Glimr applications.
//// Migrations are SQL files stored in `src/data/_migrations/`
//// with timestamped filenames: 
//// (e.g., `20251217120000_create_users.sql`).
////
//// Tracks applied migrations in the `_glimr_migrations` table 
//// to ensure each migration runs only once.
////

import dot_env
import gleam/dynamic/decode
import gleam/int
import gleam/io
import gleam/list
import gleam/order
import gleam/string
import glimr/db/connection.{type Connection, type DbError}
import glimr/db/db
import glimr/db/pool
import glimr/db/query
import simplifile

// ------------------------------------------------------------- Public Types

/// ------------------------------------------------------------
/// Migration Type
/// ------------------------------------------------------------
///
/// Represents a database migration loaded from a SQL file.
/// The version is extracted from the filename prefix 
/// (timestamp), the name from the remainder of the filename, 
/// and sql contains the migration statements to execute.
///
pub type Migration {
  Migration(version: String, name: String, sql: String)
}

/// ------------------------------------------------------------
/// Migration Status Type
/// ------------------------------------------------------------
///
/// Represents the status of a migration - whether it has been
/// applied to the database or is still pending.
///
pub type MigrationStatus {
  MigrationStatus(version: String, name: String, applied: Bool)
}

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Migrate
/// ------------------------------------------------------------
///
/// Applies all pending migrations to the database. Creates the
/// migrations tracking table if it doesn't exist, then runs any
/// migrations that haven't been applied yet in version order.
///
/// Returns a list of version strings for successfully applied
/// migrations, or an error if any migration fails.
///
/// ------------------------------------------------------------
///
/// *Example:*
///
/// ```gleam
/// use conn <- pool.get_connection_or(pool)
/// case migrate.migrate(conn) {
///   Ok(applied) -> io.println("Applied " <> int.to_string(list.length(applied)))
///   Error(err) -> io.println("Migration failed")
/// }
/// ```
///
pub fn migrate(conn: Connection) -> Result(List(String), DbError) {
  // Ensure migrations table exists
  case ensure_migrations_table(conn) {
    Ok(_) -> {
      // Get applied migrations
      case get_applied_migrations(conn) {
        Ok(applied) -> {
          // Get pending migrations
          case get_pending_migrations(applied) {
            Ok(pending) -> {
              // Apply each pending migration
              apply_migrations(conn, pending, [])
            }
            Error(err) -> Error(err)
          }
        }
        Error(err) -> Error(err)
      }
    }
    Error(err) -> Error(err)
  }
}

/// ------------------------------------------------------------
/// Rollback
/// ------------------------------------------------------------
///
/// Removes the last N migrations from the tracking table. Note
/// that this only removes the migration record - it does not
/// reverse the SQL changes. Actual rollback SQL would need to
/// be stored separately or generated.
///
/// Returns a list of version strings for rolled-back migrations.
///
pub fn rollback(conn: Connection, steps: Int) -> Result(List(String), DbError) {
  case ensure_migrations_table(conn) {
    Ok(_) -> {
      case get_applied_migrations(conn) {
        Ok(applied) -> {
          let to_rollback =
            applied
            |> list.sort(by: fn(a, b) {
              case string.compare(a, b) {
                order.Gt -> order.Lt
                order.Lt -> order.Gt
                order.Eq -> order.Eq
              }
            })
            |> list.take(steps)

          rollback_migrations(conn, to_rollback, [])
        }
        Error(err) -> Error(err)
      }
    }
    Error(err) -> Error(err)
  }
}

/// ------------------------------------------------------------
/// Status
/// ------------------------------------------------------------
///
/// Returns the status of all migrations - both applied and
/// pending. Useful for displaying migration status in CLI tools
/// or admin interfaces.
///
pub fn status(conn: Connection) -> Result(List(MigrationStatus), DbError) {
  case ensure_migrations_table(conn) {
    Ok(_) -> {
      case get_applied_migrations(conn) {
        Ok(applied) -> {
          case load_all_migrations() {
            Ok(all) -> {
              let statuses =
                list.map(all, fn(m) {
                  MigrationStatus(
                    version: m.version,
                    name: m.name,
                    applied: list.contains(applied, m.version),
                  )
                })
              Ok(statuses)
            }
            Error(err) -> Error(err)
          }
        }
        Error(err) -> Error(err)
      }
    }
    Error(err) -> Error(err)
  }
}

/// ------------------------------------------------------------
/// Main
/// ------------------------------------------------------------
///
/// Entry point for the migration CLI. Run with:
/// `gleam run -m glimr/db/migrate`
///
pub fn main() {
  run_migrate()
}

/// ------------------------------------------------------------
/// Run Migrate
/// ------------------------------------------------------------
///
/// CLI command that applies all pending migrations. Loads
/// database configuration from environment variables, connects
/// to the database, and runs pending migrations.
///
/// Prints progress to stdout and exits cleanly.
///
pub fn run_migrate() {
  // Load .env file
  dot_env.load_default()

  io.println("Glimr Migration Runner")
  io.println("======================")

  // Build config from environment
  let config = db.load_config()

  case pool.start(config) {
    Ok(db_pool) -> {
      let result = pool.get_connection_or(db_pool, fn(conn) { migrate(conn) })
      case result {
        Ok(applied) -> {
          case applied {
            [] -> io.println("No pending migrations.")
            _ -> {
              io.println(
                "Applied "
                <> int.to_string(list.length(applied))
                <> " migration(s):",
              )
              list.each(applied, fn(version) { io.println("  ✓ " <> version) })
            }
          }
        }
        Error(err) -> {
          io.println("Migration failed: " <> string.inspect(err))
        }
      }
      pool.stop(db_pool)
    }
    Error(err) -> {
      io.println("Failed to connect to database: " <> string.inspect(err))
    }
  }
}

/// ------------------------------------------------------------
/// Run Fresh
/// ------------------------------------------------------------
///
/// CLI command that drops all tables and re-runs all migrations
/// from scratch. For SQLite, deletes the database file. For
/// PostgreSQL, drops all tables in the public schema.
///
/// Use this during development to reset the database to a clean
/// state. Do not use in production.
///
pub fn run_fresh() {
  // Load .env file
  dot_env.load_default()

  io.println("Glimr Database Fresh")
  io.println("====================")

  let config = db.load_config()

  // Drop the database
  let _ = case config {
    connection.SqliteConfig(path, _) -> {
      io.println("Dropping database: " <> path)
      simplifile.delete(path)
    }
    connection.PostgresConfig(_, _) -> {
      io.println("Dropping all tables...")
      case pool.start(config) {
        Ok(db_pool) -> {
          let _ =
            pool.get_connection_or(db_pool, fn(conn) { drop_all_tables(conn) })
          pool.stop(db_pool)
          Ok(Nil)
        }
        Error(_) -> Ok(Nil)
      }
    }
  }

  io.println("")

  // Run migrations
  case pool.start(config) {
    Ok(db_pool) -> {
      let result = pool.get_connection_or(db_pool, fn(conn) { migrate(conn) })
      case result {
        Ok(applied) -> {
          io.println(
            "Applied "
            <> int.to_string(list.length(applied))
            <> " migration(s):",
          )
          list.each(applied, fn(version) { io.println("  ✓ " <> version) })
        }
        Error(err) -> {
          io.println("Migration failed: " <> string.inspect(err))
        }
      }
      pool.stop(db_pool)
    }
    Error(err) -> {
      io.println("Failed to connect to database: " <> string.inspect(err))
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// ------------------------------------------------------------
/// Drop All Tables
/// ------------------------------------------------------------
///
/// Drops all tables in the PostgreSQL public schema. Used by
/// `run_fresh` to reset the database before re-running 
/// migrations.
///
fn drop_all_tables(conn: Connection) -> Result(Nil, DbError) {
  // For PostgreSQL, drop all tables in public schema
  let sql =
    "DO $$ DECLARE
      r RECORD;
    BEGIN
      FOR r IN (SELECT tablename FROM pg_tables WHERE schemaname = 'public') LOOP
        EXECUTE 'DROP TABLE IF EXISTS ' || quote_ident(r.tablename) || ' CASCADE';
      END LOOP;
    END $$;"
  case query.execute(conn, sql, []) {
    Ok(_) -> Ok(Nil)
    Error(err) -> Error(err)
  }
}

/// ------------------------------------------------------------
/// Ensure Migrations Table
/// ------------------------------------------------------------
///
/// Creates the `_glimr_migrations` table if it doesn't exist.
/// Uses driver-appropriate column types for version and 
/// timestamp.
///
fn ensure_migrations_table(conn: Connection) -> Result(Nil, DbError) {
  let sql = case connection.driver(conn) {
    connection.Postgres -> {
      "CREATE TABLE IF NOT EXISTS _glimr_migrations (
        version VARCHAR(255) PRIMARY KEY,
        applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )"
    }
    connection.Sqlite -> {
      "CREATE TABLE IF NOT EXISTS _glimr_migrations (
        version TEXT PRIMARY KEY,
        applied_at TEXT DEFAULT CURRENT_TIMESTAMP
      )"
    }
  }

  case query.execute(conn, sql, []) {
    Ok(_) -> Ok(Nil)
    Error(err) -> Error(err)
  }
}

/// ------------------------------------------------------------
/// Get Applied Migrations
/// ------------------------------------------------------------
///
/// Queries the `_glimr_migrations` table to get a list of all
/// migration versions that have already been applied, sorted
/// by version.
///
fn get_applied_migrations(conn: Connection) -> Result(List(String), DbError) {
  let sql = "SELECT version FROM _glimr_migrations ORDER BY version"
  let decoder = {
    use version <- decode.field(0, decode.string)
    decode.success(version)
  }

  query.select_all(conn, sql, [], decoder)
}

/// ------------------------------------------------------------
/// Get Pending Migrations
/// ------------------------------------------------------------
///
/// Filters the list of all migrations to find those that 
/// haven't been applied yet, sorted by version for correct 
/// ordering.
///
fn get_pending_migrations(
  applied: List(String),
) -> Result(List(Migration), DbError) {
  case load_all_migrations() {
    Ok(all) -> {
      let pending =
        list.filter(all, fn(m) { !list.contains(applied, m.version) })
        |> list.sort(by: fn(a, b) { string.compare(a.version, b.version) })
      Ok(pending)
    }
    Error(err) -> Error(err)
  }
}

/// ------------------------------------------------------------
/// Load All Migrations
/// ------------------------------------------------------------
///
/// Reads all SQL files from `src/data/_migrations/` and parses
/// them into Migration records. Creates the migrations 
/// directory if it doesn't exist. Filenames must follow the 
/// format `TIMESTAMP_name.sql`:
/// (e.g., `20251217120000_create_users.sql`).
///
fn load_all_migrations() -> Result(List(Migration), DbError) {
  let migrations_path = "src/data/_migrations"

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
              // Parse filename: 20251217120000_create_users.sql
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

/// ------------------------------------------------------------
/// Apply Migrations
/// ------------------------------------------------------------
///
/// Recursively applies a list of pending migrations. 
/// Accumulates successfully applied version strings and returns 
/// them when complete. Stops on the first error.
///
fn apply_migrations(
  conn: Connection,
  pending: List(Migration),
  applied: List(String),
) -> Result(List(String), DbError) {
  case pending {
    [] -> Ok(list.reverse(applied))
    [migration, ..rest] -> {
      case apply_single_migration(conn, migration) {
        Ok(_) -> apply_migrations(conn, rest, [migration.version, ..applied])
        Error(err) -> Error(err)
      }
    }
  }
}

/// ------------------------------------------------------------
/// Apply Single Migration
/// ------------------------------------------------------------
///
/// Executes a single migration's SQL statements and records it
/// in the migrations table. Splits SQL by semicolons and 
/// executes each statement individually.
///
fn apply_single_migration(
  conn: Connection,
  migration: Migration,
) -> Result(Nil, DbError) {
  let sql = extract_sql_for_driver(migration.sql, conn)

  // Split SQL into individual statements and execute each
  let statements =
    sql
    |> string.split(";")
    |> list.map(string.trim)
    |> list.filter(fn(s) { s != "" })

  case execute_statements(conn, statements) {
    Ok(_) -> {
      let record_sql = "INSERT INTO _glimr_migrations (version) VALUES ($1)"
      case
        query.execute(conn, record_sql, [connection.string(migration.version)])
      {
        Ok(_) -> Ok(Nil)
        Error(err) -> Error(err)
      }
    }
    Error(err) -> Error(err)
  }
}

/// ------------------------------------------------------------
/// Execute Statements
/// ------------------------------------------------------------
///
/// Recursively executes a list of SQL statements. Stops on the
/// first error, returning that error. Used to run multi-
/// statement migrations where each statement is separated by 
/// semicolons.
///
fn execute_statements(
  conn: Connection,
  statements: List(String),
) -> Result(Nil, DbError) {
  case statements {
    [] -> Ok(Nil)
    [stmt, ..rest] -> {
      case query.execute(conn, stmt, []) {
        Ok(_) -> execute_statements(conn, rest)
        Error(err) -> Error(err)
      }
    }
  }
}

/// ------------------------------------------------------------
/// Rollback Migrations
/// ------------------------------------------------------------
///
/// Recursively removes migration records from the tracking 
/// table. Accumulates successfully rolled-back version strings 
/// and returns them when complete.
///
fn rollback_migrations(
  conn: Connection,
  versions: List(String),
  rolled_back: List(String),
) -> Result(List(String), DbError) {
  case versions {
    [] -> Ok(list.reverse(rolled_back))
    [version, ..rest] -> {
      case rollback_single_migration(conn, version) {
        Ok(_) -> rollback_migrations(conn, rest, [version, ..rolled_back])
        Error(err) -> Error(err)
      }
    }
  }
}

/// ------------------------------------------------------------
/// Rollback Single Migration
/// ------------------------------------------------------------
///
/// Removes a single migration from the tracking table. Note 
/// that this only removes the record - it does not reverse the 
/// SQL changes made by the migration.
///
fn rollback_single_migration(
  conn: Connection,
  version: String,
) -> Result(Nil, DbError) {
  let sql = "DELETE FROM _glimr_migrations WHERE version = $1"
  case query.execute(conn, sql, [connection.string(version)]) {
    Ok(_) -> Ok(Nil)
    Error(err) -> Error(err)
  }
}

/// ------------------------------------------------------------
/// Extract SQL for Driver
/// ------------------------------------------------------------
///
/// Filters out comment lines from migration SQL. Migration 
/// files are driver-specific, so this just removes lines 
/// starting with `--` to clean up the SQL before execution.
///
fn extract_sql_for_driver(sql: String, _conn: Connection) -> String {
  sql
  |> string.split("\n")
  |> list.filter(fn(line) {
    let trimmed = string.trim(line)
    !string.starts_with(trimmed, "--")
  })
  |> string.join("\n")
  |> string.trim()
}
