//// Database Migration Utilities
////
//// Both PostgreSQL and SQLite adapters need the same migration
//// workflow — load files, diff against the applied set,
//// execute pending ones, and record the results. Centralising
//// that logic here avoids duplicating file-parsing and
//// tracking-table code in each driver adapter while still
//// branching on SQL dialect differences like TIMESTAMP vs
//// TEXT.

import gleam/dynamic/decode
import gleam/list
import gleam/result
import gleam/string
import glimr/db/db.{type Connection, type DbError}
import simplifile

// ------------------------------------------------------------- Public Types

/// Grouping version, name, and SQL into a single record lets
/// the loader return one sorted list that downstream functions
/// can filter and execute without re-reading files or
/// re-parsing filenames.
///
pub type Migration {
  Migration(version: String, name: String, sql: String)
}

// ------------------------------------------------------------- Public Functions

/// Scanning the migrations directory at startup avoids a
/// hard-coded migration registry that must be updated every
/// time a new file is added. Sorting by the version prefix
/// guarantees chronological execution order regardless of
/// filesystem listing order.
///
pub fn load_all_migrations(
  connection_name: String,
) -> Result(List(Migration), String) {
  let migrations_path = "src/data/" <> connection_name <> "/_migrations"

  let _ = simplifile.create_directory_all(migrations_path)

  case simplifile.read_directory(migrations_path) {
    Ok(files) -> {
      let migrations = {
        files
        |> list.filter(fn(f) { string.ends_with(f, ".sql") })
        |> list.filter_map(fn(file) {
          let path = migrations_path <> "/" <> file

          use content <- result.try(
            simplifile.read(path) |> result.replace_error(Nil),
          )

          let base = string.replace(file, ".sql", "")

          use #(version, name) <- result.try(
            string.split_once(base, "_") |> result.replace_error(Nil),
          )

          Ok(Migration(version, name, content))
        })
        |> list.sort(by: fn(a, b) { string.compare(a.version, b.version) })
      }

      Ok(migrations)
    }

    Error(_) -> Ok([])
  }
}

/// Re-running migrations must be idempotent, so filtering out
/// already-applied versions before execution prevents duplicate
/// DDL statements that would fail or corrupt the schema.
///
pub fn get_pending_migrations(
  all: List(Migration),
  applied: List(String),
) -> List(Migration) {
  list.filter(all, fn(m) { !list.contains(applied, m.version) })
}

/// Migration files contain header comments added by the
/// generator (driver tag, timestamp). Stripping them before
/// execution avoids sending comment-only lines to drivers that
/// might choke on leading `--` lines in multi-statement
/// strings.
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

/// The tracking table must exist before any migration can be
/// recorded. Using CREATE IF NOT EXISTS makes this safe to call
/// on every run. The applied_at column branches on driver
/// because PostgreSQL supports TIMESTAMP natively while SQLite
/// stores dates as TEXT.
///
pub fn ensure_table(conn: Connection) -> Result(Nil, DbError) {
  let applied_at_type = case db.connection_driver(conn) {
    db.Postgres -> "TIMESTAMP"
    db.Sqlite -> "TEXT"
  }

  db.exec_with(conn, "CREATE TABLE IF NOT EXISTS _glimr_migrations (
        version TEXT PRIMARY KEY,
        applied_at " <> applied_at_type <> " DEFAULT CURRENT_TIMESTAMP
      )", [])
  |> result.replace(Nil)
}

/// The pending-migration filter needs to know which versions
/// are already in the database. Returning them sorted keeps the
/// output deterministic for logging and debugging.
///
pub fn get_applied(conn: Connection) -> Result(List(String), DbError) {
  let decoder = {
    use version <- decode.field(0, decode.string)
    decode.success(version)
  }

  db.query_with(
    conn,
    "SELECT version FROM _glimr_migrations ORDER BY version",
    [],
    decoder,
  )
  |> result.map(fn(qr) { qr.rows })
}

/// Stopping on the first error leaves the database in a known
/// state — all migrations up to the failure are applied and
/// recorded, so re-running picks up exactly where it left off.
///
pub fn apply_pending(
  conn: Connection,
  pending: List(Migration),
) -> Result(List(String), DbError) {
  do_apply_pending(conn, pending, [])
}

// ------------------------------------------------------------- Private Functions

/// Separated from apply_pending so the public API hides the
/// accumulator. Processing one migration at a time and
/// prepending to the accumulator avoids repeated list appends,
/// with a final reverse to restore order.
///
fn do_apply_pending(
  conn: Connection,
  pending: List(Migration),
  applied: List(String),
) -> Result(List(String), DbError) {
  case pending {
    [] -> Ok(list.reverse(applied))
    [migration, ..rest] -> {
      case apply_single(conn, migration) {
        Ok(_) -> do_apply_pending(conn, rest, [migration.version, ..applied])
        Error(err) -> Error(err)
      }
    }
  }
}

/// Migration files may contain multiple statements (CREATE
/// TABLE followed by CREATE INDEX, etc.). Splitting on
/// semicolons and executing individually works around drivers
/// that reject multi-statement strings. Recording the version
/// only after all succeed keeps the tracking table accurate on
/// partial failures.
///
fn apply_single(conn: Connection, migration: Migration) -> Result(Nil, DbError) {
  let sql = extract_sql(migration.sql)

  let statements =
    sql
    |> string.split(";")
    |> list.map(string.trim)
    |> list.filter(fn(s) { s != "" })

  use _ <- result.try(execute_statements(conn, statements))

  db.exec_with(conn, "INSERT INTO _glimr_migrations (version) VALUES ($1)", [
    db.string(migration.version),
  ])
  |> result.replace(Nil)
}

/// Sequential execution with early return on error ensures
/// later statements that depend on earlier DDL (like an index
/// referencing a just-created table) never run against a
/// partially applied schema.
///
fn execute_statements(
  conn: Connection,
  statements: List(String),
) -> Result(Nil, DbError) {
  list.try_each(statements, fn(stmt) {
    db.exec_with(conn, stmt, []) |> result.replace(Nil)
  })
}
