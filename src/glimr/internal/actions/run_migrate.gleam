//// Migration Runner
////
//// Orchestrates the full migration lifecycle — ensuring the
//// tracking table exists, loading files from disk, diffing
//// against already-applied versions, and running whatever is
//// pending. Centralising this here means the CLI, the fresh
//// runner, and any future callers all share the same
//// setup-and-apply sequence.

import gleam/int
import gleam/list
import gleam/result
import gleam/string
import glimr/console/console
import glimr/db/db.{type DbPool}
import glimr/db/migrate

// ------------------------------------------------------------- Public Functions

/// Chains the setup steps with result.try so a failure at any
/// point (missing table, unreadable files, bad SQL) stops early
/// with a descriptive error rather than cascading into
/// confusing downstream failures.
///
pub fn run(pool: DbPool, database: String) -> Nil {
  use conn <- db.get_connection(pool)

  let setup = {
    use _ <- result.try(
      migrate.ensure_table(conn)
      |> result.map_error(fn(e) {
        #("Failed to create migrations table:", string.inspect(e))
      }),
    )
    use applied <- result.try(
      migrate.get_applied(conn)
      |> result.map_error(fn(e) {
        #("Failed to get applied migrations:", string.inspect(e))
      }),
    )
    migrate.load_all_migrations(database)
    |> result.map_error(fn(e) { #("Failed to load migrations:", e) })
    |> result.map(fn(all) { #(applied, all) })
  }

  case setup {
    Error(#(header, detail)) -> {
      console.line_error(header)
      console.line(detail)
    }

    Ok(#(applied, all)) -> {
      let pending = migrate.get_pending_migrations(all, applied)
      apply_pending(conn, pending)
    }
  }
}

/// Read-only view of migration state — useful for verifying
/// what has and hasn't been applied before committing to a run,
/// especially in CI where you want to assert "no pending
/// migrations" as a gate.
///
pub fn show_status(pool: DbPool, database: String) -> Nil {
  use conn <- db.get_connection(pool)

  {
    use _ <- result.try(migrate.ensure_table(conn) |> result.replace_error(Nil))
    use applied <- result.try(
      migrate.get_applied(conn) |> result.replace_error(Nil),
    )
    use all <- result.try(
      migrate.load_all_migrations(database)
      |> result.replace_error(Nil),
    )
    print_status(all, applied)
    Ok(Nil)
  }
  |> result.unwrap(Nil)
}

// ------------------------------------------------------------- Private Functions

/// Separated from run so the public API stays clean. The
/// empty-list branch gives a distinct "nothing to do" message
/// instead of silently succeeding, which helps developers
/// confirm their migration file was actually picked up.
///
fn apply_pending(conn: db.Connection, pending: List(migrate.Migration)) -> Nil {
  case pending {
    [] -> console.line_warning("No pending migrations.")

    _ ->
      case migrate.apply_pending(conn, pending) {
        Ok(applied_versions) -> {
          let count = int.to_string(list.length(applied_versions))
          console.line_success("Applied " <> count <> " migration(s):")
          list.each(applied_versions, fn(version) {
            console.line(console.success("  ✓ ") <> version)
          })
        }
        Error(e) -> {
          console.line_error("Migration failed:")
          console.line(string.inspect(e))
        }
      }
  }
}

/// Visual checkmark/circle indicators make it immediately
/// obvious which migrations are applied vs pending without
/// having to mentally diff two lists.
///
fn print_status(all: List(migrate.Migration), applied: List(String)) -> Nil {
  let pending = migrate.get_pending_migrations(all, applied)

  console.line_warning("Migration Status:")
  console.new_line(1)
  list.each(all, fn(m) {
    let status = case list.contains(applied, m.version) {
      True -> console.success("✓")
      False -> console.warning("○")
    }
    console.line(status <> " " <> m.version <> "_" <> m.name)
  })
  console.new_line(1)
  console.line(
    console.success("Applied: ") <> int.to_string(list.length(applied)),
  )
  console.line(
    console.warning("Pending: ") <> int.to_string(list.length(pending)),
  )
}
