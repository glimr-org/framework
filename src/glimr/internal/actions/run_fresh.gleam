//// Fresh Migration Runner
////
//// During development the schema often drifts far enough from
//// the migration history that incremental fixes are harder
//// than starting over. Dropping every table and re-running all
//// migrations gives a guaranteed clean state without requiring
//// the developer to manually identify and drop individual
//// tables. Driver-specific catalog queries (pg_tables vs
//// sqlite_master) keep this portable across both backends.

import gleam/dynamic/decode
import gleam/list
import gleam/string
import glimr/console/console
import glimr/db/pool_connection.{type Connection, type DbPool}
import glimr/internal/actions/run_migrate

// ------------------------------------------------------------- Public Functions

/// Provides the "nuclear reset" that developers reach for when
/// migrations have gotten tangled. Dropping first and then
/// delegating to run_migrate means we reuse the existing apply
/// logic without duplicating it.
///
pub fn run(pool: DbPool, database: String) -> Nil {
  let drop_result = {
    use conn <- pool_connection.get_connection(pool)
    drop_all_tables(conn)
  }

  case drop_result {
    Error(e) -> {
      console.output()
      |> console.line_error("Failed to drop tables:")
      |> console.line(string.inspect(e))
      |> console.print()
    }
    Ok(_) -> {
      console.output()
      |> console.line_warning("Tables dropped.")
      |> console.blank_line(1)
      |> console.print()

      run_migrate.run(pool, database)
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// Branching on the driver for the catalog query is unavoidable
/// — Postgres and SQLite expose table lists through completely
/// different system tables. CASCADE is only appended for
/// Postgres because SQLite doesn't support it and would error.
///
fn drop_all_tables(conn: Connection) -> Result(Nil, pool_connection.DbError) {
  let decoder = {
    use name <- decode.field(0, decode.string)
    decode.success(name)
  }

  let #(list_sql, drop_suffix) = case pool_connection.connection_driver(conn) {
    pool_connection.Postgres -> #(
      "SELECT tablename FROM pg_tables WHERE schemaname = 'public'",
      " CASCADE",
    )
    pool_connection.Sqlite -> #(
      "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%'",
      "",
    )
  }

  case pool_connection.query_with(conn, list_sql, [], decoder) {
    Ok(pool_connection.QueryResult(_, tables)) -> {
      list.each(tables, fn(table) {
        let _ =
          pool_connection.exec_with(
            conn,
            "DROP TABLE IF EXISTS \"" <> table <> "\"" <> drop_suffix,
            [],
          )
        Nil
      })
      Ok(Nil)
    }
    Error(_) -> Ok(Nil)
  }
}
