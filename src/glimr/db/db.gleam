//// Database Utilities
////
//// High-level database utilities for multi-database support.
////
//// NOTE: Pool management and transactions are now in driver packages:
//// - glimr_sqlite.start_pool(), glimr_sqlite.transaction()
//// - glimr_postgres.start_pool(), glimr_postgres.transaction()

import gleam/list
import gleam/string
import glimr/db/driver.{type Connection}

// ------------------------------------------------------------- Public Functions

/// Validates that all connection names are unique. Panics with
/// a helpful message listing duplicate names if any are found.
/// Call this when setting up database connections to catch
/// configuration errors early.
///
pub fn validate_connections(connections: List(Connection)) -> Nil {
  let names = list.map(connections, driver.connection_name)
  let duplicates = find_duplicates(names)

  case duplicates {
    [] -> Nil
    _ ->
      panic as {
        "Duplicate database connection names: "
        <> string.join(duplicates, ", ")
        <> ". Each connection must have a unique name in your config_db.gleam file."
      }
  }
}

/// Finds a connection by name from a list of connections. Panics if
/// the connection is not found.
///
pub fn get_connection(connections: List(Connection), name: String) -> Connection {
  case list.find(connections, fn(c) { driver.connection_name(c) == name }) {
    Ok(c) -> c
    Error(_) ->
      panic as {
        "Database connection '"
        <> name
        <> "' not found. Available connections: "
        <> string.join(list.map(connections, driver.connection_name), ", ")
      }
  }
}

/// Finds a connection by name from a list of connections. Returns
/// Error(Nil) if the connection is not found instead of panicking.
///
pub fn get_connection_safe(
  connections: List(Connection),
  name: String,
) -> Result(Connection, Nil) {
  list.find(connections, fn(c) { driver.connection_name(c) == name })
}

// ------------------------------------------------------------- Private Functions

/// Finds duplicate strings in a list. Returns a list of strings
/// that appear more than once, with each duplicate listed only once.
///
fn find_duplicates(items: List(String)) -> List(String) {
  items
  |> list.fold(#([], []), fn(acc, item) {
    let #(seen, dups) = acc
    case list.contains(seen, item) {
      True ->
        case list.contains(dups, item) {
          True -> #(seen, dups)
          False -> #(seen, [item, ..dups])
        }
      False -> #([item, ..seen], dups)
    }
  })
  |> fn(result) { result.1 }
  |> list.reverse
}
