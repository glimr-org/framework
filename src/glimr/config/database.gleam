//// Database Configuration
////
//// An app may have multiple database connections (primary,
//// read replica, analytics) across different engines (Postgres,
//// SQLite). Parsing the TOML file once and caching the result
//// in persistent_term lets every process access the connection
//// list without re-reading the file or re-resolving env vars.
//// The ${VAR} syntax keeps credentials out of version control
//// while allowing per-environment configuration.
////

import gleam/dict
import gleam/list
import glimr/config/config
import glimr/db/driver.{
  type Connection, PostgresConnection, PostgresUriConnection, SqliteConnection,
}
import simplifile
import tom

// ------------------------------------------------------------- Public Functions

/// Called by start functions and console commands that need
/// the connection list, so it must be fast on repeated calls.
/// The persistent_term cache makes subsequent calls near-zero-
/// cost. Returning an empty list on missing or invalid config
/// lets apps start without a database.toml file — not every
/// app uses a database.
///
pub fn load() -> List(Connection) {
  case get_cached() {
    Ok(connections) -> connections
    Error(_) -> {
      let connections = load_from_file()
      cache(connections)
      connections
    }
  }
}

/// persistent_term values survive until explicitly erased, so
/// tests that swap database configs between runs need a way to
/// force a fresh read. Without this, a test that modifies the
/// TOML or env vars would still see the stale cached values.
///
pub fn clear_cache() -> Nil {
  do_clear_cache()
}

// ------------------------------------------------------------- Private Functions

/// Separating file I/O from caching lets load() decide whether
/// to read the file or use the cache. Returning an empty list
/// on read failure avoids crashing apps that don't use a
/// database.
///
fn load_from_file() -> List(Connection) {
  case simplifile.read("config/database.toml") {
    Ok(content) -> parse(content)
    Error(_) -> []
  }
}

/// The [connections.*] nesting lets users define multiple named
/// connections (e.g., default, readonly, analytics) each with
/// its own driver and credentials. Iterating dict.to_list
/// preserves all entries so no connection definition is silently
/// dropped.
///
fn parse(content: String) -> List(Connection) {
  case tom.parse(content) {
    Ok(toml) -> {
      case dict.get(toml, "connections") {
        Ok(tom.Table(connections)) -> {
          connections
          |> dict.to_list
          |> list.map(fn(entry) {
            let #(name, conn_toml) = entry
            parse_connection(name, conn_toml)
          })
        }
        _ -> []
      }
    }
    Error(_) -> []
  }
}

/// Each driver string maps to a specific Connection variant
/// with its own required fields. Defaulting to PostgresConnection
/// for unknown or missing driver values preserves backward
/// compatibility — configs written before the driver field existed
/// continue working without changes.
///
fn parse_connection(name: String, toml: tom.Toml) -> Connection {
  let driver = config.get_string(toml, "driver", "postgres")

  case driver {
    "postgres" ->
      PostgresConnection(
        name: name,
        host: config.get_env_string(toml, "host"),
        port: config.get_env_int(toml, "port"),
        database: config.get_env_string(toml, "database"),
        username: config.get_env_string(toml, "username"),
        password: config.get_env_string(toml, "password"),
        pool_size: config.get_env_int(toml, "pool_size"),
      )
    "postgres_url" ->
      PostgresUriConnection(
        name: name,
        url: config.get_env_string(toml, "url"),
        pool_size: config.get_env_int(toml, "pool_size"),
      )
    "sqlite" ->
      SqliteConnection(
        name: name,
        database: config.get_env_string(toml, "database"),
        pool_size: config.get_env_int(toml, "pool_size"),
      )
    _ ->
      PostgresConnection(
        name: name,
        host: config.get_env_string(toml, "host"),
        port: config.get_env_int(toml, "port"),
        database: config.get_env_string(toml, "database"),
        username: config.get_env_string(toml, "username"),
        password: config.get_env_string(toml, "password"),
        pool_size: config.get_env_int(toml, "pool_size"),
      )
  }
}

// ------------------------------------------------------------- FFI Bindings

/// Stores the parsed connection list in persistent_term so every
/// subsequent call to load() across all BEAM processes gets a
/// near-zero-cost read instead of re-parsing TOML and re-
/// resolving env vars.
///
@external(erlang, "glimr_kernel_ffi", "cache_db_config")
fn cache(connections: List(Connection)) -> Nil

/// Returns the cached connection list if it exists, or
/// Error(Nil) on the first call before cache() has been called.
/// load() uses this to skip file I/O on every request after the
/// initial load.
///
@external(erlang, "glimr_kernel_ffi", "get_cached_db_config")
fn get_cached() -> Result(List(Connection), Nil)

/// Erases the persistent_term entry so the next load() call
/// falls through to load_from_file(). Used by clear_cache()
/// to support tests that need to swap configs between runs.
///
@external(erlang, "glimr_kernel_ffi", "clear_db_config")
fn do_clear_cache() -> Nil
