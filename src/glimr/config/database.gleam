//// Database Configuration
////
//// Centralizes database connection setup so application code
//// doesn't need to know about config file locations or parsing.
//// Uses ${VAR} syntax for secrets to keep credentials out of
//// version control while remaining simple to configure.
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

/// Safe to call repeatedly from hot paths like request handlers
/// since results are cached after first load. Returns empty
/// list on missing/invalid config to let apps start without a
/// database.
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

/// Clears the cached database connections, forcing the next
/// call to load() to re-read from config/database.toml. Useful
/// for testing when you need to load different configurations.
///
pub fn clear_cache() -> Nil {
  do_clear_cache()
}

// ------------------------------------------------------------- Private Functions

/// Reads and parses config/database.toml from disk. Separated
/// from load() to keep caching logic distinct from file I/O,
/// making the caching behavior easier to test and reason about.
///
fn load_from_file() -> List(Connection) {
  case simplifile.read("config/database.toml") {
    Ok(content) -> parse(content)
    Error(_) -> []
  }
}

/// Expects [connections.*] tables so users can define multiple
/// named connections (e.g., default, readonly, analytics) and
/// switch between them per-command or per-request.
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

/// Maps driver field to the appropriate Connection variant.
/// Defaults to postgres for unknown drivers so existing configs
/// without explicit driver fields continue working.
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

/// Stores parsed connections in persistent_term for fast access
/// across all processes. Avoids re-parsing TOML and re-reading
/// env vars on every request, which would add latency and I/O.
///
@external(erlang, "glimr_kernel_ffi", "cache_db_config")
fn cache(connections: List(Connection)) -> Nil

/// Retrieves cached connections from persistent_term. Returns
/// Error if not yet cached, signaling that load_from_file()
/// should be called to populate the cache.
///
@external(erlang, "glimr_kernel_ffi", "get_cached_db_config")
fn get_cached() -> Result(List(Connection), Nil)

/// Removes the cached connections from persistent_term. This
/// forces the next load() call to re-read the config file and
/// re-parse environment variables for fresh values.
///
@external(erlang, "glimr_kernel_ffi", "clear_db_config")
fn do_clear_cache() -> Nil
