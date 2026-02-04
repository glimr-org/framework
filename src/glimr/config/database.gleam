//// Database Configuration
////
//// Centralizes database connection setup so application code
//// doesn't need to know about config file locations or parsing.
//// Uses ${VAR} syntax for secrets to keep credentials out of
//// version control while remaining simple to configure.
////

import dot_env/env
import gleam/dict
import gleam/int
import gleam/list
import gleam/string
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
  let driver = get_string(toml, "driver", "postgres")

  case driver {
    "postgres" ->
      PostgresConnection(
        name: name,
        host: get_env_string(toml, "host"),
        port: get_env_int(toml, "port"),
        database: get_env_string(toml, "database"),
        username: get_env_string(toml, "username"),
        password: get_env_string(toml, "password"),
        pool_size: get_env_int(toml, "pool_size"),
      )
    "postgres_url" ->
      PostgresUriConnection(
        name: name,
        url: get_env_string(toml, "url"),
        pool_size: get_env_int(toml, "pool_size"),
      )
    "sqlite" ->
      SqliteConnection(
        name: name,
        database: get_env_string(toml, "database"),
        pool_size: get_env_int(toml, "pool_size"),
      )
    _ ->
      PostgresConnection(
        name: name,
        host: get_env_string(toml, "host"),
        port: get_env_int(toml, "port"),
        database: get_env_string(toml, "database"),
        username: get_env_string(toml, "username"),
        password: get_env_string(toml, "password"),
        pool_size: get_env_int(toml, "pool_size"),
      )
  }
}

/// Extracts a string value from a TOML table without env
/// interpolation. Used for fields like driver that should
/// not contain environment variables.
///
fn get_string(toml: tom.Toml, key: String, default: String) -> String {
  case toml {
    tom.Table(table) -> {
      case dict.get(table, key) {
        Ok(tom.String(s)) -> s
        _ -> default
      }
    }
    _ -> default
  }
}

/// Extracts a string value with environment variable support.
/// Recognizes ${VAR} syntax and replaces it with the actual
/// value from the environment. Returns an error if the env
/// var is missing so config issues surface early.
///
fn get_env_string(toml: tom.Toml, key: String) -> Result(String, String) {
  case toml {
    tom.Table(table) -> {
      case dict.get(table, key) {
        Ok(tom.String(s)) -> interpolate_env(s)
        _ -> Error("Missing key: " <> key)
      }
    }
    _ -> Error("Invalid TOML structure")
  }
}

/// Accepts both TOML integers and strings to give users
/// flexibility in how they specify numeric config. Strings
/// allow env var references like "${DB_PORT}" for values
/// that vary between environments.
///
fn get_env_int(toml: tom.Toml, key: String) -> Result(Int, String) {
  case toml {
    tom.Table(table) -> {
      case dict.get(table, key) {
        Ok(tom.String(s)) -> {
          case interpolate_env(s) {
            Ok(value) ->
              case int.parse(value) {
                Ok(i) -> Ok(i)
                Error(_) -> Error("Invalid int: " <> value)
              }
            Error(e) -> Error(e)
          }
        }
        Ok(tom.Int(i)) -> Ok(i)
        _ -> Error("Missing key: " <> key)
      }
    }
    _ -> Error("Invalid TOML structure")
  }
}

/// Only supports full-value substitution (${VAR}, not mixed
/// strings) to keep parsing simple and predictable. Partial
/// interpolation would require escaping rules and edge cases
/// that aren't worth the complexity for config values.
///
fn interpolate_env(value: String) -> Result(String, String) {
  case string.starts_with(value, "${") && string.ends_with(value, "}") {
    True -> {
      let var_name =
        value
        |> string.drop_start(2)
        |> string.drop_end(1)

      case env.get_string(var_name) {
        Ok(s) -> Ok(s)
        Error(_) -> Error("Env var not set: " <> var_name)
      }
    }
    False -> Ok(value)
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
