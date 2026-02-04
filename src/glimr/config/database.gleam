//// Database Configuration
////
//// Loads and parses database connection configuration from the
//// config/database.toml file. Supports environment variable
//// interpolation using ${VAR} syntax for sensitive values.
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

// ---------------------------------------------------------- Public Functions

/// Entry point for loading database configuration at startup.
/// Reads config/database.toml and returns all defined 
/// connections. Falls back to an empty list if the file is 
/// missing or malformed to allow the application to start 
/// without a database.
///
pub fn load() -> List(Connection) {
  case simplifile.read("config/database.toml") {
    Ok(content) -> parse(content)
    Error(_) -> []
  }
}

// --------------------------------------------------------- Private Functions

/// Parses the TOML content and extracts the [connections.*]
/// tables. Each sub-table under connections becomes a named
/// database connection with driver-specific parameters.
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

/// Converts a single TOML table into a Connection variant.
/// Uses the driver field to determine which connection type
/// to construct. Defaults to postgres if driver is unknown
/// to maintain backwards compatibility.
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
      // Default to postgres if unknown driver
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

/// Extracts an integer value with environment variable support.
/// Handles both literal TOML integers and string values that
/// may contain ${VAR} references. Parses the final value as
/// an integer after interpolation.
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

/// Performs environment variable substitution on a string.
/// Only handles the exact ${VAR} format where the entire
/// string is a single variable reference. Literal strings
/// pass through unchanged.
///
fn interpolate_env(value: String) -> Result(String, String) {
  case string.starts_with(value, "${") && string.ends_with(value, "}") {
    True -> {
      // Extract var name from ${VAR}
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
