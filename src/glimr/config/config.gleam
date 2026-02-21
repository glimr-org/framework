//// Config Helpers
////
//// Every config module (database, cache, session, auth) needs
//// to extract typed values from TOML and interpolate environment
//// variables. Without shared helpers each module would reimplement
//// the same dict lookup, type matching, and ${VAR} substitution,
//// leading to inconsistent error messages and subtle differences
//// in fallback behavior. Centralizing these here guarantees all
//// config files are parsed the same way.
////

import dot_env/env
import gleam/dict
import gleam/int
import gleam/string
import tom

// ------------------------------------------------------------- Public Functions

/// Skips env interpolation because values like driver names and
/// file paths are static and safe to commit. Returning a default
/// on missing or wrong-typed keys lets config files stay minimal
/// — users only override what differs from the defaults without
/// needing to specify every field.
///
pub fn get_string(toml: tom.Toml, key: String, default: String) -> String {
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

/// Returns Result instead of a default because env-backed values
/// are typically secrets or connection strings — silently falling
/// back to a hardcoded default would cause confusing runtime
/// errors (wrong database, no Redis) instead of a clear "env var
/// not set" message at startup.
///
pub fn get_env_string(toml: tom.Toml, key: String) -> Result(String, String) {
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

/// Accepts both TOML integers and strings because some integer
/// values (like pool_size or port) need to come from env vars
/// in production but are convenient as plain integers in dev
/// config. Checking the string case first lets "${DB_PORT}"
/// work while still accepting `port = 5432` without quotes.
///
pub fn get_env_int(toml: tom.Toml, key: String) -> Result(Int, String) {
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
/// strings like "host:${PORT}") to keep parsing simple. Partial
/// interpolation would need escaping rules and edge case handling
/// that aren't worth the complexity for config values. Plain
/// strings without ${} pass through unchanged so literal values
/// work without special syntax.
///
pub fn interpolate_env(value: String) -> Result(String, String) {
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
