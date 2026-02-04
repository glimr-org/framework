//// Config Helpers
////
//// Shared utilities for TOML parsing so database, cache, and
//// route config modules don't duplicate env var interpolation
//// and type conversion logic. Keeps parsing behavior consistent
//// across all config files.
////

import dot_env/env
import gleam/dict
import gleam/int
import gleam/string
import tom

// ------------------------------------------------------------- Public Functions

/// No env interpolation since values like driver names and paths
/// are typically static and safe to commit. Provides a sensible
/// default when keys are missing to reduce required config.
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

/// Returns Result to surface missing env vars early. Secrets
/// should fail loudly at startup rather than silently falling
/// back to defaults that might cause confusing runtime errors.
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

/// Accepts both TOML integers and strings for flexibility.
/// Strings allow env var references like "${DB_PORT}" for values
/// that vary between environments (dev, staging, prod).
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
/// strings) to keep parsing simple. Partial interpolation would
/// require escaping rules not worth the complexity for configs.
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
