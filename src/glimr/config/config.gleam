//// Unified Configuration
////
//// Every module in the framework needs config values — session
//// secrets, database URLs, feature flags — but scattering
//// simplifile.read and tom.parse calls everywhere would be a
//// mess. This module reads all config/*.toml files once at
//// boot, caches the merged result in persistent_term, and
//// gives the rest of the codebase a clean dot-path API like
//// `config.get_string("session.cookie")`. Environment variable
//// interpolation (${VAR} and ${VAR:-fallback}) is resolved
//// transparently so secrets never live in TOML files.
////

import dot_env
import dot_env/env
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import simplifile
import tom

// ------------------------------------------------------------- Public Functions

/// The entire config system hinges on this running once at boot
/// before anything else touches config values. It loads .env
/// first (so TOML files can reference those vars), then reads
/// every .toml in config/ and merges them into one dict keyed
/// by filename — session.toml becomes "session", so
/// `get_string("session.cookie")` just works.
///
pub fn load() -> Nil {
  load_env()

  let toml_dict = case simplifile.read_directory("config") {
    Ok(files) -> {
      files
      |> list.filter(fn(f) { string.ends_with(f, ".toml") })
      |> list.fold(dict.new(), fn(acc, file) {
        let key = string.replace(file, ".toml", "")
        case simplifile.read("config/" <> file) {
          Ok(content) -> {
            case tom.parse(content) {
              Ok(parsed) -> dict.insert(acc, key, tom.Table(parsed))
              Error(_) -> acc
            }
          }
          Error(_) -> acc
        }
      })
    }
    Error(_) -> dict.new()
  }

  cache("toml", toml_dict)
}

/// The panicking variant for callers that consider a missing
/// config value a hard stop — like the session module needing
/// its secret key. If the key is absent or the env var isn't
/// set, you'll get a crash at boot rather than a subtle bug at
/// runtime.
///
pub fn get_string(path: String) -> String {
  let assert Ok(value) = get_string_or(path)
  value
}

/// The safe variant for callers that want to handle missing
/// config gracefully — like optional features that fall back to
/// defaults. Returns Error(Nil) if the key doesn't exist or an
/// env var can't be resolved, so you can chain it with
/// result.unwrap or result.try.
///
pub fn get_string_or(path: String) -> Result(String, Nil) {
  let keys = string.split(path, ".")
  use toml <- result.try(get_cached("toml"))
  use value <- result.try(
    tom.get_string(toml, keys) |> result.replace_error(Nil),
  )
  resolve_env(value) |> result.replace_error(Nil)
}

/// Panicking int getter for values that must exist, like port
/// numbers or pool sizes. Handles both native TOML integers and
/// string values containing ${VAR} references — because hosting
/// providers often expose numeric config as strings through env
/// vars.
///
pub fn get_int(path: String) -> Int {
  let assert Ok(value) = get_int_or(path)
  value
}

/// Safe int getter that tries native TOML ints first, then
/// falls back to parsing a string with env interpolation. The
/// two-phase lookup matters because developers might write
/// `pool_size = 10` or `pool_size = "${POOL_SIZE}"` and both
/// should work without them thinking about it.
///
pub fn get_int_or(path: String) -> Result(Int, Nil) {
  let keys = string.split(path, ".")
  use toml <- result.try(get_cached("toml"))
  case tom.get_int(toml, keys) {
    Ok(value) -> Ok(value)
    Error(_) -> {
      // Try as string with env interpolation
      use value <- result.try(
        tom.get_string(toml, keys) |> result.replace_error(Nil),
      )
      use resolved <- result.try(
        resolve_env(value) |> result.replace_error(Nil),
      )
      int.parse(resolved) |> result.replace_error(Nil)
    }
  }
}

/// Panicking bool getter for feature flags and toggles that
/// must be explicitly configured — like auto_compile or
/// auto_gen. A crash on missing values forces developers to set
/// them in their TOML rather than silently defaulting.
///
pub fn get_bool(path: String) -> Bool {
  let assert Ok(value) = get_bool_or(path)
  value
}

/// Safe bool getter for optional config where a missing value
/// is fine — the caller decides the default. Unlike strings and
/// ints, bools don't support env interpolation since TOML
/// booleans are unambiguous.
///
pub fn get_bool_or(path: String) -> Result(Bool, Nil) {
  let keys = string.split(path, ".")
  use toml <- result.try(get_cached("toml"))
  tom.get_bool(toml, keys) |> result.replace_error(Nil)
}

/// Accepts both TOML arrays and single string values, wrapping
/// the latter in a list automatically. This means developers
/// can write `allowed = "admin"` instead of `allowed =
/// ["admin"]` when there's only one value, and upgrade to an
/// array later without breaking anything.
///
pub fn get_string_list(path: String) -> List(String) {
  let keys = string.split(path, ".")
  case get_cached("toml") {
    Ok(toml) -> {
      case tom.get_array(toml, keys) {
        Ok(items) -> {
          list.filter_map(items, fn(item) {
            case item {
              tom.String(s) -> Ok(s)
              _ -> Error(Nil)
            }
          })
        }
        Error(_) -> {
          // Try single string value
          case tom.get_string(toml, keys) {
            Ok(s) -> [s]
            Error(_) -> []
          }
        }
      }
    }
    Error(_) -> []
  }
}

/// Int list getter for config values like allowed port ranges
/// or rate limit tiers. Returns empty on missing keys so
/// callers can treat absence as "no restrictions" without
/// special-casing.
///
pub fn get_int_list(path: String) -> List(Int) {
  let keys = string.split(path, ".")
  case get_cached("toml") {
    Ok(toml) -> {
      case tom.get_array(toml, keys) {
        Ok(items) -> {
          list.filter_map(items, fn(item) {
            case item {
              tom.Int(i) -> Ok(i)
              _ -> Error(Nil)
            }
          })
        }
        Error(_) -> []
      }
    }
    Error(_) -> []
  }
}

/// Bool list getter — included for completeness with the other
/// list getters, though it's rare to need an array of booleans
/// in practice. Returns empty on missing keys for consistency
/// with the string and int variants.
///
pub fn get_bool_list(path: String) -> List(Bool) {
  let keys = string.split(path, ".")
  case get_cached("toml") {
    Ok(toml) -> {
      case tom.get_array(toml, keys) {
        Ok(items) -> {
          list.filter_map(items, fn(item) {
            case item {
              tom.Bool(b) -> Ok(b)
              _ -> Error(Nil)
            }
          })
        }
        Error(_) -> []
      }
    }
    Error(_) -> []
  }
}

/// Driver modules like db/driver and cache/driver need access
/// to the raw TOML table so they can iterate over sub-keys and
/// parse each entry their own way. The simple scalar getters
/// above aren't enough when the config is a dict of named
/// connections or stores.
///
pub fn get_table(path: String) -> Result(Dict(String, tom.Toml), Nil) {
  let keys = string.split(path, ".")
  use toml <- result.try(get_cached("toml"))
  tom.get_table(toml, keys) |> result.replace_error(Nil)
}

/// Tests that exercise different config scenarios need to start
/// fresh between runs. Without clearing, the persistent_term
/// cache would serve stale values from the previous test case
/// and produce confusing failures.
///
pub fn clear_cache() -> Nil {
  clear_cached("toml")
}

// ------------------------------------------------------------- Internal Public Functions

/// persistent_term gives us near-zero-cost reads after the
/// first write, which matters because config values get read on
/// every request. Driver modules also use this to cache their
/// own parsed structures (like db connections) on top of the
/// raw TOML cache.
///
@internal
pub fn cache(key: String, value: a) -> Nil {
  do_cache(key, value)
}

/// The read side of the persistent_term cache. Returns
/// Error(Nil) on a cache miss so callers like load_connections
/// can detect the first access and populate the cache on
/// demand.
///
@internal
pub fn get_cached(key: String) -> Result(a, Nil) {
  do_get_cached(key)
}

/// Evicts a single key from persistent_term without touching
/// other cached config. Driver modules use this in their own
/// clear_cache functions so clearing db connections doesn't
/// also flush the TOML cache.
///
@internal
pub fn clear_cached(key: String) -> Nil {
  do_clear_cached(key)
}

/// New projects should just work out of the box without
/// requiring a .env file, so this defaults to 8000 if APP_PORT
/// isn't set. Production deployments override it via the env
/// var without touching any config files.
///
@internal
pub fn app_port() -> Int {
  env.get_int("APP_PORT") |> result.unwrap(8000)
}

/// The dev proxy sits in front of the app server to inject
/// live-reload scripts. It defaults to 8001 — one above the app
/// port — so both can run simultaneously without conflicts
/// during development.
///
@internal
pub fn dev_proxy_port() -> Int {
  env.get_int("DEV_PROXY_PORT") |> result.unwrap(8001)
}

/// TOML values like `"${DATABASE_URL}"` or
/// `"${HOST:-localhost}"` need to become actual values before
/// anyone uses them. This handles the interpolation so secrets
/// stay in env vars rather than config files, and the `:-`
/// fallback syntax lets developers provide sensible defaults
/// for optional vars.
///
@internal
pub fn resolve_env(value: String) -> Result(String, String) {
  case string.starts_with(value, "${") && string.ends_with(value, "}") {
    True -> {
      let inner =
        value
        |> string.drop_start(2)
        |> string.drop_end(1)

      case string.split_once(inner, ":-") {
        Ok(#(var_name, fallback)) -> {
          case env.get_string(var_name) {
            Ok(s) -> Ok(s)
            Error(_) -> Ok(fallback)
          }
        }
        Error(_) -> {
          case env.get_string(inner) {
            Ok(s) -> Ok(s)
            Error(_) -> Error("Env var not set: " <> inner)
          }
        }
      }
    }
    False -> Ok(value)
  }
}

/// Reads a string directly from a TOML value without env
/// interpolation — because some config fields like driver names
/// ("postgres", "sqlite", "file") are never secrets and should
/// never be wrapped in ${...}. Skipping interpolation avoids
/// confusing errors if someone writes `driver = "postgres"`
/// with a literal dollar sign.
///
@internal
pub fn toml_get_string(toml: tom.Toml, key: String, default: String) -> String {
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

/// Unlike toml_get_string, this runs env interpolation because
/// the value is expected to reference a secret — database URLs,
/// passwords, Redis credentials. Returns a Result so the driver
/// module can defer the error to pool startup rather than
/// panicking during config parsing.
///
@internal
pub fn toml_get_env_string(
  toml: tom.Toml,
  key: String,
) -> Result(String, String) {
  case toml {
    tom.Table(table) -> {
      case dict.get(table, key) {
        Ok(tom.String(s)) -> resolve_env(s)
        _ -> Error("Missing key: " <> key)
      }
    }
    _ -> Error("Invalid TOML structure")
  }
}

/// Like toml_get_env_string but for integers. Accepts both
/// native TOML ints (`pool_size = 10`) and env-backed strings
/// (`pool_size = "${POOL_SIZE}"`) because different deployment
/// environments express numeric config different ways. Returns
/// a Result for the same deferred-error reason as the string
/// variant.
///
@internal
pub fn toml_get_env_int(toml: tom.Toml, key: String) -> Result(Int, String) {
  case toml {
    tom.Table(table) -> {
      case dict.get(table, key) {
        Ok(tom.String(s)) -> {
          use resolved <- result.try(resolve_env(s))
          int.parse(resolved)
          |> result.replace_error("Invalid int: " <> resolved)
        }
        Ok(tom.Int(i)) -> Ok(i)
        _ -> Error("Missing key: " <> key)
      }
    }
    _ -> Error("Invalid TOML structure")
  }
}

// ------------------------------------------------------------- Private Functions

/// .env loading happens before TOML parsing so that env vars
/// referenced in config files (like ${DATABASE_URL}) are
/// already available when resolve_env runs. Debug mode is off
/// because dot_env's output clutters the boot log with every
/// var it loads.
///
fn load_env() -> Nil {
  dot_env.new()
  |> dot_env.set_path(".env")
  |> dot_env.set_debug(False)
  |> dot_env.load()

  Nil
}

// ------------------------------------------------------------- FFI Bindings

@external(erlang, "glimr_kernel_ffi", "cache_config")
fn do_cache(key: String, value: a) -> Nil

@external(erlang, "glimr_kernel_ffi", "get_cached_config")
fn do_get_cached(key: String) -> Result(a, Nil)

@external(erlang, "glimr_kernel_ffi", "clear_cached_config")
fn do_clear_cached(key: String) -> Nil
