//// App Configuration
////
//// Framework-level settings like the static file directory are
//// needed on every request by middleware, but re-parsing TOML
//// each time would add unnecessary latency. Loading once from
//// config/app.toml and caching in persistent_term gives every
//// process fast, lock-free access without threading the config
//// through function arguments.
////

import gleam/dict
import glimr/config/config
import simplifile
import tom

// ------------------------------------------------------------- Public Types

/// A typed struct ensures all consumers agree on the config
/// shape at compile time. Currently holds only static_directory
/// but structured as a record so new app-level settings can be
/// added without changing the load/cache API.
///
pub type AppConfig {
  AppConfig(static_directory: String)
}

// ------------------------------------------------------------- Public Functions

/// Static file middleware calls this on every request to check
/// the URL prefix, so it must be fast. The persistent_term
/// cache makes repeated calls effectively free after the first
/// load. Falling back to sensible defaults on missing or
/// invalid config lets apps start without requiring an app.toml
/// file.
///
pub fn load() -> AppConfig {
  case get_cached() {
    Ok(cfg) -> cfg
    Error(_) -> {
      let cfg = load_from_file()
      cache(cfg)
      cfg
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// Separating file I/O from caching lets load() decide whether
/// to read the file or use the cache. Falling back to defaults
/// on read failure avoids crashing apps that don't have an
/// app.toml — the defaults work for most projects.
///
fn load_from_file() -> AppConfig {
  case simplifile.read("config/app.toml") {
    Ok(content) -> parse(content)
    Error(_) -> default_config()
  }
}

/// The TOML file may contain other sections beyond [static].
/// Extracting the static table first and falling back to
/// defaults if it's missing lets the config file evolve without
/// breaking app loading.
///
fn parse(content: String) -> AppConfig {
  case tom.parse(content) {
    Ok(toml) -> {
      case dict.get(toml, "static") {
        Ok(static_toml) -> parse_static(static_toml)
        _ -> default_config()
      }
    }
    Error(_) -> default_config()
  }
}

/// Defaults to "/static" which is the conventional URL prefix
/// for serving static assets. Most apps won't need to change
/// this, so the config field is optional.
///
fn parse_static(toml: tom.Toml) -> AppConfig {
  AppConfig(static_directory: config.get_string(toml, "directory", "/static"))
}

/// Centralizing defaults here ensures every fallback path
/// (missing file, parse error, missing section) produces
/// identical config so behavior is predictable regardless of
/// which error path was taken.
///
fn default_config() -> AppConfig {
  AppConfig(static_directory: "/static")
}

// ------------------------------------------------------------- FFI Bindings

/// Stores the config in persistent_term so every subsequent
/// call to load() across all BEAM processes gets a near-zero-
/// cost read instead of re-parsing the TOML file.
///
@external(erlang, "glimr_kernel_ffi", "cache_app_config")
fn cache(config: AppConfig) -> Nil

/// Returns the cached config if it exists, or Error(Nil) on the
/// first call before cache() has been called. load() uses this
/// to skip file I/O on every request after the initial load.
///
@external(erlang, "glimr_kernel_ffi", "get_cached_app_config")
fn get_cached() -> Result(AppConfig, Nil)
