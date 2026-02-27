//// Session Configuration
////
//// Session middleware, cookie stores, and ETS stores all need
//// the same settings (cookie name, lifetime, etc.) but
//// shouldn't each parse the TOML file independently. Loading
//// once and caching in persistent_term gives every process
//// fast, lock-free access to the same config without passing
//// it through function arguments or application state.
////

import gleam/dict
import glimr/config/config
import simplifile
import tom

// ------------------------------------------------------------- Public Types

/// A typed struct ensures all consumers agree on the config
/// shape at compile time. The table name controls where ETS-
/// backed stores persist data, cookie sets the browser cookie
/// name, lifetime controls max-age in minutes, and
/// expire_on_close determines whether the cookie survives
/// browser restarts.
///
pub type SessionConfig {
  SessionConfig(
    table: String,
    cookie: String,
    lifetime: Int,
    expire_on_close: Bool,
  )
}

// ------------------------------------------------------------- Public Functions

/// Session middleware calls this on every request, so it must
/// be fast. The persistent_term cache makes repeated calls
/// effectively free after the first load. Falling back to
/// sensible defaults on missing or invalid config lets apps
/// start without requiring a session.toml file.
///
pub fn load() -> SessionConfig {
  case get_cached() {
    Ok(config) -> config
    Error(_) -> {
      let session_config = load_from_file()
      cache(session_config)
      session_config
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// Separating file I/O from caching lets load() decide whether
/// to read the file or use the cache. Falling back to defaults
/// on read failure avoids crashing apps that don't use
/// sessions.
///
fn load_from_file() -> SessionConfig {
  case simplifile.read("config/session.toml") {
    Ok(content) -> parse(content)
    Error(_) -> default_config()
  }
}

/// The TOML file may contain other sections beyond [session].
/// Extracting the session table first and falling back to
/// defaults if it's missing lets the config file evolve without
/// breaking session loading.
///
fn parse(content: String) -> SessionConfig {
  case tom.parse(content) {
    Ok(toml) -> {
      case dict.get(toml, "session") {
        Ok(session_toml) -> parse_session(session_toml)
        _ -> default_config()
      }
    }
    Error(_) -> default_config()
  }
}

/// Each field has a sensible default so partial configs work —
/// users only need to override what differs from the defaults.
/// This keeps the minimum config file empty.
///
fn parse_session(toml: tom.Toml) -> SessionConfig {
  SessionConfig(
    table: config.get_string(toml, "table", "sessions"),
    cookie: config.get_string(toml, "cookie", "glimr_session"),
    lifetime: parse_int(toml, "lifetime", 120),
    expire_on_close: parse_bool(toml, "expire_on_close", False),
  )
}

/// tom's API returns typed Toml variants, so extracting an Int
/// requires pattern matching on the table and value. A helper
/// avoids repeating this boilerplate for each integer config
/// field.
///
fn parse_int(toml: tom.Toml, key: String, default: Int) -> Int {
  case toml {
    tom.Table(table) -> {
      case dict.get(table, key) {
        Ok(tom.Int(i)) -> i
        _ -> default
      }
    }
    _ -> default
  }
}

/// Same extraction pattern as parse_int but for Bool values.
/// Keeping it separate avoids a generic helper that would need
/// dynamic typing to return different Gleam types.
///
fn parse_bool(toml: tom.Toml, key: String, default: Bool) -> Bool {
  case toml {
    tom.Table(table) -> {
      case dict.get(table, key) {
        Ok(tom.Bool(b)) -> b
        _ -> default
      }
    }
    _ -> default
  }
}

/// Centralizing defaults here ensures every fallback path
/// (missing file, parse error, missing section) produces
/// identical config. The 120-minute lifetime balances usability
/// against the security risk of long-lived sessions.
///
fn default_config() -> SessionConfig {
  SessionConfig(
    table: "sessions",
    cookie: "glimr_session",
    lifetime: 120,
    expire_on_close: False,
  )
}

// ------------------------------------------------------------- FFI Bindings

/// Stores the config in persistent_term so every subsequent
/// call to load() across all BEAM processes gets a near-zero-
/// cost read instead of re-parsing the TOML file. The FFI lives
/// in glimr_kernel_ffi because Gleam has no native
/// persistent_term API.
///
@external(erlang, "glimr_kernel_ffi", "cache_session_config")
fn cache(config: SessionConfig) -> Nil

/// Returns the cached config if it exists, or Error(Nil) on the
/// first call before cache() has been called. load() uses this
/// to skip file I/O on every request after the initial load.
///
@external(erlang, "glimr_kernel_ffi", "get_cached_session_config")
fn get_cached() -> Result(SessionConfig, Nil)
