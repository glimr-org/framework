//// Auth Configuration
////
//// Auth middleware and the auth module both need the same
//// settings (redirect path, session key) but shouldn't each
//// parse the TOML file independently. Loading once and caching
//// in persistent_term gives every process fast, lock-free
//// access to the same config without passing it through
//// function arguments or application state.
////

import gleam/dict
import glimr/config/config
import simplifile
import tom

// ------------------------------------------------------------- Public Types

/// A typed struct ensures all consumers agree on the config
/// shape at compile time. session_key sets which session entry
/// holds the user ID so it's defined in one place rather than
/// hardcoded across modules.
///
pub type AuthConfig {
  AuthConfig(session_key: String)
}

// ------------------------------------------------------------- Public Functions

/// Auth middleware calls this on every request, so it must be
/// fast. The persistent_term cache makes repeated calls
/// effectively free after the first load. Falling back to
/// sensible defaults on missing or invalid config lets apps
/// start without requiring an auth.toml file.
///
pub fn load() -> AuthConfig {
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
/// on read failure avoids crashing apps that don't use auth.
///
fn load_from_file() -> AuthConfig {
  case simplifile.read("config/auth.toml") {
    Ok(content) -> parse(content)
    Error(_) -> default_config()
  }
}

/// The TOML file may contain other sections beyond [auth].
/// Extracting the auth table first and falling back to defaults
/// if it's missing lets the config file evolve without breaking
/// auth loading.
///
fn parse(content: String) -> AuthConfig {
  case tom.parse(content) {
    Ok(toml) -> {
      case dict.get(toml, "auth") {
        Ok(auth_toml) -> parse_auth(auth_toml)
        _ -> default_config()
      }
    }
    Error(_) -> default_config()
  }
}

/// Each field has a sensible default so partial configs work —
/// users only need to override what differs from the defaults.
/// "_auth_user_id" is conventional enough that most apps won't
/// need to change it.
///
fn parse_auth(toml: tom.Toml) -> AuthConfig {
  AuthConfig(session_key: config.get_string(
    toml,
    "session_key",
    "_auth_user_id",
  ))
}

/// Centralizing defaults here ensures every fallback path
/// (missing file, parse error, missing section) produces
/// identical config so behavior is predictable regardless
/// of which error path was taken.
///
fn default_config() -> AuthConfig {
  AuthConfig(session_key: "_auth_user_id")
}

// ------------------------------------------------------------- FFI Bindings

/// Stores the config in persistent_term so every subsequent
/// call to load() across all BEAM processes gets a near-zero-
/// cost read instead of re-parsing the TOML file.
///
@external(erlang, "glimr_kernel_ffi", "cache_auth_config")
fn cache(config: AuthConfig) -> Nil

/// Returns the cached config if it exists, or Error(Nil) on
/// the first call before cache() has been called. load() uses
/// this to skip file I/O on every request after the initial
/// load.
///
@external(erlang, "glimr_kernel_ffi", "get_cached_auth_config")
fn get_cached() -> Result(AuthConfig, Nil)
