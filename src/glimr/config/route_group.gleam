//// Route Groups Configuration
////
//// A single monolithic route file becomes unwieldy as apps
//// grow and different URL prefixes often need different
//// middleware — API routes skip CSRF while web routes skip
//// JSON parsing. Route groups let the compiler split routes by
//// prefix into separate files, each wired to its own
//// middleware stack at compile time so there's zero runtime
//// dispatch overhead for picking the right middleware.
////

import gleam/dict
import gleam/list
import glimr/config/config
import glimr/http/kernel.{type MiddlewareGroup}
import simplifile
import tom

// ------------------------------------------------------------- Public Types

/// A typed struct so the route compiler can pattern match on
/// the middleware variant at code generation time. The prefix
/// determines which routes belong to this group, the name
/// becomes the generated module name, and the middleware
/// controls which kernel pipeline is wired in at compile time.
///
pub type RouteGroupConfig {
  RouteGroupConfig(name: String, prefix: String, middleware: MiddlewareGroup)
}

// ------------------------------------------------------------- Public Functions

/// Called by the route compiler during code generation, so it
/// must be fast on repeated calls. The persistent_term cache
/// makes subsequent calls near-zero-cost. Returning an empty
/// list on missing or invalid config lets the compiler fall
/// back to a single default group — route grouping is optional.
///
pub fn load() -> List(RouteGroupConfig) {
  case get_cached() {
    Ok(groups) -> groups
    Error(_) -> {
      let groups = load_from_file()
      cache(groups)
      groups
    }
  }
}

/// Separating file I/O from caching lets load() decide whether
/// to read the file or use the cache. Returning an empty list
/// on read failure avoids crashing when route_group.toml
/// doesn't exist — most apps won't need groups at all.
///
fn load_from_file() -> List(RouteGroupConfig) {
  case simplifile.read("config/route_group.toml") {
    Ok(content) -> parse(content)
    Error(_) -> []
  }
}

// ------------------------------------------------------------- Private Functions

/// The [groups.*] nesting lets users define multiple named
/// groups (e.g., web, api, admin) each with its own prefix and
/// middleware stack. Iterating dict.to_list preserves all
/// entries so no group definition is silently dropped.
///
fn parse(content: String) -> List(RouteGroupConfig) {
  case tom.parse(content) {
    Ok(toml) -> {
      case dict.get(toml, "groups") {
        Ok(tom.Table(groups)) -> {
          groups
          |> dict.to_list
          |> list.map(fn(entry) {
            let #(name, group_toml) = entry
            parse_group(name, group_toml)
          })
        }
        _ -> []
      }
    }
    Error(_) -> []
  }
}

/// "web" and "api" map to built-in kernel middleware pipelines
/// with sensible defaults (CSRF for web, JSON parsing for api).
/// The Custom fallback lets users define their own middleware
/// stacks without modifying the framework — the string is
/// passed through so the generated code can reference it by
/// name.
///
fn parse_group(name: String, toml: tom.Toml) -> RouteGroupConfig {
  let prefix = config.get_string(toml, "prefix", "")
  let middleware_str = config.get_string(toml, "middleware", "web")

  let middleware = case middleware_str {
    "web" -> kernel.Web
    "api" -> kernel.Api
    _ -> kernel.Custom(middleware_str)
  }

  RouteGroupConfig(name: name, prefix: prefix, middleware: middleware)
}

// ------------------------------------------------------------- FFI Bindings

/// Stores the parsed group list in persistent_term so every
/// subsequent call to load() across all BEAM processes gets a
/// near-zero-cost read instead of re-parsing the TOML file.
///
@external(erlang, "glimr_kernel_ffi", "cache_route_groups")
fn cache(groups: List(RouteGroupConfig)) -> Nil

/// Returns the cached group list if it exists, or Error(Nil) on
/// the first call before cache() has been called. load() uses
/// this to skip file I/O on every call after the initial load.
///
@external(erlang, "glimr_kernel_ffi", "get_cached_route_groups")
fn get_cached() -> Result(List(RouteGroupConfig), Nil)
