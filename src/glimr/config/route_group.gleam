//// Route Groups Configuration
////
//// Enables splitting compiled routes into separate files by URL
//// prefix (e.g., /api/* vs /admin/*). This keeps generated route
//// modules focused and allows different middleware stacks per
//// group without runtime overhead.
////

import gleam/dict
import gleam/list
import glimr/config/config
import glimr/http/kernel.{type MiddlewareGroup}
import simplifile
import tom

// ------------------------------------------------------------- Public Types

/// Compile-time configuration for grouping routes by URL prefix.
/// The route compiler uses these to split routes into separate
/// output files, each with its own middleware stack applied at
/// compile time rather than runtime.
///
pub type RouteGroupConfig {
  RouteGroupConfig(name: String, prefix: String, middleware: MiddlewareGroup)
}

// ------------------------------------------------------------- Public Functions

/// Safe to call repeatedly since results are cached after first
/// load. Returns empty list on missing/invalid config so the
/// route compiler can fall back to a single default group.
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

/// Separated from load() to keep caching logic distinct from
/// file I/O. This makes the caching behavior easier to test
/// and reason about independently.
///
fn load_from_file() -> List(RouteGroupConfig) {
  case simplifile.read("config/route_group.toml") {
    Ok(content) -> parse(content)
    Error(_) -> []
  }
}

// ------------------------------------------------------------- Private Functions

/// Expects [groups.*] tables so users can define multiple named
/// groups (e.g., web, api, admin) with different prefixes and
/// middleware stacks applied at compile time.
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

/// Maps middleware string to MiddlewareGroup variant. Supports
/// "web" and "api" as built-in groups, with Custom fallback for
/// user-defined middleware stacks without framework changes.
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

/// Stores parsed groups in persistent_term for fast access 
/// across all processes. Avoids re-parsing TOML on every route 
/// compile which would add unnecessary latency and I/O.
///
@external(erlang, "glimr_kernel_ffi", "cache_route_groups")
fn cache(groups: List(RouteGroupConfig)) -> Nil

/// Retrieves cached groups from persistent_term. Returns Error
/// if not yet cached, signaling that load_from_file() should be
/// called to populate the cache.
///
@external(erlang, "glimr_kernel_ffi", "get_cached_route_groups")
fn get_cached() -> Result(List(RouteGroupConfig), Nil)
