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
import glimr/http/middleware.{type MiddlewareGroup}
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
  case config.get_cached("route_groups") {
    Ok(groups) -> groups
    Error(_) -> {
      let groups = load_from_config()
      config.cache("route_groups", groups)
      groups
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// Reads route group config from the unified config system.
///
fn load_from_config() -> List(RouteGroupConfig) {
  case config.get_table("route_group.groups") {
    Ok(groups_table) -> {
      groups_table
      |> dict.to_list
      |> list.map(fn(entry) {
        let #(name, group_toml) = entry
        parse_group(name, group_toml)
      })
    }
    Error(_) -> []
  }
}

/// "web" and "api" map to built-in kernel middleware pipelines
/// with sensible defaults (CSRF for web, JSON parsing for api).
/// The Custom fallback lets users define their own middleware
/// stacks without modifying the framework.
///
fn parse_group(name: String, toml: tom.Toml) -> RouteGroupConfig {
  let prefix = config.toml_get_string(toml, "prefix", "")
  let middleware_str = config.toml_get_string(toml, "middleware", "web")

  let middleware = case middleware_str {
    "web" -> middleware.Web
    "api" -> middleware.Api
    _ -> middleware.Custom(middleware_str)
  }

  RouteGroupConfig(name: name, prefix: prefix, middleware: middleware)
}
