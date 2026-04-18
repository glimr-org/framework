//// Router
////
//// Web apps and API endpoints need different middleware --
//// HTML error pages vs JSON responses, static file serving vs
//// CORS headers. Route groups let you split routes by prefix
//// (e.g. "/api" vs everything else) and wire each group to the
//// right middleware stack automatically.

import gleam/dict
import gleam/http.{type Method}
import gleam/http/response
import gleam/list
import gleam/string
import glimr/config
import glimr/http/context.{type Context}
import glimr/http/middleware.{type MiddlewareGroup}
import glimr/http/response.{type Response} as _glimr_response
import tom
import wisp

// ------------------------------------------------------------- Public Types

/// Each group bundles a URL prefix, a middleware stack, and the
/// compiled route handler for that section of the app. The
/// prefix enables fast matching -- "/api/users/123" checks if
/// the path starts with "/api" before even looking at the route
/// table, and lazily loads only the matching module.
///
pub type RouteGroup(context) {
  RouteGroup(
    prefix: String,
    middleware_group: MiddlewareGroup,
    routes: fn(List(String), Method, context) -> Response,
  )
}

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

/// The main request entry point. Splits the URL into segments,
/// finds the first route group whose prefix matches, applies
/// that group's middleware, and calls its route handler. Groups
/// are checked in order -- put specific prefixes like "/api"
/// before the catch-all "" so they get a chance to match.
///
pub fn handle(
  ctx: Context(app),
  route_groups: List(RouteGroup(Context(app))),
  kernel_handle: fn(Context(app), MiddlewareGroup, fn(Context(app)) -> Response) ->
    Response,
) -> Response {
  let path_segments = wisp.path_segments(ctx.req)
  let method = ctx.req.method

  let matching_group =
    list.find(route_groups, fn(group) {
      case group.prefix {
        "" -> True
        prefix -> {
          let prefix_segments =
            string.split(prefix, "/")
            |> list.filter(fn(segment) { segment != "" })
          starts_with(path_segments, prefix_segments)
        }
      }
    })

  case matching_group {
    Ok(group) -> {
      use ctx <- kernel_handle(ctx, group.middleware_group)
      group.routes(path_segments, method, ctx)
    }
    Error(_) -> {
      use _ctx <- kernel_handle(ctx, middleware.Web)
      response.Response(404, [], wisp.Text(""))
    }
  }
}

/// Route groups are defined in config/route_group.toml so
/// adding a new API version or admin section is a config
/// change, not a code change. The `routes_for` callback maps
/// each group name to its compiled route handler so the router
/// knows which module handles which prefix.
///
pub fn load(
  routes_for: fn(String) -> fn(List(String), Method, context) -> Response,
) -> List(RouteGroup(context)) {
  load_group_configs()
  |> list.map(fn(group_config) {
    RouteGroup(
      prefix: group_config.prefix,
      middleware_group: group_config.middleware,
      routes: routes_for(group_config.name),
    )
  })
}

// ------------------------------------------------------------- Internal Public Functions

/// Loads route group configs from config/route_group.toml.
/// Called by the route compiler during code generation, so it
/// must be fast on repeated calls. The persistent_term cache
/// makes subsequent calls near-zero-cost.
///
@internal
pub fn load_group_configs() -> List(RouteGroupConfig) {
  case config.get_cached("route_groups") {
    Ok(groups) -> groups
    Error(_) -> {
      let groups = load_configs_from_file()
      config.cache("route_groups", groups)
      groups
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// Gleam doesn't have a built-in list.starts_with, so this
/// recursively checks if the path segments begin with the
/// prefix segments. An empty prefix always matches -- that's
/// the catch-all behavior the router relies on.
///
fn starts_with(list: List(a), prefix: List(a)) -> Bool {
  case list, prefix {
    _, [] -> True
    [], _ -> False
    [h1, ..t1], [h2, ..t2] ->
      case h1 == h2 {
        True -> starts_with(t1, t2)
        False -> False
      }
  }
}

/// Reads config/route_group.toml and turns each [groups.x]
/// section into a RouteGroupConfig. Returns an empty list when
/// no config exists so apps without route groups still boot
/// cleanly with a single catch-all group.
///
fn load_configs_from_file() -> List(RouteGroupConfig) {
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
