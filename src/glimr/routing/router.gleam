//// Router
////
//// Web apps and API endpoints need different middleware — HTML
//// error pages vs JSON responses, static file serving vs CORS
//// headers. Route groups let you split routes by prefix (e.g.
//// "/api" vs everything else) and wire each group to the right
//// middleware stack automatically.

import gleam/http.{type Method}
import gleam/http/response
import gleam/list
import gleam/string
import glimr/http/context.{type Context}
import glimr/http/middleware.{type MiddlewareGroup}
import glimr/http/response.{type Response} as _glimr_response
import glimr/routing/route_group
import wisp

/// Each group bundles a URL prefix, a middleware stack, and the
/// compiled route handler for that section of the app. The
/// prefix enables fast matching — "/api/users/123" checks if
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

// ------------------------------------------------------------- Public Functions

/// The main request entry point. Splits the URL into segments,
/// finds the first route group whose prefix matches, applies
/// that group's middleware, and calls its route handler. Groups
/// are checked in order — put specific prefixes like "/api"
/// before the catch-all "" so they get a chance to match.
///
pub fn handle(
  ctx: Context(app),
  route_groups: List(RouteGroup(Context(app))),
  kernel_handle: fn(Context(app), MiddlewareGroup, fn(Context(app)) -> Response) ->
    Response,
) -> Response {
  // Parse request path into segments
  // Example: "/api/users/123" → ["api", "users", "123"]
  let path_segments = wisp.path_segments(ctx.req)
  let method = ctx.req.method

  // Find first route group whose prefix matches the path
  // Empty prefix ("") matches everything, so put it last
  let matching_group =
    list.find(route_groups, fn(group) {
      case group.prefix {
        // Empty prefix is a catch-all
        "" -> True

        // Check if path starts with prefix segments
        prefix -> {
          // Convert "/api" → ["api"]
          let prefix_segments =
            string.split(prefix, "/")
            |> list.filter(fn(segment) { segment != "" })

          // Check if ["api", "users"] starts with ["api"]
          starts_with(path_segments, prefix_segments)
        }
      }
    })

  case matching_group {
    Ok(group) -> {
      // Apply group middleware then call route handler with full path
      use ctx <- kernel_handle(ctx, group.middleware_group)
      group.routes(path_segments, method, ctx)
    }

    // No matching group (should never happen with catch-all)
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
  route_group.load()
  |> list.map(fn(group_config) {
    RouteGroup(
      prefix: group_config.prefix,
      middleware_group: group_config.middleware,
      routes: routes_for(group_config.name),
    )
  })
}

// ------------------------------------------------------------- Private Functions

/// Gleam doesn't have a built-in list.starts_with, so this
/// recursively checks if the path segments begin with the
/// prefix segments. An empty prefix always matches — that's the
/// catch-all behavior the router relies on.
///
fn starts_with(list: List(a), prefix: List(a)) -> Bool {
  case list, prefix {
    // Empty prefix always matches
    _, [] -> True

    // Prefix has items but list is empty
    [], _ -> False

    // Both have items - check if first items match
    [h1, ..t1], [h2, ..t2] ->
      case h1 == h2 {
        // First items match, check rest recursively
        True -> starts_with(t1, t2)

        // First items don't match
        False -> False
      }
  }
}
