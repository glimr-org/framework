//// ------------------------------------------------------------
//// Router
//// ------------------------------------------------------------
////
//// Pattern matching router with prefix-based route groups for
//// organizing routes with shared middleware. Provides type-safe
//// parameter extraction and lazy loading of route handlers
//// based on URL prefix matching.
////

import gleam/http.{type Method}
import gleam/http/response
import gleam/list
import gleam/string
import glimr/http/kernel.{type MiddlewareGroup}
import wisp.{type Request, type Response}

// ------------------------------------------------------------- Public Types

/// ------------------------------------------------------------
/// RouteGroup Type
/// ------------------------------------------------------------
///
/// Groups routes together with a shared middleware group and
/// handler function. Routes use pattern matching for type-safe
/// parameter extraction. The prefix field enables efficient
/// route matching and lazy loading of route modules.
///
pub type RouteGroup(context) {
  RouteGroup(
    prefix: String,
    middleware_group: MiddlewareGroup,
    routes: fn(List(String), Method, Request, context) -> Response,
  )
}

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Handle Request
/// ------------------------------------------------------------
///
/// Main entry point for routing HTTP requests. Matches the
/// request path against registered route groups, strips the
/// prefix, applies middleware, and calls the route handler.
///
/// Route groups are checked in order. The first matching
/// prefix wins. Empty prefix ("") acts as a catch-all and
/// should always be last in your route group list.
///
/// Flow for GET /api/users/123:
/// 1. Parse path into ["api", "users", "123"]
/// 2. Find group with prefix "/api"
/// 3. Strip prefix → ["users", "123"]
/// 4. Apply API middleware group
/// 5. Call routes(["users", "123"], Get, req, ctx)
///
pub fn handle(
  req: Request,
  ctx,
  route_groups: List(RouteGroup(ctx)),
  kernel_handle: fn(Request, ctx, MiddlewareGroup, fn(Request) -> Response) ->
    Response,
) -> Response {
  // Parse request path into segments
  // Example: "/api/users/123" → ["api", "users", "123"]
  let path_segments = wisp.path_segments(req)
  let method = req.method

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
      // Strip prefix from path before passing to handler
      // Example: "/api" prefix + ["api", "users"] → ["users"]
      let handler_path = case group.prefix {
        "" -> path_segments

        prefix -> {
          let prefix_segments =
            string.split(prefix, "/")
            |> list.filter(fn(segment) { segment != "" })

          // Drop prefix segments from path
          list.drop(path_segments, list.length(prefix_segments))
        }
      }

      // Apply group middleware then call route handler
      use req <- kernel_handle(req, ctx, group.middleware_group)
      group.routes(handler_path, method, req, ctx)
    }

    // No matching group (should never happen with catch-all)
    Error(_) -> {
      use _req <- kernel_handle(req, ctx, kernel.Web)
      response.Response(404, [], wisp.Text(""))
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// ------------------------------------------------------------
/// Starts With
/// ------------------------------------------------------------
///
/// Checks if a list starts with a given prefix. Used to match
/// URL path segments against route group prefixes.
///
/// Returns True if list begins with all elements in prefix,
/// False otherwise. Empty prefix always matches.
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
