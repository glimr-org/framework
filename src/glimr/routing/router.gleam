//// ------------------------------------------------------------
//// Route Matcher
//// ------------------------------------------------------------
////
//// Route matching engine that finds and executes matching routes
//// from registered route groups. Handles URL parameter
//// extraction, middleware application, and route resolution.
////

import gleam/dict.{type Dict}
import gleam/http.{type Method}
import gleam/list
import gleam/string
import glimr/http/kernel.{type Middleware, type MiddlewareGroup}
import glimr/routing/route.{type Route, type RouteGroup, type RouteRequest}
import wisp.{type Response}

// ------------------------------------------------------------- Public Types

/// ------------------------------------------------------------
/// RouteMatchError Type
/// ------------------------------------------------------------
///
/// Describes why a route match failed. This allows callers to
/// distinguish between a path not existing (404) versus a path
/// existing but the HTTP method being wrong (405).
///
pub type RouteMatchError {
  NoRouteFound
  MethodNotAllowed
}

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Apply Route Middleware
/// ------------------------------------------------------------
///
/// Recursively applies middleware to a request in order. The 
/// first middleware in the list wraps all others, so it has the 
/// final say on the response. Middleware execute in order 
/// going in, and reverse order coming out.
///
pub fn apply_middleware(
  route_req: RouteRequest,
  ctx: context,
  middleware: List(Middleware(context)),
  handler: fn(RouteRequest, context) -> Response,
) -> Response {
  case middleware {
    [] -> handler(route_req, ctx)
    [first, ..rest] -> {
      use req <- first(route_req.request, ctx)

      let req = route.RouteRequest(..route_req, request: req)
      apply_middleware(req, ctx, rest, handler)
    }
  }
}

/// ------------------------------------------------------------
/// Find Matching Route in Groups
/// ------------------------------------------------------------
///
/// Searches through route groups to find a route matching the
/// given path and HTTP method. Returns the matched route,
/// extracted URL parameters, and the middleware group. Returns
/// a specific error indicating why the match failed.
///
pub fn find_matching_route_in_groups(
  route_groups: List(RouteGroup(context)),
  path: String,
  method: Method,
) -> Result(
  #(Route(context), Dict(String, String), MiddlewareGroup),
  RouteMatchError,
) {
  do_find_in_groups(route_groups, path, method, NoRouteFound)
}

fn do_find_in_groups(
  route_groups: List(RouteGroup(context)),
  path: String,
  method: Method,
  worst_error: RouteMatchError,
) -> Result(
  #(Route(context), Dict(String, String), MiddlewareGroup),
  RouteMatchError,
) {
  case route_groups {
    [] -> Error(worst_error)
    [group, ..rest] -> {
      case find_matching_route(group.routes(), path, method) {
        Ok(#(route, params)) -> Ok(#(route, params, group.middleware_group))
        Error(MethodNotAllowed) ->
          do_find_in_groups(rest, path, method, MethodNotAllowed)
        Error(NoRouteFound) ->
          do_find_in_groups(rest, path, method, worst_error)
      }
    }
  }
}

/// ------------------------------------------------------------
/// Get All Routes
/// ------------------------------------------------------------
///
/// Extracts all routes from all route groups into a single flat
/// list. Useful for debugging, route listing, or determining if
/// a path exists (for 404 vs 405 status codes).
///
pub fn get_all_routes(
  route_groups: List(RouteGroup(context)),
) -> List(Route(context)) {
  route_groups
  |> list.flat_map(fn(group) { group.routes() })
}

/// ------------------------------------------------------------
/// Check Path Match
/// ------------------------------------------------------------
///
/// Tests if a URL path matches a route pattern. This Supports 
/// dynamic parameters like /users/{id}. Compares segment by 
/// segment, treating parameters as wildcards that match a value
///
pub fn matches_path(pattern: String, path: String) -> Bool {
  let pattern_segments = string.split(pattern, "/")
  let path_segments = string.split(path, "/")

  case list.length(pattern_segments) == list.length(path_segments) {
    False -> False
    True -> do_match_segments(pattern_segments, path_segments)
  }
}

/// ------------------------------------------------------------
/// Find Matching Route
/// ------------------------------------------------------------
///
/// Finds the first route that matches the given path and HTTP
/// method. Returns the matched route and extracted parameters,
/// or a specific error indicating why the match failed.
///
pub fn find_matching_route(
  routes: List(Route(context)),
  path: String,
  method: Method,
) -> Result(#(Route(context), Dict(String, String)), RouteMatchError) {
  // First try to find exact match (path + method)
  case
    routes
    |> list.find_map(fn(route) {
      case route.method == method && matches_path(route.path, path) {
        True -> {
          let params = extract_params(route.path, path)
          Ok(#(route, params))
        }
        False -> Error(Nil)
      }
    })
  {
    Ok(result) -> Ok(result)
    Error(_) -> {
      let path_exists =
        routes
        |> list.any(fn(route) { matches_path(route.path, path) })

      case path_exists {
        True -> Error(MethodNotAllowed)
        False -> Error(NoRouteFound)
      }
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// ------------------------------------------------------------
/// Do Match Segments
/// ------------------------------------------------------------
///
/// Recursively compares path segments, treating parameters 
/// (in curly braces) as wildcards. Both lists must have the 
/// same length and all non-parameter segments must match.
///
fn do_match_segments(
  pattern_segments: List(String),
  path_segments: List(String),
) -> Bool {
  case pattern_segments, path_segments {
    [], [] -> True
    [p, ..rest_p], [s, ..rest_s] -> {
      let matches = case is_param(p) {
        True -> True
        False -> p == s
      }
      case matches {
        True -> do_match_segments(rest_p, rest_s)
        False -> False
      }
    }
    _, _ -> False
  }
}

/// ------------------------------------------------------------
/// Is Param
/// ------------------------------------------------------------
///
/// Checks if a path segment is a parameter by testing if it's 
/// wrapped in curly braces like {id} or {user_id}.
///
fn is_param(segment: String) -> Bool {
  string.starts_with(segment, "{") && string.ends_with(segment, "}")
  // TODO: make this work with {user:id} for example, where user would be
  // the param name, but behind the scenes we can resolve the user by what comes
  // after the : (in this case id)
}

/// ------------------------------------------------------------
/// Extract Params
/// ------------------------------------------------------------
///
/// Extracts parameter values from a URL path by comparing it 
/// against the route pattern. Returns a dictionary that maps 
/// parameter names to their values from the URL.
///
fn extract_params(pattern: String, path: String) -> Dict(String, String) {
  // TODO: if param contains a : use that to extract the correct value
  // and we can possibly throw an error from here like a 404 for example
  // if we get {user:id} and the url value is "10" but a user of id 10
  // does not exist. That way it doesn't have to be handled every time
  // in the controller method

  let pattern_segments = string.split(pattern, "/")
  let path_segments = string.split(path, "/")

  do_extract_params(pattern_segments, path_segments, dict.new())
}

/// ------------------------------------------------------------
/// Do Extract Params
/// ------------------------------------------------------------
///
/// Recursively extracts parameters by walking through pattern 
/// and path segments simultaneously. When a parameter segment 
/// is found (like {id}), extracts the parameter name and stores
/// the corresponding path value.
///
fn do_extract_params(
  pattern_segments: List(String),
  path_segments: List(String),
  params: Dict(String, String),
) -> Dict(String, String) {
  case pattern_segments, path_segments {
    [], [] -> params
    [p, ..rest_p], [s, ..rest_s] -> {
      let new_params = case is_param(p) {
        True -> {
          let param_name =
            p
            |> string.drop_start(1)
            |> string.drop_end(1)
          dict.insert(params, param_name, s)
        }
        False -> params
      }
      do_extract_params(rest_p, rest_s, new_params)
    }
    _, _ -> params
  }
}
