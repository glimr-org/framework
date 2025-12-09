//// ------------------------------------------------------------
//// Route Builder
//// ------------------------------------------------------------
////
//// Core routing types and functions for defining HTTP routes
//// with type-safe parameter extraction, middleware support,
//// and route grouping capabilities.
////

import gleam/dict.{type Dict}
import gleam/http.{type Method}
import gleam/list
import gleam/result
import gleam/string
import glimr/http/kernel.{type Middleware, type MiddlewareGroup}
import wisp.{type Request, type Response}

// ------------------------------------------------------------- Public Types

/// ------------------------------------------------------------
/// RouteRequest Type
/// ------------------------------------------------------------
///
/// Wraps a Wisp request with extracted route parameters from
/// the URL path. Parameters are stored as a dictionary 
/// mapping parameter names to their string values.
///
pub type RouteRequest {
  RouteRequest(request: Request, params: Dict(String, String))
}

/// ------------------------------------------------------------
/// RouteGroup Type
/// ------------------------------------------------------------
///
/// Groups routes together with a shared middleware group which
/// determines which global middlewares are applied to all
/// routes in the group. Routes are lazily loaded with a function
/// to avoid loading unnecessary route modules. The prefix field
/// is used for efficient route matching.
///
pub type RouteGroup(context) {
  RouteGroup(
    prefix: String,
    middleware_group: MiddlewareGroup,
    routes: fn() -> List(Route(context)),
  )
}

/// ------------------------------------------------------------
/// RouteHandler Type
/// ------------------------------------------------------------
///
/// A function that handles a route request and returns a wisp 
/// response. This would typically be a controller modules 
/// function but it can be an anonymous function as well.
///
pub type RouteHandler(context) =
  fn(RouteRequest, context) -> Response

/// ------------------------------------------------------------
/// Route Type
/// ------------------------------------------------------------
///
/// Represents a single route with its HTTP method, the path 
/// pattern, handler function, middleware stack, and optional 
/// name for URL generation. Paths also can include wildcard 
/// parameters like /users/{id}.
///
pub type Route(context) {
  Route(
    method: Method,
    path: String,
    handler: RouteHandler(context),
    middleware: List(Middleware(context)),
    name: String,
  )
}

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Get Route Parameter
/// ------------------------------------------------------------
///
/// Extracts a parameter value from the route request by key.
/// Returns Ok(value) if the parameter exists, or Error(Nil)
/// if not found. Use for required parameters that must exist.
///
/// ------------------------------------------------------------
///
/// *Example:*
///
/// ```gleam
/// pub fn show(req: RouteRequest, ctx: Context) -> Response {
///   let assert Ok(user_id) = route.get_param(req, "id")
/// 
///   view.build()
///   |> view.html("users/show.html")
///   |> view.data([#("user_id", user_id)])
///   |> view.render()
/// }
/// ```
///
pub fn get_param(req: RouteRequest, key: String) -> Result(String, Nil) {
  dict.get(req.params, key)
}

/// ------------------------------------------------------------
/// Get Route Parameter with Default
/// ------------------------------------------------------------
///
/// Extracts a parameter value from the route request by key,
/// returning the provided default value if not found. Use for
/// optional parameters with fallback values.
///
/// ------------------------------------------------------------
///
/// *Example:*
///
/// ```gleam
/// pub fn index(req: RouteRequest, ctx: Context) -> Response {
///   let page = route.get_param_or(req, "page", "1")
/// 
///   view.build()
///   |> view.html("users/index.html")
///   |> view.data([#("current_page", page)])
///   |> view.render()
/// }
/// ```
///
pub fn get_param_or(req: RouteRequest, key: String, default: String) -> String {
  dict.get(req.params, key) |> result.unwrap(default)
}

/// ------------------------------------------------------------
/// GET Route Builder
/// ------------------------------------------------------------
///
/// Creates a GET route with the specified path and handler.
/// Path is normalized (adds leading slash, removes trailing 
/// slash). Supports parameters like /users/{id}.
///
/// ------------------------------------------------------------
///
/// *Example:*
/// 
/// ```gleam
/// route.get("/users", user_controller.index)
/// ```
///
pub fn get(path: String, handler: RouteHandler(context)) -> Route(context) {
  Route(
    method: http.Get,
    path: normalize_path(path),
    handler: handler,
    middleware: [],
    name: "",
  )
}

/// ------------------------------------------------------------
/// POST Route Builder
/// ------------------------------------------------------------
///
/// Creates a POST route with the specified path and handler.
/// Path is normalized (adds leading slash, removes trailing 
/// slash). Supports parameters like /users/{id}.
///
/// ------------------------------------------------------------
///
/// *Example:*
/// 
/// ```gleam
/// route.post("/users", user_controller.store)
/// ```
///
pub fn post(path: String, handler: RouteHandler(context)) -> Route(context) {
  Route(
    method: http.Post,
    path: normalize_path(path),
    handler: handler,
    middleware: [],
    name: "",
  )
}

/// ------------------------------------------------------------
/// PUT Route Builder
/// ------------------------------------------------------------
///
/// Creates a PUT route with the specified path and handler.
/// Path is normalized (adds leading slash, removes trailing 
/// slash). Supports parameters like /users/{id}.
///
/// ------------------------------------------------------------
///
/// *Example:*
/// 
/// ```gleam
/// route.put("/users/{user_id}", user_controller.update)
/// ```
///
pub fn put(path: String, handler: RouteHandler(context)) -> Route(context) {
  Route(
    method: http.Put,
    path: normalize_path(path),
    handler: handler,
    middleware: [],
    name: "",
  )
}

/// ------------------------------------------------------------
/// DELETE Route Builder
/// ------------------------------------------------------------
///
/// Creates a DELETE route with the specified path and handler.
/// Path is normalized (adds leading slash, removes trailing 
/// slash). Supports parameters like /users/{id}.
///
/// ------------------------------------------------------------
///
/// *Example:*
/// 
/// ```gleam
/// route.delete("/users/{user_id}", user_controller.delete)
/// ```
///
pub fn delete(path: String, handler: RouteHandler(context)) -> Route(context) {
  Route(
    method: http.Delete,
    path: normalize_path(path),
    handler: handler,
    middleware: [],
    name: "",
  )
}

/// ------------------------------------------------------------
/// Route Redirect Builder
/// ------------------------------------------------------------
///
/// Creates a GET route that redirects to another path. Useful
/// for handling old URLs, enforcing canonical paths, or 
/// routing users to login pages. Both old and new paths are 
/// automatically normalized.
///
/// ------------------------------------------------------------
///
/// *Example:*
/// 
/// ```gleam
/// route.redirect("/old-page", "/new-page")
/// ```
///
pub fn redirect(path: String, to: String) -> Route(context) {
  let redirect_handler = fn(_req: RouteRequest, _ctx: context) -> Response {
    wisp.redirect(normalize_path(to))
  }

  Route(
    method: http.Get,
    path: normalize_path(path),
    handler: redirect_handler,
    middleware: [],
    name: "",
  )
}

/// ------------------------------------------------------------
/// Apply Middleware to Route
/// ------------------------------------------------------------
///
/// Attaches middleware to a route. Middleware executes before
/// the handler and can modify the request or response. Use the
/// pipe operator to chain.
///
/// ------------------------------------------------------------
///
/// *Example:*
/// 
/// ```gleam
/// route.get(...)
///   |> router.middleware([logger.handle])
/// ```
///
pub fn middleware(
  route: Route(context),
  middleware: List(Middleware(context)),
) -> Route(context) {
  Route(..route, middleware: middleware)
}

/// ------------------------------------------------------------
/// Name a Route
/// ------------------------------------------------------------
///
/// Assigns a name to a route for URL generation and reference.
/// Names should be unique and descriptive, like "users.show" or
/// "admin.dashboard". Use the pipe operator to chain.
///
/// ------------------------------------------------------------
///
/// *Example:*
/// 
/// ```gleam
/// route.get(...)
///   |> router.name("users.get")
/// ```
///
pub fn name(route: Route(context), name: String) -> Route(context) {
  Route(..route, name: name)
}

/// ------------------------------------------------------------
/// Group Routes with Middleware
/// ------------------------------------------------------------
///
/// Applies middleware to multiple routes at once. Middleware
/// is prepended to any existing route-level middleware, so 
/// group middleware executes first (outermost wrapper).
///
/// ------------------------------------------------------------
///
/// *Example:*
/// 
/// ```gleam
/// route.group_middleware([logger.handle], [
///   [
///     router.get(...),
///     router.get(...),
///     router.post(...),
///   ]
/// ])
/// ```
///
pub fn group_middleware(
  middleware: List(Middleware(context)),
  routes: List(List(Route(context))),
) -> List(Route(context)) {
  use route <- list.map(list.flatten(routes))

  Route(..route, middleware: list.append(middleware, route.middleware))
}

/// ------------------------------------------------------------
/// Group Routes with Path Prefix
/// ------------------------------------------------------------
///
/// Adds a path prefix to multiple routes. Useful for versioning
/// (/v1/users) or organizing by section (/admin/users). The
/// prefix is automatically normalized.
///
/// ------------------------------------------------------------
/// 
/// *Example:*
/// 
/// ```gleam
/// route.group_path_prefix("/users", [
///   [
///     router.get(...),
///     router.get(...),
///     router.post(...),
///   ]
/// ])
/// ```
///
pub fn prefix_path(
  prefix: String,
  routes: List(List(Route(context))),
) -> List(Route(context)) {
  use route <- list.map(list.flatten(routes))

  let path = case route.path {
    "/" -> normalize_path(prefix)
    _ -> normalize_path(prefix) <> route.path
  }

  Route(..route, path: path)
}

/// ------------------------------------------------------------
/// Group Routes with Name Prefix
/// ------------------------------------------------------------
///
/// Adds a name prefix to multiple routes. This is Useful for 
/// namespacing route names like "admin." or "api.v1.". The 
/// prefix is prepended to each route's existing name.
///
/// ------------------------------------------------------------
///
/// *Example:*
/// 
/// ```gleam
/// route.group_name_prefix("users.", [
///   [
///     router.get(...),
///     router.get(...),
///     router.post(...),
///   ]
/// ])
/// ```
///
pub fn prefix_name(
  name: String,
  routes: List(List(Route(context))),
) -> List(Route(context)) {
  use route <- list.map(list.flatten(routes))

  Route(..route, name: name <> route.name)
}

// ------------------------------------------------------------- Private Functions

/// ------------------------------------------------------------
/// Normalize Path
/// ------------------------------------------------------------
///
/// Normalizes a path by ensuring it has a leading slash and no
/// trailing slash (except for the root path "/"). This ensures
/// consistent path formatting across all routes.
///
fn normalize_path(path: String) -> String {
  path
  |> ensure_leading_slash
  |> remove_trailing_slash
}

/// ------------------------------------------------------------
/// Ensure Leading Slash
/// ------------------------------------------------------------
///
/// Adds a leading slash to the path if it doesn't already have 
/// one. Returns the path unchanged if it already starts with 
/// a slash.
///
fn ensure_leading_slash(path: String) -> String {
  case string.starts_with(path, "/") {
    True -> path
    False -> "/" <> path
  }
}

/// ------------------------------------------------------------
/// Remove Trailing Slash
/// ------------------------------------------------------------
/// Removes the trailing slash from a path, except for the root 
/// path "/" which must keep its slash. This prevents paths like 
/// "/users/" from being treated differently than "/users".
///
fn remove_trailing_slash(path: String) -> String {
  case path {
    "/" -> "/"
    _ ->
      case string.ends_with(path, "/") {
        True -> string.drop_end(path, 1)
        False -> path
      }
  }
}
