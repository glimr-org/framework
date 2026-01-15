//// Route Definition DSL
////
//// Provides a type-safe DSL for defining HTTP routes. Routes
//// are parsed by the compiler to generate optimized dispatch
//// code with pattern matching.

import gleam/http.{type Method, Delete, Get, Head, Options, Patch, Post, Put}

// ------------------------------------------------------------- Public Types

/// Represents a route definition in the routing DSL. Can be a
/// single route with method and path, a group of routes with
/// shared prefix, or a redirect rule.
///
pub type Route {
  Route(method: Method, path: String, middleware: List(Nil))
  Group(routes: List(Route), path_prefix: String, middleware: List(Nil))
  Redirect(from: String, to: String, status: Int)
}

// ------------------------------------------------------------- Public Functions

/// Creates a GET route for the given path. The handler function
/// is captured by the compiler for code generation but not
/// stored in the route struct at runtime.
///
pub fn get(path: String, _handler: a) -> Route {
  Route(method: Get, path: path, middleware: [])
}

/// Creates a POST route for the given path. Used for form
/// submissions, API endpoints that create resources, and
/// other data-sending operations.
///
pub fn post(path: String, _handler: a) -> Route {
  Route(method: Post, path: path, middleware: [])
}

/// Creates a PUT route for the given path. Used for full
/// resource updates where the client sends the complete
/// replacement representation.
///
pub fn put(path: String, _handler: a) -> Route {
  Route(method: Put, path: path, middleware: [])
}

/// Creates a PATCH route for the given path. Used for partial
/// resource updates where only changed fields are sent rather
/// than the complete resource.
///
pub fn patch(path: String, _handler: a) -> Route {
  Route(method: Patch, path: path, middleware: [])
}

/// Creates a DELETE route for the given path. Used for
/// removing resources from the server, typically returning
/// empty response or confirmation on success.
///
pub fn delete(path: String, _handler: a) -> Route {
  Route(method: Delete, path: path, middleware: [])
}

/// Creates a HEAD route for the given path. Returns headers
/// identical to GET but without response body, useful for
/// checking resource existence or metadata.
///
pub fn head(path: String, _handler: a) -> Route {
  Route(method: Head, path: path, middleware: [])
}

/// Creates an OPTIONS route for the given path. Used for
/// CORS preflight requests and discovering allowed methods
/// on a resource endpoint.
///
pub fn options(path: String, _handler: a) -> Route {
  Route(method: Options, path: path, middleware: [])
}

/// Creates a temporary redirect from one path to another.
/// Returns HTTP 302 status code indicating the resource
/// has temporarily moved to the target location.
///
pub fn redirect(from: String, to: String) -> Route {
  Redirect(from: from, to: to, status: 302)
}

/// Creates a permanent redirect from one path to another.
/// Returns HTTP 301 status code indicating the resource
/// has permanently moved to the target location.
///
pub fn redirect_permanent(from: String, to: String) -> Route {
  Redirect(from: from, to: to, status: 301)
}

/// Attaches middleware to a route. The middleware list is
/// extracted by the compiler from source text and applied
/// during request processing. Returns route unchanged.
///
pub fn middleware(route: Route, _middleware_list: List(a)) -> Route {
  // Middleware is extracted by the compiler from source text
  // This function exists only for type-checking
  route
}

/// Groups routes under a common path prefix. All routes in
/// the group will have the prefix prepended to their paths
/// during compilation.
///
pub fn prefix(path_prefix: String, routes: List(Route)) -> Route {
  Group(routes: routes, path_prefix: path_prefix, middleware: [])
}

/// Groups routes with shared middleware. The middleware list
/// is extracted by the compiler and applied to all routes
/// within the group during request processing.
///
pub fn group_middleware(_middleware_list: List(a), routes: List(Route)) -> Route {
  // Middleware is extracted by the compiler from source text
  // and applied to all routes within this group
  Group(routes: routes, path_prefix: "", middleware: [])
}
