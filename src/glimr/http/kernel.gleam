//// HTTP Kernel
////
//// Core HTTP kernel types that define the middleware group
//// classifications. Used to determine which middleware stack
//// is applied to different types of routes.

import wisp.{type Request, type Response}

// ------------------------------------------------------------- Public Types

/// A type alias for request handler functions that accept a
/// request and context, and return a response. This is the
/// signature used by controllers and the 'next' callback in
/// middleware.
///
/// This type is commonly used when defining middleware, as the
/// 'next' parameter has this signature.
///
pub type Next(context) =
  fn(Request, context) -> Response

/// A function that intercepts requests before they reach the
/// handler. Can modify both the request and context, execute
/// logic, and modify the response. Uses the 'next' callback to
/// continue the chain with updated request and context.
///
pub type Middleware(context) =
  fn(Request, context, Next(context)) -> Response

/// Defines the type of middleware stack to apply to routes. Web
/// routes use HTML error responses and serve static files,
/// while Api routes use JSON error responses. Custom groups
/// allow for application-specific middleware configurations.
///
pub type MiddlewareGroup {
  Web
  Api
  Custom(String)
}
