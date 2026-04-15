//// HTTP Kernel
////
//// The framework's middleware types and middleware group
//// definitions. Controllers, middleware, and the route
//// compiler reference the Next and Middleware types from here,
//// while the raw Request and Response aliases live in
//// glimr/http/http — the leaf module that wraps wisp.
////

import glimr/http/context.{type Context}
import glimr/http/middleware
import glimr/http/response.{type Response}
import wisp

// ------------------------------------------------------------- Public Types

/// Middleware functions receive a `next` callback they can call
/// to continue the chain. Naming this signature avoids
/// repeating `fn(Context(app)) -> Response` in every middleware
/// definition and makes it clear what `next` actually is when
/// you're reading middleware code.
///
@deprecated("use glimr/http/middleware.Next instead")
pub type Next(app) =
  fn(Context(app)) -> Response

/// The shape of a middleware function — takes a context and the
/// next handler in the chain. Having a named type for this
/// means the route compiler can generate middleware wiring code
/// without spelling out the full function signature every time.
///
@deprecated("use glimr/http/middleware.Middleware instead")
pub type Middleware(app) =
  fn(Context(app), middleware.Next(app)) -> Response

/// Web routes need HTML error pages and static file serving,
/// API routes need JSON errors and CORS headers — lumping them
/// together means one group gets the wrong defaults. Splitting
/// into groups lets the route compiler wire the right
/// middleware stack automatically based on what the developer
/// declared in their route annotations.
///
@deprecated("use glimr/http/middleware.MiddlewareGroup instead")
pub type MiddlewareGroup {
  Web
  Api
  Custom(String)
}

// ------------------------------------------------------------- Public Functions

/// Wisp's logger setup needs to run before the HTTP server
/// starts or you get raw Erlang crash reports instead of
/// readable request logs. Wrapping it here keeps the boot
/// sequence free of direct wisp imports.
///
@deprecated("use glimr/http/glimr_mist.configure_logger instead")
pub fn configure_logger() -> Nil {
  wisp.configure_logger()
}
