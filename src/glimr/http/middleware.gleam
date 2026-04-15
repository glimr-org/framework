//// Middleware Helper
////
//// Sometimes a single route needs extra middleware that the
//// rest of its group doesn't — auth checks on an admin page,
//// rate limiting on a login endpoint. Rather than creating a
//// whole new route group for one handler, this module lets you
//// apply a list of middleware inline with `use`.

import glimr/http/context.{type Context}
import glimr/http/response.{type Response}

// ------------------------------------------------------------- Public Types

/// Middleware functions receive a `next` callback they can call
/// to continue the chain. Naming this signature avoids
/// repeating `fn(Context(app)) -> Response` in every middleware
/// definition and makes it clear what `next` actually is when
/// you're reading middleware code.
///
pub type Next(app) =
  fn(Context(app)) -> Response

/// The shape of a middleware function — takes a context and the
/// next handler in the chain. Having a named type for this
/// means the route compiler can generate middleware wiring code
/// without spelling out the full function signature every time.
///
pub type Middleware(app) =
  fn(Context(app), Next(app)) -> Response

/// Web routes need HTML error pages and static file serving,
/// API routes need JSON errors and CORS headers — lumping them
/// together means one group gets the wrong defaults. Splitting
/// into groups lets the route compiler wire the right
/// middleware stack automatically based on what the developer
/// declared in their route annotations.
///
pub type MiddlewareGroup {
  Web
  Api
  Custom(String)
}

// ------------------------------------------------------------- Public Functions

/// Wraps a handler in multiple middleware without needing a
/// route group. Middleware execute in list order — [auth,
/// rate_limit] means auth runs first, then rate_limit, then
/// your handler. Context modifications flow through the chain
/// so auth can add user info that the handler sees.
///
/// *Example:*
///
/// ```gleam
/// // admin_controller.gleam
/// pub fn show(ctx: Context(App)) -> Response {
///   use ctx <- middleware.apply([auth, admin_check], ctx)
///
///   // handle the rest of your controller logic
/// }
/// ```
///
pub fn apply(
  middleware_list: List(Middleware(app)),
  ctx: Context(app),
  next: fn(Context(app)) -> Response,
) -> Response {
  do_apply(middleware_list, ctx, next)
}

// ------------------------------------------------------------- Private Functions

/// The recursion peels off one middleware at a time, passing
/// the rest of the chain as the `next` callback. When the list
/// is empty we've reached the actual handler. This nesting
/// means each middleware can run code both before and after the
/// handler — useful for timing, logging, or cleanup.
///
fn do_apply(
  middleware_list: List(Middleware(app)),
  ctx: Context(app),
  next: fn(Context(app)) -> Response,
) -> Response {
  case middleware_list {
    [] -> next(ctx)

    [first, ..rest] -> {
      first(ctx, fn(updated_ctx) { do_apply(rest, updated_ctx, next) })
    }
  }
}
