//// ------------------------------------------------------------
//// Middleware Helper
//// ------------------------------------------------------------
////
//// Utility for applying multiple middleware functions in sequence.
//// Middleware are applied in order, with each having access to the
//// request and context, and ability to call the next middleware
//// in the chain.
////

import glimr/http/kernel.{type Middleware}
import wisp.{type Request, type Response}

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Apply Middleware
/// ------------------------------------------------------------
///
/// Applies a list of middleware functions in sequence to a 
/// request. Each middleware receives the request, context, and 
/// a 'next' function to continue the chain. Middleware execute 
/// in order: [first, second, third] → first wraps second wraps 
/// third.
///
/// This is useful when you want to apply multiple middleware to
/// a specific route without adding them to the route group's
/// global middleware stack.
///
/// ------------------------------------------------------------
///
/// *Example:*
///
/// ```gleam
/// pub fn routes(path, method, req, ctx) {
///   case path, method {
///     ["admin"], Get -> {
///       use req <- middleware.apply([auth, admin_check], req, ctx)
///       admin_controller.show(req, ctx)
///     }
///
///     ["contact"], Get -> contact_controller.show(req, ctx)
///
///     _, _ -> wisp.response(404)
///   }
/// }
/// ```
///
pub fn apply(
  middleware_list: List(Middleware(context)),
  req: Request,
  ctx: context,
  next: fn(Request) -> Response,
) -> Response {
  do_apply(middleware_list, req, ctx, next)
}

// ------------------------------------------------------------- Private Functions

/// ------------------------------------------------------------
/// Apply Middleware Recursively
/// ------------------------------------------------------------
///
/// Recursively applies middleware from the list. When the list
/// is empty, calls the final handler. Otherwise, calls the 
/// first middleware and continues with the rest of the list.
///
fn do_apply(
  middleware_list: List(Middleware(context)),
  req: Request,
  ctx: context,
  next: fn(Request) -> Response,
) -> Response {
  case middleware_list {
    [] -> next(req)

    [first, ..rest] -> {
      use updated_req <- first(req, ctx)
      do_apply(rest, updated_req, ctx, next)
    }
  }
}
