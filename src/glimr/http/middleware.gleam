//// Middleware Helper
////
//// Utility for applying multiple middleware functions in
//// sequence. Middleware are applied in order, with each having
//// access to the request and context, and ability to call the
//// next middleware in the chain.

import glimr/http/kernel.{type Middleware, type Next}
import wisp.{type Request, type Response}

// ------------------------------------------------------------- Public Functions

/// Applies a list of middleware functions in sequence to a
/// request. Each middleware receives the request, context, and
/// a 'next' function to continue the chain. Middleware can
/// modify both the request and context, with changes flowing
/// through to subsequent middleware and the final handler.
///
/// Middleware execute in order: [first, second, third] → first
/// wraps second wraps third.
///
/// This is useful when you want to apply multiple middleware to
/// a specific route without adding them to the route group's
/// global middleware stack.
///
/// *Example:*
///
/// ```gleam
/// // admin_controller.gleam
/// pub fn show(req: Request, ctx: Context) -> Response {
///   use req, ctx <- middleware.apply([auth, admin_check], req, ctx)
///
///   // handle the rest of your controller logic
/// }
/// ```
///
pub fn apply(
  middleware_list: List(Middleware(context)),
  req: Request,
  ctx: context,
  next: Next(context),
) -> Response {
  do_apply(middleware_list, req, ctx, next)
}

// ------------------------------------------------------------- Private Functions

/// Recursively applies middleware from the list. When the list
/// is empty, calls the final handler with the (potentially
/// modified) request and context. Otherwise, calls the first
/// middleware and continues with the rest of the list,
/// threading both request and context modifications through the
/// chain.
///
fn do_apply(
  middleware_list: List(Middleware(context)),
  req: Request,
  ctx: context,
  next: fn(Request, context) -> Response,
) -> Response {
  case middleware_list {
    [] -> next(req, ctx)

    [first, ..rest] -> {
      first(req, ctx, fn(updated_req, updated_ctx) {
        do_apply(rest, updated_req, updated_ctx, next)
      })
    }
  }
}
