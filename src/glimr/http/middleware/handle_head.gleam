//// Handle HEAD Middleware
////
//// The HTTP spec requires HEAD responses to return the same
//// headers as GET but with no body. Without this middleware,
//// every handler would need explicit HEAD pattern matches that
//// duplicate GET logic and strip the body — easy to forget and
//// tedious to maintain. Converting HEAD to GET and stripping
//// the body after the handler runs keeps this concern
//// centralized.
////

import glimr/http/context.{type Context, Context}
import glimr/http/middleware.{type Next}
import glimr/http/response.{type Response}
import wisp

// ------------------------------------------------------------- Public Functions

/// Rewriting the method before passing the request downstream
/// means handlers only need a GET clause to serve both GET and
/// HEAD. The use callback gives wisp a chance to strip the
/// response body after the handler runs, so Content-Length and
/// other headers remain accurate from the original GET
/// response.
///
pub fn run(ctx: Context(app), next: Next(app)) -> Response {
  use req <- wisp.handle_head(ctx.req)

  next(Context(..ctx, req: req))
}
