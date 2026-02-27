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

import glimr/http/kernel.{type Next}
import wisp.{type Request, type Response}

// ------------------------------------------------------------- Public Functions

/// Rewriting the method before passing the request downstream
/// means handlers only need a GET clause to serve both GET and
/// HEAD. The use callback gives wisp a chance to strip the
/// response body after the handler runs, so Content-Length and
/// other headers remain accurate from the original GET
/// response.
///
pub fn run(req: Request, ctx: context, next: Next(context)) -> Response {
  use req <- wisp.handle_head(req)

  next(req, ctx)
}
