//// Log Request Middleware
////
//// Without centralized request logging, every handler
//// would need its own log call to track incoming requests
//// — that's both repetitive and fragile since a forgotten
//// log means blind spots in production. Placing logging as
//// middleware ensures every request is recorded regardless
//// of which handler serves it.
////

import glimr/http/kernel.{type Next}
import wisp.{type Request, type Response}

// ------------------------------------------------------------- Public Functions

/// The use callback wraps the downstream chain so the logger
/// captures both the request method and path along with the
/// response status code in one log entry. Placing it early in
/// the pipeline ensures even requests that fail in later
/// middleware steps are still logged for debugging.
///
pub fn run(req: Request, ctx: context, next: Next(context)) -> Response {
  use <- wisp.log_request(req)

  next(req, ctx)
}
