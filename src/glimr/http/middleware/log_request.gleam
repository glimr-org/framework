//// Log Request Middleware
////
//// Without centralized request logging, every handler would
//// need its own log call to track incoming requests — that's
//// both repetitive and fragile since a forgotten log means
//// blind spots in production. Placing logging as middleware
//// ensures every request is recorded regardless of which
//// handler serves it.
////

import glimr/http/context.{type Context}
import glimr/http/middleware.{type Next}
import glimr/http/response.{type Response}
import wisp

// ------------------------------------------------------------- Public Functions

/// The use callback wraps the downstream chain so the logger
/// captures both the request method and path along with the
/// response status code in one log entry. Placing it early in
/// the pipeline ensures even requests that fail in later
/// middleware steps are still logged for debugging.
///
pub fn run(ctx: Context(app), next: Next(app)) -> Response {
  use <- wisp.log_request(ctx.req)

  next(ctx)
}
