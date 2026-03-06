//// Expects HTML Middleware
////
//// Sets the response format to HTML and configures the error
//// handler to render HTML error pages. Place this at the start
//// of your web middleware group so all downstream middleware and
//// handlers know responses should be HTML.
////

import glimr/http/context.{type Context, Context}
import glimr/http/error_handler
import glimr/http/http.{type Response}
import glimr/http/kernel.{type Next}
import glimr/response/response

// ------------------------------------------------------------- Public Functions

/// Sets the response format to HTML for this request and wraps
/// the handler with HTML error page responses.
///
pub fn run(ctx: Context(app), next: Next(app)) -> Response {
  let ctx = Context(..ctx, response_format: response.HTML)
  use <- error_handler.default_responses(response.HTML)

  next(ctx)
}
