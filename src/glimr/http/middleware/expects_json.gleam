//// Expects JSON Middleware
////
//// Sets the response format to JSON and configures the error
//// handler to return JSON error responses. Place this at the
//// start of your API middleware group so all downstream
//// middleware and handlers know responses should be JSON.
////

import glimr/http/context.{type Context, Context}
import glimr/http/error_handler
import glimr/http/http.{type Response}
import glimr/http/kernel.{type Next}
import glimr/response/response

// ------------------------------------------------------------- Public Functions

/// Sets the response format to JSON for this request and wraps
/// the handler with JSON error responses.
///
pub fn run(ctx: Context(app), next: Next(app)) -> Response {
  let ctx = Context(..ctx, response_format: response.JSON)
  use <- error_handler.default_responses(response.JSON)

  next(ctx)
}
