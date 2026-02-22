//// HTML Errors Middleware
////
//// Web routes that return bare status codes without a body
//// show a blank page in the browser — confusing for users
//// who can't tell a 404 from a server crash. This middleware
//// catches empty error responses and replaces them with
//// styled HTML error pages so users see a clear message and
//// developers see the status code at a glance.
////

import glimr/http/error_handler
import glimr/http/kernel.{type Next}
import wisp.{type Request, type Response}

// ------------------------------------------------------------- Public Functions

/// The use callback wraps the downstream chain so it can
/// inspect the response after handlers run. Only responses with 
/// error status codes and empty bodies are modified — handlers 
/// that already render custom error templates keep their output 
/// untouched.
///
pub fn run(req: Request, ctx: context, next: Next(context)) -> Response {
  use <- error_handler.default_html_responses()

  next(req, ctx)
}
