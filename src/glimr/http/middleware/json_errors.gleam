//// JSON Errors Middleware
////
//// API endpoints that return bare status codes without a
//// body leave clients guessing — HTTP libraries may treat
//// an empty 404 differently than a JSON 404 with an error
//// message. This middleware catches empty error responses
//// and fills them with a structured JSON body so API
//// consumers always get machine-readable errors.
////

import glimr/http/error_handler
import glimr/http/kernel.{type Next}
import wisp.{type Request, type Response}

// ------------------------------------------------------------- Public Functions

/// The use callback wraps the downstream chain so it can
/// inspect the response after handlers run. Only responses
/// with error status codes and empty bodies are modified —
/// handlers that already set a JSON body keep their custom
/// error messages untouched.
///
pub fn run(req: Request, ctx: context, next: Next(context)) -> Response {
  use <- error_handler.default_json_responses()

  next(req, ctx)
}
