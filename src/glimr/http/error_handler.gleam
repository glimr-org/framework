//// Error Handler
////
//// Ever called `wisp.not_found()` and gotten a completely
//// blank page? That's because bare status-code responses have
//// empty bodies — browsers show nothing and API clients get no
//// clue what went wrong. This module intercepts those empty
//// error responses and fills them with real content: an HTML
//// error page for browsers, a JSON `{"error": "..."}` for
//// APIs. Handlers never need to manually render error
//// templates or build error JSON — it just happens.
////

import gleam/json
import glimr/response/response.{type ResponseFormat}
import wisp.{type Response}

// ------------------------------------------------------------- Public Functions

/// A single middleware entry handles both web and API error
/// formatting by checking the response format flag. This means
/// routes that serve both HTML and JSON traffic don't need
/// separate error middleware — the format is already known from
/// earlier in the pipeline, so we just dispatch to the right
/// handler automatically.
///
pub fn default_responses(
  response_format: ResponseFormat,
  handle_request: fn() -> Response,
) -> Response {
  case response_format {
    response.HTML -> default_html_responses(handle_request)
    response.JSON -> default_json_responses(handle_request)
  }
}

// ------------------------------------------------------------- Private Functions

/// Successful responses pass through untouched — if a handler
/// already rendered its own 200 page, we don't want to
/// interfere. But any error status (400+) gets replaced with a
/// proper error page via response.error, which checks the app's
/// views/errors/ directory for a custom template first, then
/// falls back to the framework's built-in page.
///
fn default_html_responses(handle_request: fn() -> Response) -> Response {
  let res = handle_request()

  case res.status >= 400 {
    True -> response.error(res.status)
    False -> res
  }
}

/// API clients need a predictable JSON shape for every error so
/// they can parse failures without status-code-specific logic.
/// Returning `{"error": "Not Found"}` or `{"error":
/// "Forbidden"}` lets client code handle all errors with one
/// code path instead of special-casing each status code.
///
fn default_json_responses(handle_request: fn() -> Response) -> Response {
  let res = handle_request()

  case res.status >= 400 {
    True ->
      json.object([
        #("error", json.string(response.status_reason(res.status))),
      ])
      |> response.json(res.status)
    False -> res
  }
}
