//// Error Handler
////
//// Handlers that return bare status codes like wisp.not_found()
//// produce empty bodies — browsers show a blank page and API
//// clients get no indication of what went wrong. This module
//// intercepts those empty error responses and fills them with
//// appropriate content based on the response format, so every
//// error path produces a useful response without handlers
//// needing to render error templates or build JSON manually.

import gleam/bool
import gleam/json
import glimr/response/response.{type ResponseFormat}
import wisp.{type Response}

// ------------------------------------------------------------- Public Functions

/// Dispatches to the HTML or JSON error handler based on the
/// response format so the kernel pipeline doesn't need separate
/// middleware for each format. Routes that serve both web and
/// API traffic can use a single middleware entry and let the
/// format flag pick the right error style automatically.
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

/// Success responses pass through untouched so handlers that
/// already render their own content are never interfered with.
/// Only specific error codes get replaced — unknown status codes
/// are left as-is so custom error handling in handlers isn't
/// overridden by the default pages.
///
fn default_html_responses(handle_request: fn() -> Response) -> Response {
  let res = handle_request()

  use <- bool.guard(
    // Return the response as is if it's not an error response.
    when: res.status >= 200 && res.status < 300,
    return: res,
  )

  case res.status {
    404 | 405 | 400 | 422 | 413 | 500 -> response.error(res.status)
    _ -> res
  }
}

/// API clients expect a consistent JSON shape for every error
/// so they can parse the response without status-code-specific
/// logic. Each status gets its own human-readable message to
/// aid debugging, while the uniform {"error": "..."} structure
/// lets client code handle all errors with one code path.
///
fn default_json_responses(handle_request: fn() -> Response) -> Response {
  let res = handle_request()

  use <- bool.guard(
    // Return the response as is if it's not an error response.
    when: res.status >= 200 && res.status < 300,
    return: res,
  )

  case res.status {
    404 ->
      json.object([
        #("error", json.string("Not Found")),
      ])
      |> response.json(res.status)

    405 ->
      json.object([
        #("error", json.string("Method Not Allowed")),
      ])
      |> response.json(res.status)

    400 ->
      json.object([
        #("error", json.string("Bad Request")),
      ])
      |> response.json(res.status)

    422 ->
      json.object([
        #("error", json.string("Bad Request")),
      ])
      |> response.json(res.status)

    413 ->
      json.object([
        #("error", json.string("Request Entity Too Large")),
      ])
      |> response.json(res.status)

    500 ->
      json.object([
        #("error", json.string("Internal Server Error")),
      ])
      |> response.json(res.status)

    _ -> res
  }
}
