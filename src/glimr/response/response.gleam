//// HTTP Response Helpers
////
//// Wisp's response API is low-level — building a JSON response
//// means serializing, setting headers, and wrapping in a
//// response struct manually. Controllers that repeat this for
//// every endpoint accumulate boilerplate that obscures what
//// the handler is actually trying to return. These helpers
//// reduce each response type to a one-liner so controllers
//// stay focused on the data, not the encoding.
////

import gleam/int
import gleam/json.{type Json}
import gleam/string
import simplifile
import wisp.{type Response}

// ------------------------------------------------------------- Public Consts

/// Every function that reads a view file — html_file, error,
/// and anything else that loads templates — needs to agree on
/// where views live. Putting the path here means changing the
/// directory structure is a one-line fix instead of a
/// find-and-replace across the codebase.
///
pub const views_path = "src/resources/views/"

// ------------------------------------------------------------- Public Types

/// The error handler middleware needs to know whether to render
/// an HTML error page or a JSON `{"error": "..."}`, and
/// checking Accept headers at every call site would be tedious
/// and error-prone. Storing the format as a typed enum on the
/// request context lets downstream code branch cleanly with a
/// single pattern match.
///
pub type ResponseFormat {
  HTML
  JSON
}

// ------------------------------------------------------------- Public Functions

/// Controllers shouldn't need to import wisp just to return
/// some HTML. Keeping all response constructors in this module
/// means the underlying HTTP library is only referenced here —
/// if we ever swap wisp for something else, controllers don't
/// change at all.
///
/// *Example:*
///
/// ```gleam
/// let html = "<h1>Hello World</h1>"
/// response.html(html, 200)
/// ```
///
pub fn html(content: String, status: Int) -> Response {
  wisp.html_response(content, status)
}

/// Loading HTML from a file keeps large page content out of
/// Gleam source code, which is cleaner for big pages and lets
/// designers edit templates without touching application logic.
/// The assert panics on missing files immediately rather than
/// serving an empty response — a typo in a template path shows
/// up the first time you hit the route, not as a silent blank
/// page in production.
///
/// *Example:*
/// ```gleam
/// response.html_file("contact/success.html", 200)
/// ```
///
pub fn html_file(file_path: String, status: Int) -> Response {
  let path = strip_leading_slashes(file_path)
  let assert Ok(content) = simplifile.read(views_path <> path)

  wisp.html_response(content, status)
}

/// Serializing and setting the content-type in one call
/// prevents the common mistake of returning JSON with an HTML
/// content type — which causes browsers and API clients to
/// misparse the response body.
///
/// *Example:*
///
/// ```gleam
/// json.string("This is a json response!")
/// |> response.json(200)
///
/// json.object([
///   #("name", json.string("John Doe")),
///   #("email", json.string("johndoe@email.com")),
///   #("age", json.int(30)),
/// ])
/// |> response.json(200)
/// ```
///
pub fn json(json: Json, status: Int) -> Response {
  json.to_string(json)
  |> wisp.json_response(status)
}

/// Lets controllers chain headers with the pipe operator
/// without importing wisp. This matters because once you import
/// wisp in a controller for headers, it's tempting to use
/// wisp's response functions directly too — and then the
/// abstraction boundary this module provides falls apart.
///
/// *Example:*
///
/// ```gleam
/// response.html("\"This is actually json\"", 200)
/// |> response.header("content-type", "application/json")
/// ```
///
pub fn header(response: Response, key: String, value: String) -> Response {
  response
  |> wisp.set_header(key, value)
}

/// Apps should be able to brand their error pages without
/// modifying framework code. Checking the app's views/errors/
/// directory first lets developers drop in a custom 404.html or
/// 500.html that takes priority, while the framework's built-in
/// generic page serves as a sensible default until custom ones
/// are created.
///
/// *Example:*
///
/// ```gleam
/// // Returns 404 response with custom or default error page
/// response.error(404)
///
/// // Returns 500 response with custom or default error page
/// response.error(500)
/// ```
///
pub fn error(status: Int) -> Response {
  let file_name = "errors/" <> int.to_string(status) <> ".html"

  case simplifile.read(views_path <> file_name) {
    Ok(content) -> html(content, status)
    Error(_) -> generic_error_page(status)
  }
}

/// Showing users "404" alone means nothing to most people, but
/// "Not Found" immediately tells them what happened. The error
/// handler middleware uses this for JSON error messages, and
/// the generic error page uses it for HTML — keeping the lookup
/// here means both formats always agree on what to call each
/// status code.
///
pub fn status_reason(status: Int) -> String {
  case status {
    400 -> "Bad Request"
    401 -> "Unauthorized"
    403 -> "Forbidden"
    404 -> "Not Found"
    405 -> "Method Not Allowed"
    408 -> "Request Timeout"
    409 -> "Conflict"
    410 -> "Gone"
    413 -> "Payload Too Large"
    415 -> "Unsupported Media Type"
    422 -> "Unprocessable Entity"
    429 -> "Too Many Requests"
    500 -> "Internal Server Error"
    502 -> "Bad Gateway"
    503 -> "Service Unavailable"
    504 -> "Gateway Timeout"
    _ -> "Error"
  }
}

// ------------------------------------------------------------- Private Functions

/// When a developer hasn't created a custom error page yet —
/// which is most of the time during early development — this
/// gives them something presentable instead of a blank screen
/// or the browser's ugly default. It's intentionally minimal so
/// it doesn't look broken, just unbranded, nudging them to add
/// their own page when they're ready.
///
fn generic_error_page(status: Int) -> Response {
  let status_str = int.to_string(status)
  let reason = status_reason(status)

  let content =
    "<!doctype html><html lang=\"en\"><head><meta charset=\"UTF-8\"/><meta name=\"viewport\" content=\"width=device-width,initial-scale=1.0\"/><title>"
    <> status_str
    <> " - Error</title><link rel=\"preconnect\" href=\"https://fonts.googleapis.com\"/><link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin/><link href=\"https://fonts.googleapis.com/css2?family=Inter:ital,opsz,wght@0,14..32,100..900;1,14..32,100..900\" rel=\"stylesheet\"/><style>*{margin:0;padding:0;box-sizing:border-box;-webkit-font-smoothing:antialiased;-moz-osx-font-smoothing:grayscale}body{font-family:\"Inter\",-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,\"Helvetica Neue\",Arial,sans-serif;min-height:100vh;display:flex;align-items:center;justify-content:center;padding:20px;background:#fafafa}.container{background:white;border-radius:12px;padding:60px 40px;text-align:center;max-width:500px;width:100%;box-shadow:0 1px 3px rgba(0,0,0,0.1)}.status{font-size:32px;font-weight:700;color:#181818;line-height:1;margin-bottom:5px;letter-spacing:1px}.message{font-size:16px;color:#999;line-height:1.6;display:inline-block}</style></head><body><div class=\"container\"><div class=\"status\">"
    <> status_str
    <> "</div><p class=\"message\">"
    <> reason
    <> "</p></div></body></html>"

  wisp.html_response(content, status)
}

/// Callers pass paths like "/errors/404.html" or
/// "errors/404.html" depending on context — some include a
/// leading slash from URL routing, some don't. Stripping it
/// here normalizes both forms so the views_path concatenation
/// always produces a valid filesystem path instead of doubling
/// up slashes.
///
fn strip_leading_slashes(value: String) -> String {
  case string.starts_with(value, "/") {
    True -> string.drop_start(value, 1)
    False -> value
  }
}
