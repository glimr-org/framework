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

import gleam/http.{type Method}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/string
import gleam/string_tree.{type StringTree}
import glimr/http/request.{type Request}
import simplifile
import wisp

// ------------------------------------------------------------- Public Consts

/// Every function that reads a view file — html_file, error,
/// and anything else that loads templates — needs to agree on
/// where views live. Putting the path here means changing the
/// directory structure is a one-line fix instead of a
/// find-and-replace across the codebase.
///
pub const views_path = "src/resources/views/"

// ------------------------------------------------------------- Public Types

/// Same idea as Request — the response type is re-exported here
/// so controllers and middleware never depend on wisp directly.
/// Keeps the HTTP library as a swappable implementation detail.
///
pub type Response =
  wisp.Response

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

/// Loom templates return StringTree for efficient rendering.
/// This passes the tree directly to the response body without
/// flattening into a String first, avoiding O(n²) memory usage
/// for large pages.
///
/// *Example:*
///
/// ```gleam
/// response.loom(welcome.render(), 200)
/// ```
///
@deprecated("Use response.string_tree instead")
pub fn loom(content: StringTree, status: Int) -> Response {
  wisp.response(status)
  |> wisp.set_header("content-type", "text/html; charset=utf-8")
  |> wisp.string_tree_body(content)
}

/// You may need to return StringTree for efficient rendering.
/// This passes the tree directly to the response body without
/// flattening into a String first, avoiding O(n²) memory usage
/// for large pages.
///
/// *Example:*
///
/// ```gleam
/// response.string_tree(welcome.render(), 200)
/// ```
///
pub fn string_tree(content: StringTree, status: Int) -> Response {
  wisp.response(status)
  |> wisp.set_header("content-type", "text/html; charset=utf-8")
  |> wisp.string_tree_body(content)
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

/// Some responses don't need a body — a 204 after a successful
/// DELETE, a 401 rejection, a 301 redirect where only headers
/// matter. Forcing controllers to build an html or json
/// response in those cases just adds noise.
///
/// *Example:*
///
/// ```gleam
/// response.empty(401)
/// ```
///
pub fn empty(status: Int) -> Response {
  wisp.response(status)
}

/// The route compiler's generated dispatch code needs a 404 to
/// return when no route matches, and controllers need one when
/// a resource lookup comes up empty. Having it here keeps wisp
/// out of both — and if we ever want 404s to go through the
/// custom error page system, there's one place to change.
///
pub fn not_found() -> Response {
  wisp.not_found()
}

/// When something genuinely breaks — a database write fails, an
/// external service is down — controllers need a clean way to
/// bail out. Like the other status helpers, this keeps wisp
/// imports out of controller code so the abstraction boundary
/// holds.
///
pub fn internal_server_error() -> Response {
  wisp.internal_server_error()
}

/// When a URL matches but the HTTP method doesn't — say a POST
/// to a GET-only route — the spec requires a 405 with an Allow
/// header listing what methods actually work. The route
/// compiler generates these automatically so developers get
/// correct HTTP semantics without thinking about it.
///
pub fn method_not_allowed(allowed: List(Method)) -> Response {
  wisp.method_not_allowed(allowed)
}

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
    HTML -> default_html_responses(handle_request)
    JSON -> default_json_responses(handle_request)
  }
}

/// Sends a temporary redirect (HTTP 303 See Other) to the
/// specified path. The location header directs the browser to
/// the new URL for this request only.
///
/// *Example:*
///
/// ```gleam
/// response.redirect("/dashboard")
/// ```
///
pub fn redirect(path: String) -> Response {
  wisp.redirect(normalize_path(path))
}

/// Sends a permanent redirect (HTTP 308) to the specified path.
/// Browsers will cache this redirect and automatically use the
/// new location for all subsequent requests to the original
/// URL.
///
/// *Example:*
///
/// ```gleam
/// response.redirect_permanent("/dashboard")
/// ```
///
pub fn redirect_permanent(path: String) -> Response {
  wisp.permanent_redirect(normalize_path(path))
}

/// Sets the redirect path to the previous page from the Referer
/// header. Panics if no referer is found. Useful for cancel or
/// back buttons that must have a referrer.
///
/// *Example:*
/// ```gleam
/// response.redirect_back(ctx)
/// ```
///
pub fn redirect_back(req: Request) -> Response {
  let assert Ok(path) =
    req.headers
    |> list.key_find("referer")

  redirect(path)
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
    <> " - Error</title><link rel=\"preconnect\" href=\"https://fonts.googleapis.com\"/><link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin/><link href=\"https://fonts.googleapis.com/css2?family=Barlow:wght@400;600\" rel=\"stylesheet\"/><style>*{margin:0;padding:0;box-sizing:border-box;-webkit-font-smoothing:antialiased;-moz-osx-font-smoothing:grayscale}body{font-family:\"Barlow\",-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,\"Helvetica Neue\",Arial,sans-serif;font-size:15px;min-height:100vh;display:flex;align-items:center;justify-content:center;padding:20px;background:#f9fbfb}.container{background:white;border-radius:12px;padding:60px 40px;text-align:center;max-width:448px;width:100%;box-shadow:0 4px 6px -1px rgba(0,0,0,0.1),0 2px 4px -2px rgba(0,0,0,0.1)}.status{font-size:35px;font-weight:600;color:#22292b;line-height:1;margin-bottom:5px;letter-spacing:1px}.message{font-size:17px;color:#9ca8ab;line-height:1.6;display:inline-block}</style></head><body><div class=\"container\"><div class=\"status\">"
    <> status_str
    <> "</div><p class=\"message\">"
    <> reason
    <> "</p></div></body></html>"

  html(content, status)
}

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
    True -> error(res.status)
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
        #("error", json.string(status_reason(res.status))),
      ])
      |> json(res.status)
    False -> res
  }
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

/// Removes the final leading slash from the path if present.
/// Used to normalize file paths for consistent reading when
/// setting the location for your redirects.
///
fn normalize_path(path: String) -> String {
  case path == "/" {
    True -> path
    False -> {
      case string.ends_with(path, "/") {
        True -> string.drop_end(path, 1)
        False -> path
      }
    }
  }
}
