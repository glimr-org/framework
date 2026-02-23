//// HTTP Response Helpers
////
//// Wisp's response API is low-level — building a JSON response
//// requires serializing, setting headers, and wrapping in a
//// response manually. Controllers that repeat this for every
//// endpoint accumulate boilerplate that obscures the actual
//// response logic. These helpers reduce each response type to
//// a one-liner so controllers stay focused on what to return
//// rather than how to encode it.

import gleam/int
import gleam/json.{type Json}
import gleam/string
import simplifile
import wisp.{type Response}

// ------------------------------------------------------------- Public Constants

/// Centralizing the views path here means html_file and error
/// both resolve templates from the same root without callers
/// needing to know the directory structure. Changing this
/// constant moves all view resolution at once.
///
pub const views_path = "src/resources/views/"

// ------------------------------------------------------------- Public Types

/// Middleware like error handling and validation need to know
/// whether to produce HTML or JSON without inspecting headers
/// at every call site. Storing the format as a typed enum on
/// the request context lets downstream code branch cleanly
/// rather than parsing Accept headers repeatedly.
///
pub type ResponseFormat {
  HTML
  JSON
}

// ------------------------------------------------------------- Public Functions

/// Wraps wisp.html_response so controllers don't import wisp
/// directly for response building. Keeping all response
/// constructors in one module means swapping the underlying
/// HTTP library only requires changes here.
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

/// Loading HTML from a file keeps response content out of Gleam
/// source code, which is cleaner for large pages and lets
/// designers edit templates without touching application logic.
/// The assert panic on missing files surfaces the error
/// immediately rather than serving an empty response.
///
/// *Example:*
/// 
/// ```gleam
/// response.html_file("contact/success.html", 200)
/// ```
///
pub fn html_file(file_path: String, status: Int) -> Response {
  let path = strip_leading_slashes(file_path)
  let assert Ok(content) = simplifile.read(views_path <> path)

  wisp.html_response(content, status)
}

/// Serializing and setting the content-type in one call prevents
/// the common mistake of returning JSON with an HTML content
/// type — which causes browsers and API clients to misparse
/// the response body.
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

/// Wraps wisp.set_header so controllers can chain headers
/// using the pipe operator without importing wisp. Keeping
/// response mutation in this module maintains the same
/// abstraction boundary as the constructors above.
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
/// directory first lets developers drop in a custom 404.html
/// or 500.html that takes priority, while the framework's
/// built-in pages serve as a sensible default until custom
/// ones are created.
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
  let custom_error_page =
    simplifile.read(views_path <> "errors/" <> int.to_string(status) <> ".html")

  case custom_error_page {
    Ok(content) -> {
      html(content, status)
    }
    Error(_) -> {
      framework_html_file("errors/" <> int.to_string(status) <> ".html", status)
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// Framework-provided error pages live in priv/views/ so they
/// ship with the compiled package and are available regardless
/// of the app's directory structure. This is the fallback when
/// no custom error page exists in the app's views directory.
///
fn framework_html_file(file_path: String, status: Int) -> Response {
  let path = strip_leading_slashes(file_path)
  let assert Ok(priv_dir) = wisp.priv_directory("glimr")
  let assert Ok(content) = simplifile.read(priv_dir <> "/views/" <> path)

  wisp.html_response(content, status)
}

/// Callers may pass paths like "/errors/404.html" or
/// "errors/404.html" depending on context. Stripping the
/// leading slash normalizes both forms so the views_path
/// concatenation always produces a valid filesystem path.
///
fn strip_leading_slashes(value: String) -> String {
  case string.starts_with(value, "/") {
    True -> string.drop_start(value, 1)
    False -> value
  }
}
