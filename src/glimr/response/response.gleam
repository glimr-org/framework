//// HTTP Response Helpers
////
//// Provides convenience functions for building HTTP responses
//// including HTML, JSON, and error pages. Supports both inline
//// content and file-based templates.

import gleam/int
import gleam/json.{type Json}
import gleam/string
import simplifile
import wisp.{type Response}

// ------------------------------------------------------------- Public Constants

/// Returns the base path for application view files
pub const views_path = "src/resources/views/"

// ------------------------------------------------------------- Public Functions

/// Sets the view content directly from a string without reading
/// from a file. Useful for rendering complete HTML documents
/// or when the HTML is already loaded in memory.
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

/// Creates a view from a static HTML file. The file path is
/// relative to src/resources/views/ and leading slashes are
/// automatically stripped. Panics if the file doesn't exist.
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

/// Creates a JSON response from a Json value. Serializes the
/// JSON to a string and sets appropriate content-type headers
/// for JSON responses.
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

/// Adds a header to an existing response. Can be chained to
/// add multiple headers to your response. The key and value
/// are set as HTTP headers.
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

/// Generates an error response with the given HTTP status code.
/// Attempts to load a custom error page from the application's
/// src/resources/views/errors/{status}.html. If no custom page
/// exists, falls back to the framework's default error page 
/// from the framework's priv directory with the error.html 
/// layout.
///
/// This allows applications to override default error pages 
/// while maintaining consistent fallback behavior.
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

/// Creates a view from an HTML file in the framework's priv
/// directory. Used internally by the framework for error pages
/// and other built-in views. The file path is relative to
/// priv/views/ and leading slashes are stripped.
///
fn framework_html_file(file_path: String, status: Int) -> Response {
  let path = strip_leading_slashes(file_path)
  let assert Ok(priv_dir) = wisp.priv_directory("glimr")
  let assert Ok(content) = simplifile.read(priv_dir <> "/views/" <> path)

  wisp.html_response(content, status)
}

/// Removes the first leading slash from a string if present.
/// This helper function is used to normalize file paths 
/// for consistent reading.
///
fn strip_leading_slashes(value: String) -> String {
  case string.starts_with(value, "/") {
    True -> string.drop_start(value, 1)
    False -> value
  }
}
