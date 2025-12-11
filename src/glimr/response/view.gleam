//// ------------------------------------------------------------
//// View Helpers
//// ------------------------------------------------------------
////
//// Builder pattern for rendering views with layouts and 
//// template variables. Supports both static HTML files and 
//// Lustre components with automatic variable replacement.
////

import gleam/dict.{type Dict}
import gleam/int
import gleam/string
import lustre/element.{type Element}
import simplifile
import wisp.{type Response}

// ------------------------------------------------------------- Public Types

/// ------------------------------------------------------------
/// View Type
/// ------------------------------------------------------------
///
/// View builder for constructing HTML responses with layouts.
/// Contains the content, layout template, and template 
/// variables for dynamic rendering.
///
pub type View {
  View(content: String, layout: String, data: Dict(String, String))
}

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Build View
/// ------------------------------------------------------------
///
/// Creates a new view with empty content, empty layout, and
/// empty template data. Used internally to initialize views.
///
/// ------------------------------------------------------------
///
/// *Example:*
/// 
/// ```gleam
/// view.build()
/// |> view.html("contact/success.html")
/// |> view.data([#("title", "My Page")])
/// |> view.render()
/// ```
///
pub fn build() -> View {
  View(content: "", layout: "", data: dict.from_list([]))
}

/// ------------------------------------------------------------
/// Load HTML File
/// ------------------------------------------------------------
///
/// Creates a view from a static HTML file. The file path is
/// relative to src/resources/views/ and leading slashes are
/// automatically stripped. Panics if the file doesn't exist.
///
/// ------------------------------------------------------------
///
/// *Example:*
/// 
/// ```gleam
/// view.build()
/// |> view.html("contact/success.html")
/// |> view.render()
/// ```
///
pub fn html(view: View, file_path: String) -> View {
  let path = strip_leading_slashes(file_path)
  let assert Ok(content) = simplifile.read(views_path() <> path)

  View(..view, content: content)
}

/// ------------------------------------------------------------
/// Load Raw HTML Content
/// ------------------------------------------------------------
///
/// Sets the view content directly from a string without reading
/// from a file. Useful for rendering complete HTML documents
/// or when the HTML is already loaded in memory.
///
/// ------------------------------------------------------------
///
/// *Example:*
///
/// ```gleam
/// let html = "<h1>Hello World</h1>"
/// view.build()
/// |> view.html_raw(html)
/// |> view.render()
/// ```
///
pub fn html_raw(view: View, content: String) -> View {
  View(..view, content: content)
}

/// ------------------------------------------------------------
/// Load Lustre Component
/// ------------------------------------------------------------
///
/// Creates a view from a Lustre Element by converting it to
/// an HTML string. Use for interactive components rendered
/// on the server side.
///
/// ------------------------------------------------------------
///
/// *Example:*
/// 
/// ```gleam
/// view.build()
/// |> view.lustre(contact_form.view(model))
/// |> view.render()
/// ```
///
pub fn lustre(view: View, content: Element(msg)) -> View {
  let content = element.to_string(content)

  View(..view, content: content)
}

/// ------------------------------------------------------------
/// Set Layout Template
/// ------------------------------------------------------------
///
/// Sets a custom layout template for the view. The layout path
/// is relative to src/resources/views/layouts/ and leading
/// slashes are stripped. Panics if layout file doesn't exist.
///
/// ------------------------------------------------------------
///
/// *Example:*
/// 
/// ```gleam
/// view.build()
/// |> view.html("dashboard.html")
/// |> view.layout("admin.html")
/// |> view.render()
/// ```
///
pub fn layout(view: View, layout_path: String) -> View {
  let path = strip_leading_slashes(layout_path)
  let assert Ok(layout) = simplifile.read(layouts_path() <> path)

  View(..view, layout: layout)
}

/// ------------------------------------------------------------
/// Add Template Variables
/// ------------------------------------------------------------
///
/// Adds a key-value pair to the template data. Variables are
/// replaced in the layout using {{key}} syntax. The special
/// {{_content_}} variable is reserved for the main content.
///
/// ------------------------------------------------------------
///
/// *Example:*
/// 
/// ```gleam
/// view.build()
/// |> view.html("page.html")
/// |> view.data([
///     #("title", "My Page"),
///     #("author", "John Doe"),
/// ])
/// |> view.render()
/// ```
///
pub fn data(view: View, data: List(#(String, String))) -> View {
  let data = dict.merge(view.data, dict.from_list(data))
  View(..view, data: data)
}

/// ------------------------------------------------------------
/// Render View
/// ------------------------------------------------------------
///
/// Converts the view builder into an HTTP response. Replaces
/// {{_content_}} with the content, substitutes all template
/// variables, and removes any unused {{variables}}.
///
/// ------------------------------------------------------------
///
/// *Example:*
/// 
/// ```gleam
/// view.build()
/// |> view.html("contact/form.html")
/// |> view.render()
/// ```
///
pub fn render(view: View) -> Response {
  render_with_status(view, 200)
}

/// ------------------------------------------------------------
/// Render View With Status
/// ------------------------------------------------------------
///
/// Converts the view builder into an HTTP response. Replaces
/// {{_content_}} with the content, substitutes all template
/// variables, and removes any unused {{variables}}, while also
/// allowing you to set a custom status code like 404/405
///
/// ------------------------------------------------------------
///
/// *Example:*
/// 
/// ```gleam
/// view.build()
/// |> view.html("contact/form.html")
/// |> view.render()
/// ```
///
pub fn render_with_status(view: View, status: Int) -> Response {
  let html = case view.layout {
    "" -> view.content
    _ -> view.layout |> string.replace("{{_content_}}", view.content)
  }

  let html = replace_variables(view.data, html)

  wisp.html_response(html, status)
}

/// ------------------------------------------------------------
/// Error Response
/// ------------------------------------------------------------
///
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
/// ------------------------------------------------------------
///
/// *Example:*
///
/// ```gleam
/// // Returns 404 response with custom or default error page
/// view.error_response(404, "Page Not Found")
///
/// // Returns 500 response with custom or default error page
/// view.error_response(500, "Internal Server Error")
/// ```
///
pub fn error_response(status: Int, message: String) -> Response {
  let custom_error_page =
    simplifile.read(
      views_path() <> "errors/" <> int.to_string(status) <> ".html",
    )

  case custom_error_page {
    Ok(content) -> {
      build()
      |> html_raw(content)
      |> render_with_status(status)
    }
    Error(_) -> {
      build()
      |> framework_html("error.html")
      |> data([
        #("status", int.to_string(status)),
        #("message", message),
      ])
      |> render_with_status(status)
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// ------------------------------------------------------------
/// Load Framework HTML File
/// ------------------------------------------------------------
///
/// Creates a view from an HTML file in the framework's priv
/// directory. Used internally by the framework for error pages
/// and other built-in views. The file path is relative to
/// priv/views/ and leading slashes are stripped.
///
fn framework_html(view: View, file_path: String) -> View {
  let path = strip_leading_slashes(file_path)
  let assert Ok(priv_dir) = wisp.priv_directory("glimr")
  let assert Ok(content) = simplifile.read(priv_dir <> "/views/" <> path)

  View(..view, content: content)
}

/// ------------------------------------------------------------
/// Strip Leading Slashes
/// ------------------------------------------------------------
///
/// Removes the first leading slash from a string if present.
/// Used to normalize file paths for consistent reading.
///
fn strip_leading_slashes(value: String) -> String {
  case string.starts_with(value, "/") {
    True -> string.drop_start(value, 1)
    False -> value
  }
}

/// ------------------------------------------------------------
/// Replace Variables
/// ------------------------------------------------------------
///
/// Replaces all {{key}} patterns in the HTML with their values
/// from the data dictionary. Supports both {{key}} and {{ key }}
/// syntax (with or without spaces). Strips any unused variables
/// that weren't provided.
///
fn replace_variables(data: Dict(String, String), html: String) -> String {
  let html =
    dict.fold(data, html, fn(acc, key, value) {
      acc
      |> string.replace("{{" <> key <> "}}", value)
      |> string.replace("{{ " <> key <> " }}", value)
      |> string.replace("{{ " <> key <> "}}", value)
      |> string.replace("{{" <> key <> " }}", value)
    })

  strip_unused_variables(html)
}

/// ------------------------------------------------------------
/// Strip Unused Variables
/// ------------------------------------------------------------
///
/// Recursively removes all {{variable}} patterns that weren't
/// replaced by template data. This prevents showing placeholder
/// text in the rendered output.
///
fn strip_unused_variables(html: String) -> String {
  case string.split_once(html, "{{") {
    Ok(#(before, after)) -> {
      case string.split_once(after, "}}") {
        Ok(#(_, rest)) -> before <> strip_unused_variables(rest)
        Error(_) -> html
      }
    }
    Error(_) -> html
  }
}

/// ------------------------------------------------------------
/// Views Path
/// ------------------------------------------------------------
///
/// Returns the base path for application view files. All view
/// files should be located within this directory or its
/// subdirectories.
///
fn views_path() -> String {
  "src/resources/views/"
}

/// ------------------------------------------------------------
/// Layouts Path
/// ------------------------------------------------------------
///
/// Returns the base path for application layout files. Layout
/// templates should be stored in this directory.
///
fn layouts_path() -> String {
  "src/resources/views/layouts/"
}
