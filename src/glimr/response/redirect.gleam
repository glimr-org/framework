//// Redirect Helpers
////
//// Builder pattern for creating HTTP redirects with support
//// for flash messages and returning to previous pages. Use the
//// builder to construct redirects before sending.

import gleam/list
import gleam/string
import glimr/http/context.{type Context}
import glimr/http/response.{type Response}
import wisp

// ------------------------------------------------------------- Public Functions

/// Sends a temporary redirect (HTTP 303 See Other) to the
/// specified path. The location header directs the browser to
/// the new URL for this request only.
///
/// *Example:*
///
/// ```gleam
/// redirect.to("/dashboard")
/// ```
///
@deprecated("use glimr/http/response.redirect instead")
pub fn to(path: String) -> Response {
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
/// redirect.permanent("/dashboard")
/// ```
///
@deprecated("use glimr/http/response.redirect_permanent instead")
pub fn permanent(path: String) -> Response {
  wisp.permanent_redirect(normalize_path(path))
}

/// Sets the redirect path to the previous page from the Referer
/// header. Panics if no referer is found. Useful for cancel or
/// back buttons that must have a referrer.
///
/// *Example:*
/// ```gleam
/// redirect.back(ctx)
/// ```
///
@deprecated("use glimr/http/response.redirect_back instead")
pub fn back(ctx: Context(app)) -> Response {
  let assert Ok(path) =
    ctx.req.headers
    |> list.key_find("referer")

  response.redirect(path)
}

// ------------------------------------------------------------- Private Functions

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
