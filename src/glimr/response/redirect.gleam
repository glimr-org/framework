//// Redirect Helpers
////
//// Builder pattern for creating HTTP redirects with support
//// for flash messages and returning to previous pages. Use the
//// builder to construct redirects before sending.

import gleam/list
import gleam/string
import wisp.{type Request, type Response}

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
pub fn permanent(path: String) -> Response {
  wisp.permanent_redirect(normalize_path(path))
}

/// Sets the redirect path to the previous page from the Referer
/// header. Panics if no referer is found. Useful for cancel or
/// back buttons that must have a referrer.
///
/// *Example:*
/// ```gleam
/// redirect.back(req.request)
/// ```
///
pub fn back(req: Request) -> Response {
  let assert Ok(path) =
    req.headers
    |> list.key_find("referer")

  to(path)
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
