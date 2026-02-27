//// Glimr Mist Handler
////
//// Wisp has no built-in WebSocket support, but Loom Live needs
//// it for real-time template updates. Rather than requiring
//// users to manually wire up WebSocket routes and static file
//// serving, this drop-in replacement for wisp_mist.handler
//// transparently intercepts the Loom-specific paths (/loom/ws,
//// /loom.js) while passing everything else through to wisp
//// unchanged.
////

import gleam/bytes_tree
import gleam/http/request.{type Request as HttpRequest}
import gleam/http/response.{type Response as HttpResponse}
import gleam/list
import gleam/result
import gleam/string
import glimr/loom/live
import mist
import simplifile
import wisp
import wisp/wisp_mist

// ------------------------------------------------------------- Public Functions

/// Users shouldn't need to understand WebSocket plumbing to use
/// Loom Live — swapping one import is enough. This function
/// wraps wisp_mist.handler with a routing layer that intercepts
/// Loom paths before they reach wisp, keeping the upgrade
/// seamless and reversible.
///
/// ```gleam
/// // Before:
/// wisp_mist.handler(app.init(), config_app.key())
///
/// // After:
/// glimr_mist.handler(app.init(), config_app.key())
/// ```
///
pub fn handler(
  handler: fn(wisp.Request) -> wisp.Response,
  secret_key_base: String,
) -> fn(HttpRequest(mist.Connection)) -> HttpResponse(mist.ResponseData) {
  // Create the normal wisp handler for HTTP requests
  let wisp_handler = wisp_mist.handler(handler, secret_key_base)

  // Return a function that checks for WebSocket upgrades and loom.js first
  fn(request: HttpRequest(mist.Connection)) -> HttpResponse(mist.ResponseData) {
    let path = request.path

    case path {
      // Serve Loom JS from framework's priv/static
      "/loom.js" -> serve_loom_asset("loom.js", "application/javascript")
      // Check if this is a Loom Live WebSocket upgrade request
      "/loom/ws" ->
        case is_websocket_upgrade(request) {
          True -> live.upgrade(request)
          False -> wisp_handler(request)
        }
      // All other requests go to wisp
      _ -> wisp_handler(request)
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// Loom's JS bundle is shipped with the framework, not the
/// user's app. Serving it from the framework's priv directory
/// means users don't need to copy or manage the file — it's
/// always available and in sync with the framework version
/// they're running.
///
fn serve_loom_asset(
  filename: String,
  content_type: String,
) -> HttpResponse(mist.ResponseData) {
  let content = {
    use priv_path <- result.try(
      wisp.priv_directory("glimr") |> result.replace_error(Nil),
    )
    simplifile.read(priv_path <> "/static/" <> filename)
    |> result.replace_error(Nil)
  }

  case content {
    Ok(body) ->
      response.new(200)
      |> response.set_header("content-type", content_type)
      |> response.set_body(mist.Bytes(bytes_tree.from_string(body)))
    Error(_) ->
      response.new(404)
      |> response.set_body(
        mist.Bytes(bytes_tree.from_string(filename <> " not found")),
      )
  }
}

/// A request to /loom/ws could be a regular HTTP request (e.g.,
/// a browser navigating directly to the URL) rather than a
/// WebSocket upgrade. Checking the Connection and Upgrade
/// headers ensures we only hand off to mist's WebSocket handler
/// when the client actually wants one, falling back to wisp for
/// normal HTTP otherwise.
///
fn is_websocket_upgrade(request: HttpRequest(mist.Connection)) -> Bool {
  let result = {
    use #(_, conn) <- result.try(
      list.find(request.headers, fn(h) { string.lowercase(h.0) == "connection" }),
    )

    use #(_, upgrade) <- result.try(
      list.find(request.headers, fn(h) { string.lowercase(h.0) == "upgrade" }),
    )

    Ok(
      string.lowercase(conn) |> string.contains("upgrade")
      && string.lowercase(upgrade) == "websocket",
    )
  }

  result |> result.unwrap(False)
}
