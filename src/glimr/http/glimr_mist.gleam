//// Glimr Mist Handler
////
//// Wisp has no built-in WebSocket support, but Loom Live needs
//// it for real-time template updates. Rather than requiring
//// users to manually wire up WebSocket routes, this drop-in
//// replacement for wisp_mist.handler transparently intercepts
//// the Loom WebSocket path (/loom/ws) while passing everything
//// else through to wisp unchanged.
////

import gleam/http/request.{type Request as HttpRequest}
import gleam/http/response.{type Response as HttpResponse}
import gleam/list
import gleam/result
import gleam/string
import glimr/loom/live
import mist
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
