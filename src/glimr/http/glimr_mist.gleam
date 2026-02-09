//// Glimr Mist Handler
////
//// Drop-in replacement for wisp_mist.handler that adds WebSocket
//// support for Loom Live templates. Intercepts WebSocket upgrade
//// requests for /loom/ws, serves /loom.js from framework priv,
//// and delegates all other requests to the normal wisp handler.
////

import gleam/bytes_tree
import gleam/http/request.{type Request as HttpRequest}
import gleam/http/response.{type Response as HttpResponse}
import gleam/list
import gleam/string
import glimr/loom/live
import mist
import simplifile
import wisp
import wisp/wisp_mist

/// Convert a Wisp request handler into a function that can be run with
/// the Mist web server, with automatic WebSocket support for Loom Live.
///
/// This is a drop-in replacement for wisp_mist.handler. Simply change:
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
      // Serve loom.js from framework's priv/static
      "/loom.js" -> serve_loom_js()
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

/// Serves the loom.js file from the framework's priv/static directory.
///
fn serve_loom_js() -> HttpResponse(mist.ResponseData) {
  // Get the framework's priv directory
  case wisp.priv_directory("glimr") {
    Ok(priv_path) -> {
      let js_path = priv_path <> "/static/loom.js"
      case simplifile.read(js_path) {
        Ok(content) ->
          response.new(200)
          |> response.set_header("content-type", "application/javascript")
          |> response.set_body(mist.Bytes(bytes_tree.from_string(content)))
        Error(_) ->
          response.new(404)
          |> response.set_body(
            mist.Bytes(bytes_tree.from_string("loom.js not found")),
          )
      }
    }
    Error(_) ->
      response.new(404)
      |> response.set_body(
        mist.Bytes(bytes_tree.from_string(
          "loom.js not found: priv directory not found",
        )),
      )
  }
}

/// Checks if a request is a WebSocket upgrade request.
///
fn is_websocket_upgrade(request: HttpRequest(mist.Connection)) -> Bool {
  let connection_header =
    request.headers
    |> list.find(fn(h) { string.lowercase(h.0) == "connection" })

  let upgrade_header =
    request.headers
    |> list.find(fn(h) { string.lowercase(h.0) == "upgrade" })

  case connection_header, upgrade_header {
    Ok(#(_, conn_value)), Ok(#(_, upgrade_value)) -> {
      let has_upgrade =
        conn_value
        |> string.lowercase
        |> string.contains("upgrade")

      let is_websocket = string.lowercase(upgrade_value) == "websocket"

      has_upgrade && is_websocket
    }
    _, _ -> False
  }
}
