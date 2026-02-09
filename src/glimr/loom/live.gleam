//// Loom Live
////
//// WebSocket handler for Loom Live templates. Manages the WebSocket
//// connection lifecycle and message passing with live_socket actors.
////

import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/http/request.{type Request as HttpRequest}
import gleam/http/response.{type Response as HttpResponse}
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import glimr/loom/live_socket
import glimr/loom/live_types.{
  type ClientEvent, type SocketMessage, ClientEvent, SpecialVars,
}
import glimr/loom/registry
import mist.{
  type Connection, type ResponseData, type WebsocketConnection,
  type WebsocketMessage,
}

/// State held by the WebSocket handler
/// Before initialization, socket is None. After init message, socket is Some.
pub type WsState {
  WsState(
    socket: Option(Subject(SocketMessage)),
    reply_subject: Subject(SocketMessage),
  )
}

/// Init message from client with module and props
pub type InitMessage {
  InitMessage(module: String, props: String)
}

/// Upgrade an HTTP request to a WebSocket connection for Loom Live.
/// Spawns a live_socket actor to manage state and handle events.
///
pub fn upgrade(request: HttpRequest(Connection)) -> HttpResponse(ResponseData) {
  mist.websocket(
    request: request,
    on_init: on_init,
    on_close: on_close,
    handler: handle_message,
  )
}

/// Called when WebSocket connection is established.
/// Creates a reply subject but defers actor creation until init message.
///
fn on_init(
  _conn: WebsocketConnection,
) -> #(WsState, Option(process.Selector(SocketMessage))) {
  // Create a subject for receiving replies from the actor
  let reply_subject = process.new_subject()

  // Create a selector to receive messages from the reply subject
  let selector =
    process.new_selector()
    |> process.select(reply_subject)

  #(WsState(socket: None, reply_subject: reply_subject), Some(selector))
}

/// Called when WebSocket connection is closed.
/// Stops the live_socket actor if it exists.
///
fn on_close(state: WsState) -> Nil {
  case state.socket {
    Some(socket) -> process.send(socket, live_types.Stop)
    None -> Nil
  }
  Nil
}

/// Handle incoming WebSocket messages.
/// First message should be init with module/props, then event messages.
///
fn handle_message(
  state: WsState,
  message: WebsocketMessage(SocketMessage),
  conn: WebsocketConnection,
) -> mist.Next(WsState, SocketMessage) {
  case message {
    // Handle text messages from client
    mist.Text(text) -> {
      case state.socket {
        // Not yet initialized - expect init message
        None -> {
          case parse_init_message(text) {
            Ok(init) -> {
              // Validate module is registered (security check)
              case registry.is_valid_module(init.module) {
                True -> {
                  // Spawn the actor with module and props
                  case
                    live_socket.start(
                      state.reply_subject,
                      init.module,
                      init.props,
                    )
                  {
                    Ok(socket) -> {
                      mist.continue(WsState(..state, socket: Some(socket)))
                    }
                    Error(_) -> {
                      // Failed to start actor, close connection
                      mist.stop()
                    }
                  }
                }
                False -> {
                  // Module not registered, reject connection
                  mist.stop()
                }
              }
            }
            Error(_) -> {
              // Invalid init message, ignore
              mist.continue(state)
            }
          }
        }

        // Already initialized - expect event messages
        Some(socket) -> {
          case parse_client_message(text) {
            Ok(event) -> {
              // Forward event to the socket actor
              process.send(socket, live_types.Event(event))
              mist.continue(state)
            }
            Error(_) -> {
              // Invalid message, ignore
              mist.continue(state)
            }
          }
        }
      }
    }

    // Handle binary messages (not used)
    mist.Binary(_) -> {
      mist.continue(state)
    }

    // Handle custom messages from selector (responses from socket actor)
    mist.Custom(socket_msg) -> {
      case socket_msg {
        live_types.SendPatch(html) -> {
          let response =
            json.object([
              #("type", json.string("patch")),
              #("html", json.string(html)),
            ])
          let assert Ok(_) =
            mist.send_text_frame(conn, json.to_string(response))
          mist.continue(state)
        }

        live_types.SendRedirect(url) -> {
          let response =
            json.object([
              #("type", json.string("redirect")),
              #("url", json.string(url)),
            ])
          let assert Ok(_) =
            mist.send_text_frame(conn, json.to_string(response))
          mist.continue(state)
        }

        live_types.Stop -> {
          mist.stop()
        }

        _ -> mist.continue(state)
      }
    }

    // Handle closed connection
    mist.Closed -> {
      mist.stop()
    }

    // Handle shutdown
    mist.Shutdown -> {
      mist.stop()
    }
  }
}

/// Parse an init message from the client.
///
fn parse_init_message(text: String) -> Result(InitMessage, Nil) {
  let decoder = {
    use msg_type <- decode.field("type", decode.string)
    use module <- decode.field("module", decode.string)
    use props <- decode.field("props", decode.string)
    case msg_type {
      "init" -> decode.success(InitMessage(module: module, props: props))
      _ -> decode.failure(InitMessage("", ""), "init")
    }
  }

  json.parse(from: text, using: decoder)
  |> result.replace_error(Nil)
}

/// Parse an event message from the client.
///
fn parse_client_message(text: String) -> Result(ClientEvent, Nil) {
  let special_vars_decoder = {
    use value <- decode.optional_field(
      "value",
      None,
      decode.optional(decode.string),
    )
    use checked <- decode.optional_field(
      "checked",
      None,
      decode.optional(decode.bool),
    )
    use key <- decode.optional_field(
      "key",
      None,
      decode.optional(decode.string),
    )
    decode.success(SpecialVars(value: value, checked: checked, key: key))
  }

  let decoder = {
    use handler <- decode.field("handler", decode.string)
    use event <- decode.field("event", decode.string)
    use special_vars <- decode.field("special_vars", special_vars_decoder)
    decode.success(ClientEvent(
      handler: handler,
      event: event,
      special_vars: special_vars,
    ))
  }

  json.parse(from: text, using: decoder)
  |> result.replace_error(Nil)
}
