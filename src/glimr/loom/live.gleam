//// Loom Live
////
//// Live templates need a persistent bidirectional channel
//// between the browser and server to push UI updates in
//// response to user events. This module implements the
//// WebSocket handler that bridges mist's connection lifecycle 
//// with the live_socket actor, translating raw JSON frames 
//// into typed messages and routing actor responses back as 
//// WebSocket text frames.
////

import dot_env/env
import gleam/bit_array
import gleam/bool
import gleam/crypto
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/http/request.{type Request as HttpRequest}
import gleam/http/response.{type Response as HttpResponse}
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import glimr/loom/live_socket
import glimr/loom/loom.{
  type ClientEvent, type SocketMessage, ClientEvent, SpecialVars,
}
import glimr/loom/registry
import mist.{
  type Connection, type ResponseData, type WebsocketConnection,
  type WebsocketMessage,
}

// ------------------------------------------------------------- Public Types

/// The WebSocket connection has two phases: before the client
/// sends an init message (socket is None) and after (socket is 
/// Some). Tracking both the actor subject and the reply subject 
/// lets the handler forward events to the actor and receive 
/// patches back over the same connection.
///
pub type WsState {
  WsState(
    socket: Option(Subject(SocketMessage)),
    reply_subject: Subject(SocketMessage),
  )
}

/// The client must identify which template module to run and
/// provide the initial props as JSON. A typed init message
/// ensures both fields are present before the handler attempts 
/// to start a live_socket actor, avoiding partial 
/// initialization.
///
pub type InitMessage {
  InitMessage(module: String, token: String)
}

// ------------------------------------------------------------- Public Functions

/// The router calls this to hand off an HTTP request to the
/// WebSocket subsystem. Wiring up on_init, on_close, and
/// handle_message here keeps the connection lifecycle in one 
/// place while delegating state management to the live_socket 
/// actor.
///
pub fn upgrade(request: HttpRequest(Connection)) -> HttpResponse(ResponseData) {
  mist.websocket(
    request: request,
    on_init: on_init,
    on_close: on_close,
    handler: handle_message,
  )
}

/// Verifies a signed init token and extracts the props JSON.
/// The token contains `module_name:props_json` signed with the
/// app key. Verification ensures the signature is valid and the
/// embedded module name matches the claimed module, preventing
/// token reuse across modules or tampered props.
///
pub fn verify_init_token(
  token: String,
  module_name: String,
  app_key: String,
) -> Result(String, Nil) {
  use payload_bits <- result.try(
    crypto.verify_signed_message(token, <<app_key:utf8>>)
    |> result.replace_error(Nil),
  )

  use payload <- result.try(
    bit_array.to_string(payload_bits) |> result.replace_error(Nil),
  )

  use #(token_module, props_json) <- result.try(
    string.split_once(payload, ":") |> result.replace_error(Nil),
  )

  use <- bool.guard(token_module != module_name, Error(Nil))

  Ok(props_json)
}

// ------------------------------------------------------------- Private Functions

/// Actor creation is deferred until the client sends an init
/// message with module and props because the WebSocket opens 
/// before we know which template to run. The reply subject and 
/// selector are set up immediately so the handler is ready to 
/// receive actor messages once started.
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

/// When the browser navigates away or the connection drops, the 
/// live_socket actor must be stopped to free its resources. 
/// Checking for Some guards against the case where the 
/// connection closes before the init message was received.
///
fn on_close(state: WsState) -> Nil {
  case state.socket {
    Some(socket) -> process.send(socket, loom.Stop)
    None -> Nil
  }
}

/// Central dispatch for all WebSocket traffic. Client text
/// frames carry either init or event JSON, actor messages carry 
/// patches or redirects to send back. Routing both directions 
/// through one handler keeps the protocol logic cohesive and 
/// the state transitions explicit.
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
        None -> handle_init(state, text)

        // Already initialized - expect event messages
        Some(socket) -> {
          case parse_client_message(text) {
            Ok(event) -> process.send(socket, loom.Event(event))
            Error(_) -> Nil
          }
          mist.continue(state)
        }
      }
    }

    mist.Binary(_) -> mist.continue(state)

    // Handle messages from the socket actor
    mist.Custom(socket_msg) -> {
      case socket_msg {
        loom.SendTrees(tree_json) -> {
          send_raw_json(conn, "{\"type\":\"trees\"," <> string.drop_start(tree_json, 1))
          mist.continue(state)
        }
        loom.SendPatch(diff) -> {
          send_raw_json(conn, "{\"type\":\"patch\",\"d\":" <> diff <> "}")
          mist.continue(state)
        }
        loom.SendRedirect(url) -> {
          send_json(conn, "redirect", "url", url)
          mist.continue(state)
        }
        loom.Stop -> mist.stop()
        _ -> mist.continue(state)
      }
    }

    mist.Closed | mist.Shutdown -> mist.stop()
  }
}

/// The first client message must be an init with a valid module 
/// name. Validating against the registry prevents arbitrary 
/// module execution, and starting the actor only on a 
/// successful init avoids orphaned actors from malformed or 
/// malicious init attempts.
///
fn handle_init(
  state: WsState,
  text: String,
) -> mist.Next(WsState, SocketMessage) {
  case parse_init_message(text) {
    Error(_) -> mist.continue(state)
    Ok(init) -> {
      let result = {
        use <- bool.guard(!registry.is_valid_module(init.module), Error(Nil))

        let assert Ok(app_key) = env.get_string("APP_KEY")
        use props_json <- result.try(verify_init_token(
          init.token,
          init.module,
          app_key,
        ))

        live_socket.start(state.reply_subject, init.module, props_json)
        |> result.replace_error(Nil)
      }

      case result {
        Ok(socket) -> mist.continue(WsState(..state, socket: Some(socket)))
        Error(_) -> mist.stop()
      }
    }
  }
}

/// The client-side JS runtime expects JSON frames with a "type" 
/// field and a payload field. Centralizing the serialization 
/// here ensures all outbound messages share the same envelope 
/// format, making the client protocol parser straightforward.
///
fn send_json(
  conn: WebsocketConnection,
  msg_type: String,
  key: String,
  value: String,
) -> Nil {
  let response =
    json.object([#("type", json.string(msg_type)), #(key, json.string(value))])
  let assert Ok(_) = mist.send_text_frame(conn, json.to_string(response))
  Nil
}

/// Sends a pre-built JSON string directly as a WebSocket text
/// frame. Used for tree and patch messages where the JSON is
/// already constructed by the runtime.
///
fn send_raw_json(conn: WebsocketConnection, raw_json: String) -> Nil {
  let assert Ok(_) = mist.send_text_frame(conn, raw_json)
  Nil
}

/// Init messages must be validated before starting an actor
/// because the module name drives dynamic dispatch. Parsing
/// into a typed InitMessage with a type-field check rejects
/// non-init messages early without attempting actor creation.
///
fn parse_init_message(text: String) -> Result(InitMessage, Nil) {
  let decoder = {
    use msg_type <- decode.field("type", decode.string)
    use module <- decode.field("module", decode.string)
    use token <- decode.field("token", decode.string)
    case msg_type {
      "init" -> decode.success(InitMessage(module: module, token: token))
      _ -> decode.failure(InitMessage("", ""), "init")
    }
  }

  json.parse(from: text, using: decoder)
  |> result.replace_error(Nil)
}

/// Event messages carry handler ID, event type, and optional
/// special variables ($value, $checked, $key). Parsing them
/// into a typed ClientEvent ensures the live_socket actor
/// receives well-formed data and can pattern match on fields 
/// without runtime JSON access.
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
