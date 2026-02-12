//// Loom Live
////
//// Live templates need a persistent bidirectional channel
//// between the browser and server to push UI updates in
//// response to user events. This module implements a
//// multiplexed WebSocket handler that routes multiple live
//// components over a single connection, tagging each message
//// with a component ID so the server dispatches to the correct
//// actor.
////

import dot_env/env
import gleam/bit_array
import gleam/bool
import gleam/crypto
import gleam/dict.{type Dict}
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

/// A single WebSocket connection multiplexes multiple live
/// components. Each component's actor is stored in a dict keyed
/// by the client-assigned component ID. The shared reply_subject
/// receives messages from all actors; the id field in each
/// message identifies which component it belongs to.
///
pub type WsState {
  WsState(
    actors: Dict(String, Subject(SocketMessage)),
    reply_subject: Subject(SocketMessage),
  )
}

/// The client sends a join message to register a new live
/// component on the multiplexed connection. The id is assigned
/// by the client, the module identifies the template, and the
/// token carries signed initial props.
///
pub type JoinMessage {
  JoinMessage(id: String, module: String, token: String)
}

/// An event message targets a specific component by id and
/// carries the handler name, event type, and special variables.
///
pub type EventMessage {
  EventMessage(id: String, event: ClientEvent)
}

/// A leave message tells the server to stop the actor for
/// the given component id.
///
pub type LeaveMessage {
  LeaveMessage(id: String)
}

// ------------------------------------------------------------- Public Functions

/// The router calls this to hand off an HTTP request to the
/// WebSocket subsystem. Wiring up on_init, on_close, and
/// handle_message here keeps the connection lifecycle in one
/// place while delegating state management to the live_socket
/// actors.
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

/// The reply subject and selector are set up once at connection
/// start. All actors share the same reply_subject — the id
/// field in each message identifies the target component. The
/// actors dict starts empty and is populated as join messages
/// arrive.
///
fn on_init(
  _conn: WebsocketConnection,
) -> #(WsState, Option(process.Selector(SocketMessage))) {
  let reply_subject = process.new_subject()

  let selector =
    process.new_selector()
    |> process.select(reply_subject)

  #(WsState(actors: dict.new(), reply_subject: reply_subject), Some(selector))
}

/// When the connection drops, all component actors must be
/// stopped to free their resources.
///
fn on_close(state: WsState) -> Nil {
  dict.each(state.actors, fn(_id, actor) {
    process.send(actor, loom.Stop)
  })
}

/// Central dispatch for all WebSocket traffic. Client text
/// frames carry join, event, or leave JSON. Actor messages
/// carry patches or redirects tagged with a component id.
///
fn handle_message(
  state: WsState,
  message: WebsocketMessage(SocketMessage),
  conn: WebsocketConnection,
) -> mist.Next(WsState, SocketMessage) {
  case message {
    mist.Text(text) -> {
      case parse_message_type(text) {
        Ok("join") -> handle_join(state, text)
        Ok("event") -> handle_event(state, text)
        Ok("leave") -> handle_leave(state, text)
        _ -> mist.continue(state)
      }
    }

    mist.Binary(_) -> mist.continue(state)

    mist.Custom(socket_msg) -> {
      case socket_msg {
        loom.SendTrees(id, tree_json) -> {
          send_raw_json(
            conn,
            "{\"type\":\"trees\",\"id\":\""
              <> id
              <> "\","
              <> string.drop_start(tree_json, 1),
          )
          mist.continue(state)
        }
        loom.SendPatch(id, diff) -> {
          send_raw_json(
            conn,
            "{\"type\":\"patch\",\"id\":\""
              <> id
              <> "\",\"d\":"
              <> diff
              <> "}",
          )
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

/// Validates the module against the registry, verifies the
/// token, and starts a new live_socket actor for this component.
/// A failed join sends an error message but does NOT kill the
/// connection — other components on the same page continue
/// working.
///
fn handle_join(
  state: WsState,
  text: String,
) -> mist.Next(WsState, SocketMessage) {
  case parse_join_message(text) {
    Error(_) -> mist.continue(state)
    Ok(join) -> {
      let result = {
        use <- bool.guard(!registry.is_valid_module(join.module), Error(Nil))

        let assert Ok(app_key) = env.get_string("APP_KEY")
        use props_json <- result.try(verify_init_token(
          join.token,
          join.module,
          app_key,
        ))

        live_socket.start(join.id, state.reply_subject, join.module, props_json)
        |> result.replace_error(Nil)
      }

      case result {
        Ok(actor) -> {
          let actors = dict.insert(state.actors, join.id, actor)
          mist.continue(WsState(..state, actors: actors))
        }
        Error(_) -> mist.continue(state)
      }
    }
  }
}

/// Looks up the actor by component id and forwards the event.
/// Unknown ids are silently ignored — the component may have
/// been removed by a concurrent leave.
///
fn handle_event(
  state: WsState,
  text: String,
) -> mist.Next(WsState, SocketMessage) {
  case parse_event_message(text) {
    Ok(msg) -> {
      case dict.get(state.actors, msg.id) {
        Ok(actor) -> process.send(actor, loom.Event(msg.event))
        Error(_) -> Nil
      }
    }
    Error(_) -> Nil
  }
  mist.continue(state)
}

/// Stops the actor for the given component and removes it from
/// the dict.
///
fn handle_leave(
  state: WsState,
  text: String,
) -> mist.Next(WsState, SocketMessage) {
  case parse_leave_message(text) {
    Ok(leave) -> {
      case dict.get(state.actors, leave.id) {
        Ok(actor) -> {
          process.send(actor, loom.Stop)
          let actors = dict.delete(state.actors, leave.id)
          mist.continue(WsState(..state, actors: actors))
        }
        Error(_) -> mist.continue(state)
      }
    }
    Error(_) -> mist.continue(state)
  }
}

/// Sends a JSON message with a type and a single key-value pair.
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
/// frame.
///
fn send_raw_json(conn: WebsocketConnection, raw_json: String) -> Nil {
  let assert Ok(_) = mist.send_text_frame(conn, raw_json)
  Nil
}

/// Extracts just the "type" field from a JSON message to
/// determine which handler to dispatch to.
///
fn parse_message_type(text: String) -> Result(String, Nil) {
  let decoder = {
    use msg_type <- decode.field("type", decode.string)
    decode.success(msg_type)
  }

  json.parse(from: text, using: decoder)
  |> result.replace_error(Nil)
}

/// Parses a join message: {type: "join", id, module, token}.
///
fn parse_join_message(text: String) -> Result(JoinMessage, Nil) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    use module <- decode.field("module", decode.string)
    use token <- decode.field("token", decode.string)
    decode.success(JoinMessage(id: id, module: module, token: token))
  }

  json.parse(from: text, using: decoder)
  |> result.replace_error(Nil)
}

/// Parses an event message: {type: "event", id, handler, event, special_vars}.
///
fn parse_event_message(text: String) -> Result(EventMessage, Nil) {
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
    use id <- decode.field("id", decode.string)
    use handler <- decode.field("handler", decode.string)
    use event <- decode.field("event", decode.string)
    use special_vars <- decode.field("special_vars", special_vars_decoder)
    decode.success(EventMessage(
      id: id,
      event: ClientEvent(
        handler: handler,
        event: event,
        special_vars: special_vars,
      ),
    ))
  }

  json.parse(from: text, using: decoder)
  |> result.replace_error(Nil)
}

/// Parses a leave message: {type: "leave", id}.
///
fn parse_leave_message(text: String) -> Result(LeaveMessage, Nil) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    decode.success(LeaveMessage(id: id))
  }

  json.parse(from: text, using: decoder)
  |> result.replace_error(Nil)
}
