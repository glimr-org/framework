//// Loom Live Socket
////
//// OTP actor that manages state for a single live template connection.
//// Receives events from the WebSocket handler, dispatches to compiled
//// handlers, and sends back HTML patches or redirects.
////

import gleam/erlang/process.{type Subject}
import gleam/option
import gleam/otp/actor
import glimr/loom/live_dispatch
import glimr/loom/live_types.{type SocketMessage, Event, SendPatch, Stop}

/// State held by the live socket actor
pub type LiveSocketState {
  LiveSocketState(
    /// Subject to send responses back to the WebSocket handler
    reply_to: Subject(SocketMessage),
    /// Module name for dynamic dispatch (e.g., "compiled/loom/counter")
    module: String,
    /// Current props state as JSON string
    props_json: String,
  )
}

/// Opaque type alias for the live socket subject
pub type LiveSocket =
  Subject(SocketMessage)

/// Start a new live socket actor with the given module and initial props.
/// The reply_to subject is used to send patches back to the WebSocket handler.
///
pub fn start(
  reply_to: Subject(SocketMessage),
  module: String,
  props_json: String,
) -> Result(Subject(SocketMessage), actor.StartError) {
  let initial_state = LiveSocketState(reply_to:, module:, props_json:)

  let result =
    actor.new(initial_state)
    |> actor.on_message(handle_message)
    |> actor.start

  case result {
    Ok(started) -> Ok(started.data)
    Error(err) -> Error(err)
  }
}

/// Handle incoming messages to the actor
fn handle_message(
  state: LiveSocketState,
  message: SocketMessage,
) -> actor.Next(LiveSocketState, SocketMessage) {
  case message {
    Event(client_event) -> {
      // Extract special vars as strings for the JSON wrapper
      let value = option.unwrap(client_event.special_vars.value, "")
      let checked = case client_event.special_vars.checked {
        option.Some(True) -> "true"
        option.Some(False) -> "false"
        option.None -> ""
      }
      let key = option.unwrap(client_event.special_vars.key, "")

      // Call handle_json on the template module via dynamic dispatch
      case
        live_dispatch.call_handle_json(
          state.module,
          client_event.handler,
          state.props_json,
          value,
          checked,
          key,
        )
      {
        Ok(new_props_json) -> {
          // Call render_json to get the new HTML
          case live_dispatch.call_render_json(state.module, new_props_json) {
            Ok(html) -> {
              // Send the patch back to the WebSocket handler
              process.send(state.reply_to, SendPatch(html))
              // Update state with new props
              actor.continue(
                LiveSocketState(..state, props_json: new_props_json),
              )
            }
            Error(_) -> {
              // Render failed, continue with unchanged state
              actor.continue(state)
            }
          }
        }
        Error(_) -> {
          // Handle failed, continue with unchanged state
          actor.continue(state)
        }
      }
    }

    SendPatch(_) | live_types.SendRedirect(_) -> {
      // These are outgoing messages, not handled by the actor
      actor.continue(state)
    }

    Stop -> {
      actor.stop()
    }
  }
}
