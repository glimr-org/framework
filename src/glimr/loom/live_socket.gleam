//// Loom Live Socket
////
//// Each live template connection needs its own isolated state
//// — the current prop values — that persists across events and
//// survives concurrent connections. An OTP actor per
//// connection provides this isolation naturally, owns the prop
//// state, and serializes event handling so two simultaneous
//// clicks on the same page don't race.
////

import gleam/erlang/process.{type Subject}
import gleam/option
import gleam/otp/actor
import gleam/result
import glimr/loom/live_dispatch
import glimr/loom/loom.{type SocketMessage, Event, SendPatch, SendTrees, Stop}
import glimr/loom/runtime

// ------------------------------------------------------------- Public Types

/// The actor needs the reply subject to push patches back, the
/// module name for dynamic dispatch into the generated
/// handle_json/render_json functions, and the current props as
/// JSON so they can be passed through the handle/render cycle
/// without the actor knowing the actual prop types.
///
pub type LiveSocketState {
  LiveSocketState(
    /// Component ID for multiplexed WebSocket routing
    id: String,
    /// Subject to send responses back to the WebSocket handler
    reply_to: Subject(SocketMessage),
    /// Module name for dynamic dispatch (e.g.,
    /// "compiled/loom/counter")
    module: String,
    /// Current props state as JSON string
    props_json: String,
    /// Previous tree JSON for diffing (set after initial render)
    prev_tree_json: String,
  )
}

/// Callers interact with the actor via its Subject but don't
/// need to know the message type. This alias provides a
/// semantic name that communicates intent while hiding the
/// internal SocketMessage protocol.
///
pub type LiveSocket =
  Subject(SocketMessage)

// ------------------------------------------------------------- Public Functions

/// Each WebSocket connection starts its own actor so prop state
/// is isolated per user session. The reply_to subject ties this
/// actor back to the specific WebSocket handler that owns it,
/// ensuring patches reach the right browser tab.
///
pub fn start(
  id: String,
  reply_to: Subject(SocketMessage),
  module: String,
  props_json: String,
) -> Result(Subject(SocketMessage), actor.StartError) {
  // Render initial tree and send it to the client
  let prev_tree_json = case
    live_dispatch.call_render_tree_json(module, props_json)
  {
    Ok(tree_json) -> {
      process.send(reply_to, SendTrees(id, tree_json))
      tree_json
    }
    Error(_) -> "{}"
  }

  let initial_state =
    LiveSocketState(id:, reply_to:, module:, props_json:, prev_tree_json:)

  actor.new(initial_state)
  |> actor.on_message(handle_message)
  |> actor.start
  |> result.map(fn(started) { started.data })
}

// ------------------------------------------------------------- Private Functions

/// Events trigger a two-step cycle: call handle_json to update
/// props, then call render_json to produce new HTML from those
/// props. Keeping both steps in the actor ensures the
/// props_json state is updated atomically before the re-render,
/// and errors in either step leave the state unchanged rather
/// than half-updated.
///
fn handle_message(
  state: LiveSocketState,
  message: SocketMessage,
) -> actor.Next(LiveSocketState, SocketMessage) {
  case message {
    Event(client_event) -> {
      let value = option.unwrap(client_event.special_vars.value, "")

      let checked = case client_event.special_vars.checked {
        option.Some(True) -> "true"
        option.Some(False) -> "false"
        option.None -> ""
      }

      let key = option.unwrap(client_event.special_vars.key, "")

      let handle_result = {
        use new_props_json <- result.try(live_dispatch.call_handle_json(
          state.module,
          client_event.handler,
          state.props_json,
          value,
          checked,
          key,
        ))

        use new_tree_json <- result.try(live_dispatch.call_render_tree_json(
          state.module,
          new_props_json,
        ))

        Ok(#(new_props_json, new_tree_json))
      }

      case handle_result {
        Ok(#(new_props_json, new_tree_json)) -> {
          // Always send patch so the client can clear loading states,
          // even when the diff is empty
          let diff = runtime.diff_tree_json(state.prev_tree_json, new_tree_json)
          process.send(state.reply_to, SendPatch(state.id, diff))
          actor.continue(
            LiveSocketState(
              ..state,
              props_json: new_props_json,
              prev_tree_json: new_tree_json,
            ),
          )
        }

        Error(_) -> actor.continue(state)
      }
    }

    SendTrees(_, _) | SendPatch(_, _) | loom.SendRedirect(_) ->
      actor.continue(state)
    Stop -> actor.stop()
  }
}
