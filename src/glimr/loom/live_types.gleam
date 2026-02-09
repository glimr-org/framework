//// Loom Live Types
////
//// Shared types for Loom Live WebSocket communication.
//// Separated to avoid circular imports between live and live_socket modules.
////

import gleam/option.{type Option}

/// Event received from the client via WebSocket
pub type ClientEvent {
  ClientEvent(handler: String, event: String, special_vars: SpecialVars)
}

/// Special variables passed from client events
pub type SpecialVars {
  SpecialVars(value: Option(String), checked: Option(Bool), key: Option(String))
}

/// Messages that can be sent to/from the live socket actor
pub type SocketMessage {
  /// Event received from the client via WebSocket
  Event(ClientEvent)
  /// Send an HTML patch back to the client
  SendPatch(html: String)
  /// Send a redirect back to the client
  SendRedirect(url: String)
  /// Stop the actor
  Stop
}
