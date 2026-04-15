//// HTTP Types
////
//// The framework's core HTTP type aliases live here as the
//// leaf module in the dependency graph. Every other module
//// that needs Request or Response imports from here instead
//// of from wisp directly. If we ever swap wisp for another
//// HTTP library, this is the only module that changes.
////

import wisp

// ------------------------------------------------------------- Public Types

/// Controllers and middleware import Request from here rather
/// than from wisp. This indirection means swapping the HTTP
/// library only requires changing this alias, not every file
/// that handles requests.
///
@deprecated("use glimr/http/request.Request instead")
pub type Request =
  wisp.Request

/// Same idea as Request — the response type is re-exported here
/// so controllers and middleware never depend on wisp directly.
/// Keeps the HTTP library as a swappable implementation detail.
///
@deprecated("use glimr/http/response.Response instead")
pub type Response =
  wisp.Response
