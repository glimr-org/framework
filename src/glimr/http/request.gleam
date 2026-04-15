//// HTTP Request
////
//// The framework's request type alias. Every controller and
//// middleware imports Request from here instead of from wisp
//// directly, so the underlying HTTP library stays a swappable
//// implementation detail rather than a dependency baked into
//// every file that touches requests.
////

import wisp

// ------------------------------------------------------------- Public Types

/// Controllers and middleware import Request from here rather
/// than from wisp. This indirection means swapping the HTTP
/// library only requires changing this alias, not every file
/// that handles requests.
///
pub type Request =
  wisp.Request
