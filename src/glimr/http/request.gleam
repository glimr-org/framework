//// TODO: add module docs

import wisp

// ------------------------------------------------------------- Public Types

/// Controllers and middleware import Request from here rather
/// than from wisp. This indirection means swapping the HTTP
/// library only requires changing this alias, not every file
/// that handles requests.
///
pub type Request =
  wisp.Request
