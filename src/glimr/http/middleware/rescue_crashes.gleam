//// Rescue Crashes Middleware
////
//// Without crash rescue, an unhandled exception in a
//// handler kills the request process and returns nothing
//// to the client — the browser sees a connection reset.
//// Wrapping the pipeline in wisp's rescue catches panics
//// and returns a proper 500 response so the error shows
//// in logs and the client gets a meaningful status code.
////

import glimr/http/kernel.{type Next}
import wisp.{type Request, type Response}

// ------------------------------------------------------------- Public Functions

/// Placed early in the pipeline so it catches crashes from
/// every downstream middleware and handler. The use callback
/// wraps the entire remaining chain, ensuring no unhandled
/// exception reaches the process boundary without being
/// converted to a 500 response.
///
pub fn run(req: Request, ctx: context, next: Next(context)) -> Response {
  use <- wisp.rescue_crashes

  next(req, ctx)
}
