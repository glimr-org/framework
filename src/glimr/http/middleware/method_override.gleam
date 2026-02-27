//// Method Override Middleware
////
//// HTML forms only support GET and POST, but RESTful routing
//// needs PUT, PATCH, and DELETE. Without method override,
//// every destructive action would need a JavaScript fetch call
//// or a POST route with an action parameter — neither is
//// acceptable for progressive enhancement. This middleware
//// lets forms submit any HTTP method via a hidden _method
//// field.
////

import glimr/http/kernel.{type Next}
import wisp.{type Request, type Response}

// ------------------------------------------------------------- Public Functions

/// Applied before routing so the router sees the overridden
/// method and matches the correct handler. Transforming the
/// request before passing it down means every downstream
/// middleware and handler sees the intended method without
/// needing to check the _method field themselves.
///
pub fn run(req: Request, ctx: context, next: Next(context)) -> Response {
  next(wisp.method_override(req), ctx)
}
