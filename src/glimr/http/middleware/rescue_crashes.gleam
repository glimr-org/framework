//// Rescue Crashes Middleware
////
//// Without this middleware, a `fail.with(404)` call deep
//// inside a handler would crash the request process and
//// produce an ugly unhandled-exception response. This sits
//// early in the pipeline and catches both intentional aborts
//// (fail.with) and genuine panics, turning them into clean
//// HTTP responses so the user sees a proper error page instead
//// of a stack trace.
////
//// Note: the `fail.rescue` branch below is now legacy. It is
//// kept only to handle any remaining direct callers of the
//// deprecated `fail.with` — the generated `_or_fail` query
//// variants no longer emit code that uses it and instead
//// return status responses directly. Once `fail.with` is
//// removed, this branch can go with it.
////

import glimr/http/context.{type Context}
import glimr/http/fail
import glimr/http/middleware.{type Next}
import glimr/http/response.{type Response}
import wisp

// ------------------------------------------------------------- Public Functions

/// This needs to be one of the first middleware in the pipeline
/// so it wraps everything downstream — controllers, other
/// middleware, all of it. When a handler calls
/// `fail.with(403)`, that raises an Erlang exception that
/// bubbles up to here and gets turned into a 403 response.
/// Unexpected panics (nil access, bad pattern match) hit wisp's
/// rescue_crashes first and become 500s, so users never see raw
/// crash output regardless of what goes wrong.
///
pub fn run(ctx: Context(app), next: Next(app)) -> Response {
  use <- wisp.rescue_crashes

  case fail.rescue(fn() { next(ctx) }) {
    fail.Ok(response) -> response
    fail.Fail(status) -> response.empty(status)
  }
}
