//// Fail
////
//// Sometimes a handler discovers halfway through that the
//// request can't proceed — the user isn't authorized, the
//// resource doesn't exist, the input is invalid. Threading
//// Result types back through every layer of middleware and
//// controller code is painful. `fail.with(404)` short-circuits
//// everything by raising an Erlang exception that the
//// rescue_crashes middleware catches and turns into a proper
//// HTTP response. Think of it like Laravel's `abort()`.
////

// ------------------------------------------------------------- Public Types

/// The rescue_crashes middleware needs to distinguish between a
/// handler that completed normally and one that called
/// `fail.with()`. This type makes that explicit — Ok wraps the
/// successful response, Fail carries the HTTP status code that
/// should be returned to the client.
///
pub type Rescue(a) {
  Ok(a)
  Fail(Int)
}

// ------------------------------------------------------------- Public Functions

/// The escape hatch for handlers that hit a dead end. Call
/// `fail.with(403)` when a user tries to access something they
/// shouldn't, or `fail.with(404)` when a database lookup comes
/// back empty. It raises an Erlang exception under the hood, so
/// it never returns — the type signature `-> a` lets it fit
/// anywhere without type errors.
///
pub fn with(status: Int) -> a {
  raise(status)
}

// ------------------------------------------------------------- Internal Public Functions

/// Wraps a function call so that any `fail.with()` inside it
/// gets caught instead of crashing the process. The
/// rescue_crashes middleware uses this to intercept aborts and
/// turn them into HTTP responses. Regular panics (nil access,
/// bad pattern match) are re-raised so they still crash loudly
/// — only intentional fail.with() calls are caught.
///
@internal
pub fn rescue(body: fn() -> a) -> Rescue(a) {
  do_rescue(body)
}

// ------------------------------------------------------------- FFI Bindings

/// Triggers the Erlang exception that fail.with() relies on.
/// The FFI module tags it so rescue can tell fail exceptions
/// apart from genuine crashes — without that tag, we'd
/// accidentally swallow real bugs.
///
@external(erlang, "glimr_fail_ffi", "raise")
fn raise(status: Int) -> a

/// The Erlang side uses try/catch to intercept only fail-tagged
/// exceptions. Anything else — badmatch, badarith,
/// function_clause — gets re-raised so it surfaces as a proper
/// crash with a stack trace rather than silently becoming a
/// 500.
///
@external(erlang, "glimr_fail_ffi", "rescue")
fn do_rescue(body: fn() -> a) -> Rescue(a)
