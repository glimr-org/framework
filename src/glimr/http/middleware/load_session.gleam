//// Session Middleware
////
//// Loads the session for each request. Reads the session cookie,
//// hydrates a session actor from the configured store, and
//// persists changes after the handler returns. Separated from
//// the Session type module to avoid an import cycle — Context
//// has a Session field, and this middleware takes Context(app).
////

import gleam/crypto
import gleam/dict
import gleam/http
import gleam/http/cookie
import gleam/http/request
import gleam/http/response
import gleam/option
import glimr/config/config
import glimr/http/context.{type Context, Context}
import glimr/http/http.{type Request, type Response} as _glimr_http
import glimr/http/kernel.{type Next}
import glimr/session/session
import glimr/session/store
import wisp

// ------------------------------------------------------------- Public Functions

/// Starts a session for the current request. Reads the session
/// cookie, loads data from the configured store, and provides a
/// live session to the next handler. After the handler returns,
/// persists changes and sets the cookie on the response.
///
/// This function's signature matches the Middleware type, so it
/// can be placed directly in middleware lists:
///
/// ```gleam
/// [serve_static.run, log_request.run, load_session.run]
/// |> middleware.apply(ctx, router)
/// ```
///
pub fn run(ctx: Context(app), next: Next(app)) -> Response {
  let req = ctx.req
  let cookie_name = config.get_string("session.cookie")
  let lifetime = config.get_int("session.lifetime")
  let expire_on_close = config.get_bool("session.expire_on_close")

  // Read or generate session ID
  let #(session_id, is_new) = case
    wisp.get_cookie(req, cookie_name, wisp.Signed)
  {
    Ok(id) -> #(id, False)
    Error(_) -> #(session.generate_id(), True)
  }

  // Load existing data from store (empty for new sessions)
  let #(data, flash) = case is_new {
    True -> #(dict.new(), dict.new())
    False -> store.load(session_id)
  }

  // Start session actor with loaded data
  // Flash from store becomes readable this request, then cleared
  let live_session = session.start(session_id, data, flash)

  // Call the actual handler with session on the context
  let resp = next(Context(..ctx, session: live_session))

  // Read final state and persist
  let resp = case session.get_state(live_session) {
    Ok(state) -> {
      // Destroy old session if invalidated
      case state.invalidated {
        True -> store.destroy(session_id)
        False -> Nil
      }

      // Save if dirty
      case state.dirty {
        True -> store.save(state.id, state.data, state.flash)
        False -> Nil
      }

      // Probabilistic gc (2% chance)
      maybe_gc()

      // Set cookie with session value (ID for server stores, payload for cookie stores)
      let value = store.cookie_value(state.id, state.data, state.flash)

      set_session_cookie(
        resp,
        req,
        cookie_name,
        value,
        lifetime,
        expire_on_close,
      )
    }

    Error(_) -> resp
  }

  // Stop the actor
  session.stop(live_session)

  resp
}

// ------------------------------------------------------------- Private Functions

/// Sets the session cookie on the response. When
/// expire_on_close is true, omits max_age so the cookie becomes
/// a session cookie that the browser deletes when closed. When
/// false, sets max_age to the configured lifetime.
///
fn set_session_cookie(
  resp: Response,
  req: Request,
  name: String,
  value: String,
  lifetime: Int,
  expire_on_close: Bool,
) -> Response {
  let scheme = case req.host {
    "localhost" | "127.0.0.1" | "[::1]" if req.scheme == http.Http ->
      case request.get_header(req, "x-forwarded-proto") {
        Ok(_) -> http.Https
        Error(_) -> http.Http
      }
    _ -> http.Https
  }

  let max_age = case expire_on_close {
    True -> option.None
    False -> option.Some(lifetime * 60)
  }

  let attributes =
    cookie.Attributes(..cookie.defaults(scheme), max_age: max_age)
  let signed_value = wisp.sign_message(req, <<value:utf8>>, crypto.Sha512)

  response.set_cookie(resp, name, signed_value, attributes)
}

/// Expired sessions accumulate in the store over time but
/// running GC on every request would add unnecessary latency. A
/// 2% probability spreads the cleanup cost across requests so
/// no single request pays the full price, while still ensuring
/// stale entries are purged within a reasonable window.
///
fn maybe_gc() -> Nil {
  case rand_uniform(100) {
    n if n <= 2 -> store.gc()
    _ -> Nil
  }
}

// ------------------------------------------------------------- FFI Helpers

/// Gleam has no built-in random number generation, so this
/// wraps Erlang's rand:uniform/1 via FFI. Only used for the
/// probabilistic GC trigger — not for security-sensitive
/// operations, which use crypto:strong_rand_bytes instead.
///
@external(erlang, "rand", "uniform")
fn rand_uniform(n: Int) -> Int
