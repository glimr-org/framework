//// Session
////
//// Gleam is immutable, but sessions need mutable state that
//// persists across multiple reads and writes within a single
//// request handler. An OTP actor per request provides that
//// mutability safely — each operation is a message, so
//// concurrent access is serialized. The middleware reads the
//// actor's final state after the handler returns to persist
//// only what actually changed, avoiding unnecessary store
//// writes.
////

import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/string
import glimr/session/cookie_store
import glimr/session/store.{type SessionStore}

// ------------------------------------------------------------- Public Types

/// Context requires a Session field at construction time, but
/// the actor can't start until a request arrives with cookie
/// data. The Empty variant satisfies the type system during
/// boot without allocating an actor, while Live wraps the real
/// per-request actor subject. Making the type opaque prevents
/// callers from pattern matching on the variant and coupling to
/// the internal representation.
///
pub opaque type Session {
  Live(subject: Subject(SessionMessage))
  Empty
}

// ------------------------------------------------------------- Internal Public Types

/// The middleware needs to inspect the actor's final state to
/// decide what to persist. Tracking dirty and invalidated flags
/// avoids unnecessary store writes when nothing changed.
///
/// Flash is split into two dicts because flash messages have
/// one-shot semantics: loaded_flash holds values from the
/// previous request (readable via get_flash), while flash holds
/// new values set during this request (persisted for the next
/// one). This separation prevents a flash set and read within
/// the same request from interfering.
///
@internal
pub type SessionState {
  SessionState(
    id: String,
    data: Dict(String, String),
    loaded_flash: Dict(String, String),
    flash: Dict(String, String),
    dirty: Bool,
    invalidated: Bool,
  )
}

// ------------------------------------------------------------- Private Types

/// Each public session function maps to exactly one message
/// variant, keeping the actor protocol a direct mirror of the
/// public API. Reads carry a reply subject for synchronous
/// responses; writes are fire-and-forget. This separation lets
/// the actor handle both patterns without extra state.
///
type SessionMessage {
  Get(key: String, reply: Subject(Result(String, Nil)))
  Put(key: String, value: String)
  Forget(key: String)
  Has(key: String, reply: Subject(Bool))
  All(reply: Subject(Dict(String, String)))
  GetId(reply: Subject(String))
  Flash(key: String, value: String)
  GetFlash(key: String, reply: Subject(Result(String, Nil)))
  Invalidate
  Regenerate
  GetState(reply: Subject(SessionState))
  Shutdown
}

// ------------------------------------------------------------- Public Functions

/// Context is constructed at boot before any HTTP request
/// arrives, so there's no cookie data to seed a real session
/// actor. This returns a no-op handle where all reads return
/// empty values and all writes are silently ignored, avoiding
/// the need for Option(Session) throughout the framework.
///
pub fn empty() -> Session {
  Empty
}

/// Reads go through actor.call (synchronous) because the caller
/// needs the value immediately to make decisions in the
/// handler. Returns Error(Nil) for missing keys so callers can
/// distinguish absence from an empty string.
///
pub fn get(session: Session, key: String) -> Result(String, Nil) {
  case session {
    Live(subject) -> actor.call(subject, 5000, Get(key, _))
    Empty -> Error(Nil)
  }
}

/// Writes use process.send (fire-and-forget) because the caller
/// doesn't need confirmation — the actor serializes all
/// mutations and the middleware reads the final state after the
/// handler returns.
///
pub fn put(session: Session, key: String, value: String) -> Nil {
  case session {
    Live(subject) -> process.send(subject, Put(key, value))
    Empty -> Nil
  }
}

/// Removing a key marks the session dirty so the middleware
/// persists the deletion. Fire-and-forget like put since the
/// caller doesn't need to wait for confirmation.
///
pub fn forget(session: Session, key: String) -> Nil {
  case session {
    Live(subject) -> process.send(subject, Forget(key))
    Empty -> Nil
  }
}

/// Existence checks are synchronous (actor.call) because the
/// caller typically branches on the result immediately, e.g. to
/// decide whether to redirect an unauthenticated user.
///
pub fn has(session: Session, key: String) -> Bool {
  case session {
    Live(subject) -> actor.call(subject, 5000, Has(key, _))
    Empty -> False
  }
}

/// Bulk reads are needed for serialization (e.g. the cookie
/// store encoding the full session into a signed cookie) and
/// for debugging. Returns a snapshot — further mutations after
/// this call won't be reflected in the returned dict.
///
pub fn all(session: Session) -> Dict(String, String) {
  case session {
    Live(subject) -> actor.call(subject, 5000, All)
    Empty -> dict.new()
  }
}

/// The session ID is needed by the store to look up or persist
/// the session data. Exposing it lets middleware and store
/// implementations access it without reaching into the actor's
/// internal state directly.
///
pub fn id(session: Session) -> String {
  case session {
    Live(subject) -> actor.call(subject, 5000, GetId)
    Empty -> ""
  }
}

/// Flash messages provide one-shot feedback across redirects
/// (e.g. "Item saved successfully"). Storing them separately
/// from regular session data and clearing them after one read
/// ensures they appear exactly once without the handler needing
/// to manage cleanup.
///
pub fn flash(session: Session, key: String, value: String) -> Nil {
  case session {
    Live(subject) -> process.send(subject, Flash(key, value))
    Empty -> Nil
  }
}

/// Most flash reads happen in templates where an empty string
/// is the natural "no flash" value. Returning "" instead of a
/// Result avoids wrapping every template interpolation in a
/// case expression.
///
pub fn get_flash(session: Session, key: String) -> String {
  case get_flash_or(session, key) {
    Ok(value) -> value
    Error(_) -> ""
  }
}

/// When the handler needs to distinguish between "no flash was
/// set" and "flash was set to an empty string", this Result-
/// returning variant provides that distinction. get_flash
/// delegates here and unwraps for the common case.
///
pub fn get_flash_or(session: Session, key: String) -> Result(String, Nil) {
  case session {
    Live(subject) -> actor.call(subject, 5000, GetFlash(key, _))
    Empty -> Error(Nil)
  }
}

/// Templates often need to conditionally render a flash banner
/// only when a message exists. A Bool check is cleaner than
/// matching on a Result when the value itself isn't needed for
/// the conditional.
///
pub fn has_flash(session: Session, key: String) -> Bool {
  case get_flash_or(session, key) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Nobody wants to retype an entire form because one field
/// failed validation. After a redirect back, the validator
/// stashes the old values as flash data so templates can
/// repopulate inputs automatically.
///
/// ```html
/// <input type="email" name="email" :value="session.old(ctx.session, 'email')" />
/// ```
///
pub fn old(session: Session, field: String) -> String {
  get_flash(session, "old." <> field)
}

/// Showing inline validation errors next to the field that
/// failed is way better than a generic "something went wrong"
/// banner. The validator flashes each field's first error so
/// templates can display it right where it matters.
///
/// ```html
/// <p l-if="session.error(ctx.session, 'email') != ''" class="text-red-600">
///   {{ session.error(ctx.session, "email") }}
/// </p>
/// ```
///
pub fn error(session: Session, field: String) -> String {
  get_flash(session, "errors." <> field)
}

/// Templates often need to conditionally show error styling or
/// error messages. Checking `error() != ""` works but reads
/// awkwardly in template expressions. This gives a clean
/// boolean for `l-if` directives.
///
/// ```html
/// <p l-if="session.has_error(ctx.session, 'email')" class="text-red-600">
///   {{ session.error(ctx.session, "email") }}
/// </p>
/// ```
///
pub fn has_error(session: Session, field: String) -> Bool {
  has_flash(session, "errors." <> field)
}

/// Logout and account deletion need to destroy all session
/// state and issue a new ID so the old session cookie can never
/// be reused. The invalidated flag tells middleware to delete
/// the old entry from the store rather than just overwriting
/// it.
///
pub fn invalidate(session: Session) -> Nil {
  case session {
    Live(subject) -> process.send(subject, Invalidate)
    Empty -> Nil
  }
}

/// After login, the session ID must change to prevent session
/// fixation attacks — an attacker who planted a known session
/// ID before authentication can't hijack the post-login session
/// if the ID rotates. Data is preserved so the user doesn't
/// lose pre-login state.
///
pub fn regenerate(session: Session) -> Nil {
  case session {
    Live(subject) -> process.send(subject, Regenerate)
    Empty -> Nil
  }
}

/// Caches the given session store in persistent_term so the
/// session middleware can access it on every request without
/// threading it through function arguments. Call this once at
/// boot after creating a store from any driver.
///
pub fn setup(session_store: SessionStore) -> Nil {
  store.cache_store(session_store)
}

/// Cookie-based sessions avoid server-side storage entirely —
/// the signed cookie is the store. This is ideal for small
/// payloads under ~4KB where the simplicity of zero
/// infrastructure outweighs the per-request bandwidth cost.
///
pub fn cookie_store() -> SessionStore {
  cookie_store.create()
}

// ------------------------------------------------------------- Internal Public Functions

/// The middleware calls this at request start to hydrate an
/// actor from the store's persisted data. If flash was loaded,
/// the session is marked dirty immediately so the middleware
/// will save the cleared flash back — this enforces one-shot
/// semantics even if the handler never writes to the session.
///
@internal
pub fn start(
  id: String,
  data: Dict(String, String),
  flash: Dict(String, String),
) -> Session {
  // Mark dirty when flash was loaded so the middleware will save
  // the cleared flash back to the store (one-shot semantics)
  let has_flash = dict.size(flash) > 0

  let state =
    SessionState(
      id: id,
      data: data,
      loaded_flash: flash,
      flash: dict.new(),
      dirty: has_flash,
      invalidated: False,
    )

  let assert Ok(started) =
    actor.new(state)
    |> actor.on_message(handle_message)
    |> actor.start

  Live(started.data)
}

/// The actor must be stopped after the middleware persists
/// changes, otherwise it would leak — one orphaned process per
/// request. Stopping via a message rather than killing the
/// process lets the actor shut down cleanly.
///
@internal
pub fn stop(session: Session) -> Nil {
  case session {
    Live(subject) -> process.send(subject, Shutdown)
    Empty -> Nil
  }
}

/// The middleware needs the full state — data, flash, dirty
/// flag, invalidated flag — to decide what to persist and
/// whether to delete the old session. Exposing the entire
/// SessionState avoids adding separate accessors for each field
/// the middleware needs.
///
@internal
pub fn get_state(session: Session) -> Result(SessionState, Nil) {
  case session {
    Live(subject) -> Ok(actor.call(subject, 5000, GetState))
    Empty -> Error(Nil)
  }
}

/// Session IDs must be unguessable to prevent hijacking. 32
/// bytes of cryptographic randomness (256 bits) provides
/// sufficient entropy that brute-force guessing is infeasible.
/// Hex encoding produces a URL-safe 64-character string
/// suitable for cookie values.
///
@internal
pub fn generate_id() -> String {
  crypto_strong_random_bytes(32)
  |> bit_array.base16_encode
  |> string.lowercase
}

// ------------------------------------------------------------- Private Functions

/// All session mutations flow through this single handler,
/// which serializes access and tracks the dirty flag. Using
/// actor.call for reads and process.send for writes gives
/// callers synchronous results when needed while keeping fire-
/// and-forget writes fast. The dirty flag ensures the
/// middleware only persists sessions that actually changed.
///
fn handle_message(
  state: SessionState,
  message: SessionMessage,
) -> actor.Next(SessionState, SessionMessage) {
  case message {
    Get(key, reply) -> {
      process.send(reply, dict.get(state.data, key))
      actor.continue(state)
    }

    Put(key, value) -> {
      actor.continue(
        SessionState(
          ..state,
          data: dict.insert(state.data, key, value),
          dirty: True,
        ),
      )
    }

    Forget(key) -> {
      actor.continue(
        SessionState(..state, data: dict.delete(state.data, key), dirty: True),
      )
    }

    Has(key, reply) -> {
      process.send(reply, dict.has_key(state.data, key))
      actor.continue(state)
    }

    All(reply) -> {
      process.send(reply, state.data)
      actor.continue(state)
    }

    GetId(reply) -> {
      process.send(reply, state.id)
      actor.continue(state)
    }

    Flash(key, value) -> {
      actor.continue(
        SessionState(
          ..state,
          flash: dict.insert(state.flash, key, value),
          dirty: True,
        ),
      )
    }

    GetFlash(key, reply) -> {
      process.send(reply, dict.get(state.loaded_flash, key))
      actor.continue(state)
    }

    Invalidate -> {
      actor.continue(SessionState(
        id: generate_id(),
        data: dict.new(),
        loaded_flash: dict.new(),
        flash: dict.new(),
        dirty: True,
        invalidated: True,
      ))
    }

    Regenerate -> {
      actor.continue(SessionState(..state, id: generate_id(), dirty: True))
    }

    GetState(reply) -> {
      process.send(reply, state)
      actor.continue(state)
    }

    Shutdown -> {
      actor.stop()
    }
  }
}

// ------------------------------------------------------------- FFI Helpers

/// Gleam has no built-in CSPRNG, so this wraps Erlang's
/// crypto:strong_rand_bytes/1 via FFI. Using the crypto module
/// guarantees OS-level entropy rather than a pseudo-random
/// generator.
///
@external(erlang, "crypto", "strong_rand_bytes")
fn crypto_strong_random_bytes(n: Int) -> BitArray
