//// Cookie Session Store
////
//// Signed cookies remove the need for any server-side session
//// storage — no file, ETS, or database backend required. The
//// full payload is baked directly into the cookie, keeping the
//// server fully stateless. Browsers cap cookies around ~4KB so
//// this only works for small payloads; use a server-side store
//// when sessions carry more data.
////

import glimr/session/payload
import glimr/session/store.{type SessionStore}

// ------------------------------------------------------------- Public Functions

/// Three of the five store callbacks are no-ops because there
/// is no server-side state to manage — save, destroy, and gc
/// have nothing to write to or clean up. Load decodes the
/// cookie value directly and cookie_value encodes the full
/// payload so middleware can write it back into the signed
/// cookie. This keeps the store interface uniform while the
/// cookie backend skips all server-side persistence logic
/// entirely.
///
pub fn create() -> SessionStore {
  store.new(
    load: fn(cookie_value) { payload.decode(cookie_value) },
    save: fn(_, _, _) { Nil },
    destroy: fn(_) { Nil },
    gc: fn() { Nil },
    cookie_value: fn(_, data, flash) { payload.encode(data, flash) },
  )
}
