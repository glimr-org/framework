//// Session Store
////
//// The session middleware needs to load, save, and destroy
//// session data without knowing which backend is in use. This
//// module defines a function-based interface that backends
//// implement, so swapping from file-based to cookie-based
//// sessions requires changing one line at boot — not
//// modifying middleware or handler code. The active store is
//// cached in persistent_term so every request can access it
//// without threading it through function arguments.
////

import gleam/dict.{type Dict}

// ------------------------------------------------------------- Public Types

/// Gleam doesn't have traits or interfaces, so the store is
/// modeled as a record of closures — each backend provides its 
/// own implementations at construction time. Making the type 
/// opaque prevents callers from reaching into the closures 
/// directly, ensuring all access goes through the public 
/// functions that handle the "no store configured" fallback.
///
pub opaque type SessionStore {
  SessionStore(
    load: fn(String) -> #(Dict(String, String), Dict(String, String)),
    save: fn(String, Dict(String, String), Dict(String, String)) -> Nil,
    destroy: fn(String) -> Nil,
    gc: fn() -> Nil,
    cookie_value: fn(String, Dict(String, String), Dict(String, String)) ->
      String,
  )
}

// ------------------------------------------------------------- Public Functions

/// Labeled arguments make construction self-documenting at the 
/// call site — each backend explicitly names every callback it 
/// provides. This is the only way to build a SessionStore, so 
/// the opaque type guarantee holds: every store has all five 
/// callbacks populated.
///
pub fn new(
  load load: fn(String) -> #(Dict(String, String), Dict(String, String)),
  save save: fn(String, Dict(String, String), Dict(String, String)) -> Nil,
  destroy destroy: fn(String) -> Nil,
  gc gc: fn() -> Nil,
  cookie_value cookie_value: fn(
    String,
    Dict(String, String),
    Dict(String, String),
  ) ->
    String,
) -> SessionStore {
  SessionStore(
    load: load,
    save: save,
    destroy: destroy,
    gc: gc,
    cookie_value: cookie_value,
  )
}

/// Middleware calls this at request start to hydrate the
/// session actor. Returning empty dicts when no store is
/// configured lets the app boot and serve requests even if 
/// sessions aren't set up — reads just return nothing rather 
/// than crashing.
///
pub fn load(session_id: String) -> #(Dict(String, String), Dict(String, String)) {
  with_store(#(dict.new(), dict.new()), fn(store) { store.load(session_id) })
}

/// Middleware calls this after the handler returns to persist 
/// mutations. The flash dict here contains only values set 
/// during this request — the previous request's flash was 
/// already consumed and cleared, enforcing one-shot semantics 
/// at the store level.
///
pub fn save(
  session_id: String,
  data: Dict(String, String),
  flash: Dict(String, String),
) -> Nil {
  with_store(Nil, fn(store) { store.save(session_id, data, flash) })
}

/// Invalidation must remove the old session immediately so a 
/// stolen session ID can never be reused. The middleware calls 
/// this before saving the new session, ensuring the old and new 
/// entries never coexist in the store.
///
pub fn destroy(session_id: String) -> Nil {
  with_store(Nil, fn(store) { store.destroy(session_id) })
}

/// Expired sessions accumulate in the store over time. Running 
/// GC probabilistically (2% of requests) spreads cleanup cost 
/// so no single request pays the full scan price, while still 
/// purging stale entries within a reasonable window.
///
pub fn gc() -> Nil {
  with_store(Nil, fn(store) { store.gc() })
}

/// Server-side stores put only the session ID in the cookie 
/// while cookie stores encode the full payload. Abstracting 
/// this behind a callback lets the middleware set the cookie 
/// without knowing which strategy is active — the store decides 
/// what goes over the wire.
///
pub fn cookie_value(
  session_id: String,
  data: Dict(String, String),
  flash: Dict(String, String),
) -> String {
  with_store(session_id, fn(store) {
    store.cookie_value(session_id, data, flash)
  })
}

/// The driver calls this once at boot so every subsequent
/// request reads the store from persistent_term instead of 
/// passing it through function arguments. This keeps the 
/// session API ergonomic — callers never need a store 
/// reference.
///
pub fn cache_store(store: SessionStore) -> Nil {
  cache(store)
}

// ------------------------------------------------------------- Private Functions

/// Every public function needs the same fallback logic: use the 
/// cached store if one exists, otherwise return a safe default. 
/// Centralizing this avoids repeating the cache lookup and 
/// Error branch in every function and guarantees consistent "no 
/// store" behavior.
///
fn with_store(default: a, f: fn(SessionStore) -> a) -> a {
  case get_cached() {
    Ok(store) -> f(store)
    Error(_) -> default
  }
}

// ------------------------------------------------------------- FFI Bindings

/// Writes the store to persistent_term so all BEAM processes 
/// can read it without message passing. This is ideal for data 
/// that changes rarely (once at boot) but is read on every 
/// request across all processes.
///
@external(erlang, "glimr_kernel_ffi", "cache_session_store")
fn cache(store: SessionStore) -> Nil

// ------------------------------------------------------------- FFI Bindings
/// Returns the cached store or Error(Nil) if none has been 
/// cached yet. The Result type lets with_store distinguish "no 
/// store configured" from a real store, so the app degrades 
/// gracefully when sessions aren't set up rather than crashing 
/// on the first request.
///
@external(erlang, "glimr_kernel_ffi", "get_cached_session_store")
fn get_cached() -> Result(SessionStore, Nil)
