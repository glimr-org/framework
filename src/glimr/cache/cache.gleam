//// Cache Unified API
////
//// Every cache backend (Redis, file, SQLite) has its own way
//// of talking to storage, but application code shouldn't care
//// which one is active. This module defines the CachePool type
//// that all backends produce, plus composite operations like
//// remember and JSON helpers that are written once here
//// instead of being reimplemented in every backend.

import gleam/dynamic/decode
import gleam/int
import gleam/json.{type Json}
import gleam/result

// ------------------------------------------------------------- Public Types

/// Having typed error variants instead of a generic string
/// error lets callers respond differently to each situation.
/// For example, remember_json treats SerializationError as "the
/// cached format changed, recompute" but treats ConnectionError
/// as "something is actually broken, bail out." A flat string
/// error would force callers to parse messages to figure out
/// what happened.
///
pub type CacheError {
  /// The key doesn't exist or has expired
  NotFound
  /// The cached value couldn't be encoded or decoded — often
  /// means the data shape changed between deployments
  SerializationError(message: String)
  /// The backend is unreachable or returned an unexpected error —
  /// usually a network or permissions issue
  ConnectionError(message: String)
  /// The entry existed but has expired — used internally by
  /// backends that do lazy expiration
  Expired
  /// The callback passed to remember() failed, so there's nothing
  /// to cache
  ComputeError(message: String)
}

/// The trick that makes multiple backends work is closures —
/// each backend captures its own connection pool inside these
/// functions at startup. Application code never imports Redis
/// or SQLite modules directly, it just calls cache.get() and
/// the right thing happens. Making this opaque means you can't
/// construct a CachePool without going through new_pool, which
/// ensures all 8 operations are wired up.
///
pub opaque type CachePool {
  CachePool(
    get: fn(String) -> Result(String, CacheError),
    put: fn(String, String, Int) -> Result(Nil, CacheError),
    put_forever: fn(String, String) -> Result(Nil, CacheError),
    forget: fn(String) -> Result(Nil, CacheError),
    flush: fn() -> Result(Nil, CacheError),
    increment: fn(String, Int) -> Result(Int, CacheError),
    has: fn(String) -> Bool,
    stop: fn() -> Nil,
  )
}

// ------------------------------------------------------------- Public Functions

/// All the core operations below are thin delegates to the
/// closures inside CachePool. The indirection means app code
/// imports glimr/cache/cache and never touches backend modules
/// — swapping from Redis to file caching is a config change,
/// not a code change.
///
pub fn get(pool: CachePool, key: String) -> Result(String, CacheError) {
  pool.get(key)
}

/// Caching without expiration is a memory leak waiting to
/// happen, so this requires an explicit TTL. If you genuinely
/// want permanent storage, use put_forever — the separate
/// function name makes that a conscious decision rather than an
/// accidental omission.
///
pub fn put(
  pool: CachePool,
  key: String,
  value: String,
  ttl_seconds: Int,
) -> Result(Nil, CacheError) {
  pool.put(key, value, ttl_seconds)
}

/// Some things genuinely need to live forever — like feature
/// flags or configuration lookups that only change on deploy.
/// Having a separate function instead of put() with ttl=0 makes
/// it obvious in code reviews that the author intended
/// permanent storage.
///
pub fn put_forever(
  pool: CachePool,
  key: String,
  value: String,
) -> Result(Nil, CacheError) {
  pool.put_forever(key, value)
}

/// Idempotent by design — deleting a key that doesn't exist
/// still returns Ok. This prevents race conditions where two
/// concurrent requests both try to invalidate the same key and
/// one of them would get a spurious error.
///
pub fn forget(pool: CachePool, key: String) -> Result(Nil, CacheError) {
  pool.forget(key)
}

/// Wipes everything in this cache pool. Backends scope this to
/// the pool's prefix, so flushing one pool won't touch keys
/// belonging to other pools or other applications sharing the
/// same storage.
///
pub fn flush(pool: CachePool) -> Result(Nil, CacheError) {
  pool.flush()
}

/// Atomic increment is essential for things like rate limiters
/// and hit counters — doing get/parse/add/set manually would
/// lose updates under concurrency. Starting from 0 when the key
/// doesn't exist means callers don't need to initialize
/// counters before using them.
///
pub fn increment(
  pool: CachePool,
  key: String,
  by: Int,
) -> Result(Int, CacheError) {
  pool.increment(key, by)
}

/// Useful when you need to know if something is cached without
/// actually fetching the value — like checking if a rate limit
/// key exists. Some backends (Redis) can do this without
/// transferring the value over the network, saving bandwidth on
/// large cached blobs.
///
pub fn has(pool: CachePool, key: String) -> Bool {
  pool.has(key)
}

/// Console commands create temporary pools that should be
/// cleaned up when they're done. Without explicit shutdown,
/// those connections would sit open until the process exits —
/// which could exhaust connection limits if someone runs
/// several CLI commands in quick succession.
///
pub fn stop(pool: CachePool) -> Nil {
  pool.stop()
}

/// Get-then-delete in one call. Perfect for one-time tokens
/// like email verification codes or CSRF tokens — you want to
/// read the value and ensure it can never be used again. Doing
/// this as two separate calls would let another request sneak
/// in and read the token before it's deleted.
///
pub fn pull(pool: CachePool, key: String) -> Result(String, CacheError) {
  use value <- result.try(get(pool, key))
  let _ = forget(pool, key)

  Ok(value)
}

/// Just increment with a negative value, but having a named
/// function makes code like "decrement remaining attempts" read
/// naturally instead of the confusing "increment(key, -1)"
/// which looks like a bug at first glance.
///
pub fn decrement(
  pool: CachePool,
  key: String,
  by: Int,
) -> Result(Int, CacheError) {
  increment(pool, key, -by)
}

/// The most common caching pattern: try the cache first, and if
/// it's a miss, run an expensive computation (database query,
/// API call, etc.) and store the result for next time. The
/// compute callback returns Result so a failed API call doesn't
/// cache an error response — only successful computations get
/// stored.
///
pub fn remember(
  pool: CachePool,
  key: String,
  ttl_seconds: Int,
  compute: fn() -> Result(String, e),
) -> Result(String, CacheError) {
  do_remember(pool, key, compute, fn(value) {
    put(pool, key, value, ttl_seconds)
  })
}

/// Same as remember but the cached result never expires. Good
/// for values that are expensive to compute but rarely change —
/// like a site's navigation menu built from the database. The
/// only way to refresh these is an explicit forget() or
/// flush().
///
pub fn remember_forever(
  pool: CachePool,
  key: String,
  compute: fn() -> Result(String, e),
) -> Result(String, CacheError) {
  do_remember(pool, key, compute, fn(value) { put_forever(pool, key, value) })
}

/// When you deploy a new version that changes a type's fields,
/// the old cached JSON no longer matches your decoder. Rather
/// than returning a cryptic decode error, this treats
/// SerializationError the same as NotFound — it recomputes the
/// value, caches the new format, and life goes on. No need to
/// manually flush the cache after every deploy.
///
pub fn remember_json(
  pool: CachePool,
  key: String,
  ttl_seconds: Int,
  decoder: decode.Decoder(a),
  compute: fn() -> Result(a, e),
  encoder: fn(a) -> Json,
) -> Result(a, CacheError) {
  case get_json(pool, key, decoder) {
    Ok(value) -> Ok(value)
    Error(NotFound) | Error(SerializationError(_)) -> {
      use value <- result.try(
        compute()
        |> result.replace_error(ComputeError("Compute function failed")),
      )
      let _ = put_json(pool, key, value, encoder, ttl_seconds)
      Ok(value)
    }
    Error(e) -> Error(e)
  }
}

/// Fetches a cached string and runs it through a JSON decoder
/// in one step. If the cached value doesn't match the decoder,
/// you get a SerializationError rather than a generic parse
/// failure — which lets remember_json distinguish "stale
/// format, recompute" from "the backend is broken."
///
pub fn get_json(
  pool: CachePool,
  key: String,
  decoder: decode.Decoder(a),
) -> Result(a, CacheError) {
  use value <- result.try(get(pool, key))

  case json.parse(value, decoder) {
    Ok(decoded) -> Ok(decoded)
    Error(_) -> Error(SerializationError("Failed to decode JSON"))
  }
}

/// Encodes a value to JSON and stores the resulting string.
/// Keeping serialization here means callers don't need to
/// manually call json.to_string before every put — and more
/// importantly, get_json and put_json always agree on the
/// format because they both go through the same path.
///
pub fn put_json(
  pool: CachePool,
  key: String,
  value: a,
  encoder: fn(a) -> Json,
  ttl_seconds: Int,
) -> Result(Nil, CacheError) {
  let json_string = json.to_string(encoder(value))
  put(pool, key, json_string, ttl_seconds)
}

/// Same as put_json but without expiration — for config
/// lookups, feature flags, or any JSON structure you want
/// cached until an explicit forget or flush clears it.
///
pub fn put_json_forever(
  pool: CachePool,
  key: String,
  value: a,
  encoder: fn(a) -> Json,
) -> Result(Nil, CacheError) {
  let json_string = json.to_string(encoder(value))
  put_forever(pool, key, json_string)
}

// ------------------------------------------------------------- Internal Public Functions

/// This is how backends register themselves. Each closure
/// captures the backend's internal pool — so when app code
/// calls cache.get(), it runs the Redis or file implementation
/// without knowing which one it is. CachePool is opaque, so
/// this constructor is the only way to create one, which
/// guarantees all 8 operations are always wired up.
///
@internal
pub fn new_pool(
  get get: fn(String) -> Result(String, CacheError),
  put put: fn(String, String, Int) -> Result(Nil, CacheError),
  put_forever put_forever: fn(String, String) -> Result(Nil, CacheError),
  forget forget: fn(String) -> Result(Nil, CacheError),
  flush flush: fn() -> Result(Nil, CacheError),
  increment increment: fn(String, Int) -> Result(Int, CacheError),
  has has: fn(String) -> Bool,
  stop stop: fn() -> Nil,
) -> CachePool {
  CachePool(
    get: get,
    put: put,
    put_forever: put_forever,
    forget: forget,
    flush: flush,
    increment: increment,
    has: has,
    stop: stop,
  )
}

/// Redis has INCRBY and databases have UPDATE ... SET x = x +
/// 1, but the file backend doesn't have any atomic increment.
/// This does get/parse/add/set as a fallback — not safe under
/// heavy concurrency, but good enough for file-cached counters
/// where you're probably the only process anyway.
///
@internal
pub fn default_increment(
  get_fn: fn(String) -> Result(String, CacheError),
  put_forever_fn: fn(String, String) -> Result(Nil, CacheError),
  key: String,
  by: Int,
) -> Result(Int, CacheError) {
  let current = case get_fn(key) {
    Ok(value) -> int.parse(value) |> result.unwrap(0)
    Error(_) -> 0
  }

  let new_value = current + by

  put_forever_fn(key, int.to_string(new_value))
  |> result.replace(new_value)
}

// ------------------------------------------------------------- Private Functions

/// Both remember() and remember_forever() have the same
/// get-or-compute logic — the only difference is how they store
/// the result (with or without TTL). Extracting the shared
/// pattern here means fixing a bug in the cache-miss path only
/// needs to happen in one place.
///
fn do_remember(
  pool: CachePool,
  key: String,
  compute: fn() -> Result(String, e),
  store: fn(String) -> Result(Nil, CacheError),
) -> Result(String, CacheError) {
  case get(pool, key) {
    Ok(value) -> Ok(value)
    Error(NotFound) -> {
      use value <- result.try(
        compute()
        |> result.replace_error(ComputeError("Compute function failed")),
      )
      let _ = store(value)
      Ok(value)
    }
    Error(e) -> Error(e)
  }
}
