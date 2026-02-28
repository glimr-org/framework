//// File Cache Entry Point
////
//// This is the mirror of glimr_redis/redis.gleam but for the
//// file backend. Application code calls file.start("default")
//// and gets back a CachePool without needing to know about
//// config loading, pool construction, or the file cache
//// internals. One line in your main module and you're caching.

import glimr/cache/cache.{type CachePool}
import glimr/cache/driver
import glimr/cache/file/cache as file_cache
import glimr/cache/file/pool
import glimr/config/cache as cache_config
import glimr/session/file_store
import glimr/session/session.{type Session}
import glimr/session/store

// ------------------------------------------------------------- Public Functions

/// Loads the cache config, finds the named file store, and
/// returns a ready-to-use CachePool. Panicking on bad config is
/// intentional — a missing cache directory at boot is better
/// caught immediately than discovered when the first request
/// tries to write a file and gets a confusing filesystem error.
///
pub fn start(name: String) -> CachePool {
  let internal = start_pool(name)
  wrap_pool(internal)
}

/// Registers a file-based session store so the session
/// middleware can load, save, and destroy sessions without
/// knowing which backend is active. Unlike Redis where TTLs
/// handle expiration, file sessions need periodic garbage
/// collection to clean up expired session files — that's
/// handled by the file_store module's GC callback.
///
pub fn start_session(pool: pool.Pool) -> Session {
  let session = file_store.create(pool)
  store.cache_store(session)

  session.empty()
}

// ------------------------------------------------------------- Internal Public Functions

/// The public start() wraps the pool into a CachePool right
/// away, but session setup needs the raw Pool type because the
/// session file store talks to the filesystem directly rather
/// than going through the cache abstraction. Exposing this as
/// @internal lets both paths share config loading.
///
@internal
pub fn start_pool(name: String) -> pool.Pool {
  let stores = cache_config.load()
  let store = driver.find_by_name(name, stores)
  pool.start_pool(store)
}

/// Wires the file cache operations into the CachePool's closure
/// slots. The increment closure is interesting — file caches
/// don't have atomic increment, so it falls back to
/// default_increment which does get/parse/add/set. Not
/// concurrency-safe, but file caches aren't typically used in
/// high-concurrency scenarios anyway.
///
@internal
pub fn wrap_pool(internal: pool.Pool) -> CachePool {
  let get_fn = fn(key) { file_cache.get(internal, key) }
  let put_forever_fn = fn(key, value) {
    file_cache.put_forever(internal, key, value)
  }

  cache.new_pool(
    get: get_fn,
    put: fn(key, value, ttl) { file_cache.put(internal, key, value, ttl) },
    put_forever: put_forever_fn,
    forget: fn(key) { file_cache.forget(internal, key) },
    flush: fn() { file_cache.flush(internal) },
    increment: fn(key, by) {
      cache.default_increment(get_fn, put_forever_fn, key, by)
    },
    has: fn(key) { file_cache.has(internal, key) },
    stop: fn() { pool.stop_pool(internal) },
  )
}
