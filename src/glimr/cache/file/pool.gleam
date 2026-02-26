//// File Cache Pool
////
//// Redis and database cache backends need connection pools
//// with supervisors and health checks, but the file backend
//// just needs to know where to write. Wrapping that path in an
//// opaque Pool type means the rest of the cache system doesn't
//// need to care which backend it's talking to — it just passes
//// the pool around and everything works.

import glimr/cache/driver.{type CacheStore, FileStore}

// ------------------------------------------------------------- Public Types

/// Making this opaque prevents anyone from constructing a Pool
/// with a bad path that hasn't gone through start_pool's
/// validation. It also keeps the internal representation
/// flexible — if we ever need to add metadata like a pool name
/// or stats, existing code won't break.
///
pub opaque type Pool {
  Pool(path: String)
}

// ------------------------------------------------------------- Public Functions

/// The FileStore variant carries the cache directory path from
/// the config. Panicking on a non-FileStore catches wiring
/// mistakes at boot — if someone accidentally passes a
/// RedisStore config to the file cache driver, they'll know
/// immediately instead of getting mysterious filesystem errors
/// at runtime.
///
pub fn start_pool(store: CacheStore) -> Pool {
  case store {
    FileStore(_, path) -> Pool(path)
    _ ->
      panic as "start_pool called with non-FileStore. Use the appropriate driver for your store type."
  }
}

/// File caches don't have connections to tear down, so this is
/// a no-op. It exists because the CachePool interface expects
/// every backend to have a stop function — without it, calling
/// cache.stop() on a file-backed pool would crash instead of
/// gracefully doing nothing.
///
pub fn stop_pool(_pool: Pool) -> Nil {
  Nil
}

/// Pool is opaque so cache operations can't read the path field
/// directly. This accessor is the trade-off for keeping the
/// type safe — cache.gleam calls this to build file paths for
/// each key without knowing the pool's internal structure.
///
pub fn get_path(pool: Pool) -> String {
  pool.path
}
