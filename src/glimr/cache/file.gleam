//// File Cache Entry Point
////
//// Single entry point for file-based caching so application code
//// doesn't need to know about config loading or pool management.
//// Keeps cache setup to one line in application supervisors.
////

import glimr/cache/driver
import glimr/cache/file/pool.{type Pool}
import glimr/config/cache
import glimr/session/file_store
import glimr/session/session.{type Session}
import glimr/session/store

// ------------------------------------------------------------- Public Functions

/// Looks up the named store in config/cache.toml and starts a
/// pool for it. Panics on missing/invalid config to fail fast
/// during app startup rather than silently degrading later.
///
pub fn start(name: String) -> Pool {
  let stores = cache.load()
  let store = driver.find_by_name(name, stores)
  pool.start_pool(store)
}

/// Initializes the session store using the filesystem as the
/// backend. Session files are stored in a sessions subdirectory
/// of the file cache pool's path. The store is cached in
/// persistent_term for fast access.
///
pub fn start_session(pool: Pool) -> Session {
  let session = file_store.create(pool)
  store.cache_store(session)

  session.empty()
}
