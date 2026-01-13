//// File Cache Pool
////
//// Provides a pool type for file-based caching. The pool holds 
//// the cache directory path and provides a consistent API with 
//// other cache drivers like Redis.

import glimr/cache/driver.{type CacheStore, FileStore}

// ------------------------------------------------------------- Public Types

/// Opaque pool type that wraps the cache directory path.
/// Provides API consistency with connection-pooled drivers like 
/// Redis.
///
pub opaque type Pool {
  Pool(path: String)
}

// ------------------------------------------------------------- Public Functions

/// Creates a new file cache pool from a FileStore configuration.
/// Panics if called with a non-FileStore. Use the file.start
/// function for a safer entry point.
///
pub fn start_pool(store: CacheStore) -> Pool {
  case store {
    FileStore(_, path) -> Pool(path)
    _ ->
      panic as "start_pool called with non-FileStore. Use the appropriate driver for your store type."
  }
}

/// Stops the pool. For file cache this is a no-op since there
/// are no connections to close, but it provides API consistency
/// with connection-pooled drivers.
///
pub fn stop_pool(_pool: Pool) -> Nil {
  Nil
}

/// Returns the base path for this cache pool. Used internally
/// by cache operations to construct file paths for cached
/// entries.
///
pub fn get_path(pool: Pool) -> String {
  pool.path
}
