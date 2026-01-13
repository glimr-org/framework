//// File Cache Entry Point
////
//// Provides the main entry point for starting file cache pools
//// from configured cache stores.

import glimr/cache/driver.{type CacheStore}
import glimr/cache/file/pool.{type Pool}

// ------------------------------------------------------------- Public Functions

/// Starts a file cache pool by name from a list of cache stores.
/// Panics if the store is not found or is not a FileStore.
/// Returns a pool handle for cache operations.
///
pub fn start(name: String, stores: List(CacheStore)) -> Pool {
  let store = driver.find_by_name(name, stores)
  pool.start_pool(store)
}
