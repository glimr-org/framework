//// Cache Configuration
////
//// Multiple cache backends (file, database, Redis) can coexist
//// in the same app, each with different settings. Parsing the
//// TOML file once and caching the result in persistent_term
//// lets every process access the store list without re-reading
//// the file. The ${VAR} syntax for Redis URLs and pool sizes
//// keeps secrets out of version control while still allowing
//// per-environment configuration.
////

import gleam/dict
import gleam/list
import glimr/cache/driver.{type CacheStore, DatabaseStore, FileStore, RedisStore}
import glimr/config/config
import simplifile
import tom

// ------------------------------------------------------------- Public Functions

/// Called by start functions and console commands that need
/// the store list, so it must be fast on repeated calls. The
/// persistent_term cache makes subsequent calls near-zero-cost.
/// Returning an empty list on missing or invalid config lets
/// apps start without a cache.toml file — caching is optional.
///
pub fn load() -> List(CacheStore) {
  case get_cached() {
    Ok(stores) -> stores
    Error(_) -> {
      let stores = load_from_file()
      cache(stores)
      stores
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// Separating file I/O from caching lets load() decide whether
/// to read the file or use the cache. Returning an empty list
/// on read failure avoids crashing apps that don't use caching.
///
fn load_from_file() -> List(CacheStore) {
  case simplifile.read("config/cache.toml") {
    Ok(content) -> parse(content)
    Error(_) -> []
  }
}

/// The [stores.*] nesting lets users define multiple named
/// stores (e.g., main, sessions, redis) each with a different
/// backend and settings. Iterating dict.to_list preserves all
/// entries so no store definition is silently dropped.
///
fn parse(content: String) -> List(CacheStore) {
  case tom.parse(content) {
    Ok(toml) -> {
      case dict.get(toml, "stores") {
        Ok(tom.Table(stores)) -> {
          stores
          |> dict.to_list
          |> list.map(fn(entry) {
            let #(name, store_toml) = entry
            parse_store(name, store_toml)
          })
        }
        _ -> []
      }
    }
    Error(_) -> []
  }
}

/// Each driver string maps to a specific CacheStore variant
/// with its own required fields. Defaulting to FileStore for
/// unknown or missing driver values means a typo degrades to
/// the simplest backend rather than crashing — file-based
/// caching works out of the box with no external dependencies.
///
fn parse_store(name: String, toml: tom.Toml) -> CacheStore {
  let driver = config.get_string(toml, "driver", "file")

  case driver {
    "redis" ->
      RedisStore(
        name: name,
        url: config.get_env_string(toml, "url"),
        pool_size: config.get_env_int(toml, "pool_size"),
      )
    "database" ->
      DatabaseStore(
        name: name,
        database: config.get_string(toml, "database", "main"),
        table: config.get_string(toml, "table", "cache"),
      )
    _ ->
      FileStore(name: name, path: config.get_string(toml, "path", "priv/cache"))
  }
}

// ------------------------------------------------------------- FFI Bindings

/// Stores the parsed store list in persistent_term so every
/// subsequent call to load() across all BEAM processes gets a
/// near-zero-cost read instead of re-parsing the TOML file.
///
@external(erlang, "glimr_kernel_ffi", "cache_cache_config")
fn cache(stores: List(CacheStore)) -> Nil

/// Returns the cached store list if it exists, or Error(Nil)
/// on the first call before cache() has been called. load()
/// uses this to skip file I/O on every request after the
/// initial load.
///
@external(erlang, "glimr_kernel_ffi", "get_cached_cache_config")
fn get_cached() -> Result(List(CacheStore), Nil)
