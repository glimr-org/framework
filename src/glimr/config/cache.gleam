//// Cache Configuration
////
//// Centralizes cache store setup so application code doesn't
//// need to know about config file locations or parsing. Uses
//// ${VAR} syntax for secrets like Redis URLs to keep credentials
//// out of version control.
////

import gleam/dict
import gleam/list
import glimr/cache/driver.{type CacheStore, DatabaseStore, FileStore, RedisStore}
import glimr/config/config
import simplifile
import tom

// ------------------------------------------------------------- Public Functions

/// Safe to call repeatedly from hot paths since results are
/// cached after first load. Returns empty list on missing/invalid
/// config to let apps start without cache configuration.
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

/// Separated from load() to keep caching logic distinct from
/// file I/O. Makes the caching behavior easier to test and
/// reason about independently.
///
fn load_from_file() -> List(CacheStore) {
  case simplifile.read("config/cache.toml") {
    Ok(content) -> parse(content)
    Error(_) -> []
  }
}

/// Expects [stores.*] tables so users can define multiple named
/// stores (e.g., main, sessions, redis) with different backends
/// and switch between them at runtime.
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

/// Maps driver field to the appropriate CacheStore variant.
/// Defaults to file for unknown drivers since it requires no
/// external dependencies and works out of the box.
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

/// Stores parsed config in persistent_term for fast access
/// across all processes. Avoids re-parsing TOML on every
/// cache operation which would add unnecessary latency.
///
@external(erlang, "glimr_kernel_ffi", "cache_cache_config")
fn cache(stores: List(CacheStore)) -> Nil

/// Retrieves cached config from persistent_term. Returns Error
/// if not yet cached, signaling that load_from_file() should
/// be called to populate the cache.
///
@external(erlang, "glimr_kernel_ffi", "get_cached_cache_config")
fn get_cached() -> Result(List(CacheStore), Nil)
