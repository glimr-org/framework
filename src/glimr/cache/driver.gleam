//// Cache Store Configuration
////
//// Provides store types for configuring cache stores in a 
//// type-safe way. Users define stores in their 
//// config_cache.gleam file which is loaded at runtime.

import gleam/list

// ------------------------------------------------------------- Public Types

/// Represents a named cache store configuration. Each store has
/// a name that identifies it and configuration parameters
/// specific to the store type.
///
/// Use `FileStore` for file-based caching.
/// Use `RedisStore` for Redis-based caching.
/// Use `DatabaseStore` for database-backed caching.
///
pub type CacheStore {
  FileStore(name: String, path: String)
  RedisStore(
    name: String,
    url: Result(String, String),
    pool_size: Result(Int, String),
  )
  DatabaseStore(name: String, database: String, table: String)
}

/// Identifies the underlying store type for a cache store.
/// Used to distinguish between File, Redis, and Database
/// backends when specific configuration details aren't needed.
///
pub type StoreType {
  File
  Redis
  Database
}

// ------------------------------------------------------------- Public Functions

/// Returns whether the store is File, Redis, or Database.
/// Extracts the underlying backend type from a CacheStore
/// without exposing the specific configuration parameters.
///
pub fn store_type(store: CacheStore) -> StoreType {
  case store {
    FileStore(_, _) -> File
    RedisStore(_, _, _) -> Redis
    DatabaseStore(_, _, _) -> Database
  }
}

/// Returns the name identifying this store configuration. The
/// name is used to look up specific stores when multiple cache
/// stores are configured in the app.
///
pub fn store_name(store: CacheStore) -> String {
  case store {
    FileStore(name, _) -> name
    RedisStore(name, _, _) -> name
    DatabaseStore(name, _, _) -> name
  }
}

/// Returns the path for a FileStore, or panics if called on
/// other store types. Use store_type to check the backend
/// before calling this function.
///
pub fn store_path(store: CacheStore) -> String {
  case store {
    FileStore(_, path) -> path
    RedisStore(name, _, _) ->
      panic as { "Cannot get path from RedisStore '" <> name <> "'" }
    DatabaseStore(name, _, _) ->
      panic as { "Cannot get path from DatabaseStore '" <> name <> "'" }
  }
}

/// Returns the table name for a DatabaseStore, or panics if
/// called on other store types. Use store_type to check the
/// backend before calling this function.
///
pub fn store_table(store: CacheStore) -> String {
  case store {
    DatabaseStore(_, _, table) -> table
    FileStore(name, _) ->
      panic as { "Cannot get table from FileStore '" <> name <> "'" }
    RedisStore(name, _, _) ->
      panic as { "Cannot get table from RedisStore '" <> name <> "'" }
  }
}

/// Searches through a list of stores to find one with
/// the specified name. Panics if no store matches the
/// given name in the configuration.
///
pub fn find_by_name(name: String, stores: List(CacheStore)) -> CacheStore {
  let store =
    list.find(stores, fn(store: CacheStore) { store_name(store) == name })

  case store {
    Ok(s) -> s
    _ ->
      panic as {
        "The cache store '"
        <> name
        <> "' does not exist in your config_cache.gleam"
      }
  }
}

/// Searches through a list of stores to find a DatabaseStore
/// for the specified database connection name. Returns Ok with
/// the store if exactly one is found, or an Error describing
/// the problem (none found or multiple found).
///
pub fn find_database_store(
  database: String,
  stores: List(CacheStore),
) -> Result(CacheStore, String) {
  let matching =
    list.filter(stores, fn(store) {
      case store {
        DatabaseStore(_, db, _) -> db == database
        _ -> False
      }
    })

  case matching {
    [] ->
      Error(
        "No cache store configured for database '"
        <> database
        <> "' in config_cache.gleam",
      )
    [store] -> Ok(store)
    _ ->
      Error(
        "Multiple cache stores configured for database '"
        <> database
        <> "'. Only one DatabaseStore per database is allowed.",
      )
  }
}
