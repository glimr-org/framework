//// Cache Store Drivers
////
//// Cache config lives in the user's config_cache.gleam file as
//// a list of CacheStore variants. This module defines those
//// variants and the lookup functions that the entry points
//// (file.gleam, redis.gleam, etc.) use to find the right store
//// at boot. It's the bridge between "what the developer
//// configured" and "what the backend needs to start."

import gleam/list

// ------------------------------------------------------------- Public Types

/// Each variant carries just enough config to start its
/// backend. RedisStore wraps url and pool_size in Result
/// because those come from environment variables that might not
/// be set — deferring the error to pool startup means the
/// config module doesn't need to panic during parsing, and the
/// error message can point at exactly which env var is missing.
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

/// Console commands need to know which backend a store uses so
/// they can dispatch to the right driver — but they don't need
/// the full config details. This enum gives them a clean way to
/// branch on backend type without pattern matching on the full
/// CacheStore and ignoring most fields.
///
pub type StoreType {
  File
  Redis
  Database
}

// ------------------------------------------------------------- Public Functions

/// Extracts just the backend type from a store config. The
/// console cache:clear command uses this to decide whether to
/// call the file, Redis, or database flush logic — it doesn't
/// need the URL or path, just which driver to dispatch to.
///
pub fn store_type(store: CacheStore) -> StoreType {
  case store {
    FileStore(_, _) -> File
    RedisStore(_, _, _) -> Redis
    DatabaseStore(_, _, _) -> Database
  }
}

/// Every store variant has a name field, but you'd need a case
/// expression to get at it since each variant has different
/// shapes. This saves callers from writing the same
/// three-branch pattern match every time they just want the
/// name — which comes up a lot in config lookup and logging.
///
pub fn store_name(store: CacheStore) -> String {
  case store {
    FileStore(name, _) -> name
    RedisStore(name, _, _) -> name
    DatabaseStore(name, _, _) -> name
  }
}

/// The file cache needs to know its directory path, but calling
/// this on a Redis or database store is a programming error —
/// those backends don't have filesystem paths. Panicking with
/// the store name in the message makes it obvious which
/// misconfigured store caused the crash.
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

/// Same idea as store_path but for the database cache's table
/// name. The database backend needs to know which table holds
/// cached entries, and calling this on a file or Redis store is
/// a wiring mistake that should be caught immediately rather
/// than silently returning garbage.
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

/// Developers reference cache stores by name in their code —
/// cache.start("default") or cache.start("sessions"). If they
/// typo the name, the worst thing we could do is silently
/// return nothing and let them debug a missing cache later.
/// Panicking with the exact name and pointing at
/// config_cache.gleam tells them exactly what to fix.
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

/// Database migrations need to auto-create the cache table, but
/// they only know the database connection name, not the cache
/// store name. This finds the DatabaseStore for a given
/// database — and catches the ambiguous case where someone
/// configured two DatabaseStores pointing at the same database,
/// which would make it impossible to know which table to use.
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
