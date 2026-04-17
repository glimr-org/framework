//// Cache API
////
//// Every cache backend (Redis, file, SQLite, Postgres) has its
//// own way of talking to storage, but application code
//// shouldn't care which one is active. This module defines the
//// CachePool type that all backends produce, composite
//// operations like remember and JSON helpers, plus the
//// built-in file and database backends. Swapping from Redis to
//// file caching is a config change, not a code change.
////

import gleam/bit_array
import gleam/crypto
import gleam/dict
import gleam/dynamic/decode
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/result
import gleam/string
import glimr/config/config
import glimr/db/db.{type DbPool}
import glimr/filesystem/filesystem
import glimr/session.{type SessionStore}
import glimr/utils/unix_timestamp
import simplifile
import tom

// ------------------------------------------------------------- Public Types

/// Having typed error variants instead of a generic string
/// error lets callers respond differently to each situation.
/// For example, try_remember_json treats SerializationError as
/// "the cached format changed, recompute" but treats
/// ConnectionError as "something is actually broken, bail out."
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
}

/// Each backend captures its own connection pool inside these
/// closures at startup. Application code never imports Redis or
/// SQLite modules directly, it just calls cache.get() and the
/// right thing happens. Making this opaque means you can't
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

// ------------------------------------------------------------- Internal Public Types

/// Each variant carries just enough config to start its
/// backend. RedisStore wraps url and pool_size in Result
/// because those come from environment variables that might not
/// be set — deferring the error to pool startup means the
/// config module doesn't need to panic during parsing.
///
@internal
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
/// they can dispatch to the right driver without pattern
/// matching on the full CacheStore and ignoring most fields.
///
@internal
pub type StoreType {
  File
  Redis
  Database
}

/// The file cache pool wraps a directory path. Making this
/// opaque prevents anyone from constructing a pool with a bad
/// path that hasn't gone through file_start_pool's validation.
///
@internal
pub opaque type FilePool {
  FilePool(path: String)
}

// ------------------------------------------------------------- Public Functions

/// Delegates to the backend's get closure. Returns NotFound if
/// the key doesn't exist or has expired.
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
/// key exists.
///
pub fn has(pool: CachePool, key: String) -> Bool {
  pool.has(key)
}

/// Console commands create temporary pools that should be
/// cleaned up when they're done. Without explicit shutdown,
/// those connections would sit open until the process exits.
///
pub fn stop(pool: CachePool) -> Nil {
  pool.stop()
}

/// Get-then-delete in one call. Perfect for one-time tokens
/// like email verification codes or CSRF tokens — you want to
/// read the value and ensure it can never be used again.
///
pub fn pull(pool: CachePool, key: String) -> Result(String, CacheError) {
  use value <- result.try(get(pool, key))
  let _ = forget(pool, key)

  Ok(value)
}

/// Just increment with a negative value, but having a named
/// function makes code like "decrement remaining attempts" read
/// naturally instead of the confusing "increment(key, -1)".
///
pub fn decrement(
  pool: CachePool,
  key: String,
  by: Int,
) -> Result(Int, CacheError) {
  increment(pool, key, -by)
}

/// Try the cache first, and if it's a miss, run a fallible
/// computation (database query, API call, etc.). On `Ok(value)`
/// the result gets cached and returned; on `Error(e)` the error
/// is propagated untouched and nothing is written to the cache.
///
pub fn remember(
  pool: CachePool,
  key: String,
  ttl_seconds: Int,
  compute: fn() -> Result(String, e),
) -> Result(String, e) {
  do_remember(pool, key, compute, fn(value) {
    put(pool, key, value, ttl_seconds)
  })
}

/// Same as remember but the cached result never expires.
/// Good for values that are expensive to compute but rarely
/// change. The only way to refresh these is an explicit
/// forget() or flush().
///
pub fn remember_forever(
  pool: CachePool,
  key: String,
  compute: fn() -> Result(String, e),
) -> Result(String, e) {
  do_remember(pool, key, compute, fn(value) { put_forever(pool, key, value) })
}

/// The JSON remember pattern with explicit error propagation.
/// On a cache hit, returns `Ok(value)`. On a miss, runs the
/// compute callback; `Ok(value)` is cached and returned, while
/// `Error(e)` is passed through without touching the cache.
///
pub fn remember_json(
  pool: CachePool,
  key: String,
  ttl_seconds: Int,
  decoder: decode.Decoder(a),
  encoder: fn(a) -> Json,
  compute: fn() -> Result(a, e),
) -> Result(a, e) {
  case get_json(pool, key, decoder) {
    Ok(value) -> Ok(value)
    Error(_) ->
      case compute() {
        Ok(value) -> {
          let _ = put_json(pool, key, value, encoder, ttl_seconds)
          Ok(value)
        }
        Error(e) -> Error(e)
      }
  }
}

/// Same as remember_json but the cached result never
/// expires. Good for things like a site's configuration or
/// navigation tree that are expensive to build from the
/// database but change so rarely that TTL-based expiry would
/// just waste computation.
///
pub fn remember_json_forever(
  pool: CachePool,
  key: String,
  decoder: decode.Decoder(a),
  encoder: fn(a) -> Json,
  compute: fn() -> Result(a, e),
) -> Result(a, e) {
  case get_json(pool, key, decoder) {
    Ok(value) -> Ok(value)
    Error(_) ->
      case compute() {
        Ok(value) -> {
          let _ = put_json_forever(pool, key, value, encoder)
          Ok(value)
        }
        Error(e) -> Error(e)
      }
  }
}

/// Fetches a cached string and runs it through a JSON decoder
/// in one step. If the cached value doesn't match the decoder,
/// you get a SerializationError rather than a generic parse
/// failure.
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
/// manually call json.to_string before every put.
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

/// Starts a file-backed cache pool. Loads config, finds the
/// named file store, and returns a ready-to-use CachePool.
///
pub fn start_file(name: String) -> CachePool {
  let pool = file_start_pool(name)
  file_wrap_pool(pool)
}

/// Starts a file-backed session store. Convenience for apps
/// that use file caching for both cache and sessions.
///
pub fn file_session_store(name: String) -> SessionStore {
  let pool = file_start_pool(name)
  session.file_store(pool.path)
}

// ------------------------------------------------------------- Internal Public Functions

/// This is how backends register themselves. Each closure
/// captures the backend's internal pool — so when app code
/// calls cache.get(), it runs the Redis or file implementation
/// without knowing which one it is.
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

/// Fallback increment for backends without atomic increment.
/// Does get/parse/add/set — not safe under heavy concurrency,
/// but good enough for file-cached counters where you're
/// probably the only process anyway.
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

/// Extracts just the backend type from a store config.
///
@internal
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
/// name.
///
@internal
pub fn store_name(store: CacheStore) -> String {
  case store {
    FileStore(name, _) -> name
    RedisStore(name, _, _) -> name
    DatabaseStore(name, _, _) -> name
  }
}

/// The file cache needs to know its directory path. Panicking
/// on a non-FileStore catches misconfiguration immediately.
///
@internal
pub fn store_path(store: CacheStore) -> String {
  case store {
    FileStore(_, path) -> path
    RedisStore(name, _, _) ->
      panic as { "Cannot get path from RedisStore '" <> name <> "'" }
    DatabaseStore(name, _, _) ->
      panic as { "Cannot get path from DatabaseStore '" <> name <> "'" }
  }
}

/// The database backend needs to know which table holds cached
/// entries. Panicking on a non-DatabaseStore catches wiring
/// mistakes immediately.
///
@internal
pub fn store_table(store: CacheStore) -> String {
  case store {
    DatabaseStore(_, _, table) -> table
    FileStore(name, _) ->
      panic as { "Cannot get table from FileStore '" <> name <> "'" }
    RedisStore(name, _, _) ->
      panic as { "Cannot get table from RedisStore '" <> name <> "'" }
  }
}

/// Stores are loaded once from config/cache.toml and then
/// cached in persistent_term so every subsequent call is a fast
/// lookup. Returns an empty list if no config exists.
///
@internal
pub fn load_stores() -> List(CacheStore) {
  case config.get_cached("cache_stores") {
    Ok(stores) -> stores
    Error(_) -> {
      let stores = load_stores_from_config()
      config.cache("cache_stores", stores)
      stores
    }
  }
}

/// Finds a cache store by name. Panics with a clear message
/// pointing at config/cache.toml if the name doesn't exist.
///
@internal
pub fn find_by_name(name: String, stores: List(CacheStore)) -> CacheStore {
  let store =
    list.find(stores, fn(store: CacheStore) { store_name(store) == name })

  case store {
    Ok(s) -> s
    _ ->
      panic as {
        "The cache store '" <> name <> "' does not exist in config/cache.toml"
      }
  }
}

/// Database migrations need to auto-create the cache table, but
/// they only know the database connection name, not the cache
/// store name. This finds the DatabaseStore for a given
/// database — and catches the ambiguous case where someone
/// configured two DatabaseStores pointing at the same database.
///
@internal
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
        <> "' in config/cache.toml",
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

/// Loads the named file store config and returns a raw
/// FilePool. Used by both start_file and file_session_store.
/// Console commands also use this to start file cache pools.
///
@internal
pub fn file_start_pool(name: String) -> FilePool {
  let stores = load_stores()
  let store = find_by_name(name, stores)
  case store {
    FileStore(_, path) -> FilePool(path)
    _ ->
      panic as "file_start_pool called with non-FileStore. Use the appropriate driver for your store type."
  }
}

/// Wires the file cache operations into a CachePool. The
/// increment closure falls back to default_increment which does
/// get/parse/add/set — not concurrency-safe, but file caches
/// aren't typically used in high-concurrency scenarios.
///
@internal
pub fn file_wrap_pool(pool: FilePool) -> CachePool {
  let get_fn = fn(key) { file_get(pool, key) }
  let put_forever_fn = fn(key, value) { file_put_forever(pool, key, value) }

  new_pool(
    get: get_fn,
    put: fn(key, value, ttl) { file_put(pool, key, value, ttl) },
    put_forever: put_forever_fn,
    forget: fn(key) { file_forget(pool, key) },
    flush: fn() { file_flush(pool) },
    increment: fn(key, by) {
      default_increment(get_fn, put_forever_fn, key, by)
    },
    has: fn(key) { file_has(pool, key) },
    stop: fn() { Nil },
  )
}

/// Returns the directory path from a FilePool.
///
@internal
pub fn file_pool_path(pool: FilePool) -> String {
  pool.path
}

/// Hashing the key with SHA256 and splitting the first four hex
/// characters into two directory levels (like git does with
/// objects) spreads files evenly and keeps each directory
/// small. The console cache:clear command needs this to resolve
/// paths for display.
///
@internal
pub fn file_key_to_path(pool: FilePool, key: String) -> String {
  let hash =
    crypto.hash(crypto.Sha256, bit_array.from_string(key))
    |> bit_array.base16_encode()
    |> string.lowercase()

  let dir1 = string.slice(hash, 0, 2)
  let dir2 = string.slice(hash, 2, 2)

  pool.path <> "/" <> dir1 <> "/" <> dir2 <> "/" <> hash
}

/// Starts a database-backed cache pool. Looks up the named
/// cache store in config, extracts the table name, and wires up
/// a CachePool against the provided database pool.
///
@internal
pub fn database_start(db_pool: DbPool, name: String) -> CachePool {
  let stores = load_stores()
  let store = find_by_name(name, stores)
  let table = extract_table(store)

  database_start_with_table(db_pool, table)
}

/// Starts a database-backed cache pool with an explicit table
/// name. Used by console commands that already know the table
/// from the store config.
///
@internal
pub fn database_start_with_table(db_pool: DbPool, table: String) -> CachePool {
  new_pool(
    get: fn(key) { db_get(db_pool, table, key) },
    put: fn(key, value, ttl) { db_put(db_pool, table, key, value, ttl) },
    put_forever: fn(key, value) { db_put_forever(db_pool, table, key, value) },
    forget: fn(key) { db_forget(db_pool, table, key) },
    flush: fn() { db_flush(db_pool, table) },
    increment: fn(key, by) { db_increment(db_pool, table, key, by) },
    has: fn(key) { db_has(db_pool, table, key) },
    stop: fn() { Nil },
  )
}

/// Creates the cache table if it doesn't exist. PostgreSQL uses
/// BIGINT for Unix timestamps because INTEGER maxes out at 2^31
/// (year 2038), while SQLite's INTEGER is already 64-bit.
///
@internal
pub fn database_create_table(
  db_pool: DbPool,
  table: String,
) -> Result(Nil, CacheError) {
  let int_type = case db.pool_driver(db_pool) {
    db.Postgres -> "BIGINT"
    db.Sqlite -> "INTEGER"
  }

  let sql = "CREATE TABLE IF NOT EXISTS " <> table <> " (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL,
    expiration " <> int_type <> " NOT NULL
  )"

  case db.exec(db_pool, sql, []) {
    Ok(_) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError(
        "Failed to create cache table: " <> string.inspect(e),
      ))
  }
}

/// Database entries don't expire automatically like Redis —
/// they sit there until someone deletes them. This cleans up
/// expired entries and can be hooked into a scheduled task.
///
@internal
pub fn database_cleanup_expired(
  db_pool: DbPool,
  table: String,
) -> Result(Nil, CacheError) {
  let now = unix_timestamp.now()
  let sql =
    "DELETE FROM " <> table <> " WHERE expiration > 0 AND expiration <= $1"

  case
    db.exec(db_pool, sql, [
      db.int(now),
    ])
  {
    Ok(_) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError(
        "Failed to cleanup expired entries: " <> string.inspect(e),
      ))
  }
}

// ------------------------------------------------------------- Private Functions

/// The core of remember and remember_forever. On a
/// cache hit, returns Ok(cached). On a miss, runs compute — Ok
/// values get stored, Error values pass through untouched. The
/// store callback is what varies between TTL and forever
/// variants.
///
fn do_remember(
  pool: CachePool,
  key: String,
  compute: fn() -> Result(String, e),
  store: fn(String) -> Result(Nil, CacheError),
) -> Result(String, e) {
  case get(pool, key) {
    Ok(value) -> Ok(value)
    Error(_) ->
      case compute() {
        Ok(value) -> {
          let _ = store(value)
          Ok(value)
        }
        Error(e) -> Error(e)
      }
  }
}

// ---- Config parsing

/// Reads config/cache.toml and turns each [stores.x] section
/// into a CacheStore variant. Returns an empty list if the file
/// doesn't exist — apps that don't use caching shouldn't need a
/// config file just to boot.
///
fn load_stores_from_config() -> List(CacheStore) {
  case config.get_table("cache.stores") {
    Ok(stores_table) -> {
      stores_table
      |> dict.to_list
      |> list.map(fn(entry) {
        let #(name, store_toml) = entry
        parse_store(name, store_toml)
      })
    }
    Error(_) -> []
  }
}

/// Turns a single TOML table into the right CacheStore variant
/// based on the "driver" key. Defaults to file if no driver is
/// specified, since that's the zero-setup option that works
/// without any external services.
///
fn parse_store(name: String, toml: tom.Toml) -> CacheStore {
  let driver_str = config.toml_get_string(toml, "driver", "file")

  case driver_str {
    "redis" ->
      RedisStore(
        name: name,
        url: config.toml_get_env_string(toml, "url"),
        pool_size: config.toml_get_env_int(toml, "pool_size"),
      )
    "database" ->
      DatabaseStore(
        name: name,
        database: config.toml_get_string(toml, "database", "main"),
        table: config.toml_get_string(toml, "table", "cache"),
      )
    _ ->
      FileStore(
        name: name,
        path: config.toml_get_string(toml, "path", "priv/cache"),
      )
  }
}

// ---- File cache backend

/// Reads a cache file from disk and checks whether it has
/// expired. Expired entries are deleted on read (lazy
/// expiration) so we don't need a background cleanup job for
/// the file backend.
///
fn file_get(pool: FilePool, key: String) -> Result(String, CacheError) {
  let path = file_key_to_path(pool, key)

  case simplifile.read(path) {
    Ok(content) -> parse_cache_file(content, path)
    Error(_) -> Error(NotFound)
  }
}

/// Writes a cache entry with an absolute expiration timestamp
/// on the first line. The format is dead simple — one line of
/// Unix timestamp, then the value — so parsing is just a single
/// split_once.
///
fn file_put(
  pool: FilePool,
  key: String,
  value: String,
  ttl_seconds: Int,
) -> Result(Nil, CacheError) {
  let expires_at = unix_timestamp.now() + ttl_seconds
  let content = int.to_string(expires_at) <> "\n" <> value
  write_cache_file(pool, key, content)
}

/// Writes a cache entry with expiration set to 0, which means
/// "never expires." The parse_cache_file function treats 0 as a
/// special sentinel that skips the timestamp check.
///
fn file_put_forever(
  pool: FilePool,
  key: String,
  value: String,
) -> Result(Nil, CacheError) {
  let content = "0\n" <> value
  write_cache_file(pool, key, content)
}

/// Deletes a single cache file. Treats "file not found" as
/// success so callers don't have to worry about whether the key
/// existed — important for idempotent invalidation.
///
fn file_forget(pool: FilePool, key: String) -> Result(Nil, CacheError) {
  let path = file_key_to_path(pool, key)

  case simplifile.delete(path) {
    Ok(_) -> Ok(Nil)
    Error(simplifile.Enoent) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError(
        "Failed to delete cache file: " <> string.inspect(e),
      ))
  }
}

/// Existence check that also respects expiration — a file with
/// an expired timestamp returns False, and the expired file
/// gets cleaned up as a side effect of the file_get call.
///
fn file_has(pool: FilePool, key: String) -> Bool {
  case file_get(pool, key) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Nukes the entire cache directory. Faster than walking every
/// file and deleting one by one, and the directory structure
/// gets recreated lazily the next time something is written.
///
fn file_flush(pool: FilePool) -> Result(Nil, CacheError) {
  case simplifile.delete(pool.path) {
    Ok(_) -> Ok(Nil)
    Error(simplifile.Enoent) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError("Failed to flush cache: " <> string.inspect(e)))
  }
}

/// Creates the two-level hash directory if needed and writes
/// the cache file. The directory creation is idempotent, so
/// concurrent writes to different keys in the same hash bucket
/// won't race.
///
fn write_cache_file(
  pool: FilePool,
  key: String,
  content: String,
) -> Result(Nil, CacheError) {
  let path = file_key_to_path(pool, key)
  let _ = filesystem.ensure_directory_exists(path)
  case simplifile.write(path, content) {
    Ok(_) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError("Failed to write cache file: " <> string.inspect(e)))
  }
}

/// Splits a cache file into its expiration timestamp and value.
/// If the timestamp is non-zero and in the past, deletes the
/// file and returns NotFound — this is the lazy expiration that
/// keeps the file backend from accumulating stale entries
/// without a cleanup job.
///
fn parse_cache_file(content: String, path: String) -> Result(String, CacheError) {
  use #(expires_str, value) <- result.try(
    string.split_once(content, "\n")
    |> result.replace_error(SerializationError("Invalid cache file format")),
  )
  use expires_at <- result.try(
    int.parse(expires_str)
    |> result.replace_error(SerializationError("Invalid expiration timestamp")),
  )
  case expires_at == 0 || expires_at > unix_timestamp.now() {
    True -> Ok(value)
    False -> {
      let _ = simplifile.delete(path)
      Error(NotFound)
    }
  }
}

// ---- Database cache backend

/// Fetches a cache entry from the database, filtering out
/// expired rows in the WHERE clause. Unlike the file backend,
/// expired entries aren't deleted on read — the
/// database_cleanup_expired function handles that on a
/// schedule.
///
fn db_get(
  db_pool: DbPool,
  table: String,
  key: String,
) -> Result(String, CacheError) {
  let now = unix_timestamp.now()
  let sql =
    "SELECT value FROM "
    <> table
    <> " WHERE key = $1 AND (expiration = 0 OR expiration > $2)"

  case
    db.query(
      db_pool,
      sql,
      [db.string(key), db.int(now)],
      decode.at([0], decode.string),
    )
  {
    Ok(db.QueryResult(_, [value])) -> Ok(value)
    Ok(_) -> Error(NotFound)
    Error(e) ->
      Error(ConnectionError("Failed to get cache key: " <> string.inspect(e)))
  }
}

/// Converts the TTL to an absolute Unix timestamp and delegates
/// to db_put_with_expiration. Absolute timestamps are simpler
/// to query ("WHERE expiration > now") than relative TTLs that
/// would need to be compared against the row's creation time.
///
fn db_put(
  db_pool: DbPool,
  table: String,
  key: String,
  value: String,
  ttl_seconds: Int,
) -> Result(Nil, CacheError) {
  let expiration = unix_timestamp.now() + ttl_seconds
  db_put_with_expiration(db_pool, table, key, value, expiration)
}

/// Stores with expiration 0, which the db_get query treats as
/// "never expires" via the OR condition.
///
fn db_put_forever(
  db_pool: DbPool,
  table: String,
  key: String,
  value: String,
) -> Result(Nil, CacheError) {
  db_put_with_expiration(db_pool, table, key, value, 0)
}

/// Deletes a single row. Like the file backend, this is
/// idempotent — deleting a key that doesn't exist still returns
/// Ok since DELETE WHERE affects zero rows without error.
///
fn db_forget(
  db_pool: DbPool,
  table: String,
  key: String,
) -> Result(Nil, CacheError) {
  let sql = "DELETE FROM " <> table <> " WHERE key = $1"

  case
    db.exec(db_pool, sql, [
      db.string(key),
    ])
  {
    Ok(_) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError("Failed to delete cache key: " <> string.inspect(e)))
  }
}

/// Deletes all rows in the cache table. Uses DELETE rather than
/// TRUNCATE for compatibility across SQLite and Postgres
/// without conditional SQL.
///
fn db_flush(db_pool: DbPool, table: String) -> Result(Nil, CacheError) {
  let sql = "DELETE FROM " <> table

  case db.exec(db_pool, sql, []) {
    Ok(_) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError("Failed to flush cache: " <> string.inspect(e)))
  }
}

/// Reads the current value, adds `by`, and writes it back,
/// preserving the original expiration. If the key doesn't exist
/// yet, creates it with the increment value and no expiration.
/// Not atomic across concurrent requests, but good enough for
/// most use cases since database caches are typically behind a
/// single app.
///
fn db_increment(
  db_pool: DbPool,
  table: String,
  key: String,
  by: Int,
) -> Result(Int, CacheError) {
  case db_get(db_pool, table, key) {
    Ok(value) -> {
      use current <- result.try(
        int.parse(value)
        |> result.replace_error(SerializationError("Value is not a number")),
      )
      let new_value = current + by
      let exp = case db_get_expiration(db_pool, table, key) {
        Ok(exp) -> exp
        Error(_) -> 0
      }
      db_put_with_expiration(db_pool, table, key, int.to_string(new_value), exp)
      |> result.replace(new_value)
    }
    Error(NotFound) -> {
      db_put_forever(db_pool, table, key, int.to_string(by))
      |> result.replace(by)
    }
    Error(e) -> Error(e)
  }
}

/// Checks existence by attempting a get — if the row exists and
/// hasn't expired, returns True.
///
fn db_has(db_pool: DbPool, table: String, key: String) -> Bool {
  case db_get(db_pool, table, key) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Fetches just the expiration column for a key. Used by
/// db_increment to preserve the original TTL when updating a
/// counter — without this, incrementing a key would
/// accidentally make it permanent.
///
fn db_get_expiration(
  db_pool: DbPool,
  table: String,
  key: String,
) -> Result(Int, CacheError) {
  let sql = "SELECT expiration FROM " <> table <> " WHERE key = $1"

  case db.query(db_pool, sql, [db.string(key)], decode.at([0], decode.int)) {
    Ok(db.QueryResult(_, [exp])) -> Ok(exp)
    Ok(_) -> Error(NotFound)
    Error(e) ->
      Error(ConnectionError("Failed to get expiration: " <> string.inspect(e)))
  }
}

/// The shared write path for all database puts. Uses INSERT ON
/// CONFLICT DO UPDATE (upsert) so setting a key that already
/// exists overwrites it in one query instead of requiring a
/// separate check-then-insert.
///
fn db_put_with_expiration(
  db_pool: DbPool,
  table: String,
  key: String,
  value: String,
  expiration: Int,
) -> Result(Nil, CacheError) {
  let sql =
    "INSERT INTO " <> table <> " (key, value, expiration) VALUES ($1, $2, $3)
    ON CONFLICT (key) DO UPDATE SET value = excluded.value, expiration = excluded.expiration"

  case
    db.exec(db_pool, sql, [
      db.string(key),
      db.string(value),
      db.int(expiration),
    ])
  {
    Ok(_) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError("Failed to set cache key: " <> string.inspect(e)))
  }
}

/// Pulls the table name out of a DatabaseStore variant. Panics
/// on other variants because calling this with a FileStore or
/// RedisStore means the wiring is wrong — better to crash
/// immediately than silently use a wrong table name.
///
fn extract_table(store: CacheStore) -> String {
  case store {
    DatabaseStore(_, _, table) -> table
    _ -> panic as "Cannot create database cache from non-Database store"
  }
}
