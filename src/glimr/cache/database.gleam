//// Database Cache Backend
////
//// Sometimes you don't want to run Redis just for caching — if
//// you already have PostgreSQL or SQLite, why not use it? This
//// backend stores cached values in a regular database table,
//// with the same API as every other backend. Both Postgres and
//// SQLite share the exact same SQL thanks to pool_connection's
//// placeholder conversion; the only difference is the integer
//// type for the expiration column.

import gleam/dynamic/decode
import gleam/int
import gleam/result
import gleam/string
import glimr/cache/cache.{
  type CacheError, type CachePool, ConnectionError, NotFound, SerializationError,
}
import glimr/cache/driver.{type CacheStore, DatabaseStore}
import glimr/config/cache as cache_config
import glimr/db/pool_connection.{type Pool as DbPool}
import glimr/utils/unix_timestamp

// ------------------------------------------------------------- Public Functions

/// Looks up the named cache store in config, extracts the table
/// name, and wires up a CachePool. This is the one-liner
/// developers use in their main module — they already have a
/// database pool from their app setup, so all they need is the
/// store name to get caching going.
///
pub fn start(db_pool: DbPool, name: String) -> CachePool {
  let stores = cache_config.load()
  let store = driver.find_by_name(name, stores)
  let table = extract_table(store)

  start_with_table(db_pool, table)
}

/// PostgreSQL uses BIGINT for Unix timestamps because INTEGER
/// maxes out at 2^31 (year 2038), while SQLite's INTEGER is
/// already 64-bit so it's fine. Checking the pool driver here
/// means developers don't need to think about this — the right
/// column type is chosen automatically based on which database
/// they're actually using.
///
pub fn create_table(db_pool: DbPool, table: String) -> Result(Nil, CacheError) {
  let int_type = case pool_connection.pool_driver(db_pool) {
    pool_connection.Postgres -> "BIGINT"
    pool_connection.Sqlite -> "INTEGER"
  }

  let sql = "CREATE TABLE IF NOT EXISTS " <> table <> " (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL,
    expiration " <> int_type <> " NOT NULL
  )"

  case pool_connection.exec(db_pool, sql, []) {
    Ok(_) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError(
        "Failed to create cache table: " <> string.inspect(e),
      ))
  }
}

/// Unlike Redis where keys expire automatically via TTLs,
/// database entries just sit there forever until someone
/// deletes them. Without periodic cleanup, the cache table
/// grows without bound. This is called by the session GC and
/// can be hooked into a scheduled task to keep the table lean.
///
pub fn cleanup_expired(
  db_pool: DbPool,
  table: String,
) -> Result(Nil, CacheError) {
  let now = unix_timestamp.now()
  let sql =
    "DELETE FROM " <> table <> " WHERE expiration > 0 AND expiration <= $1"

  case
    pool_connection.exec(db_pool, sql, [
      pool_connection.int(now),
    ])
  {
    Ok(_) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError(
        "Failed to cleanup expired entries: " <> string.inspect(e),
      ))
  }
}

// ------------------------------------------------------------- Internal Public Functions

/// Migrations and console commands already know the table name
/// from the store config, so they can skip the config lookup
/// and go straight to wiring. The stop closure is a no-op
/// because the database pool is shared with the rest of the app
/// — shutting it down here would kill everything else that uses
/// the same database connection.
///
@internal
pub fn start_with_table(db_pool: DbPool, table: String) -> CachePool {
  cache.new_pool(
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

// ------------------------------------------------------------- Private Functions

/// The WHERE clause filters out expired entries in the same
/// query — so a get() on an expired key returns NotFound
/// without needing a separate expiration check. The "expiration
/// = 0" branch handles put_forever entries that should never
/// expire.
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
    pool_connection.query(
      db_pool,
      sql,
      [pool_connection.string(key), pool_connection.int(now)],
      decode.at([0], decode.string),
    )
  {
    Ok(pool_connection.QueryResult(_, [value])) -> Ok(value)
    Ok(_) -> Error(NotFound)
    Error(e) ->
      Error(ConnectionError("Failed to get cache key: " <> string.inspect(e)))
  }
}

/// Computes the absolute expiry timestamp and delegates to
/// put_with_expiration. Storing absolute times rather than
/// durations means the expiration check in db_get is a simple
/// comparison against now() — no arithmetic needed at read
/// time.
///
fn db_put(
  db_pool: DbPool,
  table: String,
  key: String,
  value: String,
  ttl_seconds: Int,
) -> Result(Nil, CacheError) {
  let expiration = unix_timestamp.now() + ttl_seconds
  put_with_expiration(db_pool, table, key, value, expiration)
}

/// An expiration of 0 is the sentinel for "never expires." The
/// db_get query knows to treat 0 specially, so these entries
/// survive cleanup_expired too (which only deletes rows where
/// expiration > 0 AND expiration <= now).
///
fn db_put_forever(
  db_pool: DbPool,
  table: String,
  key: String,
  value: String,
) -> Result(Nil, CacheError) {
  put_with_expiration(db_pool, table, key, value, 0)
}

/// DELETE is naturally idempotent in SQL — deleting a row that
/// doesn't exist just returns 0 affected rows, not an error. So
/// we don't need any special handling for missing keys.
///
fn db_forget(
  db_pool: DbPool,
  table: String,
  key: String,
) -> Result(Nil, CacheError) {
  let sql = "DELETE FROM " <> table <> " WHERE key = $1"

  case
    pool_connection.exec(db_pool, sql, [
      pool_connection.string(key),
    ])
  {
    Ok(_) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError("Failed to delete cache key: " <> string.inspect(e)))
  }
}

/// Deletes all rows, not just expired ones. This is scoped to
/// the cache table, so other app data in the same database is
/// safe — unlike Redis flush which needs prefix matching to
/// avoid nuking unrelated keys.
///
fn db_flush(db_pool: DbPool, table: String) -> Result(Nil, CacheError) {
  let sql = "DELETE FROM " <> table

  case pool_connection.exec(db_pool, sql, []) {
    Ok(_) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError("Failed to flush cache: " <> string.inspect(e)))
  }
}

/// Database increment is trickier than Redis INCRBY because SQL
/// doesn't have an atomic increment-or-insert. We do
/// get/parse/add/put, which isn't perfectly atomic, but the key
/// detail is preserving the original expiration — if you
/// incremented a rate limit counter and reset its TTL each
/// time, it would never expire and the rate limit would never
/// reset.
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
      let exp = case get_expiration(db_pool, table, key) {
        Ok(exp) -> exp
        Error(_) -> 0
      }
      put_with_expiration(db_pool, table, key, int.to_string(new_value), exp)
      |> result.replace(new_value)
    }
    Error(NotFound) -> {
      db_put_forever(db_pool, table, key, int.to_string(by))
      |> result.replace(by)
    }
    Error(e) -> Error(e)
  }
}

/// Delegates to db_get and discards the value — we only care
/// whether the query found a non-expired row. This does
/// transfer the value over the wire unlike Redis EXISTS, but
/// database cache values are typically small enough that it
/// doesn't matter.
///
fn db_has(db_pool: DbPool, table: String, key: String) -> Bool {
  case db_get(db_pool, table, key) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Needed by db_increment to preserve the existing TTL when
/// updating a counter. Without this, incrementing would either
/// lose the expiration (defaulting to 0/forever) or reset it,
/// both of which would break time-based rate limiting.
///
fn get_expiration(
  db_pool: DbPool,
  table: String,
  key: String,
) -> Result(Int, CacheError) {
  let sql = "SELECT expiration FROM " <> table <> " WHERE key = $1"

  case
    pool_connection.query(
      db_pool,
      sql,
      [pool_connection.string(key)],
      decode.at([0], decode.int),
    )
  {
    Ok(pool_connection.QueryResult(_, [exp])) -> Ok(exp)
    Ok(_) -> Error(NotFound)
    Error(e) ->
      Error(ConnectionError("Failed to get expiration: " <> string.inspect(e)))
  }
}

/// INSERT ... ON CONFLICT DO UPDATE makes this an upsert — new
/// keys get inserted, existing keys get their value and
/// expiration replaced. This is a single atomic SQL statement,
/// so there's no window where the key exists with stale data
/// between a DELETE and INSERT.
///
fn put_with_expiration(
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
    pool_connection.exec(db_pool, sql, [
      pool_connection.string(key),
      pool_connection.string(value),
      pool_connection.int(expiration),
    ])
  {
    Ok(_) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError("Failed to set cache key: " <> string.inspect(e)))
  }
}

/// Panicking on a non-DatabaseStore catches wiring mistakes
/// immediately. If someone accidentally passes a FileStore
/// config to the database cache, they'll see a clear error at
/// boot instead of a confusing SQL error when the first cache
/// operation tries to query a nonexistent table.
///
fn extract_table(store: CacheStore) -> String {
  case store {
    DatabaseStore(_, _, table) -> table
    _ -> panic as "Cannot create database cache from non-Database store"
  }
}
