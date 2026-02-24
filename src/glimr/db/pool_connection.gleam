//// Pool Connection Abstraction
////
//// Supporting multiple database drivers (PostgreSQL, SQLite)
//// without duplicating query logic across the framework
//// requires a driver-agnostic interface. This module defines
//// the core Pool and Connection types using Dynamic-typed
//// vtables so the framework can execute queries without
//// importing any driver-specific library.

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/process
import gleam/int
import gleam/option.{type Option}
import gleam/result
import gleam/string

// ------------------------------------------------------------- Public Types

/// Some operations need driver-specific SQL (e.g., BEGIN vs
/// BEGIN TRANSACTION, placeholder styles). Tagging each pool
/// with its driver lets the framework branch on syntax without
/// inspecting the connection handle.
///
pub type Driver {
  Postgres
  Sqlite
}

/// A shared union lets console commands and startup code pass
/// config around without knowing which driver it targets.
/// The driver adapter pattern-matches at pool creation to
/// extract only the fields it understands.
///
pub type Config {
  PostgresConfig(url: String, pool_size: Int)
  PostgresParamsConfig(
    host: String,
    port: Int,
    database: String,
    username: String,
    password: Option(String),
    pool_size: Int,
  )
  SqliteConfig(path: String, pool_size: Int)
}

/// Opaque so the framework controls checkout/checkin and callers
/// can't call driver functions with the wrong handle type.
/// Storing query_fn and exec_fn as Dynamic lets each adapter
/// register its own typed implementation without a shared trait.
///
pub opaque type Pool {
  Pool(
    driver: Driver,
    query_fn: Dynamic,
    exec_fn: Dynamic,
    checkout: fn() -> Result(#(Dynamic, fn() -> Nil), String),
    stop: fn() -> Nil,
  )
}

/// Bundling the handle with its query/exec closures ensures
/// every query goes through the correct driver implementation.
/// Opaque so callers can't extract the handle and use it after
/// the connection is returned to the pool.
///
pub opaque type Connection {
  Connection(
    driver: Driver,
    handle: Dynamic,
    query_fn: Dynamic,
    exec_fn: Dynamic,
  )
}

/// A unified error type lets application code handle database
/// failures without knowing which driver produced them.
/// Specific variants like ConstraintError enable precise
/// handling (e.g., showing "email taken" on unique violations)
/// without string-matching on error messages.
///
pub type DbError {
  /// The requested row was not found (for single-row queries)
  NotFound
  /// A constraint was violated (unique, foreign key, etc.)
  ConstraintError(message: String, constraint: String)
  /// A query syntax or execution error
  QueryError(message: String)
  /// Connection to database failed or unavailable
  ConnectionError(message: String)
  /// Query timed out
  TimeoutError
  /// Result decoding failed
  DecodeError(message: String)
  /// Connection configuration is invalid or missing required parameters
  ConfigError(message: String)
}

/// Returning both the row count and decoded rows in one type
/// avoids separate queries to check affected rows after writes.
/// SELECT queries use the rows field while INSERT/UPDATE/DELETE
/// primarily use the count.
///
pub type QueryResult(t) {
  QueryResult(count: Int, rows: List(t))
}

/// A driver-agnostic value union so query parameters can be
/// passed through the vtable without the framework knowing
/// which driver-specific types they'll become. Each adapter
/// converts these to its native parameter type at execution.
///
pub type Value {
  IntValue(Int)
  FloatValue(Float)
  StringValue(String)
  BoolValue(Bool)
  NullValue
  BlobValue(BitArray)
}

// ------------------------------------------------------------- Config Functions

/// Connection URLs are the standard way to configure PostgreSQL
/// in cloud environments (DATABASE_URL). Accepting a URL avoids
/// forcing callers to parse it into individual fields.
///
/// *Example:*
///
/// ```gleam
/// let config = postgres_config(
///   "postgresql://postgres:secret@localhost:5432/myapp",
///   pool_size: 10,
/// )
/// ```
///
pub fn postgres_config(url: String, pool_size pool_size: Int) -> Config {
  PostgresConfig(url: url, pool_size: pool_size)
}

/// Some deployments provide credentials as separate fields
/// (e.g., Kubernetes secrets per field) rather than a URL.
/// Accepting discrete parameters supports that pattern without
/// forcing callers to assemble a URL string.
///
/// *Example:*
///
/// ```gleam
/// let config = postgres_params_config(
///   host: "localhost",
///   port: 5432,
///   database: "myapp",
///   username: "postgres",
///   password: Some("secret"),
///   pool_size: 10,
/// )
/// ```
///
pub fn postgres_params_config(
  host host: String,
  port port: Int,
  database database: String,
  username username: String,
  password password: Option(String),
  pool_size pool_size: Int,
) -> Config {
  PostgresParamsConfig(
    host: host,
    port: port,
    database: database,
    username: username,
    password: password,
    pool_size: pool_size,
  )
}

/// SQLite databases are just files, so the path is all that's
/// needed. The `:memory:` sentinel creates an ephemeral
/// database useful for tests.
///
/// *Example:*
///
/// ```gleam
/// let config = sqlite_config("data.db", pool_size: 5)
/// let memory_config = sqlite_config(":memory:", pool_size: 1)
/// ```
///
pub fn sqlite_config(path: String, pool_size pool_size: Int) -> Config {
  SqliteConfig(path: path, pool_size: pool_size)
}

// ------------------------------------------------------------- Pool Functions

/// Adapters call this at startup to register their driver-
/// specific implementations. Using labeled arguments makes
/// construction self-documenting and prevents argument-order
/// mistakes across the five required fields.
///
pub fn new_pool(
  driver driver: Driver,
  query_fn query_fn: Dynamic,
  exec_fn exec_fn: Dynamic,
  checkout checkout: fn() -> Result(#(Dynamic, fn() -> Nil), String),
  stop stop: fn() -> Nil,
) -> Pool {
  Pool(
    driver: driver,
    query_fn: query_fn,
    exec_fn: exec_fn,
    checkout: checkout,
    stop: stop,
  )
}

/// A callback-based API guarantees the connection is returned
/// to the pool even if the function crashes, preventing
/// connection leaks that would eventually exhaust the pool
/// under sustained traffic.
///
pub fn get_connection(pool: Pool, f: fn(Connection) -> a) -> a {
  case pool.checkout() {
    Ok(#(handle, release)) -> {
      let conn =
        Connection(
          driver: pool.driver,
          handle: handle,
          query_fn: pool.query_fn,
          exec_fn: pool.exec_fn,
        )
      let result = f(conn)
      release()
      result
    }
    Error(msg) -> panic as { "Failed to checkout connection: " <> msg }
  }
}

/// Convenience wrapper so callers that only need a single query
/// don't have to manually check out and return a connection.
/// Placeholder conversion is automatic so SQL can use $1 style
/// everywhere regardless of driver.
///
pub fn query(
  pool: Pool,
  sql: String,
  params: List(Value),
  decoder: Decoder(a),
) -> Result(QueryResult(a), DbError) {
  use conn <- get_connection(pool)
  query_with(conn, sql, params, decoder)
}

/// Transactions require all queries to run on the same
/// connection. This variant accepts a pre-checked-out
/// Connection so multiple queries share the same transaction
/// context without redundant checkout/checkin cycles.
///
pub fn query_with(
  conn: Connection,
  sql: String,
  params: List(Value),
  decoder: Decoder(a),
) -> Result(QueryResult(a), DbError) {
  let converted_sql = convert_placeholders(sql, conn.driver)
  let typed_query_fn: fn(Dynamic, String, List(Value), Decoder(a)) ->
    Result(QueryResult(a), DbError) = coerce(conn.query_fn)
  typed_query_fn(conn.handle, converted_sql, params, decoder)
}

/// Convenience wrapper for write operations that only need a
/// single statement. Checks out a connection, executes, and
/// returns the affected row count.
///
pub fn exec(
  pool: Pool,
  sql: String,
  params: List(Value),
) -> Result(Int, DbError) {
  use conn <- get_connection(pool)
  exec_with(conn, sql, params)
}

/// Transaction blocks need writes on the same connection as
/// the BEGIN statement. This variant accepts a Connection so
/// INSERT/UPDATE/DELETE execute within the active transaction.
///
pub fn exec_with(
  conn: Connection,
  sql: String,
  params: List(Value),
) -> Result(Int, DbError) {
  let converted_sql = convert_placeholders(sql, conn.driver)
  let typed_exec_fn: fn(Dynamic, String, List(Value)) -> Result(Int, DbError) =
    coerce(conn.exec_fn)
  typed_exec_fn(conn.handle, converted_sql, params)
}

/// Wrapping BEGIN/COMMIT/ROLLBACK in a callback prevents
/// callers from accidentally leaving transactions open.
/// Automatic retry on deadlock errors handles the common case
/// where concurrent writes conflict — backing off and retrying
/// resolves most transient lock contention.
///
pub fn transaction(
  pool: Pool,
  retries: Int,
  callback: fn(Connection) -> Result(a, DbError),
) -> Result(a, DbError) {
  case retries < 0 {
    True -> Error(ConnectionError("Transaction retries cannot be negative"))
    False -> do_transaction(pool, retries, callback)
  }
}

/// Database connections hold server-side resources that aren't
/// released until explicitly closed. Stopping the pool frees
/// those resources instead of waiting for process exit.
///
pub fn stop_pool(pool: Pool) -> Nil {
  pool.stop()
}

/// Code that needs driver-specific SQL (like transaction
/// syntax) can branch on the driver without accessing the
/// opaque Pool internals.
///
pub fn pool_driver(pool: Pool) -> Driver {
  pool.driver
}

/// Same as pool_driver but for an already checked-out
/// connection. Useful inside transaction callbacks where
/// only the Connection is available, not the Pool.
///
pub fn connection_driver(conn: Connection) -> Driver {
  conn.driver
}

// ------------------------------------------------------------- Value Functions

/// Wrapping raw Gleam values in a tagged union lets the
/// driver adapter convert them to the correct native type
/// at execution time without the caller knowing the driver.
///
pub fn int(value: Int) -> Value {
  IntValue(value)
}

/// Float values map to REAL in SQLite and DOUBLE PRECISION in
/// PostgreSQL. The adapter handles the conversion so callers
/// don't need to know the target column type.
///
pub fn float(value: Float) -> Value {
  FloatValue(value)
}

/// Text is the most common parameter type — used for VARCHAR,
/// TEXT, timestamps, UUIDs, and JSON columns. The database
/// handles any necessary casting from the text representation.
///
pub fn string(value: String) -> Value {
  StringValue(value)
}

/// PostgreSQL has native BOOLEAN but SQLite stores booleans
/// as 0/1 integers. Tagging the value here lets each adapter
/// do the right conversion at execution time.
///
pub fn bool(value: Bool) -> Value {
  BoolValue(value)
}

/// Explicit NULL values are needed for INSERT/UPDATE of
/// optional fields. Using a dedicated variant instead of
/// Option(Value) keeps the Value type flat and avoids
/// nested wrapping.
///
pub fn null() -> Value {
  NullValue
}

/// Binary data like file uploads or encrypted tokens can't
/// be stored as text without encoding. A dedicated blob
/// variant passes raw bytes through to the driver without
/// any encoding/decoding overhead.
///
pub fn blob(value: BitArray) -> Value {
  BlobValue(value)
}

/// Optional fields in Gleam are Option types, but the query
/// layer needs Value. This bridges the two by converting None
/// to NullValue and Some to the inner Value type, avoiding
/// a case expression at every call site.
///
pub fn nullable(inner: fn(a) -> Value, value: Option(a)) -> Value {
  case value {
    option.Some(v) -> inner(v)
    option.None -> null()
  }
}

// ------------------------------------------------------------- SQL Utilities

/// SQL files use PostgreSQL-style $1 placeholders everywhere
/// for consistency. SQLite requires ? instead, so this
/// conversion runs transparently at execution time so
/// developers don't maintain duplicate SQL files.
///
pub fn convert_placeholders(sql: String, driver: Driver) -> String {
  case driver {
    Postgres -> sql
    Sqlite -> convert_pg_to_sqlite_placeholders(sql)
  }
}

// ------------------------------------------------------------- Private Functions

/// Separated from transaction to hide the retry counter from
/// the public API. Each attempt checks out a fresh connection
/// because the previous one may be in a broken state after a
/// rollback.
///
fn do_transaction(
  pool: Pool,
  retries_remaining: Int,
  callback: fn(Connection) -> Result(a, DbError),
) -> Result(a, DbError) {
  use conn <- get_connection(pool)

  let begin_sql = case pool.driver {
    Postgres -> "BEGIN"
    Sqlite -> "BEGIN TRANSACTION"
  }

  case exec_with(conn, begin_sql, []) {
    Error(e) -> Error(e)
    Ok(_) -> {
      case callback(conn) {
        Ok(value) -> {
          case exec_with(conn, "COMMIT", []) {
            Ok(_) -> Ok(value)
            Error(e) -> {
              let _ = exec_with(conn, "ROLLBACK", [])
              maybe_retry(pool, retries_remaining, callback, e)
            }
          }
        }
        Error(e) -> {
          let _ = exec_with(conn, "ROLLBACK", [])
          maybe_retry(pool, retries_remaining, callback, e)
        }
      }
    }
  }
}

/// Only deadlock/lock errors are worth retrying — other errors
/// like constraint violations will fail again immediately.
/// Sleeping proportionally to remaining retries creates a
/// simple backoff that gives concurrent transactions time to
/// release their locks.
///
fn maybe_retry(
  pool: Pool,
  retries_remaining: Int,
  callback: fn(Connection) -> Result(a, DbError),
  error: DbError,
) -> Result(a, DbError) {
  case is_deadlock_error(error) && retries_remaining > 0 {
    True -> {
      process.sleep(50 * retries_remaining)
      do_transaction(pool, retries_remaining - 1, callback)
    }
    False -> Error(error)
  }
}

/// PostgreSQL and SQLite report lock errors with different
/// messages ("deadlock detected" vs "database is locked").
/// Checking multiple keywords in a single function handles
/// both drivers without driver-specific error inspection.
///
fn is_deadlock_error(error: DbError) -> Bool {
  case error {
    QueryError(msg) -> {
      let lower = string.lowercase(msg)
      string.contains(lower, "deadlock")
      || string.contains(lower, "lock")
      || string.contains(lower, "serialization")
      || string.contains(lower, "database is locked")
      || string.contains(lower, "busy")
    }
    _ -> False
  }
}

/// String-level replace can't be used because $1 and $10 would
/// conflict. Character-by-character processing correctly
/// replaces each $N as a unit regardless of digit count.
///
fn convert_pg_to_sqlite_placeholders(sql: String) -> String {
  do_convert_placeholders(string.to_graphemes(sql), "", False)
}

/// The in_placeholder flag skips digits after $ so "$12" emits
/// a single "?" rather than "?2". Once a non-digit is hit the
/// flag resets and normal character copying resumes.
///
fn do_convert_placeholders(
  chars: List(String),
  acc: String,
  in_placeholder: Bool,
) -> String {
  case chars {
    [] -> acc
    ["$", ..rest] -> do_convert_placeholders(rest, acc <> "?", True)
    [c, ..rest] if in_placeholder -> {
      case result.is_ok(int.parse(c)) {
        True -> do_convert_placeholders(rest, acc, True)
        False -> do_convert_placeholders(rest, acc <> c, False)
      }
    }
    [c, ..rest] -> do_convert_placeholders(rest, acc <> c, False)
  }
}

// ------------------------------------------------------------- FFI

/// The vtable stores typed functions as Dynamic. Coercing back
/// is safe because Gleam types are erased at runtime on the
/// BEAM — the Dynamic is always the exact function type we
/// stored during pool construction.
///
@external(erlang, "glimr_pool_ffi", "identity")
fn coerce(value: Dynamic) -> a

/// Adapters need to store driver-specific functions and
/// connection handles in the Dynamic-typed vtable fields.
/// Identity coercion to Dynamic is safe on the BEAM where
/// all values are already untyped at runtime.
///
@external(erlang, "glimr_pool_ffi", "identity")
pub fn to_dynamic(value: a) -> Dynamic
