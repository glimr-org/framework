//// Pool Connection Abstraction
////
//// The framework needs to run the same query logic against
//// PostgreSQL and SQLite without importing either driver
//// directly. This module defines Pool and Connection as opaque
//// types with Dynamic-typed vtable callbacks — each adapter
//// (glimr_postgres, glimr_sqlite) registers its own query/exec
//// implementations at pool creation time, and the framework
//// calls them through this unified interface. Everything from
//// migrations to generated model code goes through here.
////

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/process
import gleam/int
import gleam/option.{type Option}
import gleam/result
import gleam/string
import glimr/fail

// ------------------------------------------------------------- Public Types

/// Some SQL differs between drivers — `BEGIN` vs `BEGIN
/// TRANSACTION`, `$1` vs `?` placeholders, `BIGINT` vs
/// `INTEGER` for timestamps. Tagging each pool with its driver
/// lets the framework branch on syntax without inspecting the
/// connection handle or importing driver packages.
///
pub type Driver {
  Postgres
  Sqlite
}

/// A shared config union so console commands and startup code
/// can pass database configuration around without caring which
/// driver it targets. PostgresConfig takes a URL because that's
/// what cloud platforms give you (DATABASE_URL).
/// PostgresParamsConfig is for environments like Kubernetes
/// where credentials arrive as separate fields. SQLite just
/// needs a file path.
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

/// Opaque so the framework controls connection lifecycle and
/// callers can't accidentally use a driver-specific handle
/// directly, bypassing placeholder conversion or connection
/// pooling. The query_fn and exec_fn are stored as Dynamic
/// because Gleam can't express "a function whose first argument
/// is some driver-specific type" without generics on the record
/// — coercing through Dynamic is safe on the BEAM where types
/// are erased at runtime.
///
pub opaque type DbPool {
  DbPool(
    driver: Driver,
    query_fn: Dynamic,
    exec_fn: Dynamic,
    checkout: fn() -> Result(#(Dynamic, fn() -> Nil), String),
    stop: fn() -> Nil,
  )
}

/// Bundling the connection handle with its query/exec closures
/// ensures every query goes through the correct driver
/// implementation — you can't accidentally run a PostgreSQL
/// query function with a SQLite handle. Opaque so callers can't
/// extract the handle and use it after the connection has been
/// returned to the pool.
///
pub opaque type Connection {
  Connection(
    driver: Driver,
    handle: Dynamic,
    query_fn: Dynamic,
    exec_fn: Dynamic,
  )
}

/// A unified error type so application code can handle database
/// failures without knowing which driver produced them. The
/// specific variants matter — ConstraintError carries the
/// constraint name so you can show "email already taken" on a
/// unique violation instead of a generic "something went
/// wrong". NotFound makes single-row lookups return a clear
/// error instead of forcing callers to check empty lists.
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
  /// Connection configuration is invalid or missing required
  /// parameters
  ConfigError(message: String)
}

/// Returning both the row count and decoded rows in one type
/// avoids needing separate queries after writes. SELECT queries
/// use the rows field, while INSERT/UPDATE/ DELETE primarily
/// care about the count — "0 rows updated" tells you the WHERE
/// clause matched nothing without a separate existence check.
///
pub type QueryResult(t) {
  QueryResult(count: Int, rows: List(t))
}

/// Query parameters need to pass through the vtable without the
/// framework knowing which driver-specific types they'll
/// become. Each adapter converts these tagged values to its
/// native parameter type at execution time — pgo atoms for
/// Postgres, sqlight values for SQLite.
///
pub type Value {
  IntValue(Int)
  FloatValue(Float)
  StringValue(String)
  BoolValue(Bool)
  NullValue
  BlobValue(BitArray)
}

// ------------------------------------------------------------- Public Functions

/// Connection URLs are the standard way to configure PostgreSQL
/// in cloud environments — platforms like Heroku, Railway, and
/// Fly.io all give you a DATABASE_URL. Accepting a URL means
/// developers can just pass the env var through without parsing
/// it themselves.
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

/// Some deployments provide credentials as separate fields —
/// Kubernetes secrets per field, AWS RDS IAM tokens, or just a
/// company that prefers explicit config over URL parsing. This
/// accepts discrete parameters so developers don't have to
/// assemble a URL string from parts.
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
/// database that disappears when the pool stops — perfect for
/// tests that need isolation without cleanup.
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

/// Each adapter (glimr_postgres, glimr_sqlite) calls this at
/// startup to register its driver-specific query and exec
/// implementations. The labeled arguments are essential — five
/// Dynamic-typed fields in a row would be impossible to get
/// right without them.
///
pub fn new_pool(
  driver driver: Driver,
  query_fn query_fn: Dynamic,
  exec_fn exec_fn: Dynamic,
  checkout checkout: fn() -> Result(#(Dynamic, fn() -> Nil), String),
  stop stop: fn() -> Nil,
) -> DbPool {
  DbPool(
    driver: driver,
    query_fn: query_fn,
    exec_fn: exec_fn,
    checkout: checkout,
    stop: stop,
  )
}

/// The callback-based API guarantees the connection is returned
/// to the pool even if the function panics. Without this, a
/// crash in a handler would leak a connection — and under
/// sustained traffic, leaked connections would exhaust the pool
/// within minutes, causing every subsequent request to hang
/// waiting for a checkout that never comes.
///
pub fn get_connection(pool: DbPool, f: fn(Connection) -> a) -> a {
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

/// For one-off queries where you don't need to hold a
/// connection across multiple statements. Checks out a
/// connection, runs the query, and returns it to the pool
/// automatically. Placeholder conversion ($1 → ?) happens
/// transparently so all SQL can use Postgres-style placeholders
/// regardless of the actual driver.
///
pub fn query(
  pool: DbPool,
  sql: String,
  params: List(Value),
  decoder: Decoder(a),
) -> Result(QueryResult(a), DbError) {
  use conn <- get_connection(pool)
  query_with(conn, sql, params, decoder)
}

/// Transactions need all queries to run on the same connection
/// — that's what makes them atomic. This variant accepts a
/// pre-checked-out Connection so multiple queries share the
/// same transaction context. Also used by generated model code
/// inside transaction blocks.
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

/// For one-off write operations (INSERT, UPDATE, DELETE) where
/// you don't need to hold the connection. Checks out a
/// connection, executes, returns the affected row count, and
/// releases the connection automatically.
///
pub fn exec(
  pool: DbPool,
  sql: String,
  params: List(Value),
) -> Result(Int, DbError) {
  use conn <- get_connection(pool)
  exec_with(conn, sql, params)
}

/// The write-operation counterpart to query_with — accepts a
/// pre-checked-out Connection for use inside transaction
/// blocks. BEGIN, COMMIT, and ROLLBACK all need to run on the
/// same connection, so this is what the transaction function
/// uses internally.
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
/// callers from accidentally leaving transactions open — if you
/// forget COMMIT in manual SQL, every subsequent query on that
/// connection runs inside your abandoned transaction. Automatic
/// retry on deadlock errors handles the common case where
/// concurrent writes conflict, using proportional backoff to
/// give other transactions time to release their locks.
///
pub fn transaction(
  pool: DbPool,
  retries: Int,
  callback: fn(Connection) -> Result(a, DbError),
) -> Result(a, DbError) {
  case retries < 0 {
    True -> Error(ConnectionError("Transaction retries cannot be negative"))
    False -> do_transaction(pool, retries, callback)
  }
}

/// Database connections hold server-side resources (open file
/// handles for SQLite, TCP connections for Postgres) that
/// aren't released until explicitly closed. Console commands
/// call this after finishing their work so the process can exit
/// cleanly.
///
pub fn stop_pool(pool: DbPool) -> Nil {
  pool.stop()
}

/// Code that needs driver-specific SQL (like transaction syntax
/// or column type names) can branch on the driver without
/// accessing the opaque Pool internals. The cache module uses
/// this to pick BIGINT vs INTEGER for the expiration column.
///
pub fn pool_driver(pool: DbPool) -> Driver {
  pool.driver
}

/// Same purpose as pool_driver but for an already checked-out
/// connection. Useful inside transaction callbacks where only
/// the Connection is available, not the Pool — like when you
/// need driver-specific SQL inside a transaction block.
///
pub fn connection_driver(conn: Connection) -> Driver {
  conn.driver
}

/// Unwraps a database Result or halts the request with the
/// right HTTP status. NotFound becomes 404, connection and
/// timeout errors become 503 (service unavailable), everything
/// else becomes 500. The generated `_or_fail` query functions
/// use this so controllers can write
/// `users.find_by_id_or_fail(pool, id: id)` and get the user
/// directly — no case expression needed for the happy path.
///
pub fn expect(result: Result(a, DbError)) -> a {
  case result {
    Ok(value) -> value
    Error(NotFound) -> fail.with(404)
    Error(ConnectionError(_)) -> fail.with(503)
    Error(TimeoutError) -> fail.with(503)
    Error(_) -> fail.with(500)
  }
}

/// Wrapping raw Gleam values in a tagged union lets the driver
/// adapter convert them to the correct native parameter type at
/// execution time. You call `db.int(42)` and the Postgres
/// adapter turns it into a pgo value, the SQLite adapter turns
/// it into a sqlight value.
///
pub fn int(value: Int) -> Value {
  IntValue(value)
}

/// Float values map to REAL in SQLite and DOUBLE PRECISION in
/// PostgreSQL. The adapter handles the conversion so callers
/// don't need to know the target column type — just wrap the
/// value and the right thing happens.
///
pub fn float(value: Float) -> Value {
  FloatValue(value)
}

/// The most common parameter type by far — used for VARCHAR,
/// TEXT, timestamps, UUIDs, and JSON columns. The database
/// handles any necessary casting from the text representation,
/// so you don't need different wrappers for different
/// string-like column types.
///
pub fn string(value: String) -> Value {
  StringValue(value)
}

/// PostgreSQL has native BOOLEAN but SQLite stores booleans as
/// 0/1 integers. Tagging the value here lets each adapter do
/// the right conversion — the Postgres adapter passes
/// true/false directly while the SQLite adapter converts to
/// 1/0.
///
pub fn bool(value: Bool) -> Value {
  BoolValue(value)
}

/// Needed for INSERT/UPDATE of optional fields — when a user
/// leaves their bio empty, the query needs an explicit NULL,
/// not an empty string. A dedicated variant keeps the Value
/// type flat instead of nesting Option(Value).
///
pub fn null() -> Value {
  NullValue
}

/// Binary data like file uploads or encrypted tokens can't be
/// stored as text without base64 encoding overhead. A dedicated
/// blob variant passes raw bytes through to the driver without
/// any encoding/decoding round-trip.
///
pub fn blob(value: BitArray) -> Value {
  BlobValue(value)
}

/// Optional fields in Gleam are Option types, but the query
/// layer needs Value. This bridges the two — Some(v) becomes
/// the wrapped value, None becomes NullValue. Saves a case
/// expression at every call site where you're passing an
/// optional field to a query.
///
pub fn nullable(inner: fn(a) -> Value, value: Option(a)) -> Value {
  case value {
    option.Some(v) -> inner(v)
    option.None -> null()
  }
}

/// SQL files use PostgreSQL-style `$1` placeholders everywhere
/// for consistency — developers shouldn't maintain duplicate
/// SQL files just because SQLite uses `?` instead. This
/// conversion runs transparently at execution time so the same
/// SQL works on both drivers.
///
pub fn convert_placeholders(sql: String, driver: Driver) -> String {
  case driver {
    Postgres -> sql
    Sqlite -> convert_pg_to_sqlite_placeholders(sql)
  }
}

// ------------------------------------------------------------- Private Functions

/// Separated from transaction() to hide the retry counter from
/// the public API. Each attempt checks out a fresh connection
/// because the previous one may be in a broken state after a
/// rollback — reusing it could cause subsequent queries to fail
/// with confusing errors about aborted transactions.
///
fn do_transaction(
  pool: DbPool,
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

/// Only deadlock and lock errors are worth retrying —
/// constraint violations and syntax errors will fail again
/// immediately no matter how long you wait. The sleep time
/// scales with remaining retries (50ms * retries_remaining)
/// creating a simple backoff that gives concurrent transactions
/// time to release their locks.
///
fn maybe_retry(
  pool: DbPool,
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

/// PostgreSQL says "deadlock detected", SQLite says "database
/// is locked" — same problem, different words. Checking
/// multiple keywords in one place handles both drivers without
/// needing driver-specific error inspection throughout the
/// transaction code.
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

/// A simple string.replace won't work because `$1` and `$10`
/// would conflict — replacing `$1` first would turn `$10` into
/// `?0`. Character-by-character processing correctly replaces
/// each `$N` as a complete unit regardless of how many digits
/// it has.
///
fn convert_pg_to_sqlite_placeholders(sql: String) -> String {
  do_convert_placeholders(string.to_graphemes(sql), "", False)
}

/// The in_placeholder flag skips digits after `$` so `$12`
/// emits a single `?` rather than `?2`. Once a non-digit
/// character is hit, the flag resets and normal character
/// copying resumes. This handles any number of placeholder
/// digits correctly.
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

// ------------------------------------------------------------- FFI Bindings

/// The vtable stores typed functions as Dynamic. Coercing back
/// is safe because Gleam types are erased at runtime on the
/// BEAM — the Dynamic value is always the exact function type
/// we stored during pool construction. This is the inverse of
/// to_dynamic.
///
@external(erlang, "glimr_pool_ffi", "identity")
fn coerce(value: Dynamic) -> a

/// Adapters need to store their driver-specific functions and
/// connection handles in the Dynamic-typed vtable fields. This
/// is just the identity function with a type signature that
/// convinces the compiler to accept the conversion — safe on
/// the BEAM where all values are untyped at runtime anyway.
///
@external(erlang, "glimr_pool_ffi", "identity")
pub fn to_dynamic(value: a) -> Dynamic
