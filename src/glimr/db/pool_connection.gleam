//// Pool Connection Abstraction
////
//// Provides types and utilities for database connections.
//// The actual connection handling is delegated to driver
//// packages (glimr_sqlite, glimr_postgres) which register
//// with the driver registry.

import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/option.{type Option}
import gleam/result
import gleam/string

// ------------------------------------------------------------- Public Types

/// Identifies which database driver is being used. This allows
/// you to seamlessly use multiple connections of different
/// database drivers throughout your app.
///
pub type Driver {
  Postgres
  Sqlite
}

/// Configuration for establishing a database connection. Use
/// `postgres_config`, `postgres_params_config`, or
/// `sqlite_config` to create instances.
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

/// A pooled database connection that wraps the underlying
/// driver connection. The actual connection is stored as
/// Dynamic and unwrapped by driver packages.
///
pub opaque type PoolConnection {
  PoolConnection(driver: Driver, handle: Dynamic, pool_ref: Dynamic)
}

/// Unified error type for database operations. Provides
/// specific error variants for common failure cases, allowing
/// precise error handling in application code.
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

/// The result of a database query, containing the number of
/// affected rows and the list of decoded row data. The count
/// is useful for INSERT/UPDATE/DELETE operations.
///
pub type QueryResult(t) {
  QueryResult(count: Int, rows: List(t))
}

/// A parameter value that can be passed to a database query.
/// Use the constructor functions (int, string, bool, etc.) to
/// create type-safe parameter values.
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

/// Creates a PostgreSQL configuration from a connection URL.
/// This configuration will have its own pool of connections
/// specific to this database.
///
/// URL format: `postgresql://user:password@host:port/database`
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

/// Creates a PostgreSQL configuration from individual
/// parameters. This is an alternative to `postgres_config` when
/// you have separate host, port, database, username, and
/// password values.
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

/// Creates a SQLite configuration from a file path. This
/// configuration will have its own pool of connections
/// specific to this database.
///
/// Use `:memory:` for an in-memory database.
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

// ------------------------------------------------------------- Connection Functions

/// Creates a pool connection wrapper from driver-specific
/// handles. Called by driver packages when checking out a
/// connection from the pool.
///
pub fn wrap_connection(
  driver: Driver,
  handle: Dynamic,
  pool_ref: Dynamic,
) -> PoolConnection {
  PoolConnection(driver, handle, pool_ref)
}

/// Returns the driver type for the provided pool connection.
/// Useful for conditionally handling driver-specific behavior
/// in application code.
///
pub fn driver(connection: PoolConnection) -> Driver {
  connection.driver
}

/// Gets the raw connection handle as Dynamic. Used by driver
/// packages to unwrap and cast to the native connection type
/// for executing queries.
///
pub fn unwrap_handle(connection: PoolConnection) -> Dynamic {
  connection.handle
}

/// Extracts the pool reference from a pool connection. This
/// reference must be passed to checkin when returning the
/// connection to the pool.
///
pub fn get_pool_ref(connection: PoolConnection) -> Dynamic {
  connection.pool_ref
}

// ------------------------------------------------------------- Value Functions

/// Creates an integer parameter value for use in database
/// queries. Maps to INTEGER type in both PostgreSQL and
/// SQLite databases.
///
pub fn int(value: Int) -> Value {
  IntValue(value)
}

/// Creates a floating-point parameter value for use in
/// database queries. Maps to REAL/DOUBLE PRECISION depending
/// on the database driver.
///
pub fn float(value: Float) -> Value {
  FloatValue(value)
}

/// Creates a string/text parameter value for use in database
/// queries. Maps to TEXT/VARCHAR depending on the database
/// driver and column type.
///
pub fn string(value: String) -> Value {
  StringValue(value)
}

/// Creates a boolean parameter value for use in database
/// queries. PostgreSQL uses native BOOLEAN, while SQLite
/// stores booleans as integers (0/1).
///
pub fn bool(value: Bool) -> Value {
  BoolValue(value)
}

/// Creates a NULL parameter value for use in database queries.
/// Use this for optional fields where the value should be
/// stored as database NULL.
///
pub fn null() -> Value {
  NullValue
}

/// Creates a binary/blob parameter value for storing raw
/// bytes in the database. Maps to BYTEA in PostgreSQL and
/// BLOB in SQLite.
///
pub fn blob(value: BitArray) -> Value {
  BlobValue(value)
}

/// Creates a parameter value from an Option, converting None
/// to NULL. Pass the appropriate constructor function for the
/// inner type (e.g., nullable(int, some_option)).
///
pub fn nullable(inner: fn(a) -> Value, value: Option(a)) -> Value {
  case value {
    option.Some(v) -> inner(v)
    option.None -> null()
  }
}

// ------------------------------------------------------------- SQL Utilities

/// Converts $1, $2, etc. placeholders to ? for SQLite.
/// This allows using consistent Postgres-style placeholders
/// in SQL files.
///
pub fn convert_placeholders(sql: String, driver: Driver) -> String {
  case driver {
    Postgres -> sql
    Sqlite -> convert_pg_to_sqlite_placeholders(sql)
  }
}

// ------------------------------------------------------------- Private Functions

/// Entry point for placeholder conversion. Converts the SQL
/// string to graphemes and delegates to the recursive helper
/// that processes each character.
///
fn convert_pg_to_sqlite_placeholders(sql: String) -> String {
  do_convert_placeholders(string.to_graphemes(sql), "", False)
}

/// Recursively processes characters, replacing $N placeholders
/// with ? for SQLite. Tracks whether we're inside a placeholder
/// to skip the numeric portion after the $.
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
