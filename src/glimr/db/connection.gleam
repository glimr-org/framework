//// ------------------------------------------------------------
//// Database Connection Abstraction
//// ------------------------------------------------------------
////
//// Provides a unified interface over PostgreSQL (pog) and 
//// SQLite (sqlight) database connections. This allows 
//// application code to work with either database without 
//// changes.
////

import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/option.{type Option}
import gleam/result
import gleam/string
import pog
import sqlight

// ------------------------------------------------------------- Public Types

/// ------------------------------------------------------------
/// Driver Type
/// ------------------------------------------------------------
///
/// Identifies which database driver is being used.
///
pub type Driver {
  Postgres
  Sqlite
}

/// ------------------------------------------------------------
/// Config Type
/// ------------------------------------------------------------
///
/// Configuration for establishing a database connection.
/// Use `postgres_config` or `sqlite_config` to create instances.
///
pub type Config {
  PostgresConfig(url: String, pool_size: Int)
  SqliteConfig(path: String, pool_size: Int)
}

/// ------------------------------------------------------------
/// Connection Type
/// ------------------------------------------------------------
///
/// A database connection that abstracts over the underlying
/// driver. Use with the `query` module functions.
///
pub opaque type Connection {
  PostgresConnection(inner: pog.Connection)
  SqliteConnection(inner: sqlight.Connection, pool_ref: Dynamic)
}

/// ------------------------------------------------------------
/// DbError Type
/// ------------------------------------------------------------
///
/// Unified error type for database operations.
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
}

/// ------------------------------------------------------------
/// QueryResult Type
/// ------------------------------------------------------------
///
/// The result of a database query, containing the number of
/// affected rows and the returned data.
///
pub type QueryResult(t) {
  QueryResult(count: Int, rows: List(t))
}

/// ------------------------------------------------------------
/// Value Type
/// ------------------------------------------------------------
///
/// A parameter value that can be passed to a database query.
/// Use the constructor functions to create values.
///
pub opaque type Value {
  PgValue(pog.Value)
  SqliteValue(sqlight.Value)
  /// Placeholder for deferred conversion based on driver
  IntValue(Int)
  FloatValue(Float)
  StringValue(String)
  BoolValue(Bool)
  NullValue
  BlobValue(BitArray)
}

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Create Postgres Config
/// ------------------------------------------------------------
///
/// Creates a PostgreSQL configuration from a connection URL.
///
/// URL format: `postgresql://user:password@host:port/database`
///
/// ------------------------------------------------------------
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

/// ------------------------------------------------------------
/// Create SQLite Config
/// ------------------------------------------------------------
///
/// Creates a SQLite configuration from a file path.
///
/// Use `:memory:` for an in-memory database.
///
/// ------------------------------------------------------------
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

/// ------------------------------------------------------------
/// Get Driver
/// ------------------------------------------------------------
///
/// Returns the driver type for a connection.
///
pub fn driver(conn: Connection) -> Driver {
  case conn {
    PostgresConnection(_) -> Postgres
    SqliteConnection(_, _) -> Sqlite
  }
}

/// ------------------------------------------------------------
/// Wrap Postgres Connection
/// ------------------------------------------------------------
///
/// Wraps a raw pog connection for use with the glimr db module.
///
pub fn from_pog(conn: pog.Connection) -> Connection {
  PostgresConnection(conn)
}

/// ------------------------------------------------------------
/// Wrap SQLite Connection
/// ------------------------------------------------------------
///
/// Wraps a sqlight connection with its pool reference for use
/// with the glimr db module. The pool_ref is needed for proper
/// checkin when releasing the connection.
///
pub fn from_sqlight(conn: sqlight.Connection, pool_ref: Dynamic) -> Connection {
  SqliteConnection(conn, pool_ref)
}

/// ------------------------------------------------------------
/// Get Raw Postgres Connection
/// ------------------------------------------------------------
///
/// Extracts the underlying pog connection. Panics if not
/// Postgres.
///
pub fn to_pog(conn: Connection) -> pog.Connection {
  case conn {
    PostgresConnection(inner) -> inner
    SqliteConnection(_, _) -> panic as "Cannot convert SQLite connection to pog"
  }
}

/// ------------------------------------------------------------
/// Get Raw SQLite Connection
/// ------------------------------------------------------------
///
/// Extracts the underlying sqlight connection. Panics if not
/// SQLite.
///
pub fn to_sqlight(conn: Connection) -> sqlight.Connection {
  case conn {
    SqliteConnection(inner, _) -> inner
    PostgresConnection(_) ->
      panic as "Cannot convert Postgres connection to sqlight"
  }
}

/// ------------------------------------------------------------
/// Get Pool Reference
/// ------------------------------------------------------------
///
/// Extracts the pool reference from a SQLite connection.
/// Returns Error for Postgres connections.
///
pub fn get_pool_ref(conn: Connection) -> Result(Dynamic, Nil) {
  case conn {
    SqliteConnection(_, pool_ref) -> Ok(pool_ref)
    PostgresConnection(_) -> Error(Nil)
  }
}

/// ------------------------------------------------------------
/// Integer Value
/// ------------------------------------------------------------
///
/// Creates an integer parameter value.
///
pub fn int(value: Int) -> Value {
  IntValue(value)
}

/// ------------------------------------------------------------
/// Float Value
/// ------------------------------------------------------------
///
/// Creates a float parameter value.
///
pub fn float(value: Float) -> Value {
  FloatValue(value)
}

/// ------------------------------------------------------------
/// String Value
/// ------------------------------------------------------------
///
/// Creates a string/text parameter value.
///
pub fn string(value: String) -> Value {
  StringValue(value)
}

/// ------------------------------------------------------------
/// Boolean Value
/// ------------------------------------------------------------
///
/// Creates a boolean parameter value.
/// Note: SQLite stores booleans as integers (0/1).
///
pub fn bool(value: Bool) -> Value {
  BoolValue(value)
}

/// ------------------------------------------------------------
/// Null Value
/// ------------------------------------------------------------
///
/// Creates a NULL parameter value.
///
pub fn null() -> Value {
  NullValue
}

/// ------------------------------------------------------------
/// Blob Value
/// ------------------------------------------------------------
///
/// Creates a binary/blob parameter value.
///
pub fn blob(value: BitArray) -> Value {
  BlobValue(value)
}

/// ------------------------------------------------------------
/// Nullable Value
/// ------------------------------------------------------------
///
/// Creates a parameter value from an Option, converting None to NULL.
///
pub fn nullable(inner: fn(a) -> Value, value: Option(a)) -> Value {
  case value {
    option.Some(v) -> inner(v)
    option.None -> null()
  }
}

/// ------------------------------------------------------------
/// Convert to Pog Value
/// ------------------------------------------------------------
///
/// Converts a generic Value to a pog-specific Value for use
/// with PostgreSQL queries. Panics if given a SQLite-specific
/// value.
///
pub fn to_pog_value(value: Value) -> pog.Value {
  case value {
    PgValue(v) -> v
    IntValue(v) -> pog.int(v)
    FloatValue(v) -> pog.float(v)
    StringValue(v) -> pog.text(v)
    BoolValue(v) -> pog.bool(v)
    NullValue -> pog.null()
    BlobValue(v) -> pog.bytea(v)
    SqliteValue(_) -> panic as "Cannot convert SQLite value to pog"
  }
}

/// ------------------------------------------------------------
/// Convert to SQLite Value
/// ------------------------------------------------------------
///
/// Converts a generic Value to a sqlight-specific Value for use
/// with SQLite queries. Panics if given a PostgreSQL-specific
/// value.
///
pub fn to_sqlight_value(value: Value) -> sqlight.Value {
  case value {
    SqliteValue(v) -> v
    IntValue(v) -> sqlight.int(v)
    FloatValue(v) -> sqlight.float(v)
    StringValue(v) -> sqlight.text(v)
    BoolValue(v) -> sqlight.bool(v)
    NullValue -> sqlight.null()
    BlobValue(v) -> sqlight.blob(v)
    PgValue(_) -> panic as "Cannot convert pog value to sqlight"
  }
}

/// ------------------------------------------------------------
/// Convert Postgres Placeholder to SQLite
/// ------------------------------------------------------------
///
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

/// ------------------------------------------------------------
/// Convert Postgres to SQLite Placeholders
/// ------------------------------------------------------------
///
/// Entry point for placeholder conversion. Converts the SQL
/// string to graphemes and delegates to the recursive converter.
///
fn convert_pg_to_sqlite_placeholders(sql: String) -> String {
  do_convert_placeholders(string.to_graphemes(sql), "", False)
}

/// ------------------------------------------------------------
/// Do Convert Placeholders
/// ------------------------------------------------------------
///
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
