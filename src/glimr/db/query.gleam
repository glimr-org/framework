//// ------------------------------------------------------------
//// Query Execution
//// ------------------------------------------------------------
////
//// Provides a unified interface for executing database queries
//// on both PostgreSQL and SQLite connections.
////

import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/list
import gleam/result
import glimr/db/connection.{
  type Connection, type DbError, type Value, ConnectionError, DecodeError,
  Postgres, QueryError, Sqlite,
}
import pog
import sqlight

// ------------------------------------------------------------- Public Types

/// ------------------------------------------------------------
/// QueryResult Type
/// ------------------------------------------------------------
///
/// The result of a database query, containing the number of
/// affected/returned rows and the data.
///
pub type QueryResult(t) {
  QueryResult(count: Int, rows: List(t))
}

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Execute Query
/// ------------------------------------------------------------
///
/// Executes a SQL query that returns rows. Use this for SELECT
/// statements.
///
/// SQL should use PostgreSQL-style placeholders ($1, $2, etc.)
/// which are automatically converted for SQLite.
///
/// ------------------------------------------------------------
///
/// *Example:*
///
/// ```gleam
/// let decoder = {
///   use id <- decode.field(0, decode.int)
///   use name <- decode.field(1, decode.string)
///   decode.success(#(id, name))
/// }
///
/// query.select(
///   conn,
///   "SELECT id, name FROM users WHERE id = $1",
///   [connection.int(1)],
///   decoder,
/// )
/// ```
///
pub fn select(
  conn: Connection,
  sql: String,
  params: List(Value),
  decoder: Decoder(t),
) -> Result(QueryResult(t), DbError) {
  case connection.driver(conn) {
    Postgres -> {
      let pg_conn = connection.to_pog(conn)
      let pg_params = list.map(params, connection.to_pog_value)
      execute_postgres_query(pg_conn, sql, pg_params, decoder)
    }
    Sqlite -> {
      let sqlite_conn = connection.to_sqlight(conn)
      let converted_sql = connection.convert_placeholders(sql, Sqlite)
      let sqlite_params = list.map(params, connection.to_sqlight_value)
      execute_sqlite_query(sqlite_conn, converted_sql, sqlite_params, decoder)
    }
  }
}

/// ------------------------------------------------------------
/// Execute Statement
/// ------------------------------------------------------------
///
/// Executes a SQL statement that doesn't return rows (INSERT,
/// UPDATE, DELETE). Returns the number of affected rows.
///
/// ------------------------------------------------------------
///
/// *Example:*
///
/// ```gleam
/// query.execute(
///   conn,
///   "INSERT INTO users (name, email) VALUES ($1, $2)",
///   [connection.string("Alice"), connection.string("alice@example.com")],
/// )
/// ```
///
pub fn execute(
  conn: Connection,
  sql: String,
  params: List(Value),
) -> Result(Int, DbError) {
  case connection.driver(conn) {
    Postgres -> {
      let pg_conn = connection.to_pog(conn)
      let pg_params = list.map(params, connection.to_pog_value)
      execute_postgres_statement(pg_conn, sql, pg_params)
    }
    Sqlite -> {
      let sqlite_conn = connection.to_sqlight(conn)
      let converted_sql = connection.convert_placeholders(sql, Sqlite)
      let sqlite_params = list.map(params, connection.to_sqlight_value)
      execute_sqlite_statement(sqlite_conn, converted_sql, sqlite_params)
    }
  }
}

/// ------------------------------------------------------------
/// Select One
/// ------------------------------------------------------------
///
/// Executes a query expecting exactly zero or one row. Returns 
/// Error if more than one row is returned.
///
pub fn select_one(
  conn: Connection,
  sql: String,
  params: List(Value),
  decoder: Decoder(t),
) -> Result(Result(t, Nil), DbError) {
  use result <- result.try(select(conn, sql, params, decoder))
  case result.rows {
    [] -> Ok(Error(Nil))
    [row] -> Ok(Ok(row))
    _ -> Error(QueryError("Expected 0 or 1 rows, got multiple"))
  }
}

/// ------------------------------------------------------------
/// Select All
/// ------------------------------------------------------------
///
/// Alias for `select` that emphasizes returning multiple rows.
///
pub fn select_all(
  conn: Connection,
  sql: String,
  params: List(Value),
  decoder: Decoder(t),
) -> Result(List(t), DbError) {
  use result <- result.try(select(conn, sql, params, decoder))
  Ok(result.rows)
}

// ------------------------------------------------------------- Private Functions

/// ------------------------------------------------------------
/// Execute PostgreSQL Query
/// ------------------------------------------------------------
///
/// Executes a SQL query on a PostgreSQL connection and decodes
/// the results. Maps pog-specific errors to generic DbError 
/// types.
///
fn execute_postgres_query(
  conn: pog.Connection,
  sql: String,
  params: List(pog.Value),
  decoder: Decoder(t),
) -> Result(QueryResult(t), DbError) {
  let query =
    pog.query(sql)
    |> add_pog_params(params)
    |> pog.returning(decoder)

  case pog.execute(query, conn) {
    Ok(pog.Returned(count, rows)) -> Ok(QueryResult(count, rows))
    Error(pog.ConstraintViolated(msg, constraint, _)) ->
      Error(connection.ConstraintError(msg, constraint))
    Error(pog.PostgresqlError(_, _, msg)) -> Error(QueryError(msg))
    Error(pog.UnexpectedArgumentCount(expected, got)) ->
      Error(QueryError(
        "Expected "
        <> int.to_string(expected)
        <> " params, got "
        <> int.to_string(got),
      ))
    Error(pog.UnexpectedArgumentType(expected, got)) ->
      Error(QueryError("Expected " <> expected <> ", got " <> got))
    Error(pog.UnexpectedResultType(errors)) ->
      Error(DecodeError(format_decode_errors(errors)))
    Error(pog.QueryTimeout) -> Error(connection.TimeoutError)
    Error(pog.ConnectionUnavailable) ->
      Error(ConnectionError("Connection unavailable"))
  }
}

/// ------------------------------------------------------------
/// Execute PostgreSQL Statement
/// ------------------------------------------------------------
///
/// Executes a SQL statement on a PostgreSQL connection that 
/// does not return rows. Returns the count of affected rows.
///
fn execute_postgres_statement(
  conn: pog.Connection,
  sql: String,
  params: List(pog.Value),
) -> Result(Int, DbError) {
  let query =
    pog.query(sql)
    |> add_pog_params(params)

  case pog.execute(query, conn) {
    Ok(pog.Returned(count, _)) -> Ok(count)
    Error(pog.ConstraintViolated(msg, constraint, _)) ->
      Error(connection.ConstraintError(msg, constraint))
    Error(pog.PostgresqlError(_, _, msg)) -> Error(QueryError(msg))
    Error(pog.UnexpectedArgumentCount(expected, got)) ->
      Error(QueryError(
        "Expected "
        <> int.to_string(expected)
        <> " params, got "
        <> int.to_string(got),
      ))
    Error(pog.UnexpectedArgumentType(expected, got)) ->
      Error(QueryError("Expected " <> expected <> ", got " <> got))
    Error(pog.UnexpectedResultType(errors)) ->
      Error(DecodeError(format_decode_errors(errors)))
    Error(pog.QueryTimeout) -> Error(connection.TimeoutError)
    Error(pog.ConnectionUnavailable) ->
      Error(ConnectionError("Connection unavailable"))
  }
}

/// ------------------------------------------------------------
/// Execute SQLite Query
/// ------------------------------------------------------------
///
/// Executes a SQL query on a SQLite connection and decodes
/// the results. Maps sqlight-specific errors to generic DbError 
/// types.
///
fn execute_sqlite_query(
  conn: sqlight.Connection,
  sql: String,
  params: List(sqlight.Value),
  decoder: Decoder(t),
) -> Result(QueryResult(t), DbError) {
  case sqlight.query(sql, conn, params, decoder) {
    Ok(rows) -> Ok(QueryResult(list.length(rows), rows))
    Error(sqlight.SqlightError(code, msg, _)) ->
      Error(QueryError(sqlight_error_message(code, msg)))
  }
}

/// ------------------------------------------------------------
/// Execute SQLite Statement
/// ------------------------------------------------------------
///
/// Executes a SQL statement on a SQLite connection that does
/// not return rows. Returns the count of affected rows.
///
fn execute_sqlite_statement(
  conn: sqlight.Connection,
  sql: String,
  params: List(sqlight.Value),
) -> Result(Int, DbError) {
  // For statements, we use query with a simple decoder that ignores results
  let decoder = decode.success(Nil)
  case sqlight.query(sql, conn, params, decoder) {
    Ok(rows) -> Ok(list.length(rows))
    Error(sqlight.SqlightError(code, msg, _)) ->
      Error(QueryError(sqlight_error_message(code, msg)))
  }
}

/// ------------------------------------------------------------
/// Add Pog Parameters
/// ------------------------------------------------------------
///
/// Adds a list of parameters to a pog query using fold.
///
fn add_pog_params(query: pog.Query(t), params: List(pog.Value)) -> pog.Query(t) {
  list.fold(params, query, fn(q, param) { pog.parameter(q, param) })
}

/// ------------------------------------------------------------
/// Format Decode Errors
/// ------------------------------------------------------------
///
/// Formats a list of decode errors into a human-readable string.
/// Returns the first error's details or a generic message if 
/// empty.
///
fn format_decode_errors(errors: List(decode.DecodeError)) -> String {
  case errors {
    [] -> "Unknown decode error"
    [first, ..] -> "Expected " <> first.expected <> ", got " <> first.found
  }
}

/// ------------------------------------------------------------
/// SQLite Error Message
/// ------------------------------------------------------------
///
/// Formats a SQLite error code and message into a readable 
/// string. Maps common constraint error codes to descriptive 
/// prefixes.
///
fn sqlight_error_message(code: sqlight.ErrorCode, msg: String) -> String {
  let code_str = case code {
    sqlight.Constraint -> "CONSTRAINT"
    sqlight.ConstraintUnique -> "UNIQUE_CONSTRAINT"
    sqlight.ConstraintForeignkey -> "FOREIGN_KEY"
    sqlight.ConstraintPrimarykey -> "PRIMARY_KEY"
    sqlight.ConstraintNotnull -> "NOT_NULL"
    _ -> "ERROR"
  }
  code_str <> ": " <> msg
}
