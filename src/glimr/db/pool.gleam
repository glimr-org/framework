//// ------------------------------------------------------------
//// Connection Pooling
//// ------------------------------------------------------------
////
//// Provides connection pooling for both PostgreSQL and SQLite.
//// PostgreSQL uses pog's built-in pooling. SQLite uses an ETS
//// heir-based pool (ported from pgo) for crash-safe connection
//// management.
////

import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/otp/actor
import gleam/string
import glimr/db/connection.{
  type Config, type Connection, type DbError, ConnectionError, PostgresConfig,
  SqliteConfig,
}
import pog
import sqlight

// ------------------------------------------------------------- Public Types

/// ------------------------------------------------------------
/// Pool Type
/// ------------------------------------------------------------
///
/// A connection pool that manages database connections. Use
/// `start` to create a pool and `with_connection` to borrow
/// connections.
///
pub opaque type Pool {
  PostgresPool(name: Dynamic)
  SqlitePool(name: Dynamic)
}

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Start Pool
/// ------------------------------------------------------------
///
/// Starts a connection pool with the given configuration.
/// For Postgres, this creates a named connection pool.
/// For SQLite, this creates a pool of connections managed
/// by an Erlang process.
///
/// ------------------------------------------------------------
///
/// *Example:*
///
/// ```gleam
/// let config = connection.sqlite_config("data.db", pool_size: 15)
/// let assert Ok(pool) = pool.start(config)
/// ```
///
pub fn start(config: Config) -> Result(Pool, DbError) {
  case config {
    PostgresConfig(url, pool_size) -> start_postgres_pool(url, pool_size)
    SqliteConfig(path, pool_size) -> start_sqlite_pool(path, pool_size)
  }
}

/// ------------------------------------------------------------
/// Stop Pool
/// ------------------------------------------------------------
///
/// Gracefully shuts down a connection pool, closing all
/// connections.
///
pub fn stop(pool: Pool) -> Nil {
  case pool {
    PostgresPool(name) -> {
      let _ = pgo_pool_stop(name)
      Nil
    }
    SqlitePool(name) -> {
      let _ = sqlite_pool_stop(name)
      Nil
    }
  }
}

/// ------------------------------------------------------------
/// Get Connection
/// ------------------------------------------------------------
///
/// Borrows a connection from the pool, executes a function,
/// and returns the connection to the pool. Can be used
/// in controllers, actions, etc.
///
/// ------------------------------------------------------------
///
/// *Example:*
///
/// ```gleam
/// pub fn show(req: Request, ctx: Context) -> Response {
///   use conn <- pool.get_connection(ctx.db.pool)
///   use user <- user_repository.find(conn, 1)
///   wisp.html_response("Hello " <> user.name, 200)
/// }
/// ```
///
pub fn get_connection(pool: Pool, next: fn(Connection) -> next) -> next {
  case checkout(pool) {
    Error(e) -> panic as { "Connection pool failure: " <> string.inspect(e) }
    Ok(conn) -> {
      let next = next(conn)
      checkin(pool, conn)

      next
    }
  }
}

/// ------------------------------------------------------------
/// Get Connection (Result)
/// ------------------------------------------------------------
///
/// Borrows a connection from the pool, executes a function,
/// and returns the connection to the pool. Returns a Result,
/// useful for operations that need error handling like
/// console commands that may need a connection.
///
/// ------------------------------------------------------------
///
/// *Example:*
///
/// ```gleam
/// use conn <- pool.get_connection_or(pool)
/// query.execute(conn, "SELECT 1", [])
/// ```
///
pub fn get_connection_or(
  pool: Pool,
  f: fn(Connection) -> Result(a, DbError),
) -> Result(a, DbError) {
  case checkout(pool) {
    Ok(conn) -> {
      let result = f(conn)
      checkin(pool, conn)
      result
    }
    Error(e) -> Error(e)
  }
}

/// ------------------------------------------------------------
/// Get Connection (Unsafe)
/// ------------------------------------------------------------
///
/// Gets a connection from the pool without automatic return.
/// You MUST call `release` when done. However, if your process
/// crashes without calling release, the connection will be
/// automatically reclaimed by the pool (crash-safe).
///
pub fn checkout(pool: Pool) -> Result(Connection, DbError) {
  case pool {
    PostgresPool(name) -> {
      case pgo_pool_checkout(name) {
        Ok(#(pool_ref, pog_conn)) -> Ok(connection.from_pog(pog_conn, pool_ref))
        Error(_) -> Error(ConnectionError("No connections available in pool"))
      }
    }
    SqlitePool(name) -> {
      case sqlite_pool_checkout(name, []) {
        Ok(#(pool_ref, sqlight_conn)) ->
          Ok(connection.from_sqlight(sqlight_conn, pool_ref))
        Error(_) -> Error(ConnectionError("No connections available in pool"))
      }
    }
  }
}

/// ------------------------------------------------------------
/// Checkin Connection
/// ------------------------------------------------------------
///
/// Returns a connection to the pool. Only needed if you used
/// `checkout` directly instead of `get_connection`.
///
pub fn checkin(pool: Pool, conn: Connection) -> Nil {
  case pool {
    PostgresPool(_) -> {
      let assert Ok(pool_ref) = connection.get_pool_ref(conn)
      let _ = pgo_pool_checkin(pool_ref, connection.to_pog(conn))
      Nil
    }
    SqlitePool(_) -> {
      let assert Ok(pool_ref) = connection.get_pool_ref(conn)
      let _ = sqlite_pool_checkin(pool_ref, connection.to_sqlight(conn))
      Nil
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// ------------------------------------------------------------
/// Start PostgreSQL Pool
/// ------------------------------------------------------------
///
/// Creates a PostgreSQL connection pool using pog's built-in
/// pooling. Generates a unique pool name and configures the
/// pool size.
///
fn start_postgres_pool(url: String, pool_size: Int) -> Result(Pool, DbError) {
  // Create a unique name for this pool
  let pool_name = process.new_name(prefix: "glimr_db_pool")

  case pog.url_config(pool_name, url) {
    Ok(config) -> {
      let config = pog.pool_size(config, pool_size)
      case pog.start(config) {
        Ok(actor.Started(_, _conn)) ->
          Ok(PostgresPool(name_to_dynamic(pool_name)))
        Error(actor.InitFailed(_)) ->
          Error(ConnectionError("Failed to start Postgres pool"))
        Error(actor.InitTimeout) ->
          Error(ConnectionError("Postgres pool initialization timed out"))
        Error(actor.InitExited(_)) ->
          Error(ConnectionError("Postgres pool exited during initialization"))
      }
    }
    Error(Nil) -> Error(ConnectionError("Invalid Postgres connection URL"))
  }
}

/// ------------------------------------------------------------
/// Start SQLite Pool
/// ------------------------------------------------------------
///
/// Creates a SQLite connection pool under the SQLite pool
/// supervisor. Uses an ETS heir-based implementation (ported
/// from pgo). Connections are automatically reclaimed if the
/// borrowing process crashes, and the pool is automatically
/// restarted if it crashes.
///
fn start_sqlite_pool(path: String, pool_size: Int) -> Result(Pool, DbError) {
  let pool_name = process.new_name(prefix: "glimr_sqlite_pool")
  let config = sqlite_pool_config(pool_size)
  case sqlite_pool_sup_start(name_to_dynamic(pool_name), path, config) {
    Ok(name) -> Ok(SqlitePool(name))
    Error(_) -> Error(ConnectionError("Failed to start SQLite pool"))
  }
}

/// ------------------------------------------------------------
/// SQLite Pool Config
/// ------------------------------------------------------------
///
/// Creates the configuration map for the SQLite pool.
///
fn sqlite_pool_config(pool_size: Int) -> Dynamic {
  make_pool_config(pool_size)
}

// ------------------------------------------------------------- FFI Bindings

/// ------------------------------------------------------------
/// Make Pool Config
/// ------------------------------------------------------------
///
/// Creates the configuration map for the SQLite pool in Erlang.
///
@external(erlang, "sqlite_pool_ffi", "make_config")
fn make_pool_config(pool_size: Int) -> Dynamic

/// ------------------------------------------------------------
/// SQLite Pool Supervisor Start
/// ------------------------------------------------------------
///
/// Starts a SQLite connection pool under the supervisor.
///
@external(erlang, "sqlite_pool_sup", "start_pool")
fn sqlite_pool_sup_start(
  name: Dynamic,
  path: String,
  config: Dynamic,
) -> Result(Dynamic, Dynamic)

/// ------------------------------------------------------------
/// SQLite Pool Stop
/// ------------------------------------------------------------
///
/// Stops a SQLite pool managed by the supervisor.
///
@external(erlang, "sqlite_pool_sup", "stop_pool")
fn sqlite_pool_stop(name: Dynamic) -> Dynamic

/// ------------------------------------------------------------
/// SQLite Pool Checkout
/// ------------------------------------------------------------
///
/// Checks out a connection from the SQLite pool. Returns the
/// pool reference and the sqlight connection.
///
@external(erlang, "sqlite_pool", "checkout")
fn sqlite_pool_checkout(
  pool_name: Dynamic,
  opts: List(#(String, Dynamic)),
) -> Result(#(Dynamic, sqlight.Connection), Dynamic)

/// ------------------------------------------------------------
/// SQLite Pool Checkin
/// ------------------------------------------------------------
///
/// Returns a connection to the SQLite pool.
///
@external(erlang, "sqlite_pool", "checkin")
fn sqlite_pool_checkin(pool_ref: Dynamic, conn: sqlight.Connection) -> Dynamic

/// ------------------------------------------------------------
/// Postgres Pool Checkout
/// ------------------------------------------------------------
///
/// Checks out a connection from the Postgres pool. Returns the
/// pool reference and the pog connection.
///
@external(erlang, "pgo_pool_ffi", "checkout")
fn pgo_pool_checkout(
  pool_name: Dynamic,
) -> Result(#(Dynamic, pog.Connection), Dynamic)

/// ------------------------------------------------------------
/// Postgres Pool Checkin
/// ------------------------------------------------------------
///
/// Returns a connection to the Postgres pool.
///
@external(erlang, "pgo_pool_ffi", "checkin")
fn pgo_pool_checkin(pool_ref: Dynamic, conn: pog.Connection) -> Dynamic

/// ------------------------------------------------------------
/// Postgres Pool Stop
/// ------------------------------------------------------------
///
/// Stops a Postgres pool by terminating it via the supervisor.
///
@external(erlang, "pgo_pool_ffi", "stop_pool")
fn pgo_pool_stop(pool_name: Dynamic) -> Dynamic

/// ------------------------------------------------------------
/// Name To Dynamic
/// ------------------------------------------------------------
///
/// Converts a process Name to Dynamic for FFI calls.
///
@external(erlang, "pgo_pool_ffi", "identity")
fn name_to_dynamic(name: process.Name(a)) -> Dynamic
