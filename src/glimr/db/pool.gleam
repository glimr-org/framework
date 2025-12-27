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
import gleam/erlang/process.{type Pid}
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
  PostgresPool(connection: pog.Connection)
  SqlitePool(pid: Pid, path: String)
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
    PostgresPool(_) -> {
      // pog pools are managed by OTP supervisor,
      // no manual shutdown needed
      Nil
    }
    SqlitePool(pid, _) -> {
      let _ = sqlite_pool_stop(pid)
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
      release(pool, conn)

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
      release(pool, conn)
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
    PostgresPool(conn) -> Ok(connection.from_pog(conn))
    SqlitePool(pid, _path) -> {
      case sqlite_pool_checkout(pid, []) {
        Ok(#(pool_ref, sqlight_conn)) ->
          Ok(connection.from_sqlight(sqlight_conn, pool_ref))
        Error(_) -> Error(ConnectionError("No connections available in pool"))
      }
    }
  }
}

/// ------------------------------------------------------------
/// Release Connection
/// ------------------------------------------------------------
///
/// Returns a connection to the pool. Only needed if you used
/// `checkout` directly instead of `with_connection`.
///
pub fn release(pool: Pool, conn: Connection) -> Nil {
  case pool {
    PostgresPool(_) -> {
      // pog manages its own connections
      Nil
    }
    SqlitePool(_, _) -> {
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
        Ok(actor.Started(_, conn)) -> Ok(PostgresPool(conn))
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
/// Creates a SQLite connection pool using an ETS heir-based
/// implementation (ported from pgo). Connections are
/// automatically reclaimed if the borrowing process crashes.
///
fn start_sqlite_pool(path: String, pool_size: Int) -> Result(Pool, DbError) {
  let config = sqlite_pool_config(pool_size)
  case sqlite_pool_start_link(path, config) {
    Ok(pid) -> Ok(SqlitePool(pid, path))
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
/// SQLite Pool Start Link
/// ------------------------------------------------------------
///
/// Starts a SQLite connection pool as a linked process.
///
@external(erlang, "sqlite_pool", "start_link")
fn sqlite_pool_start_link(path: String, config: Dynamic) -> Result(Pid, Dynamic)

/// ------------------------------------------------------------
/// SQLite Pool Checkout
/// ------------------------------------------------------------
///
/// Checks out a connection from the SQLite pool. Returns the
/// pool reference and the sqlight connection.
///
@external(erlang, "sqlite_pool", "checkout")
fn sqlite_pool_checkout(
  pool: Pid,
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
/// SQLite Pool Stop
/// ------------------------------------------------------------
///
/// Stops the SQLite pool and closes all connections.
///
@external(erlang, "sqlite_pool", "stop")
fn sqlite_pool_stop(pool: Pid) -> Dynamic
