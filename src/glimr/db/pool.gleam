//// ------------------------------------------------------------
//// Connection Pooling
//// ------------------------------------------------------------
////
//// Provides connection pooling for both PostgreSQL and SQLite.
//// PostgreSQL uses pog's built-in pooling. SQLite uses a simple
//// pool implementation based on Erlang processes.
////

import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/otp/actor
import glimr/db/connection.{
  type Config, type Connection, type DbError, ConnectionError, PostgresConfig,
  SqliteConfig,
}
import pog
import sqlight
import wisp.{type Response}

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
  SqlitePool(subject: Subject(PoolMessage), path: String)
}

// ------------------------------------------------------------- Private Types

/// ------------------------------------------------------------
/// Pool Message
/// ------------------------------------------------------------
///
/// Messages sent to the SQLite pool actor for managing
/// connection checkout, checkin, and shutdown operations.
///
type PoolMessage {
  Checkout(reply_to: Subject(Result(sqlight.Connection, Nil)))
  Checkin(conn: sqlight.Connection)
  Shutdown
}

/// ------------------------------------------------------------
/// Pool State
/// ------------------------------------------------------------
///
/// Internal state of the SQLite connection pool actor.
/// Tracks available connections, in-use count, and pool limits.
///
type PoolState {
  PoolState(
    available: List(sqlight.Connection),
    in_use: Int,
    max_size: Int,
    path: String,
  )
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
/// let config = connection.sqlite_config("data.db", pool_size: 5)
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
    SqlitePool(subject, _) -> {
      process.send(subject, Shutdown)
    }
  }
}

/// ------------------------------------------------------------
/// Get Connection
/// ------------------------------------------------------------
///
/// Borrows a connection from the pool, executes a function,
/// and returns the connection to the pool. Returns a Response,
/// making it ideal for use in controller functions.
///
/// On pool error, returns `wisp.internal_server_error()`.
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
pub fn get_connection(
  pool: Pool,
  handler: fn(Connection) -> Response,
) -> Response {
  case checkout(pool) {
    Error(_) -> wisp.internal_server_error()
    Ok(conn) -> {
      let response = handler(conn)
      release(pool, conn)
      response
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
/// transactions.
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
  case pool {
    PostgresPool(conn) -> {
      // pog handles connection management internally
      f(connection.from_pog(conn))
    }
    SqlitePool(subject, _path) -> {
      // Checkout a connection
      let reply_subject = process.new_subject()
      process.send(subject, Checkout(reply_subject))

      case process.receive(reply_subject, 5000) {
        Ok(Ok(conn)) -> {
          let result = f(connection.from_sqlight(conn))
          // Always return the connection
          process.send(subject, Checkin(conn))
          result
        }
        Ok(Error(Nil)) ->
          Error(ConnectionError("No connections available in pool"))
        Error(Nil) -> Error(ConnectionError("Pool checkout timed out"))
      }
    }
  }
}

/// ------------------------------------------------------------
/// Get Connection (Unsafe)
/// ------------------------------------------------------------
///
/// Gets a connection from the pool without automatic return.
/// You MUST call `release` when done, or connections will leak.
/// Prefer `with_connection` for safety.
///
pub fn checkout(pool: Pool) -> Result(Connection, DbError) {
  case pool {
    PostgresPool(conn) -> Ok(connection.from_pog(conn))
    SqlitePool(subject, _path) -> {
      let reply_subject = process.new_subject()
      process.send(subject, Checkout(reply_subject))

      case process.receive(reply_subject, 5000) {
        Ok(Ok(conn)) -> Ok(connection.from_sqlight(conn))
        Ok(Error(Nil)) ->
          Error(ConnectionError("No connections available in pool"))
        Error(Nil) -> Error(ConnectionError("Pool checkout timed out"))
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
    SqlitePool(subject, _) -> {
      process.send(subject, Checkin(connection.to_sqlight(conn)))
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
/// Creates a SQLite connection pool managed by an OTP actor.
/// Pre-creates connections and starts the pool actor to handle
/// checkout/checkin operations.
///
fn start_sqlite_pool(path: String, pool_size: Int) -> Result(Pool, DbError) {
  // Pre-create connections
  case create_sqlite_connections(path, pool_size, []) {
    Ok(connections) -> {
      let initial_state =
        PoolState(
          available: connections,
          in_use: 0,
          max_size: pool_size,
          path: path,
        )

      let start_result =
        actor.new(initial_state)
        |> actor.on_message(handle_pool_message)
        |> actor.start

      case start_result {
        Ok(actor.Started(_, subject)) -> Ok(SqlitePool(subject, path))
        Error(_) -> Error(ConnectionError("Failed to start SQLite pool actor"))
      }
    }
    Error(err) -> Error(err)
  }
}

/// ------------------------------------------------------------
/// Create SQLite Connections
/// ------------------------------------------------------------
///
/// Recursively creates the specified number of SQLite 
/// connections. Returns an error if any connection fails to 
/// open.
///
fn create_sqlite_connections(
  path: String,
  count: Int,
  acc: List(sqlight.Connection),
) -> Result(List(sqlight.Connection), DbError) {
  case count <= 0 {
    True -> Ok(acc)
    False -> {
      case sqlight.open(path) {
        Ok(conn) -> create_sqlite_connections(path, count - 1, [conn, ..acc])
        Error(err) ->
          Error(ConnectionError(
            "Failed to open SQLite connection: " <> err.message,
          ))
      }
    }
  }
}

/// ------------------------------------------------------------
/// Handle Pool Message
/// ------------------------------------------------------------
///
/// Actor message handler for the SQLite pool. Processes 
/// checkout requests by returning available connections or 
/// creating new ones, handles checkin by returning connections 
/// to the pool, and closes all connections on shutdown.
///
fn handle_pool_message(
  state: PoolState,
  msg: PoolMessage,
) -> actor.Next(PoolState, PoolMessage) {
  case msg {
    Checkout(reply_to) -> {
      case state.available {
        [conn, ..rest] -> {
          process.send(reply_to, Ok(conn))
          actor.continue(
            PoolState(..state, available: rest, in_use: state.in_use + 1),
          )
        }
        [] -> {
          // No connections available - could create more or return error
          case state.in_use < state.max_size {
            True -> {
              // Try to create a new connection
              case sqlight.open(state.path) {
                Ok(conn) -> {
                  process.send(reply_to, Ok(conn))
                  actor.continue(PoolState(..state, in_use: state.in_use + 1))
                }
                Error(_) -> {
                  process.send(reply_to, Error(Nil))
                  actor.continue(state)
                }
              }
            }
            False -> {
              process.send(reply_to, Error(Nil))
              actor.continue(state)
            }
          }
        }
      }
    }

    Checkin(conn) -> {
      actor.continue(
        PoolState(
          ..state,
          available: [conn, ..state.available],
          in_use: state.in_use - 1,
        ),
      )
    }

    Shutdown -> {
      // Close all available connections
      list.each(state.available, fn(conn) {
        let _ = sqlight.close(conn)
        Nil
      })
      actor.stop()
    }
  }
}
