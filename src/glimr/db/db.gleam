//// Database Utilities
////
//// High-level database utilities for provider-based configuration
//// and multi-database support.
////
//// NOTE: Pool management and transactions are now in driver packages:
//// - glimr_sqlite.start_pool(), glimr_sqlite.transaction()
//// - glimr_postgres.start_pool(), glimr_postgres.transaction()

import dot_env/env
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glimr/db/driver.{type Connection}
import glimr/db/pool_connection

// ------------------------------------------------------------- Public Functions

/// Builds database configuration from environment variables.
/// Reads DB_DRIVER and DB_POOL_SIZE from the environment.
///
/// For PostgreSQL, uses DB_URL if set, otherwise uses individual
/// parameters: DB_HOST, DB_PORT, DB_DATABASE, DB_USERNAME,
/// DB_PASSWORD.
///
/// For SQLite, DB_DATABASE is preferred but DB_PATH is supported
/// for backward compatibility.
///
pub fn load_config() -> pool_connection.Config {
  let assert Ok(driver) = env.get_string("DB_DRIVER")
  let pool_size = env.get_int("DB_POOL_SIZE") |> result.unwrap(15)

  case driver {
    "postgres" -> load_postgres_config(pool_size)
    "sqlite" -> load_sqlite_config(pool_size)
    _ -> panic as "Please specify a valid DB_DRIVER in your .env file."
  }
}

/// Loads the database provider configuration by calling the
/// `database_provider.register()` function from the user's
/// application. Returns a list of Connection configurations.
///
/// Panics if the database_provider module is not found or
/// doesn't have a register() function.
///
pub fn load_provider() -> List(Connection) {
  case do_load_provider() {
    Ok(connections) -> connections
    Error(reason) -> {
      let msg = case reason {
        "module_not_found" ->
          "database_provider module not found. "
          <> "Please create src/database_provider.gleam with a register() function."
        "no_register_function" ->
          "database_provider module exists but doesn't have a register() function."
        _ -> "Failed to load database provider: " <> reason
      }
      panic as msg
    }
  }
}

/// Finds a connection by name from a list of connections. Panics if
/// the connection is not found.
///
pub fn get_connection(connections: List(Connection), name: String) -> Connection {
  case list.find(connections, fn(c) { driver.connection_name(c) == name }) {
    Ok(c) -> c
    Error(_) ->
      panic as {
        "Connection '"
        <> name
        <> "' not found in database_provider. "
        <> "Available connections: "
        <> string.join(list.map(connections, driver.connection_name), ", ")
      }
  }
}

/// Finds a connection by name from a list of connections. Returns
/// Error(Nil) if the connection is not found instead of panicking.
///
pub fn get_connection_safe(
  connections: List(Connection),
  name: String,
) -> Result(Connection, Nil) {
  list.find(connections, fn(c) { driver.connection_name(c) == name })
}

// ------------------------------------------------------------- Private Functions

/// Loads PostgreSQL configuration from environment variables.
/// Tries DB_URL first, falls back to individual parameters.
///
fn load_postgres_config(pool_size: Int) -> pool_connection.Config {
  case env.get_string("DB_URL") {
    Ok(url) -> pool_connection.postgres_config(url, pool_size: pool_size)
    Error(_) -> {
      // Use individual parameters
      let assert Ok(host) = env.get_string("DB_HOST")
      let port = env.get_int("DB_PORT") |> result.unwrap(5432)
      let assert Ok(database) = env.get_string("DB_DATABASE")
      let assert Ok(username) = env.get_string("DB_USERNAME")
      let password = case env.get_string("DB_PASSWORD") {
        Ok(pw) -> Some(pw)
        Error(_) -> None
      }
      pool_connection.postgres_params_config(
        host: host,
        port: port,
        database: database,
        username: username,
        password: password,
        pool_size: pool_size,
      )
    }
  }
}

/// Load SQLite Config
///
/// Loads SQLite configuration from environment variables.
/// Tries DB_DATABASE first, falls back to DB_PATH.
///
fn load_sqlite_config(pool_size: Int) -> pool_connection.Config {
  let path = case env.get_string("DB_DATABASE") {
    Ok(p) -> p
    Error(_) -> {
      let assert Ok(p) = env.get_string("DB_PATH")
      p
    }
  }
  pool_connection.sqlite_config(path, pool_size: pool_size)
}

// ------------------------------------------------------------- FFI Bindings

/// Calls database_provider:register() via FFI.
///
@external(erlang, "db_provider_ffi", "load_provider")
fn do_load_provider() -> Result(List(Connection), String)
