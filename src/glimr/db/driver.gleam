//// Database Connection Configuration
////
//// Provides connection types for configuring database connections
//// in a type-safe way. Users define connections in their
//// database_provider.gleam file which is loaded at runtime.

import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glimr/db/pool_connection.{type Config}

// ------------------------------------------------------------- Public Types

/// Represents a named database connection configuration.
/// Each connection has a name that identifies it and connection
/// parameters specific to the database type.
///
/// Use `PostgresUriConnection` for PostgreSQL with a connection URL.
/// Use `PostgresConnection` for PostgreSQL with individual parameters.
/// Use `SqliteConnection` for SQLite databases.
///
pub type Connection {
  PostgresUriConnection(
    name: String,
    url: Result(String, String),
    pool_size: Result(Int, String),
  )
  PostgresConnection(
    name: String,
    host: Result(String, String),
    port: Result(Int, String),
    database: Result(String, String),
    username: Result(String, String),
    password: Result(String, String),
    pool_size: Result(Int, String),
  )
  SqliteConnection(
    name: String,
    database: Result(String, String),
    pool_size: Result(Int, String),
  )
}

/// Identifies the underlying database type for a connection.
/// Currently supports Postgres and SQLite as the two available
/// database driver implementations.
///
pub type DriverType {
  Postgres
  Sqlite
}

// ------------------------------------------------------------- Public Functions

/// Returns whether the connection is for Postgres or SQLite.
/// Inspects the connection variant to determine the database
/// type without needing to access individual fields.
///
pub fn connection_type(connection: Connection) -> DriverType {
  case connection {
    PostgresUriConnection(_, _, _) -> Postgres
    PostgresConnection(_, _, _, _, _, _, _) -> Postgres
    SqliteConnection(_, _, _) -> Sqlite
  }
}

/// Returns the name identifying this connection configuration.
/// The name is used to look up specific connections when
/// multiple database connections are configured in the app.
///
pub fn connection_name(connection: Connection) -> String {
  case connection {
    PostgresUriConnection(name, _, _) -> name
    PostgresConnection(name, _, _, _, _, _, _) -> name
    SqliteConnection(name, _, _) -> name
  }
}

/// Returns a new connection with the pool size overridden to 
/// the specified value. Useful for console commands that only 
/// need a single connection.
///
pub fn with_pool_size(connection: Connection, size: Int) -> Connection {
  case connection {
    PostgresUriConnection(name, url, _) ->
      PostgresUriConnection(name, url, Ok(size))
    PostgresConnection(name, host, port, database, username, password, _) ->
      PostgresConnection(
        name,
        host,
        port,
        database,
        username,
        password,
        Ok(size),
      )
    SqliteConnection(name, database, _) ->
      SqliteConnection(name, database, Ok(size))
  }
}

/// Converts a Connection to a pool_connection.Config. Panics 
/// with a helpful message if any required environment variables 
/// are missing.
///
pub fn to_config(connection: Connection) -> Config {
  case connection {
    PostgresUriConnection(name, url_result, pool_size_r) -> {
      let url = unwrap_or_panic(url_result, name, "url")
      let pool_size = unwrap_or_panic(pool_size_r, name, "pool_size")
      pool_connection.postgres_config(url, pool_size: pool_size)
    }

    PostgresConnection(
      name,
      host_r,
      port_r,
      database_r,
      username_r,
      password_r,
      pool_size_r,
    ) -> {
      let host = unwrap_or_panic(host_r, name, "host")
      let port = result.unwrap(port_r, 5432)
      let database = unwrap_or_panic(database_r, name, "database")
      let username = unwrap_or_panic(username_r, name, "username")
      let password = case password_r {
        Ok(pw) -> Some(pw)
        Error(_) -> None
      }
      let pool_size = unwrap_or_panic(pool_size_r, name, "pool_size")
      pool_connection.postgres_params_config(
        host: host,
        port: port,
        database: database,
        username: username,
        password: password,
        pool_size: pool_size,
      )
    }

    SqliteConnection(name, database_r, pool_size_r) -> {
      let database = unwrap_or_panic(database_r, name, "database")
      let pool_size = unwrap_or_panic(pool_size_r, name, "pool_size")
      pool_connection.sqlite_config(database, pool_size: pool_size)
    }
  }
}

/// Validates that all required parameters for a connection are
/// present. Returns a list of missing parameter names, which
/// will be empty if the connection is fully configured.
///
pub fn validate(connection: Connection) -> List(String) {
  case connection {
    PostgresUriConnection(_, url_r, pool_size_r) -> {
      let missing = []
      let missing = case url_r {
        Ok(_) -> missing
        Error(_) -> ["url", ..missing]
      }
      let missing = case pool_size_r {
        Ok(_) -> missing
        Error(_) -> ["pool_size", ..missing]
      }
      missing
    }

    PostgresConnection(
      _,
      host_r,
      _port_r,
      database_r,
      username_r,
      _password_r,
      pool_size_r,
    ) -> {
      let missing = []
      let missing = case host_r {
        Ok(_) -> missing
        Error(_) -> ["host", ..missing]
      }
      let missing = case database_r {
        Ok(_) -> missing
        Error(_) -> ["database", ..missing]
      }
      let missing = case username_r {
        Ok(_) -> missing
        Error(_) -> ["username", ..missing]
      }
      let missing = case pool_size_r {
        Ok(_) -> missing
        Error(_) -> ["pool_size", ..missing]
      }
      missing
    }

    SqliteConnection(_, database_r, pool_size_r) -> {
      let missing = []
      let missing = case database_r {
        Ok(_) -> missing
        Error(_) -> ["database", ..missing]
      }
      let missing = case pool_size_r {
        Ok(_) -> missing
        Error(_) -> ["pool_size", ..missing]
      }
      missing
    }
  }
}

/// Searches through a list of connections to find one with
/// the specified name. Returns Ok with the connection if found,
/// or Error(Nil) if no connection matches the given name.
///
pub fn find_by_name(name: String, connections: List(Connection)) -> Connection {
  let conn =
    list.find(connections, fn(connection: Connection) {
      connection.name == name
    })

  case conn {
    Ok(conn) -> conn
    _ ->
      panic as {
        "The connection '" <> name <> "' does not exist in config/database.toml"
      }
  }
}

// ------------------------------------------------------------- Private Functions

/// Unwraps a Result or panics with a helpful error message
/// indicating which connection and parameter is missing. Used 
/// internally to provide clear configuration error messages.
///
fn unwrap_or_panic(
  result: Result(a, String),
  conn_name: String,
  param: String,
) -> a {
  case result {
    Ok(value) -> value
    Error(_) ->
      panic as {
        "Connection '"
        <> conn_name
        <> "' is missing required parameter: "
        <> param
        <> ". Check your database_provider.gleam configuration."
      }
  }
}

/// Converts a snake_case or lowercase name to PascalCase.
/// Used for generating type names from connection names.
/// Splits on underscores and capitalizes each segment.
///
pub fn to_pascal_case(name: String) -> String {
  name
  |> string.split("_")
  |> do_pascal_case("")
}

/// Recursive helper for to_pascal_case. Processes each segment
/// of the split string, capitalizing and concatenating them
/// into the final PascalCase result.
///
fn do_pascal_case(parts: List(String), acc: String) -> String {
  case parts {
    [] -> acc
    [part, ..rest] -> {
      let capitalized = string.capitalise(part)
      do_pascal_case(rest, acc <> capitalized)
    }
  }
}
