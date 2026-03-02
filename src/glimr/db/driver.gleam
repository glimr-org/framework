//// Database Connection Drivers
////
//// Database connections are configured in config/database.toml
//// with environment variable interpolation for secrets. This
//// module parses those configs into typed Connection values
//// that the pool startup code consumes — so a missing
//// DATABASE_URL blows up at boot with a clear message rather
//// than on the first query ten minutes later.

import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glimr/config/config
import glimr/db/db.{type Config}
import tom

// ------------------------------------------------------------- Public Types

/// Config fields are wrapped in Result because they come from
/// environment variables that might not be set yet. Deferring
/// the error to pool startup (via to_config) means the config
/// module doesn't panic during parsing, and the error message
/// can name the exact missing env var. Postgres supports both a
/// single URL and individual host/port/db params because some
/// hosting providers give you one format, some the other.
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

/// The migration generator needs to know which SQL dialect to
/// emit (SERIAL vs INTEGER PRIMARY KEY AUTOINCREMENT, BOOLEAN
/// vs INTEGER, etc.) without caring about connection details.
/// This simple enum is enough to branch on.
///
pub type DriverType {
  Postgres
  Sqlite
}

// ------------------------------------------------------------- Public Functions

/// The migration generator and SQL emitter need to know which
/// dialect to use without caring about URLs or credentials.
/// Both Postgres variants map to the same DriverType since they
/// produce the same SQL.
///
pub fn connection_type(connection: Connection) -> DriverType {
  case connection {
    PostgresUriConnection(_, _, _) -> Postgres
    PostgresConnection(_, _, _, _, _, _, _) -> Postgres
    SqliteConnection(_, _, _) -> Sqlite
  }
}

/// Every connection variant has a name field but in a different
/// position. This saves callers from writing the same
/// three-branch pattern match every time they just need the
/// name — which comes up constantly in logging and config
/// lookups.
///
pub fn connection_name(connection: Connection) -> String {
  case connection {
    PostgresUriConnection(name, _, _) -> name
    PostgresConnection(name, _, _, _, _, _, _) -> name
    SqliteConnection(name, _, _) -> name
  }
}

/// Console commands like `./glimr migrate` only need one
/// database connection, not the pool of 10 the web server uses.
/// Overriding the pool size to 1 avoids wasting connections and
/// speeds up command startup.
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

/// This is where env var errors finally surface. The Connection
/// holds Results from config parsing; this function unwraps
/// them all at once. If DATABASE_URL isn't set, you get a panic
/// naming the exact connection and parameter rather than a
/// cryptic driver error later.
///
pub fn to_config(connection: Connection) -> Config {
  case connection {
    PostgresUriConnection(name, url_result, pool_size_r) -> {
      let url = unwrap_or_panic(url_result, name, "url")
      let pool_size = unwrap_or_panic(pool_size_r, name, "pool_size")
      db.postgres_config(url, pool_size: pool_size)
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
      db.postgres_params_config(
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
      db.sqlite_config(database, pool_size: pool_size)
    }
  }
}

/// The `./glimr db:check` command calls this to report all
/// missing env vars at once instead of crashing on the first
/// one. Returning a list lets the CLI show "missing: host,
/// username, password" in a single message so you can fix
/// everything in one pass.
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

/// Some callers need to check whether a connection exists
/// without crashing — like the database watcher checking if a
/// connection name from a file path is valid. Returns Error
/// instead of panicking so those callers can handle the miss
/// gracefully.
///
pub fn get_connection_safe(
  connections: List(Connection),
  name: String,
) -> Result(Connection, Nil) {
  list.find(connections, fn(c: Connection) { c.name == name })
}

/// When a developer passes `--connection main` to a CLI command
/// and that connection doesn't exist, the worst thing we could
/// do is silently continue. Panicking with the exact name and
/// pointing at config/database.toml tells them what to fix.
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

/// Connections are loaded once and cached in persistent_term so
/// every subsequent call is a fast lookup. The first call reads
/// config/database.toml, parses each [connections.*] entry, and
/// caches the result. Returns an empty list if no config exists
/// — which is fine for projects that don't use a database.
///
pub fn load_connections() -> List(Connection) {
  case config.get_cached("db_connections") {
    Ok(connections) -> connections
    Error(_) -> {
      let connections = load_connections_from_config()
      config.cache("db_connections", connections)
      connections
    }
  }
}

/// Tests that exercise different database configurations need
/// to start fresh between runs. Without clearing, the
/// persistent_term cache would serve stale config from the
/// previous test case.
///
pub fn clear_cache() -> Nil {
  config.clear_cached("db_connections")
}

// ------------------------------------------------------------- Private Functions

/// Reads config/database.toml via the unified config system and
/// parses each [connections.*] section into a typed Connection.
/// Called once by load_connections and cached for all
/// subsequent lookups.
///
fn load_connections_from_config() -> List(Connection) {
  case config.get_table("database.connections") {
    Ok(connections_table) -> {
      connections_table
      |> dict.to_list
      |> list.map(fn(entry) {
        let #(name, conn_toml) = entry
        parse_connection(name, conn_toml)
      })
    }
    Error(_) -> []
  }
}

/// "postgres_url" gets you a single-URL connection, "postgres"
/// gives you individual host/port/db fields, and "sqlite" just
/// needs a file path. Defaulting unknown driver strings to
/// Postgres preserves backward compatibility with older config
/// files that didn't specify a driver.
///
fn parse_connection(name: String, toml: tom.Toml) -> Connection {
  let driver_str = config.toml_get_string(toml, "driver", "postgres")

  case driver_str {
    "postgres" ->
      PostgresConnection(
        name: name,
        host: config.toml_get_env_string(toml, "host"),
        port: config.toml_get_env_int(toml, "port"),
        database: config.toml_get_env_string(toml, "database"),
        username: config.toml_get_env_string(toml, "username"),
        password: config.toml_get_env_string(toml, "password"),
        pool_size: config.toml_get_env_int(toml, "pool_size"),
      )
    "postgres_url" ->
      PostgresUriConnection(
        name: name,
        url: config.toml_get_env_string(toml, "url"),
        pool_size: config.toml_get_env_int(toml, "pool_size"),
      )
    "sqlite" ->
      SqliteConnection(
        name: name,
        database: config.toml_get_env_string(toml, "database"),
        pool_size: config.toml_get_env_int(toml, "pool_size"),
      )
    _ ->
      PostgresConnection(
        name: name,
        host: config.toml_get_env_string(toml, "host"),
        port: config.toml_get_env_int(toml, "port"),
        database: config.toml_get_env_string(toml, "database"),
        username: config.toml_get_env_string(toml, "username"),
        password: config.toml_get_env_string(toml, "password"),
        pool_size: config.toml_get_env_int(toml, "pool_size"),
      )
  }
}

/// A bare `let assert Ok(x) = result` would crash with a
/// generic match error. Wrapping the panic with the connection
/// name and parameter name means the developer sees "Connection
/// 'main' is missing required parameter: host" instead of
/// digging through a stack trace.
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

/// Connection names like "main_db" need to become "MainDb" when
/// generating Gleam type names and module references. The code
/// generator calls this to produce valid PascalCase identifiers
/// from user-chosen connection names.
///
pub fn to_pascal_case(name: String) -> String {
  name
  |> string.split("_")
  |> do_pascal_case("")
}

/// Gleam doesn't have a built-in capitalize-and-join, so we
/// recurse over the underscore-split segments. Each segment
/// gets capitalized and appended — "main" + "db" becomes "Main"
/// + "Db" = "MainDb".
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
