import gleeunit/should
import glimr/db/driver.{
  Postgres, PostgresConnection, PostgresUriConnection, Sqlite, SqliteConnection,
}

// ------------------------------------------------------------- connection_type

pub fn connection_type_postgres_uri_test() {
  let conn =
    PostgresUriConnection(
      name: "main",
      url: Ok("postgres://localhost/db"),
      pool_size: Ok(10),
    )

  driver.connection_type(conn)
  |> should.equal(Postgres)
}

pub fn connection_type_postgres_params_test() {
  let conn =
    PostgresConnection(
      name: "main",
      host: Ok("localhost"),
      port: Ok(5432),
      database: Ok("mydb"),
      username: Ok("user"),
      password: Ok("pass"),
      pool_size: Ok(10),
    )

  driver.connection_type(conn)
  |> should.equal(Postgres)
}

pub fn connection_type_sqlite_test() {
  let conn =
    SqliteConnection(
      name: "local",
      database: Ok("./data.db"),
      pool_size: Ok(5),
    )

  driver.connection_type(conn)
  |> should.equal(Sqlite)
}

// ------------------------------------------------------------- connection_name

pub fn connection_name_postgres_uri_test() {
  let conn =
    PostgresUriConnection(
      name: "production",
      url: Ok("postgres://localhost/db"),
      pool_size: Ok(10),
    )

  driver.connection_name(conn)
  |> should.equal("production")
}

pub fn connection_name_postgres_params_test() {
  let conn =
    PostgresConnection(
      name: "analytics",
      host: Ok("localhost"),
      port: Ok(5432),
      database: Ok("mydb"),
      username: Ok("user"),
      password: Ok("pass"),
      pool_size: Ok(10),
    )

  driver.connection_name(conn)
  |> should.equal("analytics")
}

pub fn connection_name_sqlite_test() {
  let conn =
    SqliteConnection(
      name: "cache",
      database: Ok("./cache.db"),
      pool_size: Ok(5),
    )

  driver.connection_name(conn)
  |> should.equal("cache")
}

// ------------------------------------------------------------- with_pool_size

pub fn with_pool_size_postgres_uri_test() {
  let conn =
    PostgresUriConnection(
      name: "main",
      url: Ok("postgres://localhost/db"),
      pool_size: Ok(10),
    )

  let updated = driver.with_pool_size(conn, 1)

  case updated {
    PostgresUriConnection(_, _, pool_size) ->
      pool_size |> should.equal(Ok(1))
    _ -> should.fail()
  }
}

pub fn with_pool_size_postgres_params_test() {
  let conn =
    PostgresConnection(
      name: "main",
      host: Ok("localhost"),
      port: Ok(5432),
      database: Ok("mydb"),
      username: Ok("user"),
      password: Ok("pass"),
      pool_size: Ok(10),
    )

  let updated = driver.with_pool_size(conn, 2)

  case updated {
    PostgresConnection(_, _, _, _, _, _, pool_size) ->
      pool_size |> should.equal(Ok(2))
    _ -> should.fail()
  }
}

pub fn with_pool_size_sqlite_test() {
  let conn =
    SqliteConnection(
      name: "local",
      database: Ok("./data.db"),
      pool_size: Ok(5),
    )

  let updated = driver.with_pool_size(conn, 1)

  case updated {
    SqliteConnection(_, _, pool_size) -> pool_size |> should.equal(Ok(1))
    _ -> should.fail()
  }
}

pub fn with_pool_size_preserves_other_fields_test() {
  let conn =
    PostgresUriConnection(
      name: "main",
      url: Ok("postgres://localhost/db"),
      pool_size: Ok(10),
    )

  let updated = driver.with_pool_size(conn, 1)

  driver.connection_name(updated)
  |> should.equal("main")
}

// ------------------------------------------------------------- validate

pub fn validate_postgres_uri_valid_test() {
  let conn =
    PostgresUriConnection(
      name: "main",
      url: Ok("postgres://localhost/db"),
      pool_size: Ok(10),
    )

  driver.validate(conn)
  |> should.equal([])
}

pub fn validate_postgres_uri_missing_url_test() {
  let conn =
    PostgresUriConnection(
      name: "main",
      url: Error("DATABASE_URL not set"),
      pool_size: Ok(10),
    )

  driver.validate(conn)
  |> should.equal(["url"])
}

pub fn validate_postgres_uri_missing_pool_size_test() {
  let conn =
    PostgresUriConnection(
      name: "main",
      url: Ok("postgres://localhost/db"),
      pool_size: Error("POOL_SIZE not set"),
    )

  driver.validate(conn)
  |> should.equal(["pool_size"])
}

pub fn validate_postgres_uri_missing_both_test() {
  let conn =
    PostgresUriConnection(
      name: "main",
      url: Error("DATABASE_URL not set"),
      pool_size: Error("POOL_SIZE not set"),
    )

  let missing = driver.validate(conn)

  missing |> should.equal(["pool_size", "url"])
}

pub fn validate_postgres_params_valid_test() {
  let conn =
    PostgresConnection(
      name: "main",
      host: Ok("localhost"),
      port: Ok(5432),
      database: Ok("mydb"),
      username: Ok("user"),
      password: Ok("pass"),
      pool_size: Ok(10),
    )

  driver.validate(conn)
  |> should.equal([])
}

pub fn validate_postgres_params_missing_host_test() {
  let conn =
    PostgresConnection(
      name: "main",
      host: Error("HOST not set"),
      port: Ok(5432),
      database: Ok("mydb"),
      username: Ok("user"),
      password: Ok("pass"),
      pool_size: Ok(10),
    )

  driver.validate(conn)
  |> should.equal(["host"])
}

pub fn validate_postgres_params_missing_multiple_test() {
  let conn =
    PostgresConnection(
      name: "main",
      host: Error("HOST not set"),
      port: Ok(5432),
      database: Error("DATABASE not set"),
      username: Ok("user"),
      password: Ok("pass"),
      pool_size: Error("POOL_SIZE not set"),
    )

  let missing = driver.validate(conn)

  missing |> should.equal(["pool_size", "database", "host"])
}

pub fn validate_sqlite_valid_test() {
  let conn =
    SqliteConnection(
      name: "local",
      database: Ok("./data.db"),
      pool_size: Ok(5),
    )

  driver.validate(conn)
  |> should.equal([])
}

pub fn validate_sqlite_missing_database_test() {
  let conn =
    SqliteConnection(
      name: "local",
      database: Error("DATABASE not set"),
      pool_size: Ok(5),
    )

  driver.validate(conn)
  |> should.equal(["database"])
}

// ------------------------------------------------------------- find_by_name

pub fn find_by_name_found_test() {
  let connections = [
    PostgresUriConnection(
      name: "main",
      url: Ok("postgres://localhost/db"),
      pool_size: Ok(10),
    ),
    SqliteConnection(
      name: "cache",
      database: Ok("./cache.db"),
      pool_size: Ok(5),
    ),
  ]

  let found = driver.find_by_name("cache", connections)

  driver.connection_name(found)
  |> should.equal("cache")

  driver.connection_type(found)
  |> should.equal(Sqlite)
}

pub fn find_by_name_first_in_list_test() {
  let connections = [
    PostgresUriConnection(
      name: "primary",
      url: Ok("postgres://localhost/db"),
      pool_size: Ok(10),
    ),
    PostgresUriConnection(
      name: "secondary",
      url: Ok("postgres://localhost/db2"),
      pool_size: Ok(5),
    ),
  ]

  let found = driver.find_by_name("primary", connections)

  driver.connection_name(found)
  |> should.equal("primary")
}

// ------------------------------------------------------------- to_pascal_case

pub fn to_pascal_case_simple_test() {
  driver.to_pascal_case("main")
  |> should.equal("Main")
}

pub fn to_pascal_case_snake_case_test() {
  driver.to_pascal_case("my_database")
  |> should.equal("MyDatabase")
}

pub fn to_pascal_case_multiple_underscores_test() {
  driver.to_pascal_case("user_data_store")
  |> should.equal("UserDataStore")
}

pub fn to_pascal_case_already_capitalized_test() {
  driver.to_pascal_case("Main")
  |> should.equal("Main")
}

pub fn to_pascal_case_single_letter_test() {
  driver.to_pascal_case("a")
  |> should.equal("A")
}
