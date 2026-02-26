import gleam/dict
import gleeunit/should
import glimr/cache/driver as cache_driver
import glimr/config/database
import glimr/console/command.{Args, Argument, Command, Flag, Option}
import glimr/db/driver.{SqliteConnection}

// ------------------------------------------------------------- Command Creation

pub fn new_creates_empty_command_test() {
  let cmd = command.new()

  let Command(description, args, _) = cmd
  description |> should.equal("")
  args |> should.equal([])
}

// ------------------------------------------------------------- Fluent API

pub fn description_sets_command_description_test() {
  let cmd =
    command.new()
    |> command.description("Greet the user")

  let Command(description, _, _) = cmd
  description |> should.equal("Greet the user")
}

pub fn args_sets_argument_list_test() {
  let cmd =
    command.new()
    |> command.args([
      Argument("name", "The name"),
      Flag("verbose", "v", "Verbose output"),
    ])

  let Command(_, args, _) = cmd
  args
  |> should.equal([
    Argument("name", "The name"),
    Flag("verbose", "v", "Verbose output"),
  ])
}

// ------------------------------------------------------------- Args Access

pub fn get_arg_returns_value_when_found_test() {
  let parsed =
    Args(
      arguments: dict.from_list([#("name", "John"), #("age", "25")]),
      flags: [],
      options: dict.new(),
    )

  command.get_arg(parsed, "name")
  |> should.equal("John")

  command.get_arg(parsed, "age")
  |> should.equal("25")
}

pub fn has_flag_returns_true_when_present_test() {
  let parsed =
    Args(
      arguments: dict.from_list([]),
      flags: ["verbose", "force"],
      options: dict.new(),
    )

  command.has_flag(parsed, "verbose")
  |> should.equal(True)

  command.has_flag(parsed, "force")
  |> should.equal(True)
}

pub fn has_flag_returns_false_when_not_present_test() {
  let parsed =
    Args(arguments: dict.from_list([]), flags: ["verbose"], options: dict.new())

  command.has_flag(parsed, "quiet")
  |> should.equal(False)
}

pub fn has_flag_with_empty_flags_test() {
  let parsed =
    Args(arguments: dict.from_list([]), flags: [], options: dict.new())

  command.has_flag(parsed, "anything")
  |> should.equal(False)
}

pub fn get_option_returns_value_when_present_test() {
  let parsed =
    Args(
      arguments: dict.from_list([]),
      flags: [],
      options: dict.from_list([#("database", "analytics"), #("format", "json")]),
    )

  command.get_option(parsed, "database")
  |> should.equal("analytics")

  command.get_option(parsed, "format")
  |> should.equal("json")
}

// ------------------------------------------------------------- Full Command Flow

pub fn full_command_creation_test() {
  let cmd =
    command.new()
    |> command.description("A friendly greeting")
    |> command.handler(fn(_args) { Nil })

  let Command(description, _, _) = cmd
  description |> should.equal("A friendly greeting")
}

// ------------------------------------------------------------- resolve_connection

pub fn resolve_connection_replaces_default_with_first_connection_test() {
  // Seed the cache with a known connection
  database.clear_cache()
  cache_db_config([
    SqliteConnection(name: "mydb", database: Ok("./data.db"), pool_size: Ok(5)),
  ])

  let parsed =
    Args(
      arguments: dict.from_list([#("name", "user")]),
      flags: [],
      options: dict.from_list([#("database", "_default")]),
    )

  let assert Ok(resolved) = command.resolve_connection(parsed)

  command.get_option(resolved, "database")
  |> should.equal("mydb")

  database.clear_cache()
}

pub fn resolve_connection_keeps_explicit_connection_name_test() {
  database.clear_cache()
  cache_db_config([
    SqliteConnection(name: "mydb", database: Ok("./data.db"), pool_size: Ok(5)),
  ])

  let parsed =
    Args(
      arguments: dict.from_list([#("name", "user")]),
      flags: [],
      options: dict.from_list([#("database", "mydb")]),
    )

  let assert Ok(resolved) = command.resolve_connection(parsed)

  command.get_option(resolved, "database")
  |> should.equal("mydb")

  database.clear_cache()
}

pub fn resolve_connection_errors_when_no_connections_test() {
  database.clear_cache()
  cache_db_config([])

  let parsed =
    Args(
      arguments: dict.from_list([#("name", "user")]),
      flags: [],
      options: dict.from_list([#("database", "_default")]),
    )

  command.resolve_connection(parsed)
  |> should.be_error()

  database.clear_cache()
}

pub fn resolve_connection_errors_when_connection_not_found_test() {
  database.clear_cache()
  cache_db_config([
    SqliteConnection(name: "mydb", database: Ok("./data.db"), pool_size: Ok(5)),
  ])

  let parsed =
    Args(
      arguments: dict.from_list([#("name", "user")]),
      flags: [],
      options: dict.from_list([#("database", "fake_db")]),
    )

  command.resolve_connection(parsed)
  |> should.be_error()

  database.clear_cache()
}

pub fn resolve_connection_noop_without_database_option_test() {
  let parsed =
    Args(
      arguments: dict.from_list([#("name", "user")]),
      flags: [],
      options: dict.new(),
    )

  let assert Ok(resolved) = command.resolve_connection(parsed)

  resolved |> should.equal(parsed)
}

@external(erlang, "glimr_kernel_ffi", "cache_db_config")
fn cache_db_config(connections: List(driver.Connection)) -> Nil

@external(erlang, "glimr_kernel_ffi", "cache_cache_config")
fn cache_store_config(stores: List(cache_driver.CacheStore)) -> Nil

@external(erlang, "glimr_command_test_ffi", "clear_cache_config")
fn clear_cache_config() -> Nil

// ------------------------------------------------------------- Full Command Flow

pub fn full_command_with_args_creation_test() {
  let cmd =
    command.new()
    |> command.description("Create a new controller")
    |> command.args([
      Argument("name", "The controller name"),
      Flag("resource", "r", "Generate resource controller"),
    ])
    |> command.handler(fn(_args) { Nil })

  let Command(description, args, _) = cmd
  description |> should.equal("Create a new controller")
  args
  |> should.equal([
    Argument("name", "The controller name"),
    Flag("resource", "r", "Generate resource controller"),
  ])
}

// ------------------------------------------------------------- db_handler

pub fn db_handler_appends_db_option_test() {
  let cmd =
    command.new()
    |> command.description("Run migrations")
    |> command.args([
      Flag("seed", "s", "Seed after migrating"),
    ])
    |> command.db_handler(fn(_args, _pool) { Nil })

  let Command(_, args, _) = cmd
  args
  |> should.equal([
    Flag("seed", "s", "Seed after migrating"),
    Option("database", "Database connection to use", "_default"),
  ])
}

pub fn db_handler_preserves_existing_args_test() {
  let cmd =
    command.new()
    |> command.args([
      Argument("name", "The name"),
    ])
    |> command.db_handler(fn(_args, _pool) { Nil })

  let Command(_, args, _) = cmd
  args
  |> should.equal([
    Argument("name", "The name"),
    Option("database", "Database connection to use", "_default"),
  ])
}

// ------------------------------------------------------------- cache_handler

pub fn cache_handler_appends_cache_option_test() {
  let cmd =
    command.new()
    |> command.description("Clear cache")
    |> command.args([
      Flag("force", "f", "Skip confirmation"),
    ])
    |> command.cache_handler(fn(_args, _pool) { Nil })

  let Command(_, args, _) = cmd
  args
  |> should.equal([
    Flag("force", "f", "Skip confirmation"),
    Option("cache", "Cache store to use", "_default"),
  ])
}

pub fn cache_handler_preserves_existing_args_test() {
  let cmd =
    command.new()
    |> command.args([
      Argument("key", "Cache key"),
    ])
    |> command.cache_handler(fn(_args, _pool) { Nil })

  let Command(_, args, _) = cmd
  args
  |> should.equal([
    Argument("key", "Cache key"),
    Option("cache", "Cache store to use", "_default"),
  ])
}

// ------------------------------------------------------------- cache_db_handler

pub fn cache_db_handler_appends_cache_option_test() {
  let cmd =
    command.new()
    |> command.description("Generate cache table")
    |> command.args([
      Flag("migrate", "m", "Run migrations"),
    ])
    |> command.cache_db_handler(fn(_args, _pool, _table) { Nil })

  let Command(_, args, _) = cmd
  args
  |> should.equal([
    Flag("migrate", "m", "Run migrations"),
    Option("cache", "Cache store to use", "_default"),
  ])
}

pub fn cache_db_handler_preserves_existing_args_test() {
  let cmd =
    command.new()
    |> command.args([
      Argument("name", "The name"),
    ])
    |> command.cache_db_handler(fn(_args, _pool, _table) { Nil })

  let Command(_, args, _) = cmd
  args
  |> should.equal([
    Argument("name", "The name"),
    Option("cache", "Cache store to use", "_default"),
  ])
}

// ------------------------------------------------------------- resolve_cache

pub fn resolve_cache_replaces_default_with_first_store_test() {
  clear_cache_config()
  cache_store_config([
    cache_driver.DatabaseStore(name: "main", database: "mydb", table: "cache"),
  ])

  let parsed =
    Args(
      arguments: dict.new(),
      flags: [],
      options: dict.from_list([#("cache", "_default")]),
    )

  let assert Ok(resolved) = command.resolve_cache(parsed)

  command.get_option(resolved, "cache")
  |> should.equal("main")

  clear_cache_config()
}

pub fn resolve_cache_keeps_explicit_store_name_test() {
  clear_cache_config()
  cache_store_config([
    cache_driver.FileStore(name: "files", path: "priv/cache"),
    cache_driver.DatabaseStore(
      name: "db_cache",
      database: "mydb",
      table: "cache",
    ),
  ])

  let parsed =
    Args(
      arguments: dict.new(),
      flags: [],
      options: dict.from_list([#("cache", "db_cache")]),
    )

  let assert Ok(resolved) = command.resolve_cache(parsed)

  command.get_option(resolved, "cache")
  |> should.equal("db_cache")

  clear_cache_config()
}

pub fn resolve_cache_errors_when_no_stores_configured_test() {
  clear_cache_config()
  cache_store_config([])

  let parsed =
    Args(
      arguments: dict.new(),
      flags: [],
      options: dict.from_list([#("cache", "_default")]),
    )

  command.resolve_cache(parsed)
  |> should.be_error()

  clear_cache_config()
}

pub fn resolve_cache_errors_when_store_not_found_test() {
  clear_cache_config()
  cache_store_config([
    cache_driver.FileStore(name: "files", path: "priv/cache"),
  ])

  let parsed =
    Args(
      arguments: dict.new(),
      flags: [],
      options: dict.from_list([#("cache", "nonexistent")]),
    )

  command.resolve_cache(parsed)
  |> should.be_error()

  clear_cache_config()
}

pub fn resolve_cache_noop_without_cache_option_test() {
  let parsed = Args(arguments: dict.new(), flags: [], options: dict.new())

  let assert Ok(resolved) = command.resolve_cache(parsed)

  resolved |> should.equal(parsed)
}
