import gleam/dict
import gleeunit/should
import glimr/config/database
import glimr/console/command.{Args, Argument, Command, Flag}
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
