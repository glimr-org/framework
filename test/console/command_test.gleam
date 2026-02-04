import gleam/dict
import gleeunit/should
import glimr/console/command.{Argument, Command, Flag, Args}

// ------------------------------------------------------------- Command Creation

pub fn new_creates_empty_command_test() {
  let cmd = command.new()

  let assert Command(name, description, args, _) = cmd
  name |> should.equal("")
  description |> should.equal("")
  args |> should.equal([])
}

// ------------------------------------------------------------- Fluent API

pub fn name_sets_command_name_test() {
  let cmd =
    command.new()
    |> command.name("greet")

  let assert Command(name, _, _, _) = cmd
  name |> should.equal("greet")
}

pub fn description_sets_command_description_test() {
  let cmd =
    command.new()
    |> command.description("Greet the user")

  let assert Command(_, description, _, _) = cmd
  description |> should.equal("Greet the user")
}

pub fn args_sets_argument_list_test() {
  let cmd =
    command.new()
    |> command.args([
      Argument("name", "The name"),
      Flag("verbose", "v", "Verbose output"),
    ])

  let assert Command(_, _, args, _) = cmd
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
    Args(
      arguments: dict.from_list([]),
      flags: ["verbose"],
      options: dict.new(),
    )

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
    |> command.name("glimr:greet")
    |> command.description("A friendly greeting")
    |> command.handler(fn(_args) { Nil })

  let assert Command(name, description, _, _) = cmd
  name |> should.equal("glimr:greet")
  description |> should.equal("A friendly greeting")
}

pub fn full_command_with_args_creation_test() {
  let cmd =
    command.new()
    |> command.name("make:controller")
    |> command.description("Create a new controller")
    |> command.args([
      Argument("name", "The controller name"),
      Flag("resource", "r", "Generate resource controller"),
    ])
    |> command.handler(fn(_args) { Nil })

  let assert Command(name, description, args, _) = cmd
  name |> should.equal("make:controller")
  description |> should.equal("Create a new controller")
  args
  |> should.equal([
    Argument("name", "The controller name"),
    Flag("resource", "r", "Generate resource controller"),
  ])
}
