//// Hook Runner
////
//// Executes hook commands configured in the Glimr project.
//// Supports both internal Glimr commands and external shell
//// commands with file path substitution.
////

import gleam/dict
import gleam/io
import gleam/list
import gleam/string
import glimr/console/command.{type Command, Command, Args}
import shellout

/// Runs a list of hook commands sequentially. Stops and returns
/// an error if any hook fails. Internal commands starting with
/// "./glimr " are executed directly, others via shell.
///
pub fn run(hooks: List(String)) -> Result(Nil, String) {
  run_hooks(hooks)
}

/// Runs hooks for each file, substituting $PATH with the file
/// path. Processes files sequentially, stopping on the first
/// error encountered.
///
pub fn run_for_files(
  hooks: List(String),
  files: List(String),
) -> Result(Nil, String) {
  case files {
    [] -> Ok(Nil)
    [file, ..rest] -> {
      let substituted_hooks =
        list.map(hooks, fn(hook) { string.replace(hook, "$PATH", file) })
      case run_hooks(substituted_hooks) {
        Ok(_) -> run_for_files(hooks, rest)
        Error(e) -> Error(e)
      }
    }
  }
}

/// Recursive helper that processes hooks one at a time. Returns
/// Ok when all hooks complete successfully or Error on the
/// first failure.
///
fn run_hooks(hooks: List(String)) -> Result(Nil, String) {
  case hooks {
    [] -> Ok(Nil)
    [hook, ..rest] -> {
      case run_hook(hook) {
        Ok(_) -> run_hooks(rest)
        Error(e) -> Error(e)
      }
    }
  }
}

/// Runs a single hook command. Dispatches to internal command
/// handler if the command starts with "./glimr ", otherwise
/// runs it as an external shell command.
///
fn run_hook(cmd: String) -> Result(Nil, String) {
  case string.starts_with(cmd, "./glimr ") {
    True -> run_internal_command(cmd)
    False -> run_external_command(cmd)
  }
}

/// Executes an internal Glimr command by name. Looks up the
/// command in the stored commands list and calls its handler.
/// Falls back to external execution for database commands.
///
fn run_internal_command(cmd: String) -> Result(Nil, String) {
  let parts =
    cmd
    |> string.drop_start(8)
    |> string.trim()
    |> string.split(" ")

  let name = list.first(parts) |> unwrap_or("")
  let args = list.drop(parts, 1)
  let options = parse_options(args)
  let commands = command.get_commands()

  case find_command(commands, name) {
    Ok(Command(handler:, ..)) -> {
      let parsed_args = Args(dict.new(), [], options)
      handler(parsed_args)
      Ok(Nil)
    }
    Ok(_) -> {
      // CommandWithDb or CommandWithCache - fall back to external
      run_external_command(cmd)
    }
    Error(_) -> {
      Error("Unknown command: " <> name)
    }
  }
}

/// Parses command line options from argument list. Extracts
/// --key=value pairs and returns them as a dictionary,
/// ignoring non-option arguments.
///
fn parse_options(args: List(String)) -> dict.Dict(String, String) {
  args
  |> list.filter_map(fn(arg) {
    case string.starts_with(arg, "--") {
      True -> {
        let without_dashes = string.drop_start(arg, 2)
        case string.split_once(without_dashes, "=") {
          Ok(#(key, value)) -> Ok(#(key, value))
          Error(_) -> Error(Nil)
        }
      }
      False -> Error(Nil)
    }
  })
  |> dict.from_list
}

/// Unwraps a Result, returning the Ok value or a default.
/// Provides a fallback value when the result contains an
/// error instead of a valid value.
///
fn unwrap_or(result: Result(a, e), default: a) -> a {
  case result {
    Ok(value) -> value
    Error(_) -> default
  }
}

/// Searches the command list for a command matching the given
/// name. Returns Ok with the command if found, or Error if
/// no command matches.
///
fn find_command(commands: List(Command), name: String) -> Result(Command, Nil) {
  list.find(commands, fn(cmd) { cmd.name == name })
}

/// Executes an external command via /bin/sh. Prints any output
/// from the command and returns Error with details if the
/// command fails.
///
fn run_external_command(cmd: String) -> Result(Nil, String) {
  case shellout.command("/bin/sh", ["-c", cmd], in: ".", opt: []) {
    Ok(output) -> {
      let trimmed = string.trim_end(output)
      case trimmed {
        "" -> Nil
        _ -> io.println(trimmed)
      }
      Ok(Nil)
    }
    Error(#(_, msg)) -> {
      Error("Hook failed: " <> cmd <> "\n" <> msg)
    }
  }
}
