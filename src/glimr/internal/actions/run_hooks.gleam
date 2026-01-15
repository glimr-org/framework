import gleam/dict
import gleam/io
import gleam/list
import gleam/string
import glimr/console/command.{type Command, Command, ParsedArgs}
import shellout

/// Runs a list of hook commands sequentially. Stops and returns
/// an error if any hook fails. Internal commands starting with
/// "./glimr " are executed directly, others via shell.
///
pub fn run(hooks: List(String)) -> Result(Nil, String) {
  run_hooks(hooks)
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
  let name = string.drop_start(cmd, 8) |> string.trim()
  let commands = command.get_commands()

  case find_command(commands, name) {
    Ok(Command(handler:, ..)) -> {
      let empty_args = ParsedArgs(dict.new(), [], dict.new())
      handler(empty_args)
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
