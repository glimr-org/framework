//// Hook Runner
////
//// Hooks let developers plug custom steps (linters,
//// formatters, asset pipelines) into the Glimr lifecycle
//// without patching framework code. Internal commands run
//// inside the existing BEAM VM to avoid the startup cost of
//// spawning a new process for each hook.

import gleam/dict
import gleam/io
import gleam/list
import gleam/string
import glimr/internal/actions/compile_commands
import shellout

// ------------------------------------------------------------- Public Functions

/// Runs hooks sequentially — stopping on the first failure so
/// later hooks don't execute against a broken state (e.g., a
/// formatter shouldn't run if the linter failed).
///
pub fn run(hooks: List(String)) -> Result(Nil, String) {
  run_hooks(hooks)
}

/// Substitutes $PATH in each hook template per file so that
/// tools like formatters receive the actual changed path.
/// Processing files sequentially avoids overwhelming external
/// tools with parallel invocations.
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

// ------------------------------------------------------------- Private Functions

/// Recursive helper that short-circuits on failure. Using
/// explicit recursion instead of list.try_each keeps the error
/// propagation straightforward.
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

/// Dispatches based on the "./glimr " prefix so that internal
/// commands stay in-process (no BEAM startup overhead) while
/// external commands get full shell semantics (pipes,
/// redirects, env vars).
///
fn run_hook(cmd: String) -> Result(Nil, String) {
  case string.starts_with(cmd, "./glimr ") {
    True -> run_internal_command(cmd)
    False -> run_external_command(cmd)
  }
}

/// Looks up the command in the pre-compiled registry and calls
/// its main() via Erlang apply/3. This avoids spawning a new
/// BEAM VM for each internal hook, which would add ~1s of
/// startup overhead per invocation. Falls back to external
/// execution if the registry hasn't been compiled yet.
///
fn run_internal_command(cmd: String) -> Result(Nil, String) {
  let parts =
    cmd
    |> string.drop_start(8)
    |> string.trim()
    |> string.split(" ")

  let name = list.first(parts) |> unwrap_or("")

  case compile_commands.read_registry() {
    Ok(registry) -> {
      case dict.get(registry, name) {
        Ok(info) -> {
          // Convert module path to Erlang atom format
          // glimr/internal/console/commands/build -> glimr@internal@console@commands@build
          let module = string.replace(info.module, "/", "@")
          call_module_main(module)
          Ok(Nil)
        }
        Error(_) -> {
          Error("Unknown command: " <> name)
        }
      }
    }
    Error(_) -> {
      // Registry not found, fall back to external
      run_external_command(cmd)
    }
  }
}

/// Dynamically calls a module's main() function using Erlang
/// apply/3. The module string must be in Erlang atom format
/// (slashes replaced with @).
///
@external(erlang, "glimr_hooks_ffi", "call_module_main")
fn call_module_main(module: String) -> Nil

/// Gleam's result.unwrap requires the error type to match, so
/// this small helper avoids a verbose case expression at each
/// call site.
///
fn unwrap_or(result: Result(a, e), default: a) -> a {
  case result {
    Ok(value) -> value
    Error(_) -> default
  }
}

/// Uses /bin/sh so that shell features (pipes, globs, env
/// expansion) work in hook commands. Output is printed directly
/// so the developer sees tool feedback inline with the Glimr
/// output.
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
