//// Build Runner
////
//// Wrapping `gleam build` behind this action gives the CLI a
//// single place to attach pre/post build hooks and consistent
//// formatted output — without it, every caller would duplicate
//// the shellout call and error handling.

import glimr/console/console
import shellout

// ------------------------------------------------------------- Public Functions

/// Shells out to `gleam build` and translates the exit status
/// into a Result so callers can chain build into larger
/// workflows with result.try without inspecting shell output
/// themselves.
///
pub fn run() -> Result(Nil, String) {
  console.output()
  |> console.line_warning("Building application...")
  |> console.print()

  case
    shellout.command("gleam", ["build"], in: ".", opt: [shellout.LetBeStdout])
  {
    Ok(_) -> {
      console.output()
      |> console.line_success("Build complete! ✨")
      |> console.print()

      Ok(Nil)
    }
    Error(#(_, _)) -> {
      console.output()
      |> console.line_error("Build failed")
      |> console.print()

      Error("Build failed")
    }
  }
}
