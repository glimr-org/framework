//// Build Runner
////
//// Executes the Gleam build command and formats output.
//// Provides consistent build feedback with success and
//// error messages.
////

import glimr/console/console
import shellout

/// Runs the Gleam build command and displays formatted output.
/// Returns Ok on successful build or Error with the failure
/// message if the build fails.
///
pub fn run() -> Result(Nil, String) {
  console.output()
  |> console.blank_line(1)
  |> console.line_warning("Building application...")
  |> console.print()

  case
    shellout.command("gleam", ["build"], in: ".", opt: [shellout.LetBeStdout])
  {
    Ok(_) -> {
      console.output()
      |> console.blank_line(1)
      |> console.line_success("Build complete! ✨")
      |> console.print()

      Ok(Nil)
    }
    Error(#(_, _)) -> {
      console.output()
      |> console.blank_line(1)
      |> console.line_error("Build failed")
      |> console.print()

      Error("Build failed")
    }
  }
}
