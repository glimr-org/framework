//// Console helper functions
////
//// This module contains helper functions meant for
//// streamlining common console output needs like coloring
//// console output, for example.

import gleam/io
import gleam/list

// ------------------------------------------------------------- Public Types

/// Represents buffered console output that can be built up
/// using a fluent API and then printed all at once. Contains
/// the accumulated lines and a padding flag for formatting.
///
pub type Output {
  Output(lines: List(String))
}

// ------------------------------------------------------------- Private Constants

/// Success color (green) for terminal output
const color_success = "\u{001b}[32m"

/// Warning color (yellow) for terminal output
const color_warning = "\u{001b}[33m"

/// Error color (red) for terminal output
const color_error = "\u{001b}[31m"

/// Default color for terminal output
const color_reset = "\u{001b}[0m"

// ------------------------------------------------------------- Public Functions

/// Helper function to color console output. Meant to be used
/// with the io.print or io.println functions. This helper
/// colors output using the terminal's green color.
///
/// *Example*
///
/// ```gleam
/// io.println(console.success("This message is green"))
/// // or only have part of the message in green:
/// io.println("Hello, " <> console.success("Gleam users!"))
/// ```
///
pub fn success(output: String) -> String {
  color_success <> output <> color_reset
}

/// Helper function to color console output. Meant to be used
/// with the io.print or io.println functions. This helper
/// colors output using the terminal's yellow color.
///
/// *Example*
///
/// ```gleam
/// io.println(console.warning("This message is yellow"))
/// // or only have part of the message in yellow:
/// io.println("Hello, " <> console.warning("Gleam users!"))
/// ```
///
pub fn warning(output: String) -> String {
  color_warning <> output <> color_reset
}

/// Helper function to color console output. Meant to be used
/// with the io.print or io.println functions. This helper
/// colors output using the terminal's red color.
///
/// *Example*
///
/// ```gleam
/// io.println(console.error("This message is red"))
/// // or only have part of the message in red:
/// io.println("Hello, " <> console.error("Gleam users!"))
/// ```
///
pub fn error(output: String) -> String {
  color_error <> output <> color_reset
}

/// Creates a new Output builder with padding enabled by default.
/// Use with line(), blank_line(), and print() to build and
/// display multi-line console output with consistent formatting.
///
/// *Example*
///
/// ```gleam
/// console.output()
/// |> console.line("Processing files...")
/// |> console.line_success("Done!")
/// |> console.print()
/// ```
///
pub fn output() -> Output {
  Output(lines: [])
}

/// Adds a line of text to the output builder. Lines are
/// printed in the order they are added. Use this for
/// plain text without any color formatting applied.
///
/// *Example*
///
/// ```gleam
/// console.output()
/// |> console.line("First line")
/// |> console.line("Second line")
/// |> console.print()
/// ```
///
pub fn line(output: Output, message: String) -> Output {
  Output(lines: list.append(output.lines, [message]))
}

/// Adds an empty line to the output builder. Useful for
/// creating visual spacing between different sections
/// of your console output.
///
/// *Example*
///
/// ```gleam
/// console.output()
/// |> console.line("Section 1")
/// |> console.blank_line(1)
/// |> console.line("Section 2")
/// |> console.print()
/// ```
///
pub fn blank_line(output: Output, amount: Int) -> Output {
  let empty_lines = list.repeat("", amount)

  Output(lines: list.append(output.lines, empty_lines))
}

/// Adds a line colored green (success) to the output.
/// This is a shorthand for line(output, success(message))
/// and is useful for indicating successful operations.
///
/// *Example*
///
/// ```gleam
/// console.output()
/// |> console.line_success("File created successfully!")
/// |> console.print()
/// ```
///
pub fn line_success(output: Output, message: String) -> Output {
  line(output, success(message))
}

/// Adds a line colored red (error) to the output.
/// This is a shorthand for line(output, error(message))
/// and is useful for displaying error messages.
///
/// *Example*
///
/// ```gleam
/// console.output()
/// |> console.line_error("Failed to write file")
/// |> console.print()
/// ```
///
pub fn line_error(output: Output, message: String) -> Output {
  line(output, error(message))
}

/// Adds a line colored yellow (warning) to the output.
/// This is a shorthand for line(output, warning(message))
/// and is useful for displaying warning messages.
///
/// *Example*
///
/// ```gleam
/// console.output()
/// |> console.line_warning("File already exists, skipping")
/// |> console.print()
/// ```
///
pub fn line_warning(output: Output, message: String) -> Output {
  line(output, warning(message))
}

/// Prints the output to the console. If padded is true,
/// blank lines are printed before and after the content
/// for visual separation from other terminal output.
///
/// *Example*
///
/// ```gleam
/// console.output()
/// |> console.line("Hello, world!")
/// |> console.print()
/// ```
///
pub fn print(output: Output) -> Nil {
  do_print(output.lines)
}

/// Immediately terminates the BEAM process with the given exit 
/// code. Use this to signal success (0) or failure (non-zero) 
/// to the calling shell or parent process.
///
pub fn halt(code: Int) -> Nil {
  do_halt(code)
}

// ------------------------------------------------------------- Private Functions

/// Recursively prints each line in the list to the console.
/// This is an internal helper function used by print() to
/// iterate through and display all accumulated output lines.
///
fn do_print(lines: List(String)) -> Nil {
  case lines {
    [first, ..rest] -> {
      io.println(first)

      do_print(rest)
    }
    [] -> Nil
  }
}

// ------------------------------------------------------------- FFI Bindings

/// FFI binding to Erlang's halt/1 which forces an immediate 
/// shutdown of the runtime. Bypasses normal cleanup so it 
/// should only be used for final exits.
///
@external(erlang, "erlang", "halt")
fn do_halt(code: Int) -> Nil
