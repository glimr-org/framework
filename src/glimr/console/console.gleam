//// Console Output Helpers
////
//// Raw ANSI escape codes scattered across command handlers
//// are hard to read and easy to get wrong (missing resets
//// leave the whole terminal colored). These helpers wrap
//// the codes so callers just pick a semantic level like
//// success or error, and the Output builder lets multi-line
//// output be composed via pipes then flushed at once.

import gleam/io
import gleam/list

// ------------------------------------------------------------- Public Types

/// Buffering lines before printing lets callers build complex
/// output via pipes without interleaving with other console
/// writes. Flushing all at once keeps related lines grouped
/// in the terminal.
///
pub type Output {
  Output(lines: List(String))
}

// ------------------------------------------------------------- Private Constants

/// Success color (green) for terminal output
const color_success = "\u{001b}[32m"

/// Warning color (yellow) for terminal output
const color_warning = "\u{001b}[33m"

/// Info color (blue) for terminal output
const color_info = "\u{001b}[34m"

/// Error color (red) for terminal output
const color_error = "\u{001b}[31m"

/// Default color for terminal output
const color_reset = "\u{001b}[0m"

// ------------------------------------------------------------- Public Functions

/// Green text signals success in terminal conventions. Wrapping
/// the ANSI codes here ensures the reset is always applied so
/// subsequent output doesn't stay green.
///
/// *Example*
///
/// ```gleam
/// io.println(console.success("This message is green"))
/// io.println("Hello, " <> console.success("Gleam users!"))
/// ```
///
pub fn success(output: String) -> String {
  color_success <> output <> color_reset
}

/// Yellow text signals warnings or headings in terminal
/// conventions. Same reset-safety as success.
///
/// *Example*
///
/// ```gleam
/// io.println(console.warning("This message is yellow"))
/// io.println("Hello, " <> console.warning("Gleam users!"))
/// ```
///
pub fn warning(output: String) -> String {
  color_warning <> output <> color_reset
}

/// Red text signals errors in terminal conventions. Same
/// reset-safety as success.
///
/// *Example*
///
/// ```gleam
/// io.println(console.error("This message is red"))
/// io.println("Hello, " <> console.error("Gleam users!"))
/// ```
///
pub fn error(output: String) -> String {
  color_error <> output <> color_reset
}

/// Blue text signals informational output in terminal
/// conventions. Same reset-safety as success.
///
/// *Example*
///
/// ```gleam
/// io.println(console.info("This message is blue"))
/// io.println("Hello, " <> console.info("Gleam users!"))
/// ```
///
pub fn info(output: String) -> String {
  color_info <> output <> color_reset
}

/// Starting with an empty Output lets callers build up lines
/// incrementally via pipes. This is cleaner than calling
/// io.println multiple times because the output is buffered
/// and flushed together.
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

/// Appending preserves insertion order so lines print in the
/// sequence they were added. No color is applied — use the
/// colored variants for styled output.
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

/// Visual spacing between sections makes dense command output
/// easier to scan. Accepting an amount lets callers add
/// multiple blank lines without chaining repeated calls.
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

/// Shorthand that avoids nesting success() inside line() at
/// every call site. Keeps pipe chains readable when mixing
/// colored and plain lines.
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

/// Shorthand that avoids nesting error() inside line() at
/// every call site. Red-colored errors stand out immediately
/// in terminal output.
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

/// Shorthand that avoids nesting warning() inside line() at
/// every call site. Yellow warnings signal non-fatal issues
/// that may need attention.
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

/// Shorthand that avoids nesting info() inside line() at
/// every call site. Blue info lines distinguish progress
/// messages from results.
///
/// *Example*
///
/// ```gleam
/// console.output()
/// |> console.line_info("Running migrations...")
/// |> console.print()
/// ```
///
pub fn line_info(output: Output, message: String) -> Output {
  line(output, info(message))
}

/// Flushing all buffered lines at once keeps the output
/// grouped in the terminal. This is the terminal step of the
/// Output builder chain.
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

/// Gleam doesn't have for-loops, so recursion is the standard
/// way to iterate a list. Separated from print so the public
/// API stays clean — callers just call print() without seeing
/// the recursion.
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
