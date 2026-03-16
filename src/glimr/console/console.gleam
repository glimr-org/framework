//// Console Output Helpers
////
//// Raw ANSI escape codes scattered across command handlers are
//// hard to read and easy to get wrong (missing resets leave
//// the whole terminal colored). These helpers wrap the codes
//// so callers just pick a semantic level like success or
//// error, and each line function prints immediately.

import gleam/io

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

/// Most console output is a single line with no color. Having a
/// dedicated function for this keeps callers from importing
/// gleam/io directly, so all output flows through one module
/// and can be intercepted or styled later.
///
/// *Example*
///
/// ```gleam
/// console.line("Processing files...")
/// ```
///
pub fn line(message: String) -> Nil {
  io.println(message)
}

/// Wraps and prints in one call so callers don't have to nest
/// `io.println(console.success(...))` every time they want a
/// green line. Most command output is single-line success
/// messages, so this saves a lot of boilerplate.
///
/// *Example*
///
/// ```gleam
/// console.line_success("Created: src/app/example.gleam")
/// ```
///
pub fn line_success(message: String) -> Nil {
  io.println(success(message))
}

/// Error messages need to stand out immediately. Combining the
/// red ANSI wrap and println into one call makes the intent
/// obvious at the call site and keeps error output visually
/// consistent across all commands.
///
/// *Example*
///
/// ```gleam
/// console.line_error("Failed to write file")
/// ```
///
pub fn line_error(message: String) -> Nil {
  io.println(error(message))
}

/// Warnings aren't fatal but developers need to notice them.
/// Yellow stands out without the urgency of red, so it's
/// perfect for "this worked but you should know about this"
/// situations like skipping an existing file.
///
/// *Example*
///
/// ```gleam
/// console.line_warning("File already exists, skipping")
/// ```
///
pub fn line_warning(message: String) -> Nil {
  io.println(warning(message))
}

/// Blue sits between "everything's fine" (no color) and "pay
/// attention" (yellow). Good for progress updates like "Running
/// migrations..." where the developer wants to know what's
/// happening without alarm.
///
/// *Example*
///
/// ```gleam
/// console.line_info("Running migrations...")
/// ```
///
pub fn line_info(message: String) -> Nil {
  io.println(info(message))
}

/// Visual spacing between sections of command output makes
/// longer outputs scannable. Passing a count avoids chaining
/// multiple `io.println("")` calls when you need more than one
/// blank line of breathing room.
///
/// *Example*
///
/// ```gleam
/// console.new_line(2)
/// ```
///
pub fn new_line(amount: Int) -> Nil {
  case amount > 0 {
    True -> {
      io.println("")
      new_line(amount - 1)
    }
    False -> Nil
  }
}

/// Immediately terminates the BEAM process with the given exit
/// code. Use this to signal success (0) or failure (non-zero)
/// to the calling shell or parent process.
///
pub fn halt(code: Int) -> Nil {
  do_halt(code)
}

// ------------------------------------------------------------- FFI Bindings

/// FFI binding to Erlang's halt/1 which forces an immediate
/// shutdown of the runtime. Bypasses normal cleanup so it
/// should only be used for final exits.
///
@external(erlang, "erlang", "halt")
fn do_halt(code: Int) -> Nil
