import gleeunit/should
import glimr/console/console

// ------------------------------------------------------------- Color Functions

pub fn success_returns_green_text_test() {
  console.success("test")
  |> should.equal("\u{001b}[32mtest\u{001b}[0m")
}

pub fn warning_returns_yellow_text_test() {
  console.warning("test")
  |> should.equal("\u{001b}[33mtest\u{001b}[0m")
}

pub fn error_returns_red_text_test() {
  console.error("test")
  |> should.equal("\u{001b}[31mtest\u{001b}[0m")
}

pub fn success_with_empty_string_test() {
  console.success("")
  |> should.equal("\u{001b}[32m\u{001b}[0m")
}

// ------------------------------------------------------------- Output Builder

pub fn line_adds_message_to_output_test() {
  let output =
    console.output()
    |> console.line("Hello")

  output.lines
  |> should.equal(["Hello"])
}

pub fn line_adds_multiple_messages_in_order_test() {
  let output =
    console.output()
    |> console.line("First")
    |> console.line("Second")
    |> console.line("Third")

  output.lines
  |> should.equal(["First", "Second", "Third"])
}

pub fn blank_line_adds_empty_string_test() {
  let output =
    console.output()
    |> console.line("Before")
    |> console.blank_line(1)
    |> console.line("After")

  output.lines
  |> should.equal(["Before", "", "After"])
}

pub fn line_success_adds_green_message_test() {
  let output =
    console.output()
    |> console.line_success("Success!")

  output.lines
  |> should.equal(["\u{001b}[32mSuccess!\u{001b}[0m"])
}

pub fn line_error_adds_red_message_test() {
  let output =
    console.output()
    |> console.line_error("Error!")

  output.lines
  |> should.equal(["\u{001b}[31mError!\u{001b}[0m"])
}

pub fn line_warning_adds_yellow_message_test() {
  let output =
    console.output()
    |> console.line_warning("Warning!")

  output.lines
  |> should.equal(["\u{001b}[33mWarning!\u{001b}[0m"])
}

pub fn mixed_line_types_test() {
  let output =
    console.output()
    |> console.line("Normal")
    |> console.line_success("Good")
    |> console.line_warning("Caution")
    |> console.line_error("Bad")

  output.lines
  |> should.equal([
    "Normal",
    "\u{001b}[32mGood\u{001b}[0m",
    "\u{001b}[33mCaution\u{001b}[0m",
    "\u{001b}[31mBad\u{001b}[0m",
  ])
}
