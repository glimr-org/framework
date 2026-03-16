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
