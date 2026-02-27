import gleeunit/should
import glimr/http/fail

@external(erlang, "erlang", "is_list")
fn is_list(value: a) -> Bool

pub fn fail_return_type_unifies_test() {
  // Use a runtime condition so the compiler can't 
  // eliminate the Error branch
  let input: Result(String, Nil) = case is_list("hello") {
    True -> Error(Nil)
    False -> Ok("hello")
  }

  case input {
    Ok(v) -> v
    Error(_) -> fail.with(404)
  }
  |> should.equal("hello")
}

pub fn rescue_catches_fail_test() {
  let result = fail.rescue(fn() { fail.with(404) })

  result |> should.equal(fail.Fail(404))
}

pub fn rescue_passes_through_ok_test() {
  let result = fail.rescue(fn() { "hello" })

  result |> should.equal(fail.Ok("hello"))
}

pub fn rescue_catches_different_status_codes_test() {
  fail.rescue(fn() { fail.with(403) })
  |> should.equal(fail.Fail(403))

  fail.rescue(fn() { fail.with(500) })
  |> should.equal(fail.Fail(500))

  fail.rescue(fn() { fail.with(422) })
  |> should.equal(fail.Fail(422))
}
