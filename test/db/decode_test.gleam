import gleam/dynamic
import gleam/dynamic/decode
import gleeunit/should
import glimr/db/decode as glimr_decode

// ------------------------------------------------------------- list_of: Native Lists (Postgres path)

pub fn list_of_native_string_list_test() {
  let data = dynamic.list([dynamic.string("hello"), dynamic.string("world")])
  let decoder = glimr_decode.list_of(decode.string)

  decode.run(data, decoder)
  |> should.equal(Ok(["hello", "world"]))
}

pub fn list_of_native_int_list_test() {
  let data = dynamic.list([dynamic.int(1), dynamic.int(2), dynamic.int(3)])
  let decoder = glimr_decode.list_of(decode.int)

  decode.run(data, decoder)
  |> should.equal(Ok([1, 2, 3]))
}

pub fn list_of_native_float_list_test() {
  let data = dynamic.list([dynamic.float(1.5), dynamic.float(2.5)])
  let decoder = glimr_decode.list_of(decode.float)

  decode.run(data, decoder)
  |> should.equal(Ok([1.5, 2.5]))
}

pub fn list_of_native_empty_list_test() {
  let data = dynamic.list([])
  let decoder = glimr_decode.list_of(decode.string)

  decode.run(data, decoder)
  |> should.equal(Ok([]))
}

// ------------------------------------------------------------- list_of: JSON String Fallback (SQLite path)

pub fn list_of_json_string_list_test() {
  let data = dynamic.string("[\"hello\",\"world\"]")
  let decoder = glimr_decode.list_of(decode.string)

  decode.run(data, decoder)
  |> should.equal(Ok(["hello", "world"]))
}

pub fn list_of_json_int_list_test() {
  let data = dynamic.string("[1,2,3]")
  let decoder = glimr_decode.list_of(decode.int)

  decode.run(data, decoder)
  |> should.equal(Ok([1, 2, 3]))
}

pub fn list_of_json_float_list_test() {
  let data = dynamic.string("[1.5,2.5]")
  let decoder = glimr_decode.list_of(decode.float)

  decode.run(data, decoder)
  |> should.equal(Ok([1.5, 2.5]))
}

pub fn list_of_json_empty_array_test() {
  let data = dynamic.string("[]")
  let decoder = glimr_decode.list_of(decode.string)

  decode.run(data, decoder)
  |> should.equal(Ok([]))
}

pub fn list_of_json_single_element_test() {
  let data = dynamic.string("[\"only\"]")
  let decoder = glimr_decode.list_of(decode.string)

  decode.run(data, decoder)
  |> should.equal(Ok(["only"]))
}

// ------------------------------------------------------------- list_of: Edge Cases

pub fn list_of_invalid_json_returns_empty_test() {
  let data = dynamic.string("not valid json")
  let decoder = glimr_decode.list_of(decode.string)

  decode.run(data, decoder)
  |> should.equal(Ok([]))
}

pub fn list_of_json_with_spaces_test() {
  let data = dynamic.string("[  \"a\" , \"b\"  ]")
  let decoder = glimr_decode.list_of(decode.string)

  decode.run(data, decoder)
  |> should.equal(Ok(["a", "b"]))
}

// ------------------------------------------------------------- list_of: Nested (for Array(Array(Int)))

pub fn list_of_nested_native_test() {
  let data =
    dynamic.list([
      dynamic.list([dynamic.int(1), dynamic.int(2)]),
      dynamic.list([dynamic.int(3), dynamic.int(4)]),
    ])
  let decoder = glimr_decode.list_of(glimr_decode.list_of(decode.int))

  decode.run(data, decoder)
  |> should.equal(Ok([[1, 2], [3, 4]]))
}

pub fn list_of_nested_json_test() {
  let data = dynamic.string("[[1,2],[3,4]]")
  let decoder = glimr_decode.list_of(glimr_decode.list_of(decode.int))

  decode.run(data, decoder)
  |> should.equal(Ok([[1, 2], [3, 4]]))
}

pub fn list_of_nested_empty_test() {
  let data = dynamic.string("[[],[]]")
  let decoder = glimr_decode.list_of(glimr_decode.list_of(decode.int))

  decode.run(data, decoder)
  |> should.equal(Ok([[], []]))
}

// ------------------------------------------------------------- bool decoder (existing, verify still works)

pub fn bool_true_test() {
  let data = dynamic.bool(True)
  let decoder = glimr_decode.bool()

  decode.run(data, decoder)
  |> should.equal(Ok(True))
}

pub fn bool_false_test() {
  let data = dynamic.bool(False)
  let decoder = glimr_decode.bool()

  decode.run(data, decoder)
  |> should.equal(Ok(False))
}

pub fn bool_int_1_test() {
  let data = dynamic.int(1)
  let decoder = glimr_decode.bool()

  decode.run(data, decoder)
  |> should.equal(Ok(True))
}

pub fn bool_int_0_test() {
  let data = dynamic.int(0)
  let decoder = glimr_decode.bool()

  decode.run(data, decoder)
  |> should.equal(Ok(False))
}
