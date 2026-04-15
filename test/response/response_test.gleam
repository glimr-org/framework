import gleam/json
import gleam/list
import gleeunit/should
import glimr/http/response
import wisp

// ------------------------------------------------------------- html tests

pub fn html_test() {
  let res = response.html("<h1>Hello</h1>", 200)

  res.status
  |> should.equal(200)

  res.body
  |> should.equal(wisp.Text("<h1>Hello</h1>"))
}

pub fn html_with_status_201_test() {
  let res = response.html("<h1>Created</h1>", 201)

  res.status
  |> should.equal(201)
}

pub fn html_with_empty_content_test() {
  let res = response.html("", 200)

  res.status
  |> should.equal(200)

  res.body
  |> should.equal(wisp.Text(""))
}

pub fn html_with_error_status_test() {
  let res = response.html("<h1>Not Found</h1>", 404)

  res.status
  |> should.equal(404)

  res.body
  |> should.equal(wisp.Text("<h1>Not Found</h1>"))
}

pub fn html_with_server_error_status_test() {
  let res = response.html("<h1>Server Error</h1>", 500)

  res.status
  |> should.equal(500)
}

// ------------------------------------------------------------- json tests

pub fn json_string_test() {
  let res =
    json.string("hello")
    |> response.json(200)

  res.status
  |> should.equal(200)

  res.body
  |> should.equal(wisp.Text("\"hello\""))
}

pub fn json_object_test() {
  let res =
    json.object([#("name", json.string("John")), #("age", json.int(30))])
    |> response.json(200)

  res.status
  |> should.equal(200)

  res.body
  |> should.equal(wisp.Text("{\"name\":\"John\",\"age\":30}"))
}

pub fn json_array_test() {
  let res =
    json.array([1, 2, 3], json.int)
    |> response.json(200)

  res.status
  |> should.equal(200)

  res.body
  |> should.equal(wisp.Text("[1,2,3]"))
}

pub fn json_with_status_201_test() {
  let res =
    json.object([#("id", json.int(1))])
    |> response.json(201)

  res.status
  |> should.equal(201)
}

pub fn json_with_error_status_test() {
  let res =
    json.object([#("error", json.string("Not found"))])
    |> response.json(404)

  res.status
  |> should.equal(404)
}

pub fn json_nested_object_test() {
  let res =
    json.object([
      #("user", json.object([#("name", json.string("John"))])),
      #("active", json.bool(True)),
    ])
    |> response.json(200)

  res.status
  |> should.equal(200)

  res.body
  |> should.equal(wisp.Text("{\"user\":{\"name\":\"John\"},\"active\":true}"))
}

pub fn json_null_test() {
  let res =
    json.null()
    |> response.json(200)

  res.status
  |> should.equal(200)

  res.body
  |> should.equal(wisp.Text("null"))
}

// ------------------------------------------------------------- header tests

pub fn header_single_test() {
  let res =
    response.html("<h1>Hello</h1>", 200)
    |> response.header("x-custom-header", "custom-value")

  res.headers
  |> list.contains(#("x-custom-header", "custom-value"))
  |> should.be_true()
}

pub fn header_multiple_test() {
  let res =
    response.html("<h1>Hello</h1>", 200)
    |> response.header("x-header-one", "value1")
    |> response.header("x-header-two", "value2")

  res.headers
  |> list.contains(#("x-header-one", "value1"))
  |> should.be_true()

  res.headers
  |> list.contains(#("x-header-two", "value2"))
  |> should.be_true()
}

pub fn header_override_content_type_test() {
  let res =
    response.html("\"This is actually json\"", 200)
    |> response.header("content-type", "application/json")

  res.headers
  |> list.contains(#("content-type", "application/json"))
  |> should.be_true()
}

pub fn header_on_json_response_test() {
  let res =
    json.string("data")
    |> response.json(200)
    |> response.header("x-request-id", "abc123")

  res.headers
  |> list.contains(#("x-request-id", "abc123"))
  |> should.be_true()
}

pub fn header_preserves_status_test() {
  let res =
    response.html("<h1>Created</h1>", 201)
    |> response.header("x-custom", "value")

  res.status
  |> should.equal(201)
}

pub fn header_preserves_body_test() {
  let res =
    response.html("<h1>Hello</h1>", 200)
    |> response.header("x-custom", "value")

  res.body
  |> should.equal(wisp.Text("<h1>Hello</h1>"))
}
