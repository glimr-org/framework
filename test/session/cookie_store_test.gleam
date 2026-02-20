import gleam/dict
import gleeunit/should
import glimr/session/cookie_store
import glimr/session/store

// ------------------------------------------------------------- Load Tests

pub fn load_empty_cookie_returns_empty_test() {
  setup_cookie_store()

  let #(data, flash) = store.load("")

  data |> should.equal(dict.new())
  flash |> should.equal(dict.new())

  cleanup()
}

pub fn load_invalid_json_returns_empty_test() {
  setup_cookie_store()

  let #(data, flash) = store.load("not valid json")

  data |> should.equal(dict.new())
  flash |> should.equal(dict.new())

  cleanup()
}

pub fn load_valid_payload_returns_data_and_flash_test() {
  setup_cookie_store()

  let payload =
    "{\"_data\":{\"user_id\":\"42\",\"role\":\"admin\"},\"_flash\":{\"success\":\"Welcome!\"}}"

  let #(data, flash) = store.load(payload)

  dict.get(data, "user_id") |> should.equal(Ok("42"))
  dict.get(data, "role") |> should.equal(Ok("admin"))
  dict.get(flash, "success") |> should.equal(Ok("Welcome!"))

  cleanup()
}

// ------------------------------------------------------------- Save Tests

pub fn save_is_noop_test() {
  setup_cookie_store()

  // Should not crash — cookie store save is a no-op
  store.save("any-id", dict.new(), dict.new())

  cleanup()
}

// ------------------------------------------------------------- Destroy Tests

pub fn destroy_is_noop_test() {
  setup_cookie_store()

  // Should not crash — cookie store destroy is a no-op
  store.destroy("any-id")

  cleanup()
}

// ------------------------------------------------------------- GC Tests

pub fn gc_is_noop_test() {
  setup_cookie_store()

  // Should not crash — cookie store gc is a no-op
  store.gc()

  cleanup()
}

// ------------------------------------------------------------- Cookie Value Tests

pub fn cookie_value_returns_encoded_payload_test() {
  setup_cookie_store()

  let data =
    dict.new()
    |> dict.insert("user_id", "42")

  let flash =
    dict.new()
    |> dict.insert("msg", "Hello")

  let value = store.cookie_value("ignored-id", data, flash)

  // The cookie value should be a JSON payload, not the session ID
  should.not_equal(value, "ignored-id")

  // Verify the payload can be loaded back
  let #(loaded_data, loaded_flash) = store.load(value)

  dict.get(loaded_data, "user_id") |> should.equal(Ok("42"))
  dict.get(loaded_flash, "msg") |> should.equal(Ok("Hello"))

  cleanup()
}

pub fn cookie_value_roundtrip_test() {
  setup_cookie_store()

  let data =
    dict.new()
    |> dict.insert("key1", "value1")
    |> dict.insert("key2", "value2")

  let flash =
    dict.new()
    |> dict.insert("info", "Updated!")

  // Encode to cookie value
  let cookie = store.cookie_value("any-id", data, flash)

  // Decode back via load
  let #(loaded_data, loaded_flash) = store.load(cookie)

  dict.get(loaded_data, "key1") |> should.equal(Ok("value1"))
  dict.get(loaded_data, "key2") |> should.equal(Ok("value2"))
  dict.get(loaded_flash, "info") |> should.equal(Ok("Updated!"))

  cleanup()
}

// ------------------------------------------------------------- Helpers

fn setup_cookie_store() -> Nil {
  let session = cookie_store.create()
  store.cache_store(session)
  Nil
}

fn cleanup() -> Nil {
  clear_session_store()
}

@external(erlang, "glimr_session_test_ffi", "clear_session_store")
fn clear_session_store() -> Nil
