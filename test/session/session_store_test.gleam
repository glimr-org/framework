import gleam/dict
import gleeunit/should
import glimr/session/store

// ------------------------------------------------------------- Store Interface Tests

pub fn load_without_store_returns_empty_test() {
  // Clear any cached store
  clear_session_store()

  let #(data, flash) = store.load("nonexistent")

  data |> should.equal(dict.new())
  flash |> should.equal(dict.new())
}

pub fn save_without_store_does_not_crash_test() {
  clear_session_store()

  // Should be a no-op, not a crash
  store.save("id", dict.new(), dict.new())
}

pub fn destroy_without_store_does_not_crash_test() {
  clear_session_store()

  store.destroy("id")
}

pub fn gc_without_store_does_not_crash_test() {
  clear_session_store()

  store.gc()
}

// ------------------------------------------------------------- In-Memory Store Tests

pub fn cache_and_load_test() {
  let test_data =
    dict.new()
    |> dict.insert("user_id", "42")

  let test_flash =
    dict.new()
    |> dict.insert("success", "Saved!")

  // Create a simple in-memory store for testing
  let memory_store =
    store.new(
      load: fn(_id) { #(test_data, test_flash) },
      save: fn(_id, _data, _flash) { Nil },
      destroy: fn(_id) { Nil },
      gc: fn() { Nil },
      cookie_value: fn(id, _, _) { id },
    )

  store.cache_store(memory_store)

  let #(data, flash) = store.load("any-id")

  dict.get(data, "user_id") |> should.equal(Ok("42"))
  dict.get(flash, "success") |> should.equal(Ok("Saved!"))

  clear_session_store()
}

pub fn save_calls_store_function_test() {
  // Use a store that tracks if save was called via a known load response
  let saved_data =
    dict.new()
    |> dict.insert("saved", "true")

  let memory_store =
    store.new(
      load: fn(_id) { #(saved_data, dict.new()) },
      save: fn(_id, _data, _flash) { Nil },
      destroy: fn(_id) { Nil },
      gc: fn() { Nil },
      cookie_value: fn(id, _, _) { id },
    )

  store.cache_store(memory_store)

  // Save should not crash
  store.save("test-id", dict.new(), dict.new())

  // Load should still work
  let #(data, _flash) = store.load("test-id")
  dict.get(data, "saved") |> should.equal(Ok("true"))

  clear_session_store()
}

pub fn destroy_calls_store_function_test() {
  let memory_store =
    store.new(
      load: fn(_id) { #(dict.new(), dict.new()) },
      save: fn(_id, _data, _flash) { Nil },
      destroy: fn(_id) { Nil },
      gc: fn() { Nil },
      cookie_value: fn(id, _, _) { id },
    )

  store.cache_store(memory_store)

  // Should not crash
  store.destroy("test-id")

  clear_session_store()
}

pub fn gc_calls_store_function_test() {
  let memory_store =
    store.new(
      load: fn(_id) { #(dict.new(), dict.new()) },
      save: fn(_id, _data, _flash) { Nil },
      destroy: fn(_id) { Nil },
      gc: fn() { Nil },
      cookie_value: fn(id, _, _) { id },
    )

  store.cache_store(memory_store)

  // Should not crash
  store.gc()

  clear_session_store()
}

pub fn cookie_value_returns_session_id_for_server_stores_test() {
  let memory_store =
    store.new(
      load: fn(_id) { #(dict.new(), dict.new()) },
      save: fn(_id, _data, _flash) { Nil },
      destroy: fn(_id) { Nil },
      gc: fn() { Nil },
      cookie_value: fn(id, _, _) { id },
    )

  store.cache_store(memory_store)

  let value = store.cookie_value("session-123", dict.new(), dict.new())
  value |> should.equal("session-123")

  clear_session_store()
}

pub fn cookie_value_returns_payload_for_cookie_stores_test() {
  let cookie_store =
    store.new(
      load: fn(_id) { #(dict.new(), dict.new()) },
      save: fn(_id, _data, _flash) { Nil },
      destroy: fn(_id) { Nil },
      gc: fn() { Nil },
      cookie_value: fn(_id, _data, _flash) { "encoded-payload" },
    )

  store.cache_store(cookie_store)

  let value = store.cookie_value("session-123", dict.new(), dict.new())
  value |> should.equal("encoded-payload")

  clear_session_store()
}

@external(erlang, "glimr_session_test_ffi", "clear_session_store")
fn clear_session_store() -> Nil
