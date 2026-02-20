import gleam/dict
import gleeunit/should
import glimr/session/session

// ------------------------------------------------------------- Empty Session Tests

pub fn empty_get_returns_error_test() {
  let s = session.empty()

  session.get(s, "key")
  |> should.be_error
}

pub fn empty_has_returns_false_test() {
  let s = session.empty()

  session.has(s, "key")
  |> should.equal(False)
}

pub fn empty_all_returns_empty_dict_test() {
  let s = session.empty()

  session.all(s)
  |> should.equal(dict.new())
}

pub fn empty_id_returns_empty_string_test() {
  let s = session.empty()

  session.id(s)
  |> should.equal("")
}

pub fn empty_get_flash_returns_empty_string_test() {
  let s = session.empty()

  session.get_flash(s, "key")
  |> should.equal("")
}

pub fn empty_get_state_returns_error_test() {
  let s = session.empty()

  session.get_state(s)
  |> should.be_error
}

pub fn empty_put_does_not_crash_test() {
  let s = session.empty()
  session.put(s, "key", "value")

  // Should not crash, just no-op
  session.get(s, "key")
  |> should.be_error
}

pub fn empty_forget_does_not_crash_test() {
  let s = session.empty()
  session.forget(s, "key")
}

pub fn empty_invalidate_does_not_crash_test() {
  let s = session.empty()
  session.invalidate(s)
}

pub fn empty_regenerate_does_not_crash_test() {
  let s = session.empty()
  session.regenerate(s)
}

pub fn empty_stop_does_not_crash_test() {
  let s = session.empty()
  session.stop(s)
}

// ------------------------------------------------------------- Live Session - Basic Operations

pub fn start_and_get_id_test() {
  let s = session.start("test-id-123", dict.new(), dict.new())

  session.id(s)
  |> should.equal("test-id-123")

  session.stop(s)
}

pub fn put_and_get_test() {
  let s = session.start("sess-1", dict.new(), dict.new())

  session.put(s, "name", "Alice")

  session.get(s, "name")
  |> should.equal(Ok("Alice"))

  session.stop(s)
}

pub fn get_missing_key_returns_error_test() {
  let s = session.start("sess-2", dict.new(), dict.new())

  session.get(s, "missing")
  |> should.be_error

  session.stop(s)
}

pub fn put_overwrites_existing_test() {
  let s = session.start("sess-3", dict.new(), dict.new())

  session.put(s, "key", "first")
  session.put(s, "key", "second")

  session.get(s, "key")
  |> should.equal(Ok("second"))

  session.stop(s)
}

pub fn has_returns_true_for_existing_key_test() {
  let s = session.start("sess-4", dict.new(), dict.new())

  session.put(s, "exists", "yes")

  session.has(s, "exists")
  |> should.equal(True)

  session.stop(s)
}

pub fn has_returns_false_for_missing_key_test() {
  let s = session.start("sess-5", dict.new(), dict.new())

  session.has(s, "nope")
  |> should.equal(False)

  session.stop(s)
}

pub fn all_returns_all_data_test() {
  let s = session.start("sess-6", dict.new(), dict.new())

  session.put(s, "a", "1")
  session.put(s, "b", "2")

  let data = session.all(s)
  dict.get(data, "a") |> should.equal(Ok("1"))
  dict.get(data, "b") |> should.equal(Ok("2"))
  dict.size(data) |> should.equal(2)

  session.stop(s)
}

// ------------------------------------------------------------- Forget

pub fn forget_removes_key_test() {
  let s = session.start("sess-7", dict.new(), dict.new())

  session.put(s, "key", "value")
  session.forget(s, "key")

  session.get(s, "key")
  |> should.be_error

  session.has(s, "key")
  |> should.equal(False)

  session.stop(s)
}

pub fn forget_nonexistent_key_does_not_crash_test() {
  let s = session.start("sess-8", dict.new(), dict.new())

  session.forget(s, "nonexistent")

  session.stop(s)
}

// ------------------------------------------------------------- Initial Data

pub fn start_with_initial_data_test() {
  let data =
    dict.new()
    |> dict.insert("user_id", "42")
    |> dict.insert("role", "admin")

  let s = session.start("sess-9", data, dict.new())

  session.get(s, "user_id")
  |> should.equal(Ok("42"))

  session.get(s, "role")
  |> should.equal(Ok("admin"))

  session.stop(s)
}

// ------------------------------------------------------------- Flash Messages

pub fn flash_and_get_flash_test() {
  // Flash set during a request is NOT readable in the same request —
  // it's persisted and available on the *next* request via loaded_flash.
  let flash =
    dict.new()
    |> dict.insert("success", "Welcome back!")

  // Simulate next request: flash from store becomes loaded_flash
  let s = session.start("sess-10", dict.new(), flash)

  session.get_flash(s, "success")
  |> should.equal("Welcome back!")

  // New flash set this request should NOT appear in get_flash
  session.flash(s, "info", "New message")

  session.get_flash(s, "info")
  |> should.equal("")

  session.stop(s)
}

pub fn get_flash_missing_returns_empty_string_test() {
  let s = session.start("sess-11", dict.new(), dict.new())

  session.get_flash(s, "nonexistent")
  |> should.equal("")

  session.stop(s)
}

pub fn start_with_initial_flash_test() {
  let flash =
    dict.new()
    |> dict.insert("info", "Previous message")

  let s = session.start("sess-12", dict.new(), flash)

  session.get_flash(s, "info")
  |> should.equal("Previous message")

  session.stop(s)
}

// ------------------------------------------------------------- get_flash_or

pub fn get_flash_or_returns_ok_when_present_test() {
  let flash =
    dict.new()
    |> dict.insert("success", "Welcome back!")

  let s = session.start("sess-30", dict.new(), flash)

  session.get_flash_or(s, "success")
  |> should.equal(Ok("Welcome back!"))

  session.stop(s)
}

pub fn get_flash_or_returns_error_when_missing_test() {
  let s = session.start("sess-31", dict.new(), dict.new())

  session.get_flash_or(s, "nonexistent")
  |> should.be_error

  session.stop(s)
}

pub fn get_flash_or_returns_error_on_empty_session_test() {
  let s = session.empty()

  session.get_flash_or(s, "key")
  |> should.be_error
}

// ------------------------------------------------------------- has_flash

pub fn has_flash_returns_true_when_present_test() {
  let flash =
    dict.new()
    |> dict.insert("success", "Welcome!")

  let s = session.start("sess-33", dict.new(), flash)

  session.has_flash(s, "success")
  |> should.equal(True)

  session.stop(s)
}

pub fn has_flash_returns_false_when_missing_test() {
  let s = session.start("sess-34", dict.new(), dict.new())

  session.has_flash(s, "nonexistent")
  |> should.equal(False)

  session.stop(s)
}

pub fn has_flash_returns_false_on_empty_session_test() {
  let s = session.empty()

  session.has_flash(s, "key")
  |> should.equal(False)
}

pub fn has_flash_returns_false_for_current_request_flash_test() {
  let s = session.start("sess-35", dict.new(), dict.new())

  session.flash(s, "info", "New message")

  session.has_flash(s, "info")
  |> should.equal(False)

  session.stop(s)
}

pub fn get_flash_or_does_not_return_current_request_flash_test() {
  let s = session.start("sess-32", dict.new(), dict.new())

  session.flash(s, "info", "New message")

  session.get_flash_or(s, "info")
  |> should.be_error

  session.stop(s)
}

// ------------------------------------------------------------- Dirty Tracking

pub fn fresh_session_is_not_dirty_test() {
  let s = session.start("sess-13", dict.new(), dict.new())

  let assert Ok(state) = session.get_state(s)
  state.dirty |> should.equal(False)

  session.stop(s)
}

pub fn loaded_flash_makes_session_dirty_test() {
  let flash =
    dict.new()
    |> dict.insert("msg", "hello")

  let s = session.start("sess-36", dict.new(), flash)

  let assert Ok(state) = session.get_state(s)
  state.dirty |> should.equal(True)

  session.stop(s)
}

pub fn put_makes_session_dirty_test() {
  let s = session.start("sess-14", dict.new(), dict.new())

  session.put(s, "key", "value")

  let assert Ok(state) = session.get_state(s)
  state.dirty |> should.equal(True)

  session.stop(s)
}

pub fn forget_makes_session_dirty_test() {
  let s = session.start("sess-15", dict.new(), dict.new())

  session.forget(s, "key")

  let assert Ok(state) = session.get_state(s)
  state.dirty |> should.equal(True)

  session.stop(s)
}

pub fn flash_makes_session_dirty_test() {
  let s = session.start("sess-16", dict.new(), dict.new())

  session.flash(s, "msg", "hello")

  let assert Ok(state) = session.get_state(s)
  state.dirty |> should.equal(True)

  session.stop(s)
}

// ------------------------------------------------------------- Invalidate

pub fn invalidate_clears_all_data_test() {
  let s = session.start("sess-17", dict.new(), dict.new())

  session.put(s, "key", "value")
  session.flash(s, "msg", "hello")
  session.invalidate(s)

  session.all(s)
  |> should.equal(dict.new())

  session.get(s, "key")
  |> should.be_error

  session.get_flash(s, "msg")
  |> should.equal("")

  session.stop(s)
}

pub fn invalidate_changes_id_test() {
  let s = session.start("original-id", dict.new(), dict.new())

  session.invalidate(s)

  let new_id = session.id(s)
  should.not_equal(new_id, "original-id")
  should.not_equal(new_id, "")

  session.stop(s)
}

pub fn invalidate_marks_dirty_test() {
  let s = session.start("sess-18", dict.new(), dict.new())

  session.invalidate(s)

  let assert Ok(state) = session.get_state(s)
  state.dirty |> should.equal(True)

  session.stop(s)
}

pub fn invalidate_sets_invalidated_flag_test() {
  let s = session.start("sess-19", dict.new(), dict.new())

  session.invalidate(s)

  let assert Ok(state) = session.get_state(s)
  state.invalidated |> should.equal(True)

  session.stop(s)
}

// ------------------------------------------------------------- Regenerate

pub fn regenerate_changes_id_test() {
  let s = session.start("old-id", dict.new(), dict.new())

  session.regenerate(s)

  let new_id = session.id(s)
  should.not_equal(new_id, "old-id")
  should.not_equal(new_id, "")

  session.stop(s)
}

pub fn regenerate_keeps_data_test() {
  let data =
    dict.new()
    |> dict.insert("user_id", "42")

  let s = session.start("old-id", data, dict.new())

  session.regenerate(s)

  session.get(s, "user_id")
  |> should.equal(Ok("42"))

  session.stop(s)
}

pub fn regenerate_marks_dirty_test() {
  let s = session.start("sess-20", dict.new(), dict.new())

  session.regenerate(s)

  let assert Ok(state) = session.get_state(s)
  state.dirty |> should.equal(True)

  session.stop(s)
}

// ------------------------------------------------------------- Get State

pub fn get_state_returns_full_state_test() {
  let s = session.start("state-test", dict.new(), dict.new())

  session.put(s, "a", "1")
  session.flash(s, "msg", "hi")

  let assert Ok(state) = session.get_state(s)

  state.id |> should.equal("state-test")
  dict.get(state.data, "a") |> should.equal(Ok("1"))
  dict.get(state.flash, "msg") |> should.equal(Ok("hi"))
  state.dirty |> should.equal(True)
  state.invalidated |> should.equal(False)

  session.stop(s)
}

// ------------------------------------------------------------- Generate ID

pub fn generate_id_produces_hex_string_test() {
  let id = session.generate_id()

  // 32 bytes = 64 hex chars
  let assert 64 = string_length(id)
}

pub fn generate_id_is_unique_test() {
  let id1 = session.generate_id()
  let id2 = session.generate_id()

  should.not_equal(id1, id2)
}

fn string_length(s: String) -> Int {
  do_string_length(s)
}

@external(erlang, "string", "length")
fn do_string_length(s: String) -> Int
