import gleam/dict
import gleeunit/should
import glimr/auth/auth
import glimr/config/auth as auth_config
import glimr/session/session
import simplifile

const config_dir = "config"

const config_file = "config/auth.toml"

fn setup() -> Nil {
  let _ = simplifile.create_directory_all(config_dir)
  let _ =
    simplifile.write(
      config_file,
      "[auth]
  login_redirect = \"/login\"
  session_key = \"_auth_user_id\"
",
    )
  clear_auth_config()
  Nil
}

fn cleanup() -> Nil {
  let _ = simplifile.delete(config_file)
  clear_auth_config()
  Nil
}

// ------------------------------------------------------------- Login

pub fn login_stores_user_id_in_session_test() {
  setup()
  let s = session.start("sess-auth-1", dict.new(), dict.new())

  auth.login(s, "42")

  auth.id(s) |> should.equal(Ok("42"))

  session.stop(s)
  cleanup()
}

pub fn login_regenerates_session_id_test() {
  setup()
  let s = session.start("original-id", dict.new(), dict.new())

  auth.login(s, "42")

  let new_id = session.id(s)
  should.not_equal(new_id, "original-id")
  should.not_equal(new_id, "")

  session.stop(s)
  cleanup()
}

// ------------------------------------------------------------- Logout

pub fn logout_clears_session_test() {
  setup()
  let s = session.start("sess-auth-2", dict.new(), dict.new())

  auth.login(s, "42")
  auth.logout(s)

  auth.check(s) |> should.equal(False)
  auth.id(s) |> should.be_error

  session.stop(s)
  cleanup()
}

pub fn logout_invalidates_session_test() {
  setup()
  let s = session.start("sess-auth-3", dict.new(), dict.new())

  auth.login(s, "42")
  session.put(s, "other_data", "value")
  auth.logout(s)

  // All session data should be gone
  session.get(s, "other_data") |> should.be_error
  session.all(s) |> should.equal(dict.new())

  session.stop(s)
  cleanup()
}

// ------------------------------------------------------------- Check

pub fn check_returns_false_when_not_logged_in_test() {
  setup()
  let s = session.start("sess-auth-4", dict.new(), dict.new())

  auth.check(s) |> should.equal(False)

  session.stop(s)
  cleanup()
}

pub fn check_returns_true_when_logged_in_test() {
  setup()
  let s = session.start("sess-auth-5", dict.new(), dict.new())

  auth.login(s, "42")

  auth.check(s) |> should.equal(True)

  session.stop(s)
  cleanup()
}

// ------------------------------------------------------------- Id

pub fn id_returns_error_when_not_logged_in_test() {
  setup()
  let s = session.start("sess-auth-6", dict.new(), dict.new())

  auth.id(s) |> should.be_error

  session.stop(s)
  cleanup()
}

pub fn id_returns_user_id_when_logged_in_test() {
  setup()
  let s = session.start("sess-auth-7", dict.new(), dict.new())

  auth.login(s, "99")

  auth.id(s) |> should.equal(Ok("99"))

  session.stop(s)
  cleanup()
}

// ------------------------------------------------------------- Empty Session

pub fn check_on_empty_session_returns_false_test() {
  setup()
  let s = session.empty()

  auth.check(s) |> should.equal(False)

  cleanup()
}

pub fn id_on_empty_session_returns_error_test() {
  setup()
  let s = session.empty()

  auth.id(s) |> should.be_error

  cleanup()
}

pub fn login_on_empty_session_does_not_crash_test() {
  setup()
  let s = session.empty()

  auth.login(s, "42")

  cleanup()
}

pub fn logout_on_empty_session_does_not_crash_test() {
  setup()
  let s = session.empty()

  auth.logout(s)

  cleanup()
}

// ------------------------------------------------------------- Custom Session Key

pub fn custom_session_key_test() {
  let _ = simplifile.create_directory_all(config_dir)
  let _ =
    simplifile.write(
      config_file,
      "[auth]
  session_key = \"_custom_user\"
",
    )
  clear_auth_config()

  let s = session.start("sess-auth-8", dict.new(), dict.new())

  auth.login(s, "123")

  // Should be stored under the custom key
  session.get(s, "_custom_user") |> should.equal(Ok("123"))

  // Auth functions should still work
  auth.check(s) |> should.equal(True)
  auth.id(s) |> should.equal(Ok("123"))

  session.stop(s)
  cleanup()
}

// ------------------------------------------------------------- Preloaded Session

pub fn check_with_preloaded_auth_data_test() {
  setup()
  let config = auth_config.load()
  let data =
    dict.new()
    |> dict.insert(config.session_key, "77")

  let s = session.start("sess-auth-9", data, dict.new())

  auth.check(s) |> should.equal(True)
  auth.id(s) |> should.equal(Ok("77"))

  session.stop(s)
  cleanup()
}

// ------------------------------------------------------------- FFI Helpers

@external(erlang, "glimr_auth_test_ffi", "clear_auth_config")
fn clear_auth_config() -> Nil
