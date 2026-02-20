import gleam/http
import gleam/list
import gleam/result
import gleam/string
import gleeunit/should
import glimr/session/cookie_store
import glimr/session/session
import glimr/session/store
import simplifile
import wisp
import wisp/simulate

const config_dir = "config"

const session_config_file = "config/session.toml"

fn setup(expire_on_close: Bool) -> Nil {
  let _ = simplifile.create_directory_all(config_dir)

  let expire_str = case expire_on_close {
    True -> "true"
    False -> "false"
  }

  let _ = simplifile.write(session_config_file, "[session]
  table = \"sessions\"
  cookie = \"test_session\"
  lifetime = 120
  expire_on_close = " <> expire_str <> "
")

  clear_session_config()

  // Use cookie store — simplest, no pool needed
  let s = cookie_store.create()
  store.cache_store(s)

  Nil
}

fn cleanup() -> Nil {
  let _ = simplifile.delete(session_config_file)
  clear_session_config()
  clear_session_store()
  Nil
}

fn test_request() -> wisp.Request {
  simulate.request(http.Get, "/test")
}

// ------------------------------------------------------------- expire_on_close Tests

pub fn expire_on_close_omits_max_age_test() {
  setup(True)

  let resp = session.load(test_request(), fn(_req, _session) { wisp.ok() })

  // Find the Set-Cookie header for our session
  let cookie_header = find_set_cookie(resp.headers, "test_session")
  cookie_header |> should.be_ok

  let assert Ok(header_value) = cookie_header

  // Should NOT contain Max-Age when expire_on_close is true
  string.contains(header_value, "Max-Age")
  |> should.equal(False)

  cleanup()
}

pub fn expire_on_close_false_includes_max_age_test() {
  setup(False)

  let resp = session.load(test_request(), fn(_req, _session) { wisp.ok() })

  // Find the Set-Cookie header for our session
  let cookie_header = find_set_cookie(resp.headers, "test_session")
  cookie_header |> should.be_ok

  let assert Ok(header_value) = cookie_header

  // Should contain Max-Age=7200 (120 minutes * 60 seconds)
  string.contains(header_value, "Max-Age=7200")
  |> should.equal(True)

  cleanup()
}

pub fn session_cookie_is_set_on_response_test() {
  setup(False)

  let resp =
    session.load(test_request(), fn(_req, sess) {
      // Write something so the session is dirty and gets saved
      session.put(sess, "user", "test")
      wisp.ok()
    })

  // Should have a Set-Cookie header for our session
  let cookie_header = find_set_cookie(resp.headers, "test_session")
  cookie_header |> should.be_ok

  cleanup()
}

// ------------------------------------------------------------- Helpers

fn find_set_cookie(
  headers: List(#(String, String)),
  cookie_name: String,
) -> Result(String, Nil) {
  headers
  |> list.filter(fn(h) { h.0 == "set-cookie" })
  |> list.find(fn(h) { string.starts_with(h.1, cookie_name <> "=") })
  |> result.map(fn(h) { h.1 })
}

// ------------------------------------------------------------- FFI Helpers

@external(erlang, "glimr_session_test_ffi", "clear_session_config")
fn clear_session_config() -> Nil

@external(erlang, "glimr_session_test_ffi", "clear_session_store")
fn clear_session_store() -> Nil
