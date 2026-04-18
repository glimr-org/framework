import gleeunit/should
import glimr/config
import simplifile

const config_dir = "config"

const config_file = "config/session.toml"

fn setup_config(content: String) -> Nil {
  let _ = simplifile.create_directory_all(config_dir)
  let _ = simplifile.write(config_file, content)
  // Clear persistent_term cache so load() re-reads the file
  clear_config_cache()
  Nil
}

fn cleanup_config() -> Nil {
  let _ = simplifile.delete(config_file)
  let _ = simplifile.delete(config_dir)
  clear_config_cache()
  Nil
}

@external(erlang, "glimr_session_test_ffi", "clear_config_cache")
fn clear_config_cache() -> Nil

// ------------------------------------------------------------- Default Config Tests

pub fn returns_errors_when_no_file_test() {
  cleanup_config()

  config.load()

  config.get_string_or("session.table") |> should.be_error()
  config.get_string_or("session.cookie") |> should.be_error()
  config.get_int_or("session.lifetime") |> should.be_error()
  config.get_bool_or("session.expire_on_close") |> should.be_error()
}

// ------------------------------------------------------------- Custom Config Tests

pub fn load_custom_config_test() {
  setup_config(
    "table = \"user_sessions\"
cookie = \"my_app_session\"
lifetime = 60
expire_on_close = true
",
  )

  config.load()

  config.get_string("session.table")
  |> should.equal("user_sessions")
  config.get_string("session.cookie")
  |> should.equal("my_app_session")
  config.get_int("session.lifetime") |> should.equal(60)
  config.get_bool("session.expire_on_close") |> should.equal(True)

  cleanup_config()
}

pub fn load_partial_config_test() {
  setup_config("cookie = \"custom_cookie\"\n")

  config.load()

  config.get_string_or("session.table") |> should.be_error()
  config.get_string("session.cookie")
  |> should.equal("custom_cookie")
  config.get_int_or("session.lifetime") |> should.be_error()
  config.get_bool_or("session.expire_on_close") |> should.be_error()

  cleanup_config()
}

// ------------------------------------------------------------- Invalid Config Tests

pub fn load_invalid_toml_returns_errors_test() {
  setup_config("this is not valid toml {{{}}")

  config.load()

  config.get_string_or("session.table") |> should.be_error()
  config.get_string_or("session.cookie") |> should.be_error()
  config.get_int_or("session.lifetime") |> should.be_error()
  config.get_bool_or("session.expire_on_close") |> should.be_error()

  cleanup_config()
}

pub fn load_empty_file_returns_errors_test() {
  setup_config("")

  config.load()

  config.get_string_or("session.table") |> should.be_error()
  config.get_string_or("session.cookie") |> should.be_error()

  cleanup_config()
}
