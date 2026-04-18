import gleeunit/should
import glimr/config
import simplifile

const config_dir = "config"

const config_file = "config/auth.toml"

fn setup(content: String) -> Nil {
  let _ = simplifile.create_directory_all(config_dir)
  let _ = simplifile.write(config_file, content)
  clear_config_cache()
  Nil
}

fn cleanup() -> Nil {
  let _ = simplifile.delete(config_file)
  let _ = simplifile.delete(config_dir)
  clear_config_cache()
  Nil
}

// ------------------------------------------------------------- Default Config

pub fn returns_error_when_no_file_test() {
  clear_config_cache()
  let _ = simplifile.delete(config_file)

  config.load()

  config.get_string_or("auth.session_key")
  |> should.be_error()

  cleanup()
}

// ------------------------------------------------------------- Custom Config

pub fn parses_custom_config_test() {
  setup("session_key = \"_my_user_id\"\n")

  config.load()

  config.get_string("auth.session_key")
  |> should.equal("_my_user_id")

  cleanup()
}

pub fn invalid_toml_returns_error_test() {
  setup("not valid toml {{{}}")

  config.load()

  config.get_string_or("auth.session_key")
  |> should.be_error()

  cleanup()
}

pub fn empty_file_returns_error_test() {
  setup("")

  config.load()

  config.get_string_or("auth.session_key")
  |> should.be_error()

  cleanup()
}

// ------------------------------------------------------------- FFI Helpers

@external(erlang, "glimr_auth_test_ffi", "clear_config_cache")
fn clear_config_cache() -> Nil
