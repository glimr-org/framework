import gleeunit/should
import glimr/config/auth as auth_config
import simplifile

const config_dir = "config"

const config_file = "config/auth.toml"

fn setup(content: String) -> Nil {
  let _ = simplifile.create_directory_all(config_dir)
  let _ = simplifile.write(config_file, content)
  clear_auth_config()
  Nil
}

fn cleanup() -> Nil {
  let _ = simplifile.delete(config_file)
  clear_auth_config()
  Nil
}

// ------------------------------------------------------------- Default Config

pub fn defaults_when_no_file_test() {
  clear_auth_config()
  let _ = simplifile.delete(config_file)

  let config = auth_config.load()

  config.session_key |> should.equal("_auth_user_id")

  cleanup()
}

// ------------------------------------------------------------- Custom Config

pub fn parses_custom_config_test() {
  setup(
    "[auth]
  session_key = \"_my_user_id\"
",
  )

  let config = auth_config.load()

  config.session_key |> should.equal("_my_user_id")

  cleanup()
}

pub fn invalid_toml_uses_defaults_test() {
  setup("not valid toml {{{}}")

  let config = auth_config.load()

  config.session_key |> should.equal("_auth_user_id")

  cleanup()
}

pub fn empty_file_uses_defaults_test() {
  setup("")

  let config = auth_config.load()

  config.session_key |> should.equal("_auth_user_id")

  cleanup()
}

// ------------------------------------------------------------- FFI Helpers

@external(erlang, "glimr_auth_test_ffi", "clear_auth_config")
fn clear_auth_config() -> Nil
