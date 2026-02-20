import gleeunit/should
import glimr/config/session
import simplifile

const config_dir = "config"

const config_file = "config/session.toml"

fn setup_config(content: String) -> Nil {
  let _ = simplifile.create_directory_all(config_dir)
  let _ = simplifile.write(config_file, content)
  // Clear persistent_term cache so load() re-reads the file
  clear_session_config()
  Nil
}

fn cleanup_config() -> Nil {
  let _ = simplifile.delete(config_file)
  clear_session_config()
  Nil
}

@external(erlang, "glimr_session_test_ffi", "clear_session_config")
fn clear_session_config() -> Nil

// ------------------------------------------------------------- Default Config Tests

pub fn load_defaults_when_no_file_test() {
  cleanup_config()

  let config = session.load()

  config.table |> should.equal("sessions")
  config.cookie |> should.equal("glimr_session")
  config.lifetime |> should.equal(120)
  config.expire_on_close |> should.equal(False)
}

// ------------------------------------------------------------- Custom Config Tests

pub fn load_custom_config_test() {
  setup_config(
    "[session]
  table = \"user_sessions\"
  cookie = \"my_app_session\"
  lifetime = 60
  expire_on_close = true
",
  )

  let config = session.load()

  config.table |> should.equal("user_sessions")
  config.cookie |> should.equal("my_app_session")
  config.lifetime |> should.equal(60)
  config.expire_on_close |> should.equal(True)

  cleanup_config()
}

pub fn load_partial_config_uses_defaults_test() {
  setup_config(
    "[session]
  cookie = \"custom_cookie\"
",
  )

  let config = session.load()

  config.table |> should.equal("sessions")
  config.cookie |> should.equal("custom_cookie")
  config.lifetime |> should.equal(120)
  config.expire_on_close |> should.equal(False)

  cleanup_config()
}

// ------------------------------------------------------------- Invalid Config Tests

pub fn load_invalid_toml_returns_defaults_test() {
  setup_config("this is not valid toml {{{}}")

  let config = session.load()

  config.table |> should.equal("sessions")
  config.cookie |> should.equal("glimr_session")
  config.lifetime |> should.equal(120)
  config.expire_on_close |> should.equal(False)

  cleanup_config()
}

pub fn load_empty_file_returns_defaults_test() {
  setup_config("")

  let config = session.load()

  config.table |> should.equal("sessions")
  config.cookie |> should.equal("glimr_session")

  cleanup_config()
}

pub fn load_wrong_section_returns_defaults_test() {
  setup_config(
    "[wrong_section]
  table = \"other\"
",
  )

  let config = session.load()

  config.table |> should.equal("sessions")

  cleanup_config()
}
