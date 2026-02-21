import gleeunit/should
import glimr/config/app as app_config
import simplifile

const config_dir = "config"

const config_file = "config/app.toml"

fn setup(content: String) -> Nil {
  let _ = simplifile.create_directory_all(config_dir)
  let _ = simplifile.write(config_file, content)
  clear_app_config()
  Nil
}

fn cleanup() -> Nil {
  let _ = simplifile.delete(config_file)
  clear_app_config()
  Nil
}

// ------------------------------------------------------------- Default Config

pub fn defaults_when_no_file_test() {
  clear_app_config()
  let _ = simplifile.delete(config_file)

  let config = app_config.load()

  config.static_directory |> should.equal("/static")

  cleanup()
}

// ------------------------------------------------------------- Custom Config

pub fn parses_custom_config_test() {
  setup(
    "[static]
  directory = \"/assets\"
",
  )

  let config = app_config.load()

  config.static_directory |> should.equal("/assets")

  cleanup()
}

pub fn invalid_toml_uses_defaults_test() {
  setup("not valid toml {{{}}")

  let config = app_config.load()

  config.static_directory |> should.equal("/static")

  cleanup()
}

pub fn empty_file_uses_defaults_test() {
  setup("")

  let config = app_config.load()

  config.static_directory |> should.equal("/static")

  cleanup()
}

// ------------------------------------------------------------- FFI Helpers

@external(erlang, "glimr_app_config_test_ffi", "clear_app_config")
fn clear_app_config() -> Nil
