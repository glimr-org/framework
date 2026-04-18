import gleeunit/should
import glimr/config
import simplifile

const config_dir = "config"

const config_file = "config/app.toml"

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

  config.get_string_or("app.static.directory")
  |> should.be_error()

  cleanup()
}

// ------------------------------------------------------------- Custom Config

pub fn parses_custom_config_test() {
  setup(
    "[static]
  directory = \"/assets\"
",
  )

  config.load()

  config.get_string("app.static.directory")
  |> should.equal("/assets")

  cleanup()
}

pub fn invalid_toml_returns_error_test() {
  setup("not valid toml {{{}}")

  config.load()

  config.get_string_or("app.static.directory")
  |> should.be_error()

  cleanup()
}

pub fn empty_file_returns_error_test() {
  setup("")

  config.load()

  config.get_string_or("app.static.directory")
  |> should.be_error()

  cleanup()
}

// ------------------------------------------------------------- FFI Helpers

@external(erlang, "glimr_app_config_test_ffi", "clear_config_cache")
fn clear_config_cache() -> Nil
