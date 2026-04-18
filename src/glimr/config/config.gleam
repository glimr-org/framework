import gleam/dict.{type Dict}
import glimr/config as glimr_config
import tom

@deprecated("Use glimr/config.load instead")
pub fn load() -> Nil {
  glimr_config.load()
}

@deprecated("Use glimr/config.get_string instead")
pub fn get_string(path: String) -> String {
  glimr_config.get_string(path)
}

@deprecated("Use glimr/config.get_string_or instead")
pub fn get_string_or(path: String) -> Result(String, Nil) {
  glimr_config.get_string_or(path)
}

@deprecated("Use glimr/config.get_int instead")
pub fn get_int(path: String) -> Int {
  glimr_config.get_int(path)
}

@deprecated("Use glimr/config.get_int_or instead")
pub fn get_int_or(path: String) -> Result(Int, Nil) {
  glimr_config.get_int_or(path)
}

@deprecated("Use glimr/config.get_bool instead")
pub fn get_bool(path: String) -> Bool {
  glimr_config.get_bool(path)
}

@deprecated("Use glimr/config.get_bool_or instead")
pub fn get_bool_or(path: String) -> Result(Bool, Nil) {
  glimr_config.get_bool_or(path)
}

@deprecated("Use glimr/config.get_string_list instead")
pub fn get_string_list(path: String) -> List(String) {
  glimr_config.get_string_list(path)
}

@deprecated("Use glimr/config.get_int_list instead")
pub fn get_int_list(path: String) -> List(Int) {
  glimr_config.get_int_list(path)
}

@deprecated("Use glimr/config.get_bool_list instead")
pub fn get_bool_list(path: String) -> List(Bool) {
  glimr_config.get_bool_list(path)
}

@deprecated("Use glimr/config.get_table instead")
pub fn get_table(path: String) -> Result(Dict(String, tom.Toml), Nil) {
  glimr_config.get_table(path)
}

@deprecated("Use glimr/config.clear_cache instead")
pub fn clear_cache() -> Nil {
  glimr_config.clear_cache()
}

@deprecated("Use glimr/config.app_port instead")
pub fn app_port() -> Int {
  glimr_config.app_port()
}

@deprecated("Use glimr/config.dev_proxy_port instead")
pub fn dev_proxy_port() -> Int {
  glimr_config.dev_proxy_port()
}
