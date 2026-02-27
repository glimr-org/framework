//// Glimr Configuration
////
//// Loads and parses the glimr.toml configuration file.
//// Provides access to hooks that run for commands such as
//// `./glimr build` and `./glimr run` allowing you to extend
//// them. Also handles environment variable loading and
//// application port settings.
////

import dot_env
import dot_env/env
import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import simplifile
import tom

// ------------------------------------------------------------- Public Types

/// Top-level configuration container. Holds all configuration
/// sections loaded from glimr.toml including hooks for build
/// and run lifecycle events.
///
pub type Config {
  Config(hooks: Hooks, commands: Commands, routes: Routes, loom: Loom)
}

/// Command configuration for package discovery and auto-
/// compilation. Lists packages that provide console commands to
/// be included in the command registry.
///
pub type Commands {
  Commands(auto_compile: Bool, packages: List(String))
}

/// Route configuration for auto-compilation during development.
/// When enabled, routes are recompiled automatically when
/// source files change during development mode.
///
pub type Routes {
  Routes(auto_compile: Bool)
}

/// Loom template configuration for auto-compilation during
/// development. When enabled, Loom templates are recompiled
/// automatically when source files change.
///
pub type Loom {
  Loom(auto_compile: Bool)
}

/// Hook configuration for build and run lifecycle events. Each
/// field contains a list of shell commands to execute at the
/// corresponding lifecycle point.
///
pub type Hooks {
  Hooks(
    build_pre: List(String),
    build_post: List(String),
    run_pre: List(String),
    run_reload_pre: List(String),
    run_reload_post_modified: List(String),
  )
}

// ------------------------------------------------------------- Public Functions

/// Loads configuration from glimr.toml in the current
/// directory. Returns default configuration if the file doesn't
/// exist or cannot be parsed.
///
@internal
pub fn load() -> Config {
  case simplifile.read("glimr.toml") {
    Ok(content) -> parse(content)
    Error(_) -> default_config()
  }
}

/// Loads environment variables from the .env file into the
/// runtime environment. Called during application startup to
/// make configuration values available.
///
@internal
pub fn load_env() -> Nil {
  dot_env.new()
  |> dot_env.set_path(".env")
  |> dot_env.set_debug(False)
  |> dot_env.load()

  Nil
}

/// Returns the application server port from environment. Reads
/// APP_PORT from the environment and defaults to 8000 if not
/// set or invalid.
///
@internal
pub fn app_port() -> Int {
  env.get_int("APP_PORT") |> result.unwrap(8000)
}

/// Returns the development proxy port from environment. Reads
/// DEV_PROXY_PORT from the environment and defaults to 8001 if
/// not set or invalid.
///
@internal
pub fn dev_proxy_port() -> Int {
  env.get_int("DEV_PROXY_PORT") |> result.unwrap(8001)
}

// ------------------------------------------------------------- Private Functions

/// Parses TOML content into a Config struct. Extracts the
/// hooks, commands, routes, and loom sections and falls back to
/// defaults if parsing fails or sections are missing.
///
fn parse(content: String) -> Config {
  case tom.parse(content) {
    Ok(toml) -> {
      let hooks = case dict.get(toml, "hooks") {
        Ok(hooks_toml) -> parse_hooks(hooks_toml)
        Error(_) -> default_hooks()
      }
      let commands = parse_commands(toml)
      let routes = parse_routes(toml)
      let loom = parse_loom(toml)
      Config(hooks: hooks, commands: commands, routes: routes, loom: loom)
    }
    Error(_) -> default_config()
  }
}

/// Extracts command configuration from the [commands] TOML
/// section. Parses auto_compile flag and packages list for
/// console command discovery and registry compilation.
///
fn parse_commands(toml: Dict(String, tom.Toml)) -> Commands {
  let auto_compile = case tom.get_bool(toml, ["commands", "auto_compile"]) {
    Ok(b) -> b
    Error(_) -> True
  }
  let packages = case tom.get_array(toml, ["commands", "packages"]) {
    Ok(items) ->
      list.filter_map(items, fn(item) {
        case item {
          tom.String(s) -> Ok(s)
          _ -> Error(Nil)
        }
      })
    Error(_) -> ["glimr"]
  }
  Commands(auto_compile: auto_compile, packages: packages)
}

/// Extracts route configuration from the [routes] TOML section.
/// Parses auto_compile flag that controls whether routes are
/// recompiled automatically during development.
///
fn parse_routes(toml: Dict(String, tom.Toml)) -> Routes {
  let auto_compile = case tom.get_bool(toml, ["routes", "auto_compile"]) {
    Ok(b) -> b
    Error(_) -> True
  }
  Routes(auto_compile: auto_compile)
}

/// Extracts loom configuration from the [loom] TOML section.
/// Parses auto_compile flag that controls whether templates are
/// recompiled automatically during development.
///
fn parse_loom(toml: Dict(String, tom.Toml)) -> Loom {
  let auto_compile = case tom.get_bool(toml, ["loom", "auto_compile"]) {
    Ok(b) -> b
    Error(_) -> True
  }
  Loom(auto_compile: auto_compile)
}

/// Extracts hook configuration from the [hooks] TOML section.
/// Parses build.pre, build.post, run.pre, and run.reload
/// subsections into the Hooks struct.
///
fn parse_hooks(toml: tom.Toml) -> Hooks {
  let build = get_table(toml, "build")
  let run = get_table(toml, "run")
  let run_reload = get_nested_table(toml, "run", "reload")

  Hooks(
    build_pre: get_string_list(build, "pre"),
    build_post: get_string_list(build, "post"),
    run_pre: get_string_list(run, "pre"),
    run_reload_pre: get_string_list(run_reload, "pre"),
    run_reload_post_modified: get_string_list(run_reload, "post-modified"),
  )
}

/// Gets a nested table from TOML by key. Returns an empty table
/// if the key doesn't exist or the value is not a table,
/// allowing safe chaining.
///
fn get_table(toml: tom.Toml, key: String) -> tom.Toml {
  case toml {
    tom.Table(table) -> {
      case dict.get(table, key) {
        Ok(t) -> t
        Error(_) -> tom.Table(dict.new())
      }
    }
    _ -> tom.Table(dict.new())
  }
}

/// Gets a doubly-nested table from TOML. Chains two get_table
/// calls to access tables like [hooks.run.reload] using keys
/// "run" and "reload".
///
fn get_nested_table(toml: tom.Toml, key1: String, key2: String) -> tom.Toml {
  get_table(toml, key1) |> get_table(key2)
}

/// Extracts a list of strings from a TOML table by key. Handles
/// both array values and single string values, returning an
/// empty list if the key is missing or has wrong type.
///
fn get_string_list(toml: tom.Toml, key: String) -> List(String) {
  case toml {
    tom.Table(table) -> {
      case dict.get(table, key) {
        Ok(tom.Array(items)) -> {
          items
          |> list.filter_map(fn(item) {
            case item {
              tom.String(s) -> Ok(s)
              _ -> Error(Nil)
            }
          })
        }
        Ok(tom.String(s)) -> [s]
        _ -> []
      }
    }
    _ -> []
  }
}

/// Returns the default configuration with empty hooks. Used
/// when glimr.toml doesn't exist or cannot be parsed to provide
/// sensible defaults.
///
fn default_config() -> Config {
  Config(
    hooks: default_hooks(),
    commands: default_commands(),
    routes: default_routes(),
    loom: default_loom(),
  )
}

/// Returns default commands configuration with glimr as the
/// only package and auto_compile enabled. This ensures the
/// framework's built-in commands are always available.
///
fn default_commands() -> Commands {
  Commands(auto_compile: True, packages: ["glimr"])
}

/// Returns default routes configuration with auto_compile
/// enabled. Routes will be recompiled on changes during
/// development unless explicitly disabled.
///
fn default_routes() -> Routes {
  Routes(auto_compile: True)
}

/// Returns default loom configuration with auto_compile
/// enabled. Templates will be recompiled on changes during
/// development unless explicitly disabled.
///
fn default_loom() -> Loom {
  Loom(auto_compile: True)
}

/// Returns default hooks with all lists empty. No commands will
/// be executed at any lifecycle point unless explicitly
/// configured in glimr.toml.
///
fn default_hooks() -> Hooks {
  Hooks(
    build_pre: [],
    build_post: [],
    run_pre: [],
    run_reload_pre: [],
    run_reload_post_modified: [],
  )
}
