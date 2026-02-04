//// Glimr Configuration
////
//// Loads and parses the glimr.toml configuration file. Provides
//// access to hooks that run for commands such as `./glimr build`
//// and `./glimr run` allowing you to extend them.

import dot_env
import dot_env/env
import gleam/dict
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
  Config(hooks: Hooks, commands: Commands)
}

/// Command configuration for package discovery. Lists packages
/// that provide console commands to be included in the command
/// registry.
///
pub type Commands {
  Commands(packages: List(String))
}

/// Hook configuration for build and run lifecycle events.
/// Each field contains a list of shell commands to execute
/// at the corresponding lifecycle point.
///
pub type Hooks {
  Hooks(
    build_pre: List(String),
    build_post: List(String),
    run_pre: List(String),
    run_reload_pre: List(String),
    run_reload_route_modified: List(String),
    run_reload_loom_modified: List(String),
    run_reload_post_modified: List(String),
  )
}

// ------------------------------------------------------------- Public Functions

/// Loads configuration from glimr.toml in the current directory.
/// Returns default configuration if the file doesn't exist or
/// cannot be parsed.
///
@internal
pub fn load() -> Config {
  case simplifile.read("glimr.toml") {
    Ok(content) -> parse(content)
    Error(_) -> default_config()
  }
}

/// Loads environment variables from the .env file into the
/// runtime environment. Called during application startup
/// to make configuration values available.
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
/// DEV_PROXY_PORT from the environment and defaults to 8001
/// if not set or invalid.
///
@internal
pub fn dev_proxy_port() -> Int {
  env.get_int("DEV_PROXY_PORT") |> result.unwrap(8001)
}

// ------------------------------------------------------------- Private Functions

/// Parses TOML content into a Config struct. Extracts the hooks
/// and commands sections and falls back to defaults if parsing
/// fails or sections are missing.
///
fn parse(content: String) -> Config {
  case tom.parse(content) {
    Ok(toml) -> {
      let hooks = case dict.get(toml, "hooks") {
        Ok(hooks_toml) -> parse_hooks(hooks_toml)
        Error(_) -> default_hooks()
      }
      let commands = case dict.get(toml, "commands") {
        Ok(commands_toml) -> parse_commands(commands_toml)
        Error(_) -> default_commands()
      }
      Config(hooks: hooks, commands: commands)
    }
    Error(_) -> default_config()
  }
}

/// Extracts command configuration from the [commands] TOML section.
/// Parses the packages list which specifies which packages provide
/// console commands.
///
fn parse_commands(toml: tom.Toml) -> Commands {
  Commands(packages: get_string_list(toml, "packages"))
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
    run_reload_route_modified: get_string_list(run_reload, "route-modified"),
    run_reload_loom_modified: get_string_list(run_reload, "loom-modified"),
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
/// calls to access tables like [hooks.run.reload] using
/// keys "run" and "reload".
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
/// when glimr.toml doesn't exist or cannot be parsed to
/// provide sensible defaults.
///
fn default_config() -> Config {
  Config(hooks: default_hooks(), commands: default_commands())
}

/// Returns default commands with glimr as the only package.
/// The glimr package provides all built-in framework commands
/// like build, run, make_controller, etc.
///
fn default_commands() -> Commands {
  Commands(packages: ["glimr"])
}

/// Returns default hooks with all lists empty. No commands
/// will be executed at any lifecycle point unless explicitly
/// configured in glimr.toml.
///
fn default_hooks() -> Hooks {
  Hooks(
    build_pre: [],
    build_post: [],
    run_pre: [],
    run_reload_pre: [],
    run_reload_route_modified: [],
    run_reload_loom_modified: [],
    run_reload_post_modified: [],
  )
}
