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

/// Everything the framework needs to customize its behavior
/// lives here in one place. Passing this single value through
/// the build and watch pipelines avoids repeatedly reading
/// glimr.toml or scattering config lookups across modules.
///
pub type Config {
  Config(
    hooks: Hooks,
    commands: Commands,
    routes: Routes,
    loom: Loom,
    database: Database,
  )
}

/// Third-party packages can ship their own CLI commands (like a
/// mailer package adding `./glimr send-test-email`). The
/// packages list tells the command compiler which dependencies
/// to scan for command modules.
///
pub type Commands {
  Commands(auto_compile: Bool, packages: List(String))
}

/// Disabling auto_compile is useful when you're working on
/// something unrelated and don't want route recompilation
/// slowing down your feedback loop on every save.
///
pub type Routes {
  Routes(auto_compile: Bool)
}

/// Same idea as Routes — you can turn off template
/// recompilation when you're deep in backend work and don't
/// want the watcher spending time on templates you haven't
/// touched.
///
pub type Loom {
  Loom(auto_compile: Bool)
}

/// Schema changes trigger query regeneration and migration
/// diffing. Turning this off prevents the watcher from
/// regenerating models while you're in the middle of editing
/// multiple schema files at once.
///
pub type Database {
  Database(auto_gen: Bool)
}

/// Shell commands that run at specific points in the build and
/// dev lifecycle. Want to run `npm run build` before every
/// `./glimr build`? Add it to build_pre. Need to clear a cache
/// after hot reload? That's run_reload_pre.
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

/// A missing or broken glimr.toml shouldn't prevent the app
/// from running — sensible defaults mean everything works out
/// of the box until you need to customize something.
///
@internal
pub fn load() -> Config {
  case simplifile.read("glimr.toml") {
    Ok(content) -> parse(content)
    Error(_) -> default_config()
  }
}

/// Database URLs, API keys, and other secrets live in .env
/// rather than source code. Loading them early in startup means
/// any module can call env.get() without worrying about
/// initialization order.
///
@internal
pub fn load_env() -> Nil {
  dot_env.new()
  |> dot_env.set_path(".env")
  |> dot_env.set_debug(False)
  |> dot_env.load()

  Nil
}

/// Defaults to 8000 so new projects work without any .env file.
/// Production deploys override this via APP_PORT to match their
/// infrastructure's port assignment.
///
@internal
pub fn app_port() -> Int {
  env.get_int("APP_PORT") |> result.unwrap(8000)
}

/// The dev proxy sits in front of the app so browsers don't see
/// connection-refused errors during hot reloads. 8001 is one
/// above the default app port, keeping the mental model simple.
///
@internal
pub fn dev_proxy_port() -> Int {
  env.get_int("DEV_PROXY_PORT") |> result.unwrap(8001)
}

// ------------------------------------------------------------- Private Functions

/// Each section falls back to its own default independently, so
/// a typo in [hooks] doesn't blow away your [database] settings
/// — you only lose the section that's malformed.
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
      let database = parse_database(toml)
      Config(
        hooks: hooks,
        commands: commands,
        routes: routes,
        loom: loom,
        database: database,
      )
    }
    Error(_) -> default_config()
  }
}

/// The packages list defaults to ["glimr"] so the framework's
/// built-in commands are always available, even if you forget
/// to configure this section.
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

/// Defaults to auto_compile: True because most developers want
/// instant feedback on route changes. The rare case where you'd
/// disable this is when recompilation is too slow on a very
/// large route file.
///
fn parse_routes(toml: Dict(String, tom.Toml)) -> Routes {
  let auto_compile = case tom.get_bool(toml, ["routes", "auto_compile"]) {
    Ok(b) -> b
    Error(_) -> True
  }
  Routes(auto_compile: auto_compile)
}

/// Same pattern as parse_routes — defaults to True so templates
/// recompile on save. Template compilation is fast enough that
/// disabling this is rarely needed.
///
fn parse_loom(toml: Dict(String, tom.Toml)) -> Loom {
  let auto_compile = case tom.get_bool(toml, ["loom", "auto_compile"]) {
    Ok(b) -> b
    Error(_) -> True
  }
  Loom(auto_compile: auto_compile)
}

/// Auto-generation is on by default so editing a schema file
/// immediately regenerates the typed query module. You might
/// turn this off temporarily when restructuring multiple models
/// to avoid partial regeneration between saves.
///
fn parse_database(toml: Dict(String, tom.Toml)) -> Database {
  let auto_gen = case tom.get_bool(toml, ["database", "auto_gen"]) {
    Ok(b) -> b
    Error(_) -> True
  }
  Database(auto_gen: auto_gen)
}

/// The TOML nesting mirrors the lifecycle: [hooks.build] has
/// pre/post, [hooks.run] has pre, and [hooks.run.reload] has
/// pre and post-modified. Each resolves to a simple list of
/// shell commands.
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

/// Returning an empty table instead of Error on missing keys
/// means callers can chain lookups without checking each level
/// — a missing [hooks.build] section just produces empty hook
/// lists instead of crashing.
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

/// Reaches two levels deep into the TOML tree — needed for
/// [hooks.run.reload] which nests under both "run" and
/// "reload". Missing at either level gracefully returns an
/// empty table.
///
fn get_nested_table(toml: tom.Toml, key1: String, key2: String) -> tom.Toml {
  get_table(toml, key1) |> get_table(key2)
}

/// Accepting both `pre = "cmd"` and `pre = ["cmd1", "cmd2"]` in
/// the TOML means developers don't have to use array syntax for
/// a single hook command — a small convenience that avoids a
/// common config mistake.
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

/// Fresh projects shouldn't need a glimr.toml to work. These
/// defaults enable all auto-compilation features and leave
/// hooks empty, so everything "just works" until the developer
/// needs to customize.
///
fn default_config() -> Config {
  Config(
    hooks: default_hooks(),
    commands: default_commands(),
    routes: default_routes(),
    loom: default_loom(),
    database: default_database(),
  )
}

/// Including "glimr" in the default package list means the
/// framework's built-in commands (build, run, gen, etc.) are
/// always discovered, even without any config file.
///
fn default_commands() -> Commands {
  Commands(auto_compile: True, packages: ["glimr"])
}

/// Auto-compile on by default so the dev experience is instant
/// feedback — save a controller, see the route change without
/// manually running a compile step.
///
fn default_routes() -> Routes {
  Routes(auto_compile: True)
}

/// Templates auto-compile by default so editing a .loom.html
/// file immediately shows up in the browser on refresh,
/// matching the "save and see" workflow developers expect.
///
fn default_loom() -> Loom {
  Loom(auto_compile: True)
}

/// With auto_gen on, adding a column to a schema file
/// regenerates the typed repository module within a second, so
/// you can immediately use the new field without running a
/// manual gen command.
///
fn default_database() -> Database {
  Database(auto_gen: True)
}

/// Empty by default because hooks are opt-in — most projects
/// don't need custom build steps until they integrate frontend
/// tooling or external codegen.
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
