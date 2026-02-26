//// Glimr Console Command
////
//// Console commands need argument parsing, help output, and
//// (for database commands) pool lifecycle management. Without
//// a shared abstraction, every command would duplicate that
//// boilerplate. This module provides a fluent builder API so
//// commands declare their args and handler, and the framework
//// handles parsing, validation, and cleanup.

import gleam/bool
import gleam/dict.{type Dict}
import gleam/erlang/charlist
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import glimr/cache/cache.{type CachePool}
import glimr/cache/database as cache_database
import glimr/cache/driver.{type CacheStore, DatabaseStore, FileStore, RedisStore} as cache_driver
import glimr/cache/file as cache_file
import glimr/config/cache as cache_config
import glimr/config/database
import glimr/console/console
import glimr/db/driver
import glimr/db/pool_connection.{type Config, type Pool}
import glimr/glimr
import glimr/internal/config

// ------------------------------------------------------------- Public types

/// A single type for all commands keeps the registry and
/// dispatch simple. Database commands look identical from the
/// outside — their pool lifecycle is captured inside the
/// handler closure by db_handler/cache_db_handler.
///
pub type Command {
  Command(description: String, args: List(CommandArg), handler: fn(Args) -> Nil)
}

/// A union of all argument kinds keeps the args list
/// homogeneous so commands can mix positional args, flags, and
/// options in a single list. The parser uses the variant tag to
/// decide how to match each CLI token.
///
pub type CommandArg {
  Argument(name: String, description: String)
  Flag(name: String, short: String, description: String)
  Option(name: String, description: String, default: String)
}

/// Separating parsed results by kind (positional, flags,
/// options) lets accessor functions like get_arg and has_flag
/// look up values in the right collection without ambiguity
/// between a flag named "verbose" and an argument named
/// "verbose".
///
pub type Args {
  Args(
    arguments: Dict(String, String),
    flags: List(String),
    options: Dict(String, String),
  )
}

// ------------------------------------------------------------- Public Functions

/// Starting with empty defaults lets callers build commands
/// incrementally via pipes, setting only the fields they need.
/// The temp_handler crashes with a clear message if someone
/// forgets to set a real handler.
///
/// *Example*
///
/// ```gleam
/// command.new()
/// |> command.description("Greet the user")
/// |> command.handler(fn(args) { ... })
/// ```
///
pub fn new() -> Command {
  Command(description: "", args: [], handler: temp_handler)
}

/// The description is the only documentation users see when
/// listing commands. Keeping it settable via the fluent API
/// means it stays close to the command definition rather than
/// in a separate registry.
///
pub fn description(cmd: Command, description: String) -> Command {
  Command(..cmd, description: description)
}

/// Plain handlers receive only parsed Args — they don't need
/// database access. Database commands should use db_handler or
/// cache_db_handler which inject the pool automatically.
///
pub fn handler(cmd: Command, handler: fn(Args) -> Nil) -> Command {
  Command(..cmd, handler: handler)
}

/// Declaring args up front lets the framework validate required
/// arguments and generate help output automatically, so command
/// authors don't write their own parsing or usage strings.
///
/// *Example*
///
/// ```gleam
/// command.new()
/// |> command.args([
///   Argument("name", "The name of the controller"),
///   Flag("resource", "r", "Generate a resource controller"),
///   Option("template", "Template to use", "default"),
/// ])
/// |> command.handler(fn(args) { ... })
/// ```
///
pub fn args(cmd: Command, arguments: List(CommandArg)) -> Command {
  Command(..cmd, args: arguments)
}

/// Every database command needs the same --database option.
/// Centralizing it here avoids typos and keeps the default
/// value ("_default" → first configured connection) consistent
/// across all commands.
///
pub fn db_option() -> CommandArg {
  Option("database", "Database connection to use", "_default")
}

/// Every cache command needs a --cache option to select which
/// store to use. Centralizing it here keeps the default value
/// ("_default" → first configured store) consistent.
///
pub fn cache_option() -> CommandArg {
  Option("cache", "Cache store to use", "_default")
}

/// Database commands must start a pool, run their logic, and
/// stop the pool — repeating that at every call site is error-
/// prone. This wrapper captures the lifecycle in the handler
/// closure so the command author just receives a live Pool.
///
pub fn db_handler(cmd: Command, user_handler: fn(Args, Pool) -> Nil) -> Command {
  let new_args = list.append(cmd.args, [db_option()])
  Command(..cmd, args: new_args, handler: fn(args) {
    with_db_pool(args, user_handler)
  })
}

/// Cache commands need a CachePool for the selected store. This
/// wrapper adds --cache, resolves the store, starts the pool,
/// runs the handler, and stops the pool — matching the pattern
/// of db_handler but for cache stores.
///
pub fn cache_handler(
  cmd: Command,
  user_handler: fn(Args, CachePool) -> Nil,
) -> Command {
  let new_args = list.append(cmd.args, [cache_option()])
  Command(..cmd, args: new_args, handler: fn(args) {
    with_cache_pool(args, user_handler)
  })
}

/// Commands like cache table migrations need the raw database
/// pool behind a DatabaseStore — not the CachePool abstraction,
/// but the actual SQL connection — because they're creating or
/// altering the cache table itself. This gives them both the
/// pool and the table name from the store config, following the
/// same lifecycle pattern as db_handler and cache_handler.
///
pub fn cache_db_handler(
  cmd: Command,
  user_handler: fn(Args, Pool, String) -> Nil,
) -> Command {
  let new_args = list.append(cmd.args, [cache_option()])
  Command(..cmd, args: new_args, handler: fn(args) {
    with_cache_db_pool(args, user_handler)
  })
}

/// Required arguments are validated before the handler runs, so
/// a missing value here is a programming error, not user input.
/// The assert crash makes that obvious rather than silently
/// returning a default.
///
pub fn get_arg(parsed: Args, name: String) -> String {
  let assert Ok(value) = dict.get(parsed.arguments, name)
  value
}

/// Flags are boolean — present or absent. A simple Bool return
/// is cleaner than forcing callers to match on a Result when
/// they just need to branch on whether --verbose was passed.
///
pub fn has_flag(parsed: Args, name: String) -> Bool {
  list.contains(parsed.flags, name)
}

/// Options always have defaults (set in the arg definition), so
/// missing values shouldn't happen after parsing. Returning
/// empty string on lookup failure is a safe fallback that
/// avoids forcing callers to handle a Result.
///
pub fn get_option(parsed: Args, name: String) -> String {
  dict.get(parsed.options, name)
  |> result.unwrap("")
}

/// Centralizing dispatch ensures every command gets env
/// loading, help flag detection, argument validation, and
/// connection resolution in the same order. Without this, each
/// command would repeat those checks or skip them
/// inconsistently.
///
pub fn run(cmd: Command) -> Nil {
  config.load_env()
  let #(cmd_name, args) = extract_command_name(get_args())

  // If the args passed have help flags (--h, -h), then we can just
  // print the help information for the command, and return
  use <- bool.lazy_guard(has_help_flag(args), fn() {
    print_command_help(cmd_name, cmd)
  })

  // Validate required args are present
  case parse_and_validate(cmd_name, cmd.args, args) {
    Ok(parsed) -> {
      use resolved <- resolve_or_error(resolve_connection(parsed))
      use resolved <- resolve_or_error(resolve_cache(resolved))
      cmd.handler(resolved)
    }
    Error(_) -> Nil
  }
}

// ------------------------------------------------------------- Internal Public Functions

/// Erlang's init:get_plain_arguments returns charlists, but
/// Gleam code works with String. Converting once here keeps
/// charlist handling out of every command and parser function.
///
@internal
pub fn get_args() -> List(String) {
  erlang_get_args()
  |> list.map(charlist.to_string)
}

/// Users need a quick way to check which Glimr version they're
/// running for bug reports and compatibility checks. Displayed
/// on -V/--version and at the top of help output.
///
@internal
pub fn print_glimr_version() -> Nil {
  io.println("Glimr " <> console.success(glimr.get_version()))
}

/// Most users have a single database connection and shouldn't
/// need to specify it every time. Resolving "_default" to the
/// first configured connection makes the common case zero-
/// config while still allowing explicit selection.
///
@internal
pub fn resolve_connection(parsed: Args) -> Result(Args, String) {
  case dict.get(parsed.options, "database") {
    Error(_) -> Ok(parsed)
    Ok(db_name) -> {
      let connections = database.load()

      // Resolve "_default" to the first connection
      use name <- result.try(case db_name {
        "_default" ->
          list.first(connections)
          |> result.map(driver.connection_name)
          |> result.replace_error("No database connections configured")
        name -> Ok(name)
      })

      // Validate connection exists
      use _ <- result.try(
        list.find(connections, fn(c) { driver.connection_name(c) == name })
        |> result.replace_error("Connection not found: " <> name),
      )

      Ok(Args(..parsed, options: dict.insert(parsed.options, "database", name)))
    }
  }
}

/// Resolves the --cache option the same way resolve_connection
/// handles --database. "_default" maps to the first configured
/// store, and the resolved name replaces the placeholder so
/// downstream code sees the real store name.
///
@internal
pub fn resolve_cache(parsed: Args) -> Result(Args, String) {
  case dict.get(parsed.options, "cache") {
    Error(_) -> Ok(parsed)
    Ok(store_name) -> {
      let stores = cache_config.load()

      // Resolve "_default" to the first store
      use name <- result.try(case store_name {
        "_default" ->
          list.first(stores)
          |> result.map(cache_driver.store_name)
          |> result.replace_error("No cache stores configured")
        name -> Ok(name)
      })

      // Validate store exists
      use _ <- result.try(
        list.find(stores, fn(s) { cache_driver.store_name(s) == name })
        |> result.replace_error("Cache store not found: " <> name),
      )

      Ok(Args(..parsed, options: dict.insert(parsed.options, "cache", name)))
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// Unwraps an Ok result into the continuation, or prints the
/// error message. Used by run() to chain resolve_connection and
/// resolve_cache without duplicating error handling.
///
fn resolve_or_error(result: Result(Args, String), next: fn(Args) -> Nil) -> Nil {
  case result {
    Ok(resolved) -> next(resolved)
    Error(msg) -> {
      console.output()
      |> console.line_error(msg)
      |> console.print()
    }
  }
}

/// Console commands are short-lived, so a pool_size of 1 is
/// sufficient and avoids wasting connections. Dynamic dispatch
/// to the adapter module lets this function work for both
/// PostgreSQL and SQLite without importing either.
///
fn with_db_pool(args: Args, user_handler: fn(Args, Pool) -> Nil) -> Nil {
  let db_name = get_option(args, "database")
  let connections = database.load()

  // Connection is guaranteed to exist (validated by resolve_connection)
  let assert Ok(conn) =
    list.find(connections, fn(c) { driver.connection_name(c) == db_name })

  // Use pool_size of 1 for console commands
  let conn = driver.with_pool_size(conn, 1)
  let config = driver.to_config(conn)

  // Map driver type to adapter module name
  let module = case driver.connection_type(conn) {
    driver.Postgres -> "glimr_postgres@postgres"
    driver.Sqlite -> "glimr_sqlite@sqlite"
  }

  // Suppress pgo supervisor shutdown reports during VM exit
  suppress_pool_shutdown_reports()

  case dynamic_start_pool(module, config) {
    Ok(pool) -> user_handler(args, pool)
    Error(msg) -> {
      console.output()
      |> console.line_error("Failed to start database pool:")
      |> console.line(msg)
      |> console.print()
    }
  }
}

/// Starts a CachePool for the resolved --cache store. File
/// stores are created directly, Redis stores use dynamic
/// dispatch to the adapter, and Database stores start a db pool
/// first then wrap it with cache_database.
///
fn with_cache_pool(args: Args, user_handler: fn(Args, CachePool) -> Nil) -> Nil {
  let cache_name = get_option(args, "cache")
  let stores = cache_config.load()

  // Store is guaranteed to exist (validated by resolve_cache)
  let assert Ok(store) =
    list.find(stores, fn(s) { cache_driver.store_name(s) == cache_name })

  case store {
    FileStore(_, _) -> {
      let pool = cache_file.wrap_pool(cache_file.start_pool(cache_name))
      user_handler(args, pool)
      cache.stop(pool)
    }
    RedisStore(_, _, _) -> {
      case dynamic_start_cache("glimr_redis@redis", store) {
        Ok(pool) -> {
          user_handler(args, pool)
          cache.stop(pool)
        }
        Error(msg) -> {
          console.output()
          |> console.line_error("Failed to start cache pool:")
          |> console.line(msg)
          |> console.print()
        }
      }
    }
    DatabaseStore(_, db_name, table) -> {
      // Start a database pool for the referenced connection
      let connections = database.load()
      let assert Ok(conn) =
        list.find(connections, fn(c) { driver.connection_name(c) == db_name })

      let conn = driver.with_pool_size(conn, 1)
      let config = driver.to_config(conn)

      let module = case driver.connection_type(conn) {
        driver.Postgres -> "glimr_postgres@postgres"
        driver.Sqlite -> "glimr_sqlite@sqlite"
      }

      suppress_pool_shutdown_reports()

      case dynamic_start_pool(module, config) {
        Ok(db_pool) -> {
          let pool = cache_database.start_with_table(db_pool, table)
          user_handler(args, pool)
          pool_connection.stop_pool(db_pool)
        }
        Error(msg) -> {
          console.output()
          |> console.line_error("Failed to start database pool for cache:")
          |> console.line(msg)
          |> console.print()
        }
      }
    }
  }
}

/// The heavy lifting behind cache_db_handler — looks up the
/// selected cache store, confirms it's a DatabaseStore (not
/// file or Redis), then boots a one-connection pool against the
/// referenced database. The table name from the store config
/// and a "database" option are passed through so the handler
/// can run SQL against the right connection and table without
/// any config lookup of its own.
///
fn with_cache_db_pool(
  args: Args,
  user_handler: fn(Args, Pool, String) -> Nil,
) -> Nil {
  let cache_name = get_option(args, "cache")
  let stores = cache_config.load()

  // Store is guaranteed to exist (validated by resolve_cache)
  let assert Ok(store) =
    list.find(stores, fn(s) { cache_driver.store_name(s) == cache_name })

  case store {
    DatabaseStore(_, db_name, table) -> {
      // Insert db_name into options so handler can use get_option(args, "database")
      let updated_args =
        Args(..args, options: dict.insert(args.options, "database", db_name))

      let connections = database.load()
      let assert Ok(conn) =
        list.find(connections, fn(c) { driver.connection_name(c) == db_name })

      let conn = driver.with_pool_size(conn, 1)
      let config = driver.to_config(conn)

      let module = case driver.connection_type(conn) {
        driver.Postgres -> "glimr_postgres@postgres"
        driver.Sqlite -> "glimr_sqlite@sqlite"
      }

      suppress_pool_shutdown_reports()

      case dynamic_start_pool(module, config) {
        Ok(pool) -> {
          user_handler(updated_args, pool, table)
          pool_connection.stop_pool(pool)
        }
        Error(msg) -> {
          console.output()
          |> console.line_error("Failed to start database pool for cache:")
          |> console.line(msg)
          |> console.print()
        }
      }
    }
    _ -> {
      console.output()
      |> console.line_error(
        "Cache store '" <> cache_name <> "' is not a database store",
      )
      |> console.print()
    }
  }
}

/// The CLI script injects _c_name= so the Gleam entrypoint
/// knows which command was invoked without re-parsing the
/// command registry. Stripping it here keeps the rest of the
/// arg parser clean of this convention.
///
fn extract_command_name(args: List(String)) -> #(String, List(String)) {
  case args {
    [first, ..rest] ->
      case string.starts_with(first, "_c_name=") {
        True -> #(string.drop_start(first, 8), rest)
        False -> #("", args)
      }
    [] -> #("", [])
  }
}

/// Checking for help flags before validation means users can
/// run `command --help` without providing required arguments.
/// Without this early check, help requests would fail with
/// "missing argument" errors.
///
fn has_help_flag(raw_args: List(String)) -> Bool {
  list.any(raw_args, fn(arg) { arg == "-h" || arg == "--help" })
}

/// Per-command help shows all arguments, flags, and options
/// with aligned descriptions — more detail than the main help
/// screen which only shows command names. Column alignment is
/// calculated dynamically so labels of different lengths
/// produce clean output.
///
fn print_command_help(cmd_name: String, cmd: Command) -> Nil {
  // Description
  io.println(console.warning("Description:"))
  io.println("  " <> cmd.description)
  io.println("")

  // Usage
  io.println(console.warning("Usage:"))
  let usage_line = build_usage_line(cmd_name, cmd.args)
  io.println("  " <> usage_line)
  io.println("")

  // Arguments section
  let arguments =
    list.filter_map(cmd.args, fn(arg) {
      case arg {
        Argument(arg_name, arg_desc) -> Ok(#(arg_name, arg_desc))
        Flag(_, _, _) -> Error(Nil)
        Option(_, _, _) -> Error(Nil)
      }
    })

  case arguments {
    [] -> Nil
    _ -> {
      io.println(console.warning("Arguments:"))
      let max_arg_len =
        list.fold(arguments, 0, fn(acc, arg) {
          int.max(acc, string.length(arg.0))
        })
      list.each(arguments, fn(arg) {
        let padded = string.pad_end(arg.0, max_arg_len + 2, " ")
        io.println("  " <> console.success(padded) <> arg.1)
      })
      io.println("")
    }
  }

  // Extract flags and options
  let flags =
    list.filter_map(cmd.args, fn(arg) {
      case arg {
        Flag(flag_name, short, flag_desc) -> Ok(#(flag_name, short, flag_desc))
        Argument(_, _) -> Error(Nil)
        Option(_, _, _) -> Error(Nil)
      }
    })

  let options =
    list.filter_map(cmd.args, fn(arg) {
      case arg {
        Option(opt_name, opt_desc, opt_default) ->
          Ok(#(opt_name, opt_desc, opt_default))
        Argument(_, _) -> Error(Nil)
        Flag(_, _, _) -> Error(Nil)
      }
    })

  // Calculate max length for alignment across flags and options
  let max_flag_len =
    list.fold(flags, string.length("-h, --help"), fn(acc, flag) {
      let #(flag_name, short, _) = flag
      let label = case short {
        "" -> "    --" <> flag_name
        _ -> "-" <> short <> ", --" <> flag_name
      }
      int.max(acc, string.length(label))
    })

  let max_opt_len =
    list.fold(options, 0, fn(acc, opt) {
      let #(opt_name, _, _) = opt
      let label = "    --" <> opt_name <> "=<value>"
      int.max(acc, string.length(label))
    })

  let max_len = int.max(max_flag_len, max_opt_len)

  // Options section (only if there are options with values)
  case options {
    [] -> Nil
    _ -> {
      io.println(console.warning("Options:"))
      list.each(options, fn(opt) {
        let #(opt_name, opt_desc, opt_default) = opt
        let label = "    --" <> opt_name <> "=<value>"
        let padded = string.pad_end(label, max_len + 2, " ")
        let desc_with_default = opt_desc <> " [default: " <> opt_default <> "]"
        io.println("  " <> console.success(padded) <> desc_with_default)
      })
      io.println("")
    }
  }

  // Flags section (includes -h, --help)
  io.println(console.warning("Flags:"))
  list.each(flags, fn(flag) {
    let #(flag_name, short, flag_desc) = flag
    let label = case short {
      "" -> "    --" <> flag_name
      _ -> "-" <> short <> ", --" <> flag_name
    }
    let padded = string.pad_end(label, max_len + 2, " ")
    io.println("  " <> console.success(padded) <> flag_desc)
  })

  // Always show -h, --help
  let help_label = string.pad_end("-h, --help", max_len + 2, " ")
  io.println(
    "  " <> console.success(help_label) <> "Display help for this command",
  )
}

/// Showing [options] only when flags or options exist keeps the
/// usage line clean for simple commands. Mixing placeholders
/// like <name> with [options] follows the convention users
/// expect from CLI tools.
///
fn build_usage_line(name: String, args: List(CommandArg)) -> String {
  let has_flags_or_options =
    list.any(args, fn(arg) {
      case arg {
        Flag(_, _, _) -> True
        Option(_, _, _) -> True
        Argument(_, _) -> False
      }
    })

  let arg_placeholders =
    list.filter_map(args, fn(arg) {
      case arg {
        Argument(arg_name, _) -> Ok("<" <> arg_name <> ">")
        Flag(_, _, _) -> Error(Nil)
        Option(_, _, _) -> Error(Nil)
      }
    })
    |> string.join(" ")

  case has_flags_or_options, arg_placeholders {
    True, "" -> name <> " [options]"
    True, _ -> name <> " [options] " <> arg_placeholders
    False, "" -> name
    False, _ -> name <> " " <> arg_placeholders
  }
}

/// Validating required arguments before calling the handler
/// means command authors never see missing values — they can
/// use get_arg with assert knowing validation already passed.
/// Missing args print usage help so the user knows what to fix.
///
fn parse_and_validate(
  cmd_name: String,
  arg_defs: List(CommandArg),
  raw_args: List(String),
) -> Result(Args, Nil) {
  let argument_defs =
    list.filter_map(arg_defs, fn(def) {
      case def {
        Argument(name, _) -> Ok(name)
        Flag(_, _, _) -> Error(Nil)
        Option(_, _, _) -> Error(Nil)
      }
    })

  let flag_defs =
    list.filter_map(arg_defs, fn(def) {
      case def {
        Flag(name, short, _) -> Ok(#(name, short))
        Argument(_, _) -> Error(Nil)
        Option(_, _, _) -> Error(Nil)
      }
    })

  let option_defs =
    list.filter_map(arg_defs, fn(def) {
      case def {
        Option(name, _, _) -> Ok(name)
        Argument(_, _) -> Error(Nil)
        Flag(_, _, _) -> Error(Nil)
      }
    })

  // Get option defaults
  let option_defaults =
    list.filter_map(arg_defs, fn(def) {
      case def {
        Option(name, _, default) -> Ok(#(name, default))
        Argument(_, _) -> Error(Nil)
        Flag(_, _, _) -> Error(Nil)
      }
    })
    |> dict.from_list

  // Parse flags and options from raw args
  let #(positional_values, parsed_flags, parsed_options) =
    parse_raw_args(raw_args, flag_defs, option_defs)

  // Merge defaults with parsed options (parsed values take precedence)
  let options_with_defaults = dict.merge(option_defaults, parsed_options)

  // Match positional values to argument definitions
  let arguments =
    list.index_map(argument_defs, fn(name, i) {
      let value =
        positional_values
        |> list.drop(i)
        |> list.first
      case value {
        Ok(v) -> #(name, v)
        Error(_) -> #(name, "")
      }
    })
    |> list.filter(fn(pair) { pair.1 != "" })
    |> dict.from_list

  // Check for missing required arguments
  let missing =
    list.filter(argument_defs, fn(name) {
      case dict.get(arguments, name) {
        Ok(_) -> False
        Error(_) -> True
      }
    })

  case missing {
    [] ->
      Ok(Args(
        arguments: arguments,
        flags: parsed_flags,
        options: options_with_defaults,
      ))
    _ -> {
      io.println(
        console.error("Error: Missing required argument(s): ")
        <> string.join(missing, ", "),
      )
      io.println("")
      print_usage(cmd_name, arg_defs)
      Error(Nil)
    }
  }
}

/// Separating parsing from validation keeps this function
/// focused on tokenization. Ignoring unknown flags means
/// commands don't break when new global flags are added to the
/// CLI wrapper.
///
fn parse_raw_args(
  raw_args: List(String),
  flag_defs: List(#(String, String)),
  option_defs: List(String),
) -> #(List(String), List(String), Dict(String, String)) {
  let #(positional, flags, options) =
    list.fold(raw_args, #([], [], dict.new()), fn(acc, arg) {
      let #(pos, flgs, opts) = acc

      // Positional argument (no prefix)
      use <- bool.guard(!string.starts_with(arg, "-"), #(
        [arg, ..pos],
        flgs,
        opts,
      ))

      // Short flag (-f)
      use <- bool.lazy_guard(!string.starts_with(arg, "--"), fn() {
        let short = string.drop_start(arg, 1)
        case list.find(flag_defs, fn(def) { def.1 == short }) {
          Ok(#(name, _)) -> #(pos, [name, ..flgs], opts)
          Error(_) -> #(pos, flgs, opts)
        }
      })

      // Long form (--flag or --option=value)
      let rest = string.drop_start(arg, 2)
      case string.split_once(rest, "=") {
        Ok(#(name, value)) ->
          case list.contains(option_defs, name) {
            True -> #(pos, flgs, dict.insert(opts, name, value))
            False -> #(pos, flgs, opts)
          }
        Error(_) ->
          case list.find(flag_defs, fn(def) { def.0 == rest }) {
            Ok(#(name, _)) -> #(pos, [name, ..flgs], opts)
            Error(_) -> #(pos, flgs, opts)
          }
      }
    })

  #(list.reverse(positional), flags, options)
}

/// When validation fails, showing the expected syntax alongside
/// the error helps users fix their invocation immediately
/// instead of running a separate --help command.
///
fn print_usage(cmd_name: String, arg_defs: List(CommandArg)) -> Nil {
  let arg_names =
    list.filter_map(arg_defs, fn(def) {
      case def {
        Argument(name, _) -> Ok("<" <> name <> ">")
        Flag(_, _, _) -> Error(Nil)
        Option(_, _, _) -> Error(Nil)
      }
    })
    |> string.join(" ")

  let flag_names =
    list.filter_map(arg_defs, fn(def) {
      case def {
        Flag(name, _, _) -> Ok("[--" <> name <> "]")
        Argument(_, _) -> Error(Nil)
        Option(_, _, _) -> Error(Nil)
      }
    })
    |> string.join(" ")

  let option_names =
    list.filter_map(arg_defs, fn(def) {
      case def {
        Option(name, _, _) -> Ok("[--" <> name <> "=<value>]")
        Argument(_, _) -> Error(Nil)
        Flag(_, _, _) -> Error(Nil)
      }
    })
    |> string.join(" ")

  let flags_and_options = case flag_names, option_names {
    "", "" -> ""
    f, "" -> f
    "", o -> o
    f, o -> f <> " " <> o
  }

  let usage = case flags_and_options {
    "" -> cmd_name <> " " <> arg_names
    _ -> cmd_name <> " " <> arg_names <> " " <> flags_and_options
  }

  io.println(console.warning("Usage: ") <> usage)
}

/// A placeholder that crashes loudly prevents commands from
/// silently doing nothing if the developer forgets to call
/// handler() or db_handler() during setup.
///
fn temp_handler(_args: Args) -> Nil {
  io.println(console.error(
    "A handler function has not been set for this command.",
  ))
}

// ------------------------------------------------------------- FFI Bindings

/// Erlang's init module is the only way to access CLI arguments
/// on the BEAM. The charlist return type is converted to String
/// by get_args above.
///
@external(erlang, "init", "get_plain_arguments")
fn erlang_get_args() -> List(charlist.Charlist)

/// The framework can't import driver adapters directly without
/// creating a circular dependency. Dynamic dispatch via Erlang
/// module name lets with_db_pool start the right adapter pool
/// at runtime based on the configured driver type.
///
@external(erlang, "glimr_command_ffi", "dynamic_start_pool")
fn dynamic_start_pool(module: String, config: Config) -> Result(Pool, String)

/// The framework can't import the Redis adapter directly
/// without creating a circular dependency, same as the database
/// adapter. This lets with_cache_pool call glimr_redis's
/// try_start_cache at runtime by module name, so console cache
/// commands work without the framework depending on glimr_redis
/// at compile time.
///
@external(erlang, "glimr_command_ffi", "dynamic_start_cache")
fn dynamic_start_cache(
  module: String,
  store: CacheStore,
) -> Result(CachePool, String)

/// Console commands exit immediately after running, but the pgo
/// supervisor logs noisy shutdown reports as the VM tears down.
/// Suppressing them keeps console output clean — users running
/// `glimr migrate` don't want to see Erlang supervisor teardown
/// noise mixed in with their migration results.
///
@external(erlang, "glimr_command_ffi", "suppress_pool_shutdown_reports")
fn suppress_pool_shutdown_reports() -> Nil
