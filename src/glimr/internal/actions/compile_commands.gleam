//// Command Registry Compiler
////
//// Generates a JSON registry of available commands so the CLI
//// can show help without loading every command module. Scans
//// both third-party packages and app commands to build a
//// unified command list at compile time.
////

import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/regexp
import gleam/string
import glimr/console/console
import glimr/internal/config
import simplifile
import tom

// ------------------------------------------------------------- Private Consts

// Path where the command registry is outputted
const registry_path = "priv/storage/framework/console/commands.json"

// ------------------------------------------------------------- Public Types

/// Holds command metadata for the registry. Module path enables
/// dynamic loading when the command is actually invoked,
/// avoiding upfront loading of all command modules.
///
pub type CommandInfo {
  CommandInfo(name: String, description: String, module: String)
}

// ------------------------------------------------------------- Public Functions

/// Entry point invoked by the build command. Scans all
/// configured packages and app commands, writes results to JSON
/// for fast CLI startup without loading every command module.
///
pub fn run(verbose: Bool) -> Result(Nil, String) {
  case verbose {
    True -> {
      console.output()
      |> console.line_warning("Compiling command registry...")
      |> console.print()
    }
    False -> Nil
  }

  let cfg = config.load()
  let commands = scan_commands(cfg.commands.packages)

  // Check for collisions
  case find_collisions(commands) {
    [#(name, module1, module2), ..] -> {
      Error(
        "Command name collision: \""
        <> name
        <> "\" exists in multiple locations:\n  - "
        <> module1
        <> "\n  - "
        <> module2,
      )
    }
    [] ->
      case list.is_empty(commands) {
        True -> {
          console.output()
          |> console.line_warning("No commands found")
          |> console.print()
          Ok(Nil)
        }
        False -> {
          case write_registry(commands) {
            Ok(_) -> {
              case verbose {
                True -> {
                  console.output()
                  |> console.line_success(
                    "Compiled "
                    <> string.inspect(list.length(commands))
                    <> " commands to priv/storage/framework/console/commands.json",
                  )
                  |> console.print()
                }
                False -> {
                  console.output()
                  |> console.line_success(
                    "Compiled "
                    <> string.inspect(list.length(commands))
                    <> " commands",
                  )
                  |> console.print()
                }
              }
              Ok(Nil)
            }
            Error(msg) -> Error(msg)
          }
        }
      }
  }
}

/// Reads the pre-compiled registry for fast command lookup.
/// Returns Error if registry doesn't exist yet, signaling that
/// command:compile needs to run first.
///
pub fn read_registry() -> Result(Dict(String, CommandInfo), Nil) {
  case simplifile.read(registry_path) {
    Ok(content) -> parse_registry(content)
    Error(_) -> Error(Nil)
  }
}

// ------------------------------------------------------------- Private Functions

/// Finds command name collisions across all scanned commands.
/// Returns a list of tuples containing the colliding name and
/// the two module paths that define it.
///
fn find_collisions(
  commands: List(CommandInfo),
) -> List(#(String, String, String)) {
  commands
  |> list.fold(dict.new(), fn(seen, cmd) {
    case dict.get(seen, cmd.name) {
      Ok(existing_module) -> {
        // Already seen - this is a collision, store both modules
        dict.insert(seen, cmd.name, existing_module <> "|" <> cmd.module)
      }
      Error(_) -> dict.insert(seen, cmd.name, cmd.module)
    }
  })
  |> dict.to_list
  |> list.filter_map(fn(entry) {
    let #(name, modules) = entry
    case string.split(modules, "|") {
      [module1, module2, ..] -> Ok(#(name, module1, module2))
      _ -> Error(Nil)
    }
  })
}

/// Combines package and app commands into one list. Packages
/// are scanned first, then app commands are appended.
/// Collisions are detected and reported as errors during
/// compilation.
///
fn scan_commands(packages: List(String)) -> List(CommandInfo) {
  let package_commands =
    packages
    |> list.flat_map(scan_package_commands)

  let app_commands = scan_app_commands()

  list.append(package_commands, app_commands)
}

/// Checks manifest.toml to find package location. Local path
/// dependencies use their actual path, hex dependencies live in
/// build/packages/. This avoids hardcoding package locations.
///
fn scan_package_commands(package: String) -> List(CommandInfo) {
  let dir = case get_package_path(package) {
    Some(path) -> path <> "/src/" <> package <> "/internal/console/commands"
    None ->
      "build/packages/"
      <> package
      <> "/src/"
      <> package
      <> "/internal/console/commands"
  }

  scan_directory(dir, package)
}

/// Parses manifest.toml to distinguish local from hex packages.
/// Local packages have source="local" and a path field, hex
/// packages only have name and version.
///
fn get_package_path(package: String) -> option.Option(String) {
  case simplifile.read("manifest.toml") {
    Ok(content) -> parse_package_path(content, package)
    Error(_) -> None
  }
}

/// Extracts path for local packages from manifest.toml. Returns
/// None for hex packages so caller knows to look in the
/// standard build/packages directory instead of a custom local
/// path.
///
fn parse_package_path(content: String, package: String) -> option.Option(String) {
  case tom.parse(content) {
    Ok(toml) -> {
      case dict.get(toml, "packages") {
        Ok(tom.Array(packages)) -> {
          packages
          |> list.filter_map(fn(pkg) {
            case pkg {
              tom.InlineTable(table) -> {
                let name = case dict.get(table, "name") {
                  Ok(tom.String(n)) -> n
                  _ -> ""
                }
                let source = case dict.get(table, "source") {
                  Ok(tom.String(s)) -> s
                  _ -> ""
                }
                let path = case dict.get(table, "path") {
                  Ok(tom.String(p)) -> Ok(p)
                  _ -> Error(Nil)
                }
                case name == package && source == "local" {
                  True -> path
                  False -> Error(Nil)
                }
              }
              _ -> Error(Nil)
            }
          })
          |> list.first
          |> option.from_result
        }
        _ -> None
      }
    }
    Error(_) -> None
  }
}

/// Scans app commands separately since they live in a different
/// directory structure than package commands and use "app" as
/// their prefix in command names.
///
fn scan_app_commands() -> List(CommandInfo) {
  scan_directory("src/app/console/commands", "app")
}

/// Lists .gleam files in directory and parses each. Returns
/// empty list if directory doesn't exist, which is fine for
/// packages that don't define commands.
///
fn scan_directory(dir: String, prefix: String) -> List(CommandInfo) {
  case simplifile.get_files(dir) {
    Ok(files) -> {
      files
      |> list.filter(fn(f) { string.ends_with(f, ".gleam") })
      |> list.filter_map(fn(path) { parse_command_file(path, prefix) })
    }
    Error(_) -> []
  }
}

/// Extracts command metadata from source file. Derives name
/// from filename and parses description from const or builder
/// pattern to avoid requiring a specific export.
///
fn parse_command_file(path: String, prefix: String) -> Result(CommandInfo, Nil) {
  case simplifile.read(path) {
    Ok(content) -> {
      let name = path_to_command_name(path)
      let description = parse_description(content)
      let module = path_to_module(path, prefix)
      Ok(CommandInfo(name: name, description: description, module: module))
    }
    Error(_) -> Error(Nil)
  }
}

/// Uses filename as command name without prefix. Convention
/// over configuration - no need to specify command name in the
/// source file.
///
fn path_to_command_name(path: String) -> String {
  path
  |> string.split("/")
  |> list.last
  |> option.from_result
  |> option.unwrap("")
  |> string.replace(".gleam", "")
}

/// Converts file path to Gleam module path for dynamic loading.
/// Strips build directory prefixes and .gleam extension to get
/// the importable module name.
///
fn path_to_module(path: String, prefix: String) -> String {
  case prefix {
    "app" -> {
      path
      |> string.replace("src/", "")
      |> string.replace(".gleam", "")
    }
    _ -> {
      let src_marker = "/src/" <> prefix <> "/"
      case string.split_once(path, src_marker) {
        Ok(#(_, after)) -> prefix <> "/" <> string.replace(after, ".gleam", "")
        Error(_) -> path |> string.replace(".gleam", "")
      }
    }
  }
}

/// Tries multiple patterns to find description. Supports both
/// const description and builder pattern so commands can use
/// whichever style they prefer.
///
fn parse_description(content: String) -> String {
  case parse_const_description(content) {
    Some(desc) -> desc
    None ->
      case parse_builder_description(content) {
        Some(desc) -> desc
        None -> ""
      }
  }
}

/// Matches `const description = "..."` pattern. Simple and
/// explicit way to define description without needing to parse
/// the full command() function.
///
fn parse_const_description(content: String) -> option.Option(String) {
  let assert Ok(re) = regexp.from_string("const description = \"([^\"]*)\"")
  case regexp.scan(re, content) {
    [match, ..] ->
      case match.submatches {
        [Some(desc), ..] -> Some(desc)
        _ -> None
      }
    [] -> None
  }
}

/// Matches `command.description("...")` builder pattern.
/// Fallback for commands that set description via the builder
/// API instead of a const.
///
fn parse_builder_description(content: String) -> option.Option(String) {
  let assert Ok(re) = regexp.from_string("command\\.description\\(\"([^\"]*)\"")
  case regexp.scan(re, content) {
    [match, ..] ->
      case match.submatches {
        [Some(desc), ..] -> Some(desc)
        _ -> None
      }
    [] -> None
  }
}

/// Writes registry as JSON for fast loading at CLI startup.
/// JSON format makes it easy to inspect and debug, and avoids
/// needing a custom binary format.
///
fn write_registry(commands: List(CommandInfo)) -> Result(Nil, String) {
  let _ = simplifile.create_directory_all("priv/storage/framework/console")

  let registry =
    commands
    |> list.map(fn(cmd) {
      #(
        cmd.name,
        json.object([
          #("description", json.string(cmd.description)),
          #("module", json.string(cmd.module)),
        ]),
      )
    })
    |> json.object
    |> json.to_string

  case simplifile.write(registry_path, registry) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Failed to write " <> registry_path)
  }
}

/// Parses JSON back into dict for command lookup. Used by CLI
/// to find command module without loading all commands upfront
/// which keeps startup time fast.
///
fn parse_registry(content: String) -> Result(Dict(String, CommandInfo), Nil) {
  case json.parse(content, using: registry_decoder()) {
    Ok(commands) -> Ok(commands)
    Error(_) -> Error(Nil)
  }
}

/// Decodes the top-level registry object. Keys are command
/// names, values are command info objects containing
/// description and module path for dynamic loading.
///
fn registry_decoder() -> decode.Decoder(Dict(String, CommandInfo)) {
  decode.dict(decode.string, command_info_decoder())
}

/// Decodes a single command entry from the registry JSON. Name
/// field is set to empty string here since the actual name
/// comes from the dict key rather than the serialized value.
///
fn command_info_decoder() -> decode.Decoder(CommandInfo) {
  use description <- decode.field("description", decode.string)
  use module <- decode.field("module", decode.string)
  decode.success(CommandInfo(name: "", description: description, module: module))
}
