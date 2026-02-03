//// Route Groups Configuration
////
//// Loads and parses route group configuration from the
//// config/route_groups.toml file.

import gleam/dict
import gleam/list
import glimr/http/kernel.{type MiddlewareGroup}
import simplifile
import tom

/// Configuration for a route group used at compile time.
/// Defines the output file name, URL prefix for matching, and
/// middleware group. Used by the route compiler to split routes
/// into separate files.
///
pub type RouteGroupConfig {
  RouteGroupConfig(name: String, prefix: String, middleware: MiddlewareGroup)
}

/// Loads route group configuration from config/route_group.toml.
/// Returns a list of RouteGroupConfig parsed from the TOML file.
/// Returns an empty list if the file doesn't exist or cannot be parsed.
///
pub fn load() -> List(RouteGroupConfig) {
  case simplifile.read("config/route_group.toml") {
    Ok(content) -> parse(content)
    Error(_) -> []
  }
}

/// Parses route groups from TOML content. Extracts [groups.*]
/// sections and converts each to a RouteGroupConfig with name,
/// prefix, and middleware group.
///
fn parse(content: String) -> List(RouteGroupConfig) {
  case tom.parse(content) {
    Ok(toml) -> {
      case dict.get(toml, "groups") {
        Ok(tom.Table(groups)) -> {
          groups
          |> dict.to_list
          |> list.map(fn(entry) {
            let #(name, group_toml) = entry
            parse_group(name, group_toml)
          })
        }
        _ -> []
      }
    }
    Error(_) -> []
  }
}

/// Parses a single route group from its TOML table.
/// Extracts prefix and middleware fields, mapping middleware
/// string to the MiddlewareGroup type.
///
fn parse_group(name: String, toml: tom.Toml) -> RouteGroupConfig {
  let prefix = get_string(toml, "prefix", "")
  let middleware_str = get_string(toml, "middleware", "web")

  let middleware = case middleware_str {
    "web" -> kernel.Web
    "api" -> kernel.Api
    _ -> kernel.Custom(middleware_str)
  }

  RouteGroupConfig(name: name, prefix: prefix, middleware: middleware)
}

/// Gets a string value from a TOML table by key.
/// Returns the default if the key doesn't exist or is not a string.
///
fn get_string(toml: tom.Toml, key: String, default: String) -> String {
  case toml {
    tom.Table(table) -> {
      case dict.get(table, key) {
        Ok(tom.String(s)) -> s
        _ -> default
      }
    }
    _ -> default
  }
}
