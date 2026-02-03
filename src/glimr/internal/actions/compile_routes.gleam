//// Route Compiler Action
////
//// Compiles routes from controller annotations to executable
//// Gleam code. Discovers controllers in src/app/http/controllers
//// and generates separate route files per route group based on
//// URL prefix matching.
////

import gleam/int
import gleam/list
import gleam/string
import glimr/console/console
import glimr/config/route_groups.{type RouteGroupConfig}
import glimr/routing/annotation_parser
import glimr/routing/compiler
import simplifile

/// Compiles all controller files in src/app/http/controllers.
/// Discovers annotated handlers and generates compiled routes
/// files in compiled/routes, split by route group prefix.
///
pub fn run(verbose: Bool) -> Result(Nil, String) {
  case verbose {
    True -> {
      console.output()
      |> console.line_warning("Compiling routes...")
      |> console.print()
    }
    False -> Nil
  }

  // Ensure the output directory exists
  let _ = simplifile.create_directory_all("src/compiled/routes")

  let groups = route_groups.load()
  let controller_files = discover_controller_files("src/app/http/controllers")
  compile_controllers(controller_files, verbose, groups)
}

/// Compiles routes when specific controller files have changed.
/// Re-scans all controllers since routes from multiple controllers
/// are aggregated into output files.
///
pub fn run_for_controllers(
  _paths: List(String),
  verbose: Bool,
) -> Result(Nil, String) {
  // Always recompile all controllers since output is aggregated
  run(verbose)
}

/// Recursively finds all .gleam files in the controllers directory.
/// Returns list of file paths for controller files to parse.
///
fn discover_controller_files(dir: String) -> List(String) {
  case simplifile.get_files(dir) {
    Ok(files) ->
      files
      |> list.filter(fn(f) { string.ends_with(f, ".gleam") })
    Error(_) -> []
  }
}

/// Compiles all controller files into route files split by group.
/// Parses annotations from each controller and groups routes
/// by URL prefix for code generation.
///
fn compile_controllers(
  files: List(String),
  verbose: Bool,
  route_groups: List(RouteGroupConfig),
) -> Result(Nil, String) {
  // Parse all controller files
  let controller_results =
    files
    |> list.filter_map(fn(path) {
      case parse_controller(path) {
        Ok(#(module, result)) -> {
          case result.routes {
            [] -> Error(Nil)
            _ -> Ok(#(module, result))
          }
        }
        Error(_) -> Error(Nil)
      }
    })

  case list.is_empty(controller_results) {
    True -> {
      case verbose {
        True -> Nil
        False -> {
          console.output()
          |> console.unpadded()
          |> console.line_warning("No routes found in controllers")
          |> console.print()
        }
      }
      Ok(Nil)
    }
    False -> {
      // Group routes by their matching route group prefix
      let grouped = group_routes_by_prefix(controller_results, route_groups)

      // Compile each group to its own file
      compile_grouped_routes(grouped, verbose)
    }
  }
}

/// Groups controller routes by matching route group prefix.
/// Returns a list of (group_name, controller_results) tuples.
///
fn group_routes_by_prefix(
  controller_results: List(#(String, annotation_parser.ParseResult)),
  route_groups: List(RouteGroupConfig),
) -> List(#(String, List(#(String, annotation_parser.ParseResult)))) {
  // For each route group, collect routes that match its prefix
  route_groups
  |> list.map(fn(group) {
    let matching_controllers =
      controller_results
      |> list.filter_map(fn(entry) {
        let #(module, parse_result) = entry
        // Filter routes that match this group's prefix
        let matching_routes =
          parse_result.routes
          |> list.filter(fn(route) {
            route_matches_prefix(route, group.prefix, route_groups)
          })
        case matching_routes {
          [] -> Error(Nil)
          _ ->
            Ok(#(
              module,
              annotation_parser.ParseResult(
                group_middleware: parse_result.group_middleware,
                routes: matching_routes,
              ),
            ))
        }
      })
    #(group.name, matching_controllers)
  })
  |> list.filter(fn(entry) { !list.is_empty(entry.1) })
}

/// Checks if a route matches a group prefix. A route matches if
/// its path starts with the prefix AND no other more specific
/// prefix matches (longest prefix wins).
///
fn route_matches_prefix(
  route: annotation_parser.ParsedRoute,
  prefix: String,
  all_groups: List(RouteGroupConfig),
) -> Bool {
  let path = get_route_path(route)

  // Find the best matching prefix for this route
  let best_match =
    all_groups
    |> list.filter(fn(g) { path_starts_with_prefix(path, g.prefix) })
    |> list.sort(fn(a, b) {
      // Sort by prefix length descending (longest first)
      int.compare(string.length(b.prefix), string.length(a.prefix))
    })
    |> list.first

  case best_match {
    Ok(best) -> best.prefix == prefix
    Error(_) -> prefix == ""
  }
}

/// Gets the path from a ParsedRoute (handles both route types).
///
fn get_route_path(route: annotation_parser.ParsedRoute) -> String {
  case route {
    annotation_parser.ParsedRoute(path:, ..) -> path
    annotation_parser.ParsedRedirect(from:, ..) -> from
  }
}

/// Checks if a path starts with a given prefix.
///
fn path_starts_with_prefix(path: String, prefix: String) -> Bool {
  case prefix {
    "" -> True
    _ -> string.starts_with(path, prefix)
  }
}

/// Compiles grouped routes to separate files.
///
fn compile_grouped_routes(
  grouped: List(#(String, List(#(String, annotation_parser.ParseResult)))),
  verbose: Bool,
) -> Result(Nil, String) {
  let results =
    grouped
    |> list.map(fn(entry) {
      let #(group_name, controller_results) = entry
      case compiler.compile_routes(controller_results) {
        Ok(result) -> {
          let dest = "src/compiled/routes/" <> group_name <> ".gleam"
          case compiler.write_compiled_file(result, dest) {
            Ok(_) -> {
              case verbose {
                True -> {
                  let count =
                    list.fold(controller_results, 0, fn(acc, e) {
                      acc + list.length({ e.1 }.routes)
                    })
                  console.output()
                  |> console.unpadded()
                  |> console.line(
                    "  "
                    <> group_name
                    <> ".gleam -> "
                    <> console.success(int.to_string(count) <> " routes"),
                  )
                  |> console.print()
                }
                False -> Nil
              }
              Ok(Nil)
            }
            Error(err) -> Error(err)
          }
        }
        Error(err) -> Error(err)
      }
    })

  // Check for any errors
  case list.find(results, fn(r) { r != Ok(Nil) }) {
    Ok(Error(err)) -> Error(err)
    _ -> {
      case verbose {
        True -> Nil
        False -> {
          let total =
            list.fold(grouped, 0, fn(acc, entry) {
              list.fold(entry.1, acc, fn(a, e) {
                a + list.length({ e.1 }.routes)
              })
            })
          console.output()
          |> console.unpadded()
          |> console.line_success(
            "Compiled " <> int.to_string(total) <> " routes",
          )
          |> console.print()
        }
      }
      Ok(Nil)
    }
  }
}

/// Parses a single controller file for route annotations.
/// Returns the module path and parsed routes.
///
fn parse_controller(
  path: String,
) -> Result(#(String, annotation_parser.ParseResult), String) {
  case simplifile.read(path) {
    Ok(content) -> {
      case annotation_parser.module_from_path(path) {
        Ok(module) -> {
          let assert Ok(result) = annotation_parser.parse(content)
          Ok(#(module, result))
        }
        Error(_) -> Error("Failed to extract module path from: " <> path)
      }
    }
    Error(_) -> Error("Failed to read file: " <> path)
  }
}
