//// Route Compiler Action
////
//// Controller annotations declare routes inline next to their
//// handlers, but the router needs a single compiled module per
//// route group at startup. This action bridges that gap —
//// scanning controllers, grouping routes by URL prefix, and
//// generating one .gleam file per group so the router can load
//// them without parsing annotations at runtime.

import gleam/int
import gleam/list
import gleam/string
import glimr/config/route_group.{type RouteGroupConfig}
import glimr/console/console
import glimr/routing/annotation_parser
import glimr/routing/compiler
import simplifile

// ------------------------------------------------------------- Public Functions

/// Scans all controllers and regenerates every route group
/// file. A full rebuild is needed because a single controller
/// change can shift routes between groups when prefixes
/// overlap.
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

  let groups = route_group.load()
  let controller_files = discover_controller_files("src/app/http/controllers")
  compile_controllers(controller_files, verbose, groups)
}

/// Always delegates to a full run because routes from multiple
/// controllers are aggregated into shared output files —
/// recompiling just one controller could produce an incomplete
/// route file.
///
pub fn run_for_controllers(
  _paths: List(String),
  verbose: Bool,
) -> Result(Nil, String) {
  // Always recompile all controllers since output is aggregated
  run(verbose)
}

// ------------------------------------------------------------- Private Functions

/// Returns an empty list instead of erroring when the directory
/// doesn't exist, which handles fresh projects that haven't
/// created any controllers yet.
///
fn discover_controller_files(dir: String) -> List(String) {
  case simplifile.get_files(dir) {
    Ok(files) ->
      files
      |> list.filter(fn(f) { string.ends_with(f, ".gleam") })
    Error(_) -> []
  }
}

/// Parsing and grouping happen together so that a single error
/// in any controller is caught before any output files are
/// written — preventing a half-updated route set.
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

/// Each route group gets only the routes matching its prefix so
/// that group-level middleware doesn't apply to unrelated
/// routes. Longest-prefix-wins ensures /api/v2 routes don't
/// accidentally land in the /api group.
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
                has_wisp_request_import: parse_result.has_wisp_request_import,
                has_ctx_context_import: parse_result.has_ctx_context_import,
                validator_data_imports: parse_result.validator_data_imports,
              ),
            ))
        }
      })
    #(group.name, matching_controllers)
  })
  |> list.filter(fn(entry) { !list.is_empty(entry.1) })
}

/// Longest-prefix-wins prevents ambiguity when groups share a
/// common ancestor — e.g., /api/v2/users goes to the /api/v2
/// group, not /api, even though both prefixes match.
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

/// Unified accessor for both regular routes and redirects so
/// prefix matching doesn't need to branch on variant.
///
fn get_route_path(route: annotation_parser.ParsedRoute) -> String {
  case route {
    annotation_parser.ParsedRoute(path:, ..) -> path
    annotation_parser.ParsedRedirect(from:, ..) -> from
  }
}

/// Empty prefix matches everything — it acts as the catch-all
/// group for routes that don't belong to any more specific
/// prefix.
///
fn path_starts_with_prefix(path: String, prefix: String) -> Bool {
  case prefix {
    "" -> True
    _ -> string.starts_with(path, prefix)
  }
}

/// Writes one .gleam file per group and collects errors so that
/// all groups are attempted even if one fails — partial
/// compilation is better than no output at all during
/// development.
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

/// Reads source and extracts the module path in one step so
/// callers don't need to coordinate file I/O with path parsing.
///
fn parse_controller(
  path: String,
) -> Result(#(String, annotation_parser.ParseResult), String) {
  case simplifile.read(path) {
    Ok(content) -> {
      case annotation_parser.module_from_path(path) {
        Ok(module) -> {
          let result = annotation_parser.parse(content)
          Ok(#(module, result))
        }
        Error(_) -> Error("Failed to extract module path from: " <> path)
      }
    }
    Error(_) -> Error("Failed to read file: " <> path)
  }
}
