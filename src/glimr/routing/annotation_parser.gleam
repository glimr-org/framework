//// Annotation Parser
////
//// Parses route annotations from controller files. Extracts
//// HTTP method routes, middleware, and redirects from doc
//// comments preceding handler functions.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ------------------------------------------------------------- Public Types

/// Represents a parsed route from controller annotations.
/// Contains method, path, handler function name, middleware,
/// optional validator, and optional redirect configuration.
///
pub type ParsedRoute {
  ParsedRoute(
    method: String,
    path: String,
    handler: String,
    middleware: List(String),
    validator: Option(String),
  )
  ParsedRedirect(from: String, to: String, status: Int)
}

/// Result of parsing a controller file. Contains group-level
/// middleware that applies to all routes, and the list of
/// parsed routes.
///
pub type ParseResult {
  ParseResult(group_middleware: List(String), routes: List(ParsedRoute))
}

// ------------------------------------------------------------- Public Functions

/// Parses a controller file for route annotations. Extracts
/// group middleware from file-level comments and routes from
/// doc comments preceding handler functions.
///
pub fn parse(content: String) -> Result(ParseResult, String) {
  let group_middleware = extract_group_middleware(content)
  let routes = extract_routes(content, group_middleware)

  Ok(ParseResult(group_middleware:, routes:))
}

// ------------------------------------------------------------- Private Functions

/// Extracts group middleware from file-level comments. Looks
/// for `// @group_middleware "path"` at the top of the file
/// before any function definitions.
///
fn extract_group_middleware(content: String) -> List(String) {
  content
  |> string.split("\n")
  |> list.take_while(fn(line) {
    let trimmed = string.trim(line)
    !string.starts_with(trimmed, "pub fn")
    && !string.starts_with(trimmed, "fn ")
  })
  |> list.filter_map(fn(line) {
    let trimmed = string.trim(line)
    case string.starts_with(trimmed, "// @group_middleware ") {
      True -> extract_quoted_arg(trimmed, "// @group_middleware ")
      False -> Error(Nil)
    }
  })
}

/// Extracts routes from doc comments in the content. Scans
/// for `/// @method "path"` patterns followed by `pub fn`
/// declarations.
///
fn extract_routes(
  content: String,
  group_middleware: List(String),
) -> List(ParsedRoute) {
  let lines = string.split(content, "\n")
  parse_lines(lines, group_middleware, None, [])
}

/// Extracts a quoted string argument from an annotation.
/// Parses `@annotation "value"` to get just the value.
/// Returns Error if no valid quoted string is found.
///
fn extract_quoted_arg(line: String, prefix: String) -> Result(String, Nil) {
  let after_prefix = string.drop_start(line, string.length(prefix))

  case string.starts_with(after_prefix, "\"") {
    True -> {
      let after_quote = string.drop_start(after_prefix, 1)
      case string.split_once(after_quote, "\"") {
        Ok(#(arg, _)) -> Ok(arg)
        Error(_) -> Error(Nil)
      }
    }
    False -> Error(Nil)
  }
}

/// State for tracking annotations while parsing lines.
/// Accumulates method, path, middleware, validator, and
/// redirects until a pub fn declaration completes the route.
///
type AnnotationState {
  AnnotationState(
    method: Option(String),
    path: Option(String),
    middleware: List(String),
    validator: Option(String),
    redirects: List(#(String, Int)),
  )
}

/// Recursively parses lines to extract routes. Accumulates
/// annotations from doc comments and creates routes when
/// a pub fn declaration is found.
///
fn parse_lines(
  lines: List(String),
  group_middleware: List(String),
  current: Option(AnnotationState),
  acc: List(ParsedRoute),
) -> List(ParsedRoute) {
  case lines {
    [] -> list.reverse(acc)
    [line, ..rest] -> {
      let trimmed = string.trim(line)

      // Check if this is a doc comment with annotation
      case string.starts_with(trimmed, "///") {
        True -> {
          let state = case current {
            Some(s) -> s
            None ->
              AnnotationState(
                method: None,
                path: None,
                middleware: [],
                validator: None,
                redirects: [],
              )
          }
          let new_state = parse_annotation_line(trimmed, state)
          parse_lines(rest, group_middleware, Some(new_state), acc)
        }
        False -> {
          // Check if this is a pub fn declaration
          case string.starts_with(trimmed, "pub fn ") {
            True -> {
              case current {
                Some(state) -> {
                  let fn_name = extract_fn_name(trimmed)
                  let new_routes =
                    create_routes_from_state(state, fn_name, group_middleware)
                  parse_lines(
                    rest,
                    group_middleware,
                    None,
                    list.append(list.reverse(new_routes), acc),
                  )
                }
                None -> parse_lines(rest, group_middleware, None, acc)
              }
            }
            False -> {
              // Not a doc comment or pub fn, reset state if we hit
              // non-empty non-comment line
              case trimmed == "" || string.starts_with(trimmed, "//") {
                True -> parse_lines(rest, group_middleware, current, acc)
                False -> parse_lines(rest, group_middleware, None, acc)
              }
            }
          }
        }
      }
    }
  }
}

/// Parses a single annotation line and updates state. Handles
/// method annotations, middleware, validators, and redirects.
/// Returns updated state with any new annotation values added.
///
fn parse_annotation_line(
  line: String,
  state: AnnotationState,
) -> AnnotationState {
  let after_slashes = string.drop_start(line, 3) |> string.trim_start

  // Check for HTTP method annotations
  let methods = ["get", "post", "put", "patch", "delete", "head", "options"]
  let method_match =
    list.find(methods, fn(method) {
      string.starts_with(after_slashes, "@" <> method <> " ")
    })

  case method_match {
    Ok(method) -> {
      let prefix = "@" <> method <> " "
      case extract_quoted_arg(after_slashes, prefix) {
        Ok(path) ->
          AnnotationState(..state, method: Some(method), path: Some(path))
        Error(_) -> state
      }
    }
    Error(_) -> {
      // Check for middleware
      case string.starts_with(after_slashes, "@middleware ") {
        True -> {
          case extract_quoted_arg(after_slashes, "@middleware ") {
            Ok(mw) ->
              AnnotationState(
                ..state,
                middleware: list.append(state.middleware, [mw]),
              )
            Error(_) -> state
          }
        }
        False -> {
          // Check for validator
          case string.starts_with(after_slashes, "@validator ") {
            True -> {
              case extract_quoted_arg(after_slashes, "@validator ") {
                Ok(v) -> AnnotationState(..state, validator: Some(v))
                Error(_) -> state
              }
            }
            False -> {
              // Check for redirect_permanent
              case string.starts_with(after_slashes, "@redirect_permanent ") {
                True -> {
                  case
                    extract_quoted_arg(after_slashes, "@redirect_permanent ")
                  {
                    Ok(path) ->
                      AnnotationState(
                        ..state,
                        redirects: list.append(state.redirects, [#(path, 308)]),
                      )
                    Error(_) -> state
                  }
                }
                False -> {
                  // Check for redirect
                  case string.starts_with(after_slashes, "@redirect ") {
                    True -> {
                      case extract_quoted_arg(after_slashes, "@redirect ") {
                        Ok(path) ->
                          AnnotationState(
                            ..state,
                            redirects: list.append(state.redirects, [
                              #(path, 303),
                            ]),
                          )
                        Error(_) -> state
                      }
                    }
                    False -> state
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Extracts the function name from a pub fn declaration.
/// Parses `pub fn name(` to get just the name portion.
/// Trims whitespace and stops at the opening parenthesis.
///
fn extract_fn_name(line: String) -> String {
  let after_pub_fn = string.drop_start(line, 7)
  case string.split_once(after_pub_fn, "(") {
    Ok(#(name, _)) -> string.trim(name)
    Error(_) -> after_pub_fn |> string.trim
  }
}

/// Creates routes from accumulated annotation state. Generates
/// the main route and any redirect routes pointing to it.
/// Combines group middleware with route-specific middleware.
///
fn create_routes_from_state(
  state: AnnotationState,
  fn_name: String,
  group_middleware: List(String),
) -> List(ParsedRoute) {
  case state.method, state.path {
    Some(method), Some(path) -> {
      let all_middleware = list.append(group_middleware, state.middleware)
      let main_route =
        ParsedRoute(
          method:,
          path:,
          handler: fn_name,
          middleware: all_middleware,
          validator: state.validator,
        )

      // Create redirect routes
      let redirect_routes =
        list.map(state.redirects, fn(r) {
          let #(from, status) = r
          ParsedRedirect(from:, to: path, status:)
        })

      [main_route, ..redirect_routes]
    }
    _, _ -> []
  }
}

/// Extracts the module name from a controller file path.
/// Converts `src/app/http/controllers/user_controller.gleam`
/// to `app/http/controllers/user_controller`.
///
pub fn module_from_path(path: String) -> Result(String, Nil) {
  path
  |> string.replace(".gleam", "")
  |> string.split_once("src/")
  |> result.map(fn(parts) { parts.1 })
}
