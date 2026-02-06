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

/// Represents a function parameter with its name and type.
/// This is used to track handler signature for flexible 
/// parameter ordering.
///
pub type FunctionParam {
  FunctionParam(name: String, param_type: String)
}

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
    params: List(FunctionParam),
  )
  ParsedRedirect(from: String, to: String, status: Int)
}

/// Result of parsing a controller file. Contains group-level
/// middleware that applies to all routes, the list of parsed
/// routes, and import validation flags.
///
pub type ParseResult {
  ParseResult(
    group_middleware: List(String),
    routes: List(ParsedRoute),
    has_wisp_request_import: Bool,
    has_ctx_context_import: Bool,
    validator_data_imports: List(String),
  )
}

// ------------------------------------------------------------- Private Types

/// Annotations accumulate across multiple doc comment lines
/// before a function declaration. This state tracks all the
/// annotations seen so far until a pub fn completes the route.
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

// ------------------------------------------------------------- Public Functions

/// Parses a controller file for route annotations. Extracts
/// group middleware from file-level comments and routes from
/// doc comments preceding handler functions.
///
pub fn parse(content: String) -> ParseResult {
  let group_middleware = extract_group_middleware(content)
  let routes = extract_routes(content, group_middleware)
  let has_wisp_request_import = check_wisp_request_import(content)
  let has_ctx_context_import = check_ctx_context_import(content)
  let validator_data_imports = extract_validator_data_imports(content)

  ParseResult(
    group_middleware:,
    routes:,
    has_wisp_request_import:,
    has_ctx_context_import:,
    validator_data_imports:,
  )
}

// ------------------------------------------------------------- Private Functions

/// Group middleware applies to all routes in a controller.
/// This allows common authentication or logging middleware
/// to be declared once at the file level rather than per-route.
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

/// Routes are defined via doc comments before handler functions.
/// This convention keeps route metadata close to the handler
/// code while allowing compile-time generation of dispatch code.
///
fn extract_routes(
  content: String,
  group_middleware: List(String),
) -> List(ParsedRoute) {
  let lines = string.split(content, "\n")
  parse_lines(lines, group_middleware, None, [])
}

/// Annotation values are quoted to allow paths with spaces
/// or special characters. The quotes must be properly matched
/// for the annotation to be considered valid.
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

/// Annotations can span multiple doc comment lines before the
/// function declaration. State accumulates until we hit a pub 
/// fn, at which point we create the route and reset for the 
/// next.
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
                  // Collect full signature (may span multiple lines)
                  let #(signature, remaining) =
                    collect_signature(line, rest, "")
                  let params = extract_fn_params(signature)
                  let new_routes =
                    create_routes_from_state(
                      state,
                      fn_name,
                      params,
                      group_middleware,
                    )
                  parse_lines(
                    remaining,
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

/// Function signatures in Gleam can span multiple lines when
/// there are many parameters. We need the complete signature
/// to extract all parameter names and types for validation.
///
fn collect_signature(
  current_line: String,
  remaining: List(String),
  acc: String,
) -> #(String, List(String)) {
  let new_acc = acc <> " " <> string.trim(current_line)

  // Check if we have the complete signature (contains the opening brace)
  case string.contains(new_acc, "{") {
    True -> #(new_acc, remaining)
    False -> {
      case remaining {
        [] -> #(new_acc, [])
        [next, ..rest] -> collect_signature(next, rest, new_acc)
      }
    }
  }
}

/// Each annotation type (@get, @middleware, etc) has its own
/// parser. We try each parser in sequence and use the first
/// that succeeds, preserving existing state if none match.
///
fn parse_annotation_line(
  line: String,
  state: AnnotationState,
) -> AnnotationState {
  let content = string.drop_start(line, 3) |> string.trim_start

  try_parse_method(content, state)
  |> result.lazy_or(fn() { try_parse_middleware(content, state) })
  |> result.lazy_or(fn() { try_parse_validator(content, state) })
  |> result.lazy_or(fn() { try_parse_redirect(content, state) })
  |> result.unwrap(state)
}

/// Route methods like @get, @post define which HTTP verb 
/// handles the route. The method is required for a valid route 
/// annotation and determines how the route matches incoming 
/// requests.
///
fn try_parse_method(
  content: String,
  state: AnnotationState,
) -> Result(AnnotationState, Nil) {
  let methods = ["get", "post", "put", "patch", "delete", "head", "options"]

  list.find_map(methods, fn(method) {
    let prefix = "@" <> method <> " "
    case string.starts_with(content, prefix) {
      True ->
        extract_quoted_arg(content, prefix)
        |> result.map(fn(path) {
          AnnotationState(..state, method: Some(method), path: Some(path))
        })
      False -> Error(Nil)
    }
  })
}

/// Route-level middleware allows specific routes to have
/// additional processing like rate limiting or caching that
/// doesn't apply to all routes in the controller.
///
fn try_parse_middleware(
  content: String,
  state: AnnotationState,
) -> Result(AnnotationState, Nil) {
  case string.starts_with(content, "@middleware ") {
    True ->
      extract_quoted_arg(content, "@middleware ")
      |> result.map(fn(mw) {
        AnnotationState(..state, middleware: [mw, ..state.middleware])
      })
    False -> Error(Nil)
  }
}

/// Validators handle form data parsing and validation before
/// the handler runs. Associating a validator with a route
/// generates the validation call in the compiled dispatcher.
///
fn try_parse_validator(
  content: String,
  state: AnnotationState,
) -> Result(AnnotationState, Nil) {
  case string.starts_with(content, "@validator ") {
    True ->
      extract_quoted_arg(content, "@validator ")
      |> result.map(fn(v) { AnnotationState(..state, validator: Some(v)) })
    False -> Error(Nil)
  }
}

/// Redirects allow old URLs to point to new handlers without
/// duplicating route logic. Permanent redirects (308) tell
/// browsers to cache the redirect, while temporary (303) don't.
///
fn try_parse_redirect(
  content: String,
  state: AnnotationState,
) -> Result(AnnotationState, Nil) {
  case string.starts_with(content, "@redirect_permanent ") {
    True ->
      extract_quoted_arg(content, "@redirect_permanent ")
      |> result.map(fn(path) {
        AnnotationState(..state, redirects: [#(path, 308), ..state.redirects])
      })
    False ->
      case string.starts_with(content, "@redirect ") {
        True ->
          extract_quoted_arg(content, "@redirect ")
          |> result.map(fn(path) {
            AnnotationState(..state, redirects: [
              #(path, 303),
              ..state.redirects
            ])
          })
        False -> Error(Nil)
      }
  }
}

/// The function name links annotations to their handler in
/// the generated dispatch code. We strip everything except
/// the identifier between `pub fn` and the opening paren.
///
fn extract_fn_name(line: String) -> String {
  let after_pub_fn = string.drop_start(line, 7)
  case string.split_once(after_pub_fn, "(") {
    Ok(#(name, _)) -> string.trim(name)
    Error(_) -> after_pub_fn |> string.trim
  }
}

/// Handler parameters determine what arguments the compiler
/// passes when calling the function. We need both names (for
/// route params) and types (for Request/Context/Data matching).
///
fn extract_fn_params(signature: String) -> List(FunctionParam) {
  // Extract content between first ( and last ) before ->
  case string.split_once(signature, "(") {
    Ok(#(_, after_paren)) -> {
      // Find the closing paren - need to handle nested types like Option(String)
      let params_str = extract_params_string(after_paren, 0, "")
      parse_params_string(params_str)
    }
    Error(_) -> []
  }
}

/// Types like `Option(String)` contain parentheses that aren't
/// parameter delimiters. Tracking depth ensures we find the
/// actual closing paren of the function signature.
///
fn extract_params_string(s: String, depth: Int, acc: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#("(", rest)) -> extract_params_string(rest, depth + 1, acc <> "(")
    Ok(#(")", rest)) -> {
      case depth {
        0 -> acc
        _ -> extract_params_string(rest, depth - 1, acc <> ")")
      }
    }
    Ok(#(char, rest)) -> extract_params_string(rest, depth, acc <> char)
    Error(_) -> acc
  }
}

/// After extracting the raw parameter string, we need to
/// split it into individual parameters while respecting
/// commas inside generic types like `Dict(String, Int)`.
///
fn parse_params_string(params_str: String) -> List(FunctionParam) {
  split_params(params_str, 0, "", [])
  |> list.filter_map(fn(param_str) {
    let trimmed = string.trim(param_str)
    case trimmed {
      "" -> Error(Nil)
      _ -> parse_single_param(trimmed)
    }
  })
}

/// Generic types contain commas that aren't parameter 
/// separators. Tracking parenthesis depth ensures we only split 
/// on commas at the top level of the parameter list.
///
fn split_params(
  s: String,
  depth: Int,
  current: String,
  acc: List(String),
) -> List(String) {
  case string.pop_grapheme(s) {
    Ok(#("(", rest)) -> split_params(rest, depth + 1, current <> "(", acc)
    Ok(#(")", rest)) -> split_params(rest, depth - 1, current <> ")", acc)
    Ok(#(",", rest)) -> {
      case depth {
        0 -> split_params(rest, 0, "", [current, ..acc])
        _ -> split_params(rest, depth, current <> ",", acc)
      }
    }
    Ok(#(char, rest)) -> split_params(rest, depth, current <> char, acc)
    Error(_) -> list.reverse([current, ..acc])
  }
}

/// Parameters may have type annotations or be untyped. Both
/// the name and type are needed for the compiler to determine
/// how to pass arguments (Request vs Context vs route params).
///
fn parse_single_param(param: String) -> Result(FunctionParam, Nil) {
  case string.split_once(param, ":") {
    Ok(#(name, type_str)) -> {
      let clean_name = string.trim(name)
      let clean_type = string.trim(type_str)
      Ok(FunctionParam(name: clean_name, param_type: clean_type))
    }
    Error(_) -> {
      // No type annotation - just a param name
      let clean_name = string.trim(param)
      case clean_name {
        "" -> Error(Nil)
        _ -> Ok(FunctionParam(name: clean_name, param_type: ""))
      }
    }
  }
}

/// Once we hit a pub fn, we have all annotations for that route.
/// This combines group and route middleware, creates the main
/// route, and generates any redirect routes pointing to it.
///
fn create_routes_from_state(
  state: AnnotationState,
  fn_name: String,
  params: List(FunctionParam),
  group_middleware: List(String),
) -> List(ParsedRoute) {
  case state.method, state.path {
    Some(method), Some(path) -> {
      // Reverse middleware since it was accumulated via prepending
      let route_middleware = list.reverse(state.middleware)
      let all_middleware = list.append(group_middleware, route_middleware)
      let main_route =
        ParsedRoute(
          method:,
          path:,
          handler: fn_name,
          middleware: all_middleware,
          validator: state.validator,
          params:,
        )

      // Create redirect routes (reverse since accumulated via prepending)
      let redirect_routes =
        state.redirects
        |> list.reverse
        |> list.map(fn(r) {
          let #(from, status) = r
          ParsedRedirect(from:, to: path, status:)
        })

      [main_route, ..redirect_routes]
    }
    _, _ -> []
  }
}

/// File paths from glob need to be converted to module paths
/// for generating import statements. The src/ prefix and .gleam
/// extension aren't part of the Gleam module path.
///
pub fn module_from_path(path: String) -> Result(String, Nil) {
  path
  |> string.replace(".gleam", "")
  |> string.split_once("src/")
  |> result.map(fn(parts) { parts.1 })
}

/// Types can be imported with or without the `type` keyword.
/// Both `import mod.{type Foo}` and `import mod.{Foo}` make
/// the type available, so we need to check for both patterns.
///
fn import_contains_type(line: String, type_name: String) -> Bool {
  case string.split_once(line, "{") {
    Ok(#(_, after_brace)) ->
      case string.split_once(after_brace, "}") {
        Ok(#(imports, _)) ->
          imports
          |> string.split(",")
          |> list.any(fn(imp) {
            let clean = string.trim(imp)
            clean == "type " <> type_name || clean == type_name
          })
        Error(_) -> False
      }
    Error(_) -> False
  }
}

/// Handlers can use `Request` as a type if it's imported from
/// wisp. Without the import, the compiler would fail with a
/// confusing error, so we validate this early for better DX.
///
fn check_wisp_request_import(content: String) -> Bool {
  content
  |> string.split("\n")
  |> list.any(fn(line) {
    let trimmed = string.trim(line)
    case string.starts_with(trimmed, "//") {
      True -> False
      False ->
        string.starts_with(trimmed, "import wisp.{")
        && import_contains_type(trimmed, "Request")
    }
  })
}

/// Handlers with validators can use `Data` as a type if they
/// import it from the validator module. We track which 
/// validators have Data imported to validate handler parameters.
///
fn extract_validator_data_imports(content: String) -> List(String) {
  content
  |> string.split("\n")
  |> list.filter_map(fn(line) {
    let trimmed = string.trim(line)
    case string.starts_with(trimmed, "//") {
      True -> Error(Nil)
      False ->
        case
          string.starts_with(trimmed, "import ")
          && string.contains(trimmed, "/validators/")
          && string.contains(trimmed, ".{")
          && import_contains_type(trimmed, "Data")
        {
          True ->
            case string.split_once(trimmed, "/validators/") {
              Ok(#(_, after_validators)) ->
                case string.split_once(after_validators, ".{") {
                  Ok(#(validator_name, _)) -> Ok(validator_name)
                  Error(_) -> Error(Nil)
                }
              Error(_) -> Error(Nil)
            }
          False -> Error(Nil)
        }
    }
  })
}

/// Handlers can use `Context` as a type if it's imported from
/// the ctx module. Without the import, the compiler would fail
/// with a confusing error, so we validate this early for better
/// DX.
///
fn check_ctx_context_import(content: String) -> Bool {
  content
  |> string.split("\n")
  |> list.any(fn(line) {
    let trimmed = string.trim(line)
    case string.starts_with(trimmed, "//") {
      True -> False
      False ->
        string.starts_with(trimmed, "import ")
        && string.contains(trimmed, "/ctx.{")
        && import_contains_type(trimmed, "Context")
    }
  })
}
