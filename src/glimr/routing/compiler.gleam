//// Route Compiler
////
//// Generates optimized dispatch code from parsed routes. Takes
//// routes from the annotation parser and produces pattern
//// matching code with middleware support.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option
import gleam/order
import gleam/regexp
import gleam/result
import gleam/string
import glimr/routing/annotation_parser.{
  type ParseResult, type ParsedRoute, ParsedRedirect, ParsedRoute,
}
import shellout
import simplifile

// ------------------------------------------------------------- Private Constants

/// Base path for middleware modules. Bare middleware names
/// are expanded to this path, e.g., "logger" becomes
/// "app/http/middleware/logger".
///
const middleware_base_path = "app/http/middleware/"

/// Base path for validator modules. Bare validator names
/// are expanded to this path, e.g., "user_validator" becomes
/// "app/http/validators/user_validator".
///
const validator_base_path = "app/http/validators/"

/// Expands a bare middleware name to its full module path.
/// "logger" becomes "app/http/middleware/logger".
///
fn expand_middleware_path(name: String) -> String {
  middleware_base_path <> name
}

/// Expands a bare validator name to its full module path.
/// "user_validator" becomes "app/http/validators/user_validator".
///
fn expand_validator_path(name: String) -> String {
  validator_base_path <> name
}

// ------------------------------------------------------------- Public Types

/// Result of compiling routes. Contains the extracted imports,
/// generated dispatch code, used HTTP methods, and line-to-route
/// mapping for error reporting.
///
pub type CompileResult {
  CompileResult(
    imports: List(String),
    routes_code: String,
    used_methods: List(String),
    uses_middleware: Bool,
    line_to_route: Dict(Int, String),
  )
}

// ------------------------------------------------------------- Public Functions

/// Compiles a list of parsed routes into dispatch code. Takes
/// controller module paths and their parse results, generating 
/// optimized pattern matching code.
///
pub fn compile_routes(
  controller_results: List(#(String, ParseResult)),
) -> Result(CompileResult, String) {
  // Extract routes from ParseResults for further processing
  let controller_routes =
    list.map(controller_results, fn(entry) {
      let #(module_path, result) = entry
      #(module_path, result.routes)
    })

  // Flatten all routes and prefix handlers with module name
  let routes =
    controller_routes
    |> list.flat_map(fn(entry) {
      let #(module_path, routes) = entry
      list.map(routes, fn(route) { prefix_handler(route, module_path) })
    })

  use _ <- result.try(validate_path_format(routes))
  use _ <- result.try(validate_path_params(routes))
  use _ <- result.try(validate_group_middleware(controller_results))
  use _ <- result.try(validate_route_middleware(controller_results))
  use _ <- result.try(validate_validators(controller_results))

  let imports = generate_imports(controller_routes, routes)
  let used_methods = collect_used_methods(routes)
  let uses_middleware = check_uses_middleware(routes)

  let #(routes_code, line_to_route) =
    generate_code(
      routes,
      list.length(imports),
      used_methods != [],
      uses_middleware,
    )

  Ok(CompileResult(
    imports:,
    routes_code:,
    used_methods:,
    uses_middleware:,
    line_to_route:,
  ))
}

// ------------------------------------------------------------- Private Functions

/// Generates a unique alias for a controller module path.
/// Takes everything after "controllers/" and joins with 
/// underscores.
///
/// e.g., "app/http/controllers/api/home_controller" -> "api_home_controller"
///
fn controller_alias(module_path: String) -> String {
  case string.split_once(module_path, "controllers/") {
    Ok(#(_, after)) -> string.replace(after, "/", "_")
    Error(_) -> {
      // Fallback to last segment if no "controllers/" found
      case string.split(module_path, "/") |> list.last {
        Ok(name) -> name
        Error(_) -> module_path
      }
    }
  }
}

/// Prefixes a route handler with the controller module alias.
/// Uses a unique alias based on path after "controllers/".
/// Redirects are returned unchanged since they have no handler.
///
fn prefix_handler(route: ParsedRoute, module_path: String) -> ParsedRoute {
  let alias = controller_alias(module_path)

  case route {
    ParsedRoute(method:, path:, handler:, middleware:, validator:) ->
      ParsedRoute(
        method:,
        path:,
        handler: alias <> "." <> handler,
        middleware:,
        validator:,
      )
    ParsedRedirect(..) -> route
  }
}

/// Generates import statements for controller, middleware,
/// and validator modules. Creates `import module/path` for each.
/// Uses `as` alias for nested controllers to avoid conflicts.
///
fn generate_imports(
  controller_routes: List(#(String, List(ParsedRoute))),
  routes: List(ParsedRoute),
) -> List(String) {
  let controller_imports =
    controller_routes
    |> list.filter(fn(entry) { !list.is_empty(entry.1) })
    |> list.map(fn(entry) {
      let module_path = entry.0
      let alias = controller_alias(module_path)
      let default_name = case string.split(module_path, "/") |> list.last {
        Ok(name) -> name
        Error(_) -> module_path
      }
      // Use `as` alias when the alias differs from the default module name
      case alias == default_name {
        True -> "import " <> module_path
        False -> "import " <> module_path <> " as " <> alias
      }
    })

  let middleware_imports =
    routes
    |> list.flat_map(fn(r) {
      case r {
        ParsedRoute(middleware:, ..) -> middleware
        ParsedRedirect(..) -> []
      }
    })
    |> list.unique
    |> list.map(fn(m) { "import " <> expand_middleware_path(m) })

  let validator_imports =
    routes
    |> list.filter_map(fn(r) {
      case r {
        ParsedRoute(validator: option.Some(v), ..) ->
          Ok("import " <> expand_validator_path(v))
        _ -> Error(Nil)
      }
    })
    |> list.unique

  list.flatten([controller_imports, middleware_imports, validator_imports])
  |> list.unique
}

/// Checks if any route uses middleware. Used to determine
/// whether to include the middleware import in the generated
/// code.
///
fn check_uses_middleware(routes: List(ParsedRoute)) -> Bool {
  list.any(routes, fn(r) {
    case r {
      ParsedRoute(middleware:, ..) -> middleware != []
      ParsedRedirect(..) -> False
    }
  })
}

/// Validates route path format. Paths must start with / and 
/// contain only valid characters: letters, numbers, hyphens, 
/// underscores, slashes, and colon parameters.
///
fn validate_path_format(routes: List(ParsedRoute)) -> Result(Nil, String) {
  let invalid =
    routes
    |> list.filter_map(fn(r) {
      case r {
        ParsedRoute(path:, handler:, ..) ->
          case is_valid_path(path) {
            True -> Error(Nil)
            False -> Ok(#(path, handler))
          }
        ParsedRedirect(from:, to:, ..) ->
          case is_valid_path(from) {
            True -> Error(Nil)
            False -> Ok(#(from, "redirect to " <> to))
          }
      }
    })

  case invalid {
    [] -> Ok(Nil)
    [#(path, handler), ..] ->
      Error(
        "Invalid route '"
        <> path
        <> "' for "
        <> handler
        <> "\n"
        <> "Path must contain only slashes, letters, numbers, hyphens, underscores, or :params",
      )
  }
}

/// Checks if a path is valid using regex. Valid segments are 
/// either static (letters, numbers, hyphens, underscores) or 
/// parameters (colon followed by valid identifier).
///
fn is_valid_path(path: String) -> Bool {
  // Static segment: [a-zA-Z0-9_-]+
  // Param segment: :[a-zA-Z_][a-zA-Z0-9_]*
  let assert Ok(re) =
    regexp.from_string("^(/([a-zA-Z0-9_-]+|:[a-zA-Z_][a-zA-Z0-9_]*))+$|^/$")
  regexp.check(re, path)
}

/// Validates that no path parameters use reserved names.
/// Returns an error if req, _req, ctx, or _ctx are used
/// as path parameter names.
///
fn validate_path_params(routes: List(ParsedRoute)) -> Result(Nil, String) {
  let reserved = ["req", "_req", "ctx", "_ctx"]

  let invalid =
    routes
    |> list.filter_map(fn(r) {
      case r {
        ParsedRoute(path:, ..) -> {
          let params = extract_params_from_path(path)
          let bad = list.filter(params, fn(p) { list.contains(reserved, p) })
          case bad {
            [] -> Error(Nil)
            _ -> Ok(#(path, bad))
          }
        }
        ParsedRedirect(..) -> Error(Nil)
      }
    })

  case invalid {
    [] -> Ok(Nil)
    [#(path, params), ..] -> {
      let names = string.join(params, ", ")
      Error(
        "Reserved path parameter name in route '"
        <> path
        <> "': "
        <> names
        <> "\nThese names are reserved: req, _req, ctx, _ctx",
      )
    }
  }
}

/// Type of middleware validation error. Indicates whether
/// the middleware module file was not found or exists but
/// lacks the required public run function.
///
type MiddlewareError {
  MiddlewareNotFound(path: String)
  MiddlewareMissingHandle(path: String)
}

/// Checks if a middleware module is valid. Returns an error
/// if the file doesn't exist or lacks a public run function.
/// Searches both src/ and test/fixtures/ directories.
///
fn check_middleware(mw: String) -> Result(Nil, MiddlewareError) {
  let full_path = expand_middleware_path(mw)
  let src_path = "src/" <> full_path <> ".gleam"
  let test_path = "test/fixtures/" <> full_path <> ".gleam"

  // Try src/ first, then test/fixtures/ for test environments
  let file_path = case simplifile.is_file(src_path) {
    Ok(True) -> Ok(src_path)
    _ ->
      case simplifile.is_file(test_path) {
        Ok(True) -> Ok(test_path)
        _ -> Error(Nil)
      }
  }

  case file_path {
    Ok(path) -> {
      case simplifile.read(path) {
        Ok(content) -> {
          case string.contains(content, "pub fn run(") {
            True -> Ok(Nil)
            False -> Error(MiddlewareMissingHandle(mw))
          }
        }
        Error(_) -> Error(MiddlewareNotFound(mw))
      }
    }
    Error(_) -> Error(MiddlewareNotFound(mw))
  }
}

/// Formats a middleware error message for display to users.
/// Converts the error variant into a readable string that
/// explains what went wrong with the middleware module.
///
fn format_middleware_error(err: MiddlewareError) -> String {
  case err {
    MiddlewareNotFound(mw) -> "Middleware \"" <> mw <> "\" doesn't exist"
    MiddlewareMissingHandle(mw) ->
      "Middleware \"" <> mw <> "\" doesn't have a public \"run\" function"
  }
}

/// Validates that all group middleware modules exist and have
/// run function. Returns an error with controller-level message
/// if any group middleware is invalid or missing.
///
fn validate_group_middleware(
  controller_results: List(#(String, ParseResult)),
) -> Result(Nil, String) {
  let invalid =
    controller_results
    |> list.filter_map(fn(entry) {
      let #(module_path, result) = entry
      let alias = controller_alias(module_path)
      let errors =
        list.filter_map(result.group_middleware, fn(mw) {
          case check_middleware(mw) {
            Ok(_) -> Error(Nil)
            Error(err) -> Ok(err)
          }
        })
      case errors {
        [] -> Error(Nil)
        [err, ..] -> Ok(#(alias, err))
      }
    })

  case invalid {
    [] -> Ok(Nil)
    [#(controller, err), ..] ->
      Error(
        "Invalid group middleware for "
        <> controller
        <> "\n"
        <> format_middleware_error(err),
      )
  }
}

/// Validates that all route-specific middleware modules exist
/// and have run function. Returns an error with route-level
/// message if any route middleware is invalid or missing.
///
fn validate_route_middleware(
  controller_results: List(#(String, ParseResult)),
) -> Result(Nil, String) {
  let invalid =
    controller_results
    |> list.flat_map(fn(entry) {
      let #(module_path, result) = entry
      let alias = controller_alias(module_path)
      let group_mw = result.group_middleware
      list.filter_map(result.routes, fn(r) {
        case r {
          ParsedRoute(path:, handler:, middleware:, ..) -> {
            // Only check route-specific middleware (exclude group middleware)
            let route_specific =
              list.filter(middleware, fn(mw) { !list.contains(group_mw, mw) })
            let errors =
              list.filter_map(route_specific, fn(mw) {
                case check_middleware(mw) {
                  Ok(_) -> Error(Nil)
                  Error(err) -> Ok(err)
                }
              })
            case errors {
              [] -> Error(Nil)
              [err, ..] -> Ok(#(path, alias <> "." <> handler, err))
            }
          }
          ParsedRedirect(..) -> Error(Nil)
        }
      })
    })

  case invalid {
    [] -> Ok(Nil)
    [#(path, handler, err), ..] ->
      Error(
        "Invalid route '"
        <> path
        <> "' for "
        <> handler
        <> "\n"
        <> format_middleware_error(err),
      )
  }
}

/// Type of validator validation error. Indicates whether the 
/// validator module file was not found or exists but lacks the 
/// required public validate function.
///
type ValidatorError {
  ValidatorNotFound(path: String)
  ValidatorMissingValidate(path: String)
}

/// Checks if a validator module is valid. Returns an error
/// if the file doesn't exist or lacks a public validate
/// function. Searches both src/ and test/fixtures/ directories.
///
fn check_validator(v: String) -> Result(Nil, ValidatorError) {
  let full_path = expand_validator_path(v)
  let src_path = "src/" <> full_path <> ".gleam"
  let test_path = "test/fixtures/" <> full_path <> ".gleam"

  // Try src/ first, then test/fixtures/ for test environments
  let file_path = case simplifile.is_file(src_path) {
    Ok(True) -> Ok(src_path)
    _ ->
      case simplifile.is_file(test_path) {
        Ok(True) -> Ok(test_path)
        _ -> Error(Nil)
      }
  }

  case file_path {
    Ok(path) -> {
      case simplifile.read(path) {
        Ok(content) -> {
          case string.contains(content, "pub fn validate(") {
            True -> Ok(Nil)
            False -> Error(ValidatorMissingValidate(v))
          }
        }
        Error(_) -> Error(ValidatorNotFound(v))
      }
    }
    Error(_) -> Error(ValidatorNotFound(v))
  }
}

/// Formats a validator error message for display to users.
/// Converts the error variant into a readable string that
/// explains what went wrong with the validator module.
///
fn format_validator_error(err: ValidatorError) -> String {
  case err {
    ValidatorNotFound(v) -> "Validator \"" <> v <> "\" doesn't exist"
    ValidatorMissingValidate(v) ->
      "Validator \"" <> v <> "\" doesn't have a public \"validate\" function"
  }
}

/// Validates that all validator modules exist and have validate 
/// function. Returns an error with route-level message if any 
/// validator is invalid or missing.
///
fn validate_validators(
  controller_results: List(#(String, ParseResult)),
) -> Result(Nil, String) {
  let invalid =
    controller_results
    |> list.flat_map(fn(entry) {
      let #(module_path, result) = entry
      let alias = controller_alias(module_path)
      list.filter_map(result.routes, fn(r) {
        case r {
          ParsedRoute(path:, handler:, validator: option.Some(v), ..) -> {
            case check_validator(v) {
              Ok(_) -> Error(Nil)
              Error(err) -> Ok(#(path, alias <> "." <> handler, err))
            }
          }
          _ -> Error(Nil)
        }
      })
    })

  case invalid {
    [] -> Ok(Nil)
    [#(path, handler, err), ..] ->
      Error(
        "Invalid route '"
        <> path
        <> "' for "
        <> handler
        <> "\n"
        <> format_validator_error(err),
      )
  }
}

/// Collects unique HTTP methods used across all routes.
/// Returns capitalized method names for use in the http
/// import statement.
///
fn collect_used_methods(routes: List(ParsedRoute)) -> List(String) {
  routes
  |> list.filter_map(fn(r) {
    case r {
      ParsedRoute(method:, ..) -> Ok(string.capitalise(method))
      ParsedRedirect(..) -> Error(Nil)
    }
  })
  |> list.unique
}

/// Generates the dispatch code from parsed routes. Groups
/// routes by path, sorts them, and generates case expressions
/// with line mapping.
///
fn generate_code(
  routes: List(ParsedRoute),
  import_count: Int,
  uses_methods: Bool,
  uses_middleware: Bool,
) -> #(String, Dict(Int, String)) {
  let grouped = group_routes_by_path(routes)

  let sorted_routes =
    grouped
    |> dict.to_list
    |> list.sort(fn(a, b) { compare_paths(a.0, b.0) })

  // Calculate starting line based on what imports are actually generated
  let extra_lines =
    case uses_methods {
      True -> 1
      False -> 0
    }
    + case uses_middleware {
      True -> 1
      False -> 0
    }
    + 13
  // 13 = wisp import + blank + 8 comment lines + blank + fn line + case line
  let start_line = import_count + extra_lines

  let #(cases, line_to_route) =
    generate_path_cases_with_lines(sorted_routes, start_line, [], dict.new())

  let code = "  case path {\n" <> cases <> "\n\n    _ -> wisp.not_found()\n  }"
  #(code, line_to_route)
}

/// Generates case clauses with line number tracking. Maps
/// generated lines to route paths for error reporting when
/// compilation fails.
///
fn generate_path_cases_with_lines(
  routes: List(#(String, List(ParsedRoute))),
  current_line: Int,
  acc_cases: List(String),
  acc_mapping: Dict(Int, String),
) -> #(String, Dict(Int, String)) {
  case routes {
    [] -> #(string.join(list.reverse(acc_cases), "\n\n"), acc_mapping)
    [entry, ..rest] -> {
      let #(path, _) = entry
      let case_code = generate_path_case(entry)
      let case_lines = string.split(case_code, "\n") |> list.length

      // Map all lines of this case to the route path
      let new_mapping =
        list.range(current_line, current_line + case_lines - 1)
        |> list.fold(acc_mapping, fn(m, line) { dict.insert(m, line, path) })

      generate_path_cases_with_lines(
        rest,
        current_line + case_lines + 1,
        // +1 for blank line between cases
        [case_code, ..acc_cases],
        new_mapping,
      )
    }
  }
}

/// Compares paths for sorting in the generated code. Static
/// paths come before parameterized paths to ensure correct
/// matching order.
///
fn compare_paths(a: String, b: String) -> order.Order {
  let a_segments = path_to_segments(a)
  let b_segments = path_to_segments(b)

  let a_has_params = list.any(a_segments, is_param_segment)
  let b_has_params = list.any(b_segments, is_param_segment)

  case a_has_params, b_has_params {
    False, True -> order.Lt
    True, False -> order.Gt
    _, _ -> string.compare(a, b)
  }
}

/// Groups routes by their path. Multiple methods on the same
/// path are grouped together to generate a single case
/// clause.
///
fn group_routes_by_path(
  routes: List(ParsedRoute),
) -> Dict(String, List(ParsedRoute)) {
  list.fold(routes, dict.new(), fn(acc, route) {
    let path = case route {
      ParsedRoute(path:, ..) -> path
      ParsedRedirect(from:, ..) -> from
    }

    case dict.get(acc, path) {
      Ok(existing) -> dict.insert(acc, path, list.append(existing, [route]))
      Error(_) -> dict.insert(acc, path, [route])
    }
  })
}

/// Generates a single path case clause. Creates the pattern
/// match for the path and generates method dispatch in the
/// body.
///
fn generate_path_case(entry: #(String, List(ParsedRoute))) -> String {
  let #(path, routes) = entry
  let pattern = path_to_pattern(path)
  let body = generate_method_cases(routes)

  "    " <> pattern <> " ->\n" <> body
}

/// Converts a path to a pattern match expression. Static
/// segments become string literals, parameters become
/// variable bindings.
///
fn path_to_pattern(path: String) -> String {
  let segments = path_to_segments(path)

  case segments {
    [] -> "[]"
    _ -> {
      let patterns =
        list.map(segments, fn(seg) {
          case is_param_segment(seg) {
            True -> extract_param_name(seg)
            False -> "\"" <> seg <> "\""
          }
        })
      "[" <> string.join(patterns, ", ") <> "]"
    }
  }
}

/// Splits a path into segments. Removes empty segments from
/// leading/trailing slashes and returns clean segment
/// list.
///
fn path_to_segments(path: String) -> List(String) {
  path
  |> trim_slashes
  |> string.split("/")
  |> list.filter(fn(s) { s != "" })
}

/// Checks if a path segment is a parameter. Parameters start
/// with colon like :id or :user_id in route definitions.
/// Returns True for parameters, False for static segments.
///
fn is_param_segment(segment: String) -> Bool {
  string.starts_with(segment, ":")
}

/// Extracts the parameter name from a segment. Removes the
/// leading colon to get the variable name for binding in
/// the generated pattern match expression.
///
fn extract_param_name(segment: String) -> String {
  string.drop_start(segment, 1)
}

/// Generates method dispatch for a path's routes. Handles
/// redirects specially and generates method matching with
/// appropriate error responses.
///
fn generate_method_cases(routes: List(ParsedRoute)) -> String {
  let first = list.first(routes)

  case first {
    Ok(ParsedRedirect(to:, status:, ..)) -> {
      let redirect_fn = case status {
        308 -> "wisp.permanent_redirect"
        _ -> "wisp.redirect"
      }
      "      " <> redirect_fn <> "(\"" <> to <> "\")"
    }
    _ -> {
      let method_routes =
        list.filter_map(routes, fn(r) {
          case r {
            ParsedRoute(method:, path:, handler:, middleware:, validator:) ->
              Ok(#(method, path, handler, middleware, validator))
            ParsedRedirect(..) -> Error(Nil)
          }
        })

      case method_routes {
        [] -> "      wisp.not_found()"
        [#(method, path, handler, middleware, validator)] -> {
          let method_upper = string.capitalise(method)
          let params = extract_params_from_path(path)
          let handler_call =
            generate_handler_call(handler, params, middleware, validator)
          "      case method {\n        "
          <> method_upper
          <> " -> "
          <> handler_call
          <> "\n        _ -> wisp.method_not_allowed(["
          <> method_upper
          <> "])\n      }"
        }
        _ -> {
          let methods =
            list.map(method_routes, fn(r) { string.capitalise(r.0) })
          let methods_list = string.join(methods, ", ")

          let cases =
            list.map(method_routes, fn(r) {
              let #(method, path, handler, middleware, validator) = r
              let method_upper = string.capitalise(method)
              let params = extract_params_from_path(path)
              let handler_call =
                generate_handler_call(handler, params, middleware, validator)
              "        " <> method_upper <> " -> " <> handler_call
            })
            |> string.join("\n")

          "      case method {\n"
          <> cases
          <> "\n        _ -> wisp.method_not_allowed(["
          <> methods_list
          <> "])\n      }"
        }
      }
    }
  }
}

/// Extracts parameter names from a path. Returns list of
/// variable names that will be bound in the pattern match
/// for handler calls.
///
fn extract_params_from_path(path: String) -> List(String) {
  path_to_segments(path)
  |> list.filter(is_param_segment)
  |> list.map(extract_param_name)
}

/// Generates the handler function call. Handles middleware
/// and validator wrapping, passes appropriate arguments based
/// on handler type.
///
fn generate_handler_call(
  handler: String,
  params: List(String),
  middleware: List(String),
  validator: option.Option(String),
) -> String {
  // Build args: req, ctx, ...path_params, [validated]
  let base_args = ["req", "ctx"] |> list.append(params)
  let args = case validator {
    option.Some(_) -> list.append(base_args, ["validated"])
    option.None -> base_args
  }
  let call = handler <> "(" <> string.join(args, ", ") <> ")"

  // Wrap with validator if present
  let with_validator = case validator {
    option.Some(v) -> {
      let validator_alias = case string.split(v, "/") |> list.last {
        Ok(name) -> name
        Error(_) -> v
      }
      "{\n          use validated <- "
      <> validator_alias
      <> ".validate(req, ctx)\n          "
      <> call
      <> "\n        }"
    }
    option.None -> call
  }

  // Wrap with middleware if present
  case middleware {
    [] -> with_validator
    _ -> {
      let middleware_calls =
        middleware
        |> list.map(middleware_path_to_call)
        |> string.join(", ")
      let middleware_list = "[" <> middleware_calls <> "]"
      "{\n          use req, ctx <- middleware.apply("
      <> middleware_list
      <> ", req, ctx)\n          "
      <> with_validator
      <> "\n        }"
    }
  }
}

/// Converts a middleware module path to a function call.
/// Extracts the module alias from the path and appends .run
/// to create the callable reference for middleware.apply.
///
fn middleware_path_to_call(path: String) -> String {
  let alias = case string.split(path, "/") |> list.last {
    Ok(name) -> name
    Error(_) -> path
  }
  alias <> ".run"
}

/// Removes leading and trailing slashes from a string. Used
/// for normalizing path segments before joining them
/// together.
///
fn trim_slashes(s: String) -> String {
  s
  |> trim_start_char("/")
  |> trim_end_char("/")
}

/// Recursively removes a character from the start of a string.
/// Continues until the string no longer starts with the
/// given character.
///
fn trim_start_char(s: String, char: String) -> String {
  case string.starts_with(s, char) {
    True -> trim_start_char(string.drop_start(s, 1), char)
    False -> s
  }
}

/// Recursively removes a character from the end of a string.
/// Continues until the string no longer ends with the
/// given character.
///
fn trim_end_char(s: String, char: String) -> String {
  case string.ends_with(s, char) {
    True -> trim_end_char(string.drop_end(s, 1), char)
    False -> s
  }
}

/// Writes the compiled route file to disk. Assembles imports,
/// dispatch code, and function wrapper, then validates with
/// gleam check before formatting.
///
pub fn write_compiled_file(
  compile_result: CompileResult,
  dest_path: String,
) -> Result(Nil, String) {
  let imports_str = string.join(compile_result.imports, "\n")

  let has_routes = compile_result.used_methods != []

  let http_import = case compile_result.used_methods {
    [] -> ""
    methods -> "\nimport gleam/http.{" <> string.join(methods, ", ") <> "}"
  }

  let middleware_import = case compile_result.uses_middleware {
    True -> "\nimport glimr/http/middleware"
    False -> ""
  }

  let fn_args = case has_routes {
    True -> "path, method, req, ctx"
    False -> "path, _method, _req, _ctx"
  }

  let generated_comment =
    "// This file was generated by Glimr ✨
// https://github.com/glimr-org/glimr?tab=readme-ov-file#routes
//
// Do not edit this file. If you would like to use plain pattern
// matching over this compiled route approach, take a look at
// the docs link below detailing how to do so.
//
// See: https://github.com/glimr-org/glimr?tab=readme-ov-file#direct-pattern-matching

"

  let content =
    generated_comment
    <> imports_str
    <> http_import
    <> middleware_import
    <> "\nimport wisp"
    <> "\n\npub fn routes("
    <> fn_args
    <> ") {\n"
    <> compile_result.routes_code
    <> "\n}\n"

  let previous_content = simplifile.read(dest_path) |> result.unwrap("")

  case simplifile.write(dest_path, content) {
    Ok(_) -> {
      // Check BEFORE formatting so line numbers match our mapping
      case shellout.command("gleam", ["check"], in: ".", opt: []) {
        Ok(_) -> {
          let _ =
            shellout.command("gleam", ["format", dest_path], in: ".", opt: [])
          Ok(Nil)
        }
        Error(#(_, msg)) -> {
          case string.contains(msg, dest_path) {
            True -> {
              let _ = simplifile.write(dest_path, previous_content)
              let route_path =
                find_route_from_error(
                  msg,
                  dest_path,
                  compile_result.line_to_route,
                )
              let route_hint = case route_path {
                Ok(path) -> " (route: " <> path <> ")"
                Error(_) -> ""
              }
              let specific_hint = get_specific_error_hint(msg)
              Error(
                "Failed to compile "
                <> dest_path
                <> route_hint
                <> "\n"
                <> specific_hint,
              )
            }
            False -> {
              let _ =
                shellout.command(
                  "gleam",
                  ["format", dest_path],
                  in: ".",
                  opt: [],
                )
              Ok(Nil)
            }
          }
        }
      }
    }
    Error(_) -> Error("Failed to write file: " <> dest_path)
  }
}

/// Finds the route path associated with a compile error.
/// Uses line number from error message and line-to-route
/// mapping to identify the problematic route.
///
fn find_route_from_error(
  error_msg: String,
  file_path: String,
  line_to_route: Dict(Int, String),
) -> Result(String, Nil) {
  extract_line_number_for_file(error_msg, file_path)
  |> result.try(fn(line) { dict.get(line_to_route, line) })
}

/// Extracts line number from a Gleam error message. Parses
/// the "file:line:column" format to find where the error
/// occurred.
///
fn extract_line_number_for_file(
  msg: String,
  file_path: String,
) -> Result(Int, Nil) {
  // Error format is like "src/compiled/routes/api.gleam:12:7"
  // We need to find the specific file, not just any .gleam file
  case string.split_once(msg, file_path <> ":") {
    Ok(#(_, after)) -> {
      case string.split_once(after, ":") {
        Ok(#(line_str, _)) -> int.parse(line_str) |> result.replace_error(Nil)
        Error(_) -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}

/// Analyzes a Gleam compiler error message and returns a
/// specific hint. Detects common issues like arity mismatches
/// and unknown functions to provide helpful guidance.
///
fn get_specific_error_hint(msg: String) -> String {
  let is_arity_error =
    string.contains(msg, "Expected") && string.contains(msg, "arguments")
  let is_unknown_var = string.contains(msg, "Unknown variable")
  let is_unknown_module = string.contains(msg, "Unknown module")
  let is_not_function = string.contains(msg, "not a function")
  let is_wrong_return =
    string.contains(msg, "Response") && string.contains(msg, "Type mismatch")
  let is_param_order_mismatch =
    string.contains(msg, "Type mismatch")
    && string.contains(msg, "validated")
    && !is_wrong_return

  case Nil {
    _ if is_param_order_mismatch ->
      "Handler function has incorrect parameter order.\n"
      <> "Expected: fn(req, ctx, ...path_params, validated_data)"
    _ if is_arity_error ->
      "Handler function has incorrect number of parameters.\n"
      <> "Expected signature: fn(req, ctx) or fn(req, ctx, ...path_params)"
    _ if is_wrong_return ->
      "Handler function must return a wisp.Response.\n"
      <> "Make sure your handler returns a Response type."
    _ if is_unknown_var ->
      "Handler function not found.\n"
      <> "Make sure the function exists in the controller."
    _ if is_unknown_module ->
      "Controller module not found.\n"
      <> "Make sure the controller file exists."
    _ if is_not_function ->
      "Handler is not a function.\n"
      <> "Make sure you're referencing a function, not a value."
    _ ->
      "See: https://github.com/glimr-org/glimr?tab=readme-ov-file#route-handler-setup"
  }
}
