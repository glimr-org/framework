//// Route Compiler
////
//// Parses route definition files and generates optimized dispatch
//// code. Handles route grouping, middleware, and path parameters.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/order
import gleam/result
import gleam/string
import shellout
import simplifile

// ------------------------------------------------------------- Public Types

/// Represents a parsed route from a route definition file.
/// ParsedRoute holds HTTP method routes with handlers and
/// middleware. ParsedRedirect holds redirect configurations.
///
pub type ParsedRoute {
  ParsedRoute(
    method: String,
    path: String,
    handler: String,
    middleware: List(String),
  )
  ParsedRedirect(from: String, to: String, status: Int)
}

/// Result of compiling a route file. Contains the extracted
/// imports, generated dispatch code, used HTTP methods, and
/// line-to-route mapping for error reporting.
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

/// Compiles a route definition file into dispatch code. Reads
/// the source file, parses routes, and generates optimized
/// pattern matching code.
///
pub fn compile_file(source_path: String) -> Result(CompileResult, String) {
  use content <- result.try(
    simplifile.read(source_path)
    |> result.map_error(fn(_) { "Failed to read file: " <> source_path }),
  )

  let imports = extract_imports(content)
  let routes = parse_routes(content)

  use _ <- result.try(validate_path_params(routes))

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
    imports: imports,
    routes_code: routes_code,
    used_methods: used_methods,
    uses_middleware: uses_middleware,
    line_to_route: line_to_route,
  ))
}

// ------------------------------------------------------------- Private Functions

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

/// Extracts user imports from the source file. Filters out
/// framework imports that will be added automatically to
/// the compiled output.
///
fn extract_imports(content: String) -> List(String) {
  content
  |> string.split("\n")
  |> list.filter(fn(line) {
    let trimmed = string.trim(line)
    string.starts_with(trimmed, "import ")
    && !string.contains(trimmed, "glimr/routing/route")
    && !string.contains(trimmed, "import wisp")
    && !string.contains(trimmed, "import gleam/http")
  })
}

/// Parses all routes from the file content. Extracts the body
/// of the routes() function and recursively parses route
/// definitions.
///
fn parse_routes(content: String) -> List(ParsedRoute) {
  let define_body = extract_define_body(content)
  parse_route_list(define_body, [], [])
}

/// Extracts the body of the routes() function. Finds the
/// opening bracket and extracts content up to its matching
/// closing bracket.
///
fn extract_define_body(content: String) -> String {
  case string.split_once(content, "pub fn routes()") {
    Ok(#(_, after)) -> {
      case string.split_once(after, "[") {
        Ok(#(_, rest)) -> find_matching_bracket(rest, 1, "")
        Error(_) -> ""
      }
    }
    Error(_) -> ""
  }
}

/// Finds content up to a matching closing bracket. Tracks
/// bracket depth to handle nested arrays correctly and
/// returns accumulated content.
///
fn find_matching_bracket(content: String, depth: Int, acc: String) -> String {
  case depth, string.pop_grapheme(content) {
    0, _ -> acc
    _, Ok(#("[", rest)) -> find_matching_bracket(rest, depth + 1, acc <> "[")
    _, Ok(#("]", rest)) ->
      case depth - 1 {
        0 -> acc
        new_depth -> find_matching_bracket(rest, new_depth, acc <> "]")
      }
    _, Ok(#(char, rest)) -> find_matching_bracket(rest, depth, acc <> char)
    _, Error(_) -> acc
  }
}

/// Recursively parses a list of route items. Maintains prefix
/// and middleware stacks for nested groups and returns flat
/// list of parsed routes.
///
fn parse_route_list(
  content: String,
  prefix_stack: List(String),
  middleware_stack: List(List(String)),
) -> List(ParsedRoute) {
  let content = string.trim(content)

  case content {
    "" -> []
    _ -> {
      case find_next_route(content) {
        Ok(#(route, rest)) -> {
          let parsed = parse_single_item(route, prefix_stack, middleware_stack)
          list.append(
            parsed,
            parse_route_list(rest, prefix_stack, middleware_stack),
          )
        }
        Error(_) -> []
      }
    }
  }
}

/// Finds the next route definition in content. Skips whitespace
/// and comments, looking for route.* function calls to
/// extract.
///
fn find_next_route(content: String) -> Result(#(String, String), Nil) {
  let content = string.trim(content)

  case content {
    "" -> Error(Nil)
    _ -> {
      case
        string.starts_with(content, "route.")
        || string.starts_with(content, "//")
      {
        True -> {
          let #(item, rest) = extract_route_item(content)
          Ok(#(item, rest))
        }
        False -> {
          case string.pop_grapheme(content) {
            Ok(#(_, rest)) -> find_next_route(rest)
            Error(_) -> Error(Nil)
          }
        }
      }
    }
  }
}

/// Extracts a single route item from content. Handles comments
/// by skipping to the next line and balanced expressions for
/// actual route definitions.
///
fn extract_route_item(content: String) -> #(String, String) {
  case string.starts_with(content, "//") {
    True -> {
      case string.split_once(content, "\n") {
        Ok(#(_, rest)) -> #("", rest)
        Error(_) -> #("", "")
      }
    }
    False -> extract_balanced_expression(content, 0, 0, "")
  }
}

/// Extracts a balanced expression tracking parentheses and
/// brackets. Continues until both counts return to zero,
/// indicating the expression is complete.
///
fn extract_balanced_expression(
  content: String,
  parens: Int,
  brackets: Int,
  acc: String,
) -> #(String, String) {
  case string.pop_grapheme(content) {
    Ok(#("(", rest)) ->
      extract_balanced_expression(rest, parens + 1, brackets, acc <> "(")
    Ok(#(")", rest)) -> {
      let new_parens = parens - 1
      case new_parens == 0 && brackets == 0 {
        True -> {
          let #(piped, remaining) = check_for_pipe(rest)
          #(acc <> ")" <> piped, remaining)
        }
        False ->
          extract_balanced_expression(rest, new_parens, brackets, acc <> ")")
      }
    }
    Ok(#("[", rest)) ->
      extract_balanced_expression(rest, parens, brackets + 1, acc <> "[")
    Ok(#("]", rest)) -> {
      let new_brackets = brackets - 1
      case new_brackets == 0 && parens == 0 {
        True -> {
          let #(piped, remaining) = check_for_pipe(rest)
          #(acc <> "]" <> piped, remaining)
        }
        False ->
          extract_balanced_expression(rest, parens, new_brackets, acc <> "]")
      }
    }
    Ok(#(char, rest)) ->
      extract_balanced_expression(rest, parens, brackets, acc <> char)
    Error(_) -> #(acc, "")
  }
}

/// Checks for and extracts piped expressions. Handles chained
/// |> operators by recursively extracting each piped
/// expression.
///
fn check_for_pipe(content: String) -> #(String, String) {
  let trimmed = string.trim_start(content)
  case string.starts_with(trimmed, "|>") {
    True -> {
      let after_pipe = string.drop_start(trimmed, 2)
      let #(piped_expr, rest) =
        extract_balanced_expression(after_pipe, 0, 0, "")
      let #(more_piped, final_rest) = check_for_pipe(rest)
      #(" |>" <> piped_expr <> more_piped, final_rest)
    }
    False -> #("", content)
  }
}

/// Parses a single route item. Handles groups (prefix and
/// middleware) differently from regular routes, applying
/// accumulated context.
///
fn parse_single_item(
  item: String,
  prefix_stack: List(String),
  middleware_stack: List(List(String)),
) -> List(ParsedRoute) {
  let item = string.trim(item)

  case item {
    "" -> []
    _ -> {
      let #(base_item, item_middleware) = extract_piped_middleware(item)
      let all_middleware =
        list.flatten(middleware_stack) |> list.append(item_middleware)

      case
        string.starts_with(base_item, "route.prefix(")
        || string.starts_with(base_item, "route.group_middleware(")
      {
        True ->
          parse_group(
            base_item,
            prefix_stack,
            middleware_stack,
            item_middleware,
          )
        False -> parse_route(base_item, prefix_stack, all_middleware)
      }
    }
  }
}

/// Extracts middleware from a piped route expression. Separates
/// the base route definition from any middleware applied via
/// |> route.middleware().
///
fn extract_piped_middleware(item: String) -> #(String, List(String)) {
  case string.split_once(item, "|> route.middleware(") {
    Ok(#(base, middleware_part)) -> {
      let middleware = extract_middleware_list(middleware_part)
      #(string.trim(base), middleware)
    }
    Error(_) -> #(item, [])
  }
}

/// Extracts middleware function names from a list expression.
/// Parses the array content and splits by comma to get
/// individual middleware names.
///
fn extract_middleware_list(content: String) -> List(String) {
  case string.split_once(content, "[") {
    Ok(#(_, after_bracket)) -> {
      let list_content = find_matching_bracket(after_bracket, 1, "")
      list_content
      |> string.split(",")
      |> list.map(string.trim)
      |> list.filter(fn(s) { s != "" })
    }
    Error(_) -> []
  }
}

/// Parses a route group (prefix or middleware). Extracts the
/// group configuration and recursively parses nested routes
/// with updated stacks.
///
fn parse_group(
  item: String,
  prefix_stack: List(String),
  middleware_stack: List(List(String)),
  group_middleware: List(String),
) -> List(ParsedRoute) {
  case string.starts_with(item, "route.prefix(") {
    True -> {
      let after = string.drop_start(item, 13)
      case extract_string_arg(after) {
        Ok(#(prefix, rest)) -> {
          let inner = extract_inner_routes(rest)
          let new_prefix_stack = list.append(prefix_stack, [prefix])
          let new_middleware_stack =
            list.append(middleware_stack, [group_middleware])
          parse_route_list(inner, new_prefix_stack, new_middleware_stack)
        }
        Error(_) -> []
      }
    }
    False -> {
      // route.group_middleware([middleware], [routes])
      let after = string.drop_start(item, 23)
      let #(inline_middleware, rest) = extract_group_middleware_list(after)
      let all_group_middleware =
        list.append(group_middleware, inline_middleware)
      let inner = extract_inner_routes(rest)
      let new_middleware_stack =
        list.append(middleware_stack, [all_group_middleware])
      parse_route_list(inner, prefix_stack, new_middleware_stack)
    }
  }
}

/// Extracts middleware list from group_middleware call.
/// Returns the list of middleware names and remaining content
/// for further parsing.
///
fn extract_group_middleware_list(content: String) -> #(List(String), String) {
  let content = string.trim_start(content)
  case string.starts_with(content, "[") {
    True -> {
      let after_bracket = string.drop_start(content, 1)
      let list_content = find_matching_bracket(after_bracket, 1, "")
      let middleware =
        list_content
        |> string.split(",")
        |> list.map(string.trim)
        |> list.filter(fn(s) { s != "" })
      // Find rest after the closing bracket
      let rest_start = string.length(list_content) + 2
      let rest = string.drop_start(content, rest_start)
      #(middleware, rest)
    }
    False -> #([], content)
  }
}

/// Extracts the inner routes array from a group. Finds the
/// opening bracket and extracts content to its matching
/// close.
///
fn extract_inner_routes(content: String) -> String {
  case string.split_once(content, "[") {
    Ok(#(_, after)) -> find_matching_bracket(after, 1, "")
    Error(_) -> ""
  }
}

/// Parses a regular route definition. Extracts method, path,
/// and handler, applying prefix stack and middleware from
/// parent groups.
///
fn parse_route(
  item: String,
  prefix_stack: List(String),
  middleware: List(String),
) -> List(ParsedRoute) {
  case string.starts_with(item, "route.redirect") {
    True -> parse_redirect(item, prefix_stack)
    False -> {
      case extract_method(item) {
        Ok(method) -> {
          let after_method =
            string.drop_start(item, string.length("route." <> method <> "("))
          case extract_string_arg(after_method) {
            Ok(#(path, rest)) -> {
              case extract_handler(rest) {
                Ok(handler) -> {
                  let full_path = build_full_path(prefix_stack, path)
                  [ParsedRoute(method:, path: full_path, handler:, middleware:)]
                }
                Error(_) -> []
              }
            }
            Error(_) -> []
          }
        }
        Error(_) -> []
      }
    }
  }
}

/// Parses a redirect route definition. Extracts from/to paths
/// and determines status code (308 for permanent, 303 for
/// temporary).
///
fn parse_redirect(item: String, prefix_stack: List(String)) -> List(ParsedRoute) {
  let is_permanent = string.contains(item, "redirect_permanent")
  let status = case is_permanent {
    True -> 308
    False -> 303
  }

  let start_len = case is_permanent {
    True -> string.length("route.redirect_permanent(")
    False -> string.length("route.redirect(")
  }

  let after = string.drop_start(item, start_len)

  case extract_string_arg(after) {
    Ok(#(from, rest)) -> {
      case extract_string_arg(string.trim_start(rest)) {
        Ok(#(to, _)) -> {
          let full_from = build_full_path(prefix_stack, from)
          [ParsedRedirect(from: full_from, to:, status:)]
        }
        Error(_) -> []
      }
    }
    Error(_) -> []
  }
}

/// Extracts the HTTP method from a route call. Checks for
/// supported methods (get, post, put, patch, delete, head,
/// options).
///
fn extract_method(item: String) -> Result(String, Nil) {
  let methods = ["get", "post", "put", "patch", "delete", "head", "options"]

  list.find_map(methods, fn(method) {
    case string.starts_with(item, "route." <> method <> "(") {
      True -> Ok(method)
      False -> Error(Nil)
    }
  })
}

/// Extracts a string argument from function call syntax.
/// Parses quoted strings and returns the content with
/// remaining text.
///
fn extract_string_arg(content: String) -> Result(#(String, String), Nil) {
  let content = string.trim_start(content)

  case string.starts_with(content, "\"") {
    True -> {
      let after_quote = string.drop_start(content, 1)
      case string.split_once(after_quote, "\"") {
        Ok(#(str_content, rest)) -> {
          let rest = string.trim_start(rest)
          let rest = case string.starts_with(rest, ",") {
            True -> string.drop_start(rest, 1)
            False -> rest
          }
          Ok(#(str_content, rest))
        }
        Error(_) -> Error(Nil)
      }
    }
    False -> Error(Nil)
  }
}

/// Extracts the handler from a route definition. Handles both
/// named function references and anonymous function
/// definitions.
///
fn extract_handler(content: String) -> Result(String, Nil) {
  let content = string.trim(content)

  case string.starts_with(content, "fn") {
    True -> extract_anonymous_function(content)
    False -> {
      let handler =
        content
        |> string.split(")")
        |> list.first
        |> result.unwrap("")
        |> string.trim

      case handler {
        "" -> Error(Nil)
        h -> Ok(h)
      }
    }
  }
}

/// Extracts an anonymous function definition. Uses balanced
/// brace tracking to find the complete function body
/// including nested blocks.
///
fn extract_anonymous_function(content: String) -> Result(String, Nil) {
  let #(fn_text, _) = extract_fn_balanced(content, 0, 0, "", False)
  case fn_text {
    "" -> Error(Nil)
    text -> Ok(text)
  }
}

/// Extracts a balanced function definition tracking braces
/// and parentheses. Returns when the function body's closing
/// brace is found.
///
fn extract_fn_balanced(
  content: String,
  parens: Int,
  braces: Int,
  acc: String,
  in_body: Bool,
) -> #(String, String) {
  case string.pop_grapheme(content) {
    Ok(#("{", rest)) -> {
      extract_fn_balanced(rest, parens, braces + 1, acc <> "{", True)
    }
    Ok(#("}", rest)) -> {
      let new_braces = braces - 1
      case new_braces == 0 && in_body {
        True -> #(acc <> "}", rest)
        False ->
          extract_fn_balanced(rest, parens, new_braces, acc <> "}", in_body)
      }
    }
    Ok(#("(", rest)) ->
      extract_fn_balanced(rest, parens + 1, braces, acc <> "(", in_body)
    Ok(#(")", rest)) ->
      extract_fn_balanced(rest, parens - 1, braces, acc <> ")", in_body)
    Ok(#(char, rest)) ->
      extract_fn_balanced(rest, parens, braces, acc <> char, in_body)
    Error(_) -> #(acc, "")
  }
}

/// Builds the full path by joining prefix stack with the route
/// path. Handles slash normalization to produce clean URL
/// paths.
///
fn build_full_path(prefix_stack: List(String), path: String) -> String {
  let prefix =
    prefix_stack
    |> list.map(fn(p) { string.trim(p) |> trim_slashes })
    |> list.filter(fn(p) { p != "" })
    |> string.join("/")

  let path_clean = trim_slashes(path)

  case prefix, path_clean {
    "", "" -> "/"
    "", p -> "/" <> p
    pref, "" -> "/" <> pref
    pref, p -> "/" <> pref <> "/" <> p
  }
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

/// Checks if a path segment is a parameter. Parameters are
/// wrapped in curly braces like {id} or {user_id} in route
/// definitions.
///
fn is_param_segment(segment: String) -> Bool {
  string.starts_with(segment, "{") && string.ends_with(segment, "}")
}

/// Extracts the parameter name from a segment. Removes the
/// surrounding curly braces to get the variable name for
/// binding.
///
fn extract_param_name(segment: String) -> String {
  segment
  |> string.drop_start(1)
  |> string.drop_end(1)
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
            ParsedRoute(method:, path:, handler:, middleware:) ->
              Ok(#(method, path, handler, middleware))
            ParsedRedirect(..) -> Error(Nil)
          }
        })

      case method_routes {
        [] -> "      wisp.not_found()"
        [#(method, path, handler, middleware)] -> {
          let method_upper = string.capitalise(method)
          let params = extract_params_from_path(path)
          let handler_call = generate_handler_call(handler, params, middleware)
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
              let #(method, path, handler, middleware) = r
              let method_upper = string.capitalise(method)
              let params = extract_params_from_path(path)
              let handler_call =
                generate_handler_call(handler, params, middleware)
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
/// wrapping and passes appropriate arguments based on handler
/// type.
///
fn generate_handler_call(
  handler: String,
  params: List(String),
  middleware: List(String),
) -> String {
  let call = case string.starts_with(handler, "fn") {
    True -> generate_anon_fn_call(handler)
    False -> {
      let args = ["req", "ctx"] |> list.append(params) |> string.join(", ")
      handler <> "(" <> args <> ")"
    }
  }

  case middleware {
    [] -> call
    _ -> {
      let middleware_list = "[" <> string.join(middleware, ", ") <> "]"
      let #(use_req, use_ctx) = case string.starts_with(handler, "fn") {
        True -> {
          let fn_params = extract_fn_params(handler)
          case list.length(fn_params) {
            0 -> #("_req", "_ctx")
            1 -> #("req", "_ctx")
            _ -> #("req", "ctx")
          }
        }
        False -> #("req", "ctx")
      }
      "{\n          use "
      <> use_req
      <> ", "
      <> use_ctx
      <> " <- middleware.apply("
      <> middleware_list
      <> ", req, ctx)\n          "
      <> call
      <> "\n        }"
    }
  }
}

/// Generates a call to an anonymous function handler. Wraps
/// the function definition in braces and passes appropriate
/// arguments. Matches params by name - req/ctx are recognized
/// specially, everything else is a path param.
///
fn generate_anon_fn_call(handler: String) -> String {
  let fn_params = extract_fn_params(handler)
  let fn_param_names = list.map(fn_params, extract_param_name_from_signature)

  let args =
    fn_param_names
    |> list.map(fn(name) {
      case name {
        "req" | "_req" -> "req"
        "ctx" | "_ctx" -> "ctx"
        _ -> name
      }
    })
    |> string.join(", ")

  "{ " <> handler <> " }(" <> args <> ")"
}

/// Extracts the parameter name from a signature string.
/// Handles both typed params like "id: String" and untyped
/// params like "id".
///
fn extract_param_name_from_signature(param: String) -> String {
  let trimmed = string.trim(param)
  case string.split_once(trimmed, ":") {
    Ok(#(name, _)) -> string.trim(name)
    Error(_) -> trimmed
  }
}

/// Extracts parameter names from an anonymous function.
/// Parses the function signature to determine how many
/// arguments it expects.
///
fn extract_fn_params(handler: String) -> List(String) {
  case string.split_once(handler, "(") {
    Ok(#(_, rest)) -> {
      case string.split_once(rest, ")") {
        Ok(#(params_str, _)) -> {
          let params_str = string.trim(params_str)
          case params_str {
            "" -> []
            _ ->
              params_str
              |> string.split(",")
              |> list.map(string.trim)
              |> list.filter(fn(s) { s != "" })
          }
        }
        Error(_) -> []
      }
    }
    Error(_) -> []
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
  // Error format is like "src/bootstrap/gen/routes/api.gleam:12:7"
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

  case Nil {
    _ if is_arity_error ->
      "Handler function has incorrect number of parameters.\n"
      <> "Expected signature: fn(req, ctx) or fn(req, ctx, ...path_params)"
    _ if is_wrong_return ->
      "Handler function must return a wisp.Response.\n"
      <> "Make sure your handler returns a Response type."
    _ if is_unknown_var ->
      "Handler function not found.\n"
      <> "Make sure the module is imported and the function exists."
    _ if is_unknown_module ->
      "Handler module not found.\n"
      <> "Make sure the module is imported in your routes file."
    _ if is_not_function ->
      "Handler is not a function.\n"
      <> "Make sure you're referencing a function, not a value."
    _ ->
      "See: https://github.com/glimr-org/glimr?tab=readme-ov-file#route-handler-setup"
  }
}
