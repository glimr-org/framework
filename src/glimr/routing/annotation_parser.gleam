//// Annotation Parser
////
//// Controller files define routes via doc comment annotations
//// like `/// @get "/users"` above handler functions. This
//// parser reads those annotations and produces structured data
//// the route compiler uses to generate dispatch code. Keeping
//// route definitions as annotations means developers see the
//// URL right next to the handler — no separate routes file to
//// keep in sync.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ------------------------------------------------------------- Public Types

/// The route compiler needs to know each handler's parameter
/// names and types to wire up the right arguments — a `Request`
/// gets the HTTP request, a `Context` gets the app context, and
/// anything else maps to a route param like `:id`. Tracking
/// both lets developers put parameters in any order they like.
///
pub type FunctionParam {
  FunctionParam(name: String, param_type: String)
}

/// A route can be either a real handler (ParsedRoute) or a
/// redirect (ParsedRedirect). The route compiler turns these
/// into match arms in the generated dispatch function —
/// handlers call the controller function, while redirects emit
/// a 303/308 response pointing at the target path.
///
pub type ParsedRoute {
  ParsedRoute(
    method: String,
    path: String,
    handler: String,
    params: List(FunctionParam),
  )
  ParsedRedirect(from: String, to: String, status: Int)
}

/// Everything the route compiler needs from a single controller
/// file — the routes themselves, whether it exports a
/// `middleware()` function, and import flags. The import check
/// lets us catch a missing `Context` import early with a
/// helpful error instead of letting the Gleam compiler produce
/// a confusing "unknown type" message.
///
pub type ParseResult {
  ParseResult(
    routes: List(ParsedRoute),
    has_context_import: Bool,
    has_middleware_fn: Bool,
  )
}

// ------------------------------------------------------------- Private Types

/// A route's annotations can span several doc comment lines —
/// method, path, redirects — and they all need to be collected
/// before we hit the `pub fn` that ties them together. This
/// state bag accumulates everything until the function
/// declaration finalizes it.
///
type AnnotationState {
  AnnotationState(
    method: Option(String),
    path: Option(String),
    redirects: List(#(String, Int)),
  )
}

// ------------------------------------------------------------- Public Functions

/// The main entry point — takes the raw source of a controller
/// file and returns everything the route compiler needs to
/// generate dispatch code. Routes come from the doc comments
/// above each handler function, and we detect whether the
/// controller exports a `middleware()` function so the compiler
/// can wrap handlers automatically.
///
pub fn parse(content: String) -> ParseResult {
  let routes = extract_routes(content)
  let has_context_import = check_context_import(content)
  let has_middleware_fn = check_middleware_fn(content)

  ParseResult(routes:, has_context_import:, has_middleware_fn:)
}

// ------------------------------------------------------------- Private Functions

/// Splits the file into lines and kicks off the recursive
/// line-by-line parser that extracts routes from doc comment
/// annotations above handler functions.
///
fn extract_routes(content: String) -> List(ParsedRoute) {
  let lines = string.split(content, "\n")
  parse_lines(lines, None, [])
}

/// Route paths like `"/users/:id"` are quoted in annotations so
/// they can contain special characters without ambiguity.
/// Malformed quotes (missing closing quote) silently fail as
/// Error(Nil), which the caller treats as "no annotation found"
/// — better than crashing on a partially-typed line.
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

/// Walks through lines one at a time, building up annotation
/// state when we see `///` lines and finalizing a route when we
/// hit a `pub fn`. Non-comment, non-function lines reset the
/// state so stray annotations don't leak into the wrong
/// handler.
///
fn parse_lines(
  lines: List(String),
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
            None -> AnnotationState(method: None, path: None, redirects: [])
          }
          let new_state = parse_annotation_line(trimmed, state)
          parse_lines(rest, Some(new_state), acc)
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
                    create_routes_from_state(state, fn_name, params)
                  parse_lines(
                    remaining,
                    None,
                    list.append(list.reverse(new_routes), acc),
                  )
                }
                None -> parse_lines(rest, None, acc)
              }
            }
            False -> {
              // Not a doc comment or pub fn, reset state if we hit
              // non-empty non-comment line
              case trimmed == "" || string.starts_with(trimmed, "//") {
                True -> parse_lines(rest, current, acc)
                False -> parse_lines(rest, None, acc)
              }
            }
          }
        }
      }
    }
  }
}

/// Gleam function signatures can wrap across multiple lines
/// when there are many parameters. We need the full thing to
/// extract parameter names and types, so this keeps appending
/// lines until it finds the opening `{` that marks the start of
/// the function body.
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

/// A doc comment line might be `@get "/users"`, `@redirect
/// "/old"`, or just regular prose. We try each annotation
/// parser in sequence and take the first match — if none match,
/// the line is ignored and the existing state carries through
/// unchanged.
///
fn parse_annotation_line(
  line: String,
  state: AnnotationState,
) -> AnnotationState {
  let content = string.drop_start(line, 3) |> string.trim_start

  try_parse_method(content, state)
  |> result.lazy_or(fn() { try_parse_redirect(content, state) })
  |> result.unwrap(state)
}

/// Checks if the line is a method annotation like `@get
/// "/users"` or `@post "/login"`. All standard HTTP methods are
/// supported. The method and path are both required — without
/// them we can't generate a route match arm, so the annotation
/// is silently skipped.
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

/// When you rename a URL — say `/settings` becomes
/// `/account/settings` — old bookmarks and search engine links
/// break. `@redirect_permanent` emits a 308 so browsers cache
/// it forever, while `@redirect` uses 303 for temporary moves
/// that shouldn't be cached.
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

/// The generated dispatch code calls handlers by name, so we
/// need to pull the function name from `pub fn index(` →
/// `"index"`. Everything between `pub fn ` and `(` is the name.
///
fn extract_fn_name(line: String) -> String {
  let after_pub_fn = string.drop_start(line, 7)
  case string.split_once(after_pub_fn, "(") {
    Ok(#(name, _)) -> string.trim(name)
    Error(_) -> after_pub_fn |> string.trim
  }
}

/// The route compiler needs to know what arguments to pass to
/// each handler — a `Request` param gets the HTTP request,
/// `Context` gets the app context, and `Data` gets validated
/// form data. Parsing the full signature here lets developers
/// put parameters in any order they want.
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

/// A naive "split on `)` " would break on types like
/// `Option(String)` because of the nested parens. Tracking
/// depth ensures we find the actual closing paren of the
/// function signature, not one inside a type annotation.
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

/// Takes the raw text between the outer parentheses and
/// produces a list of typed parameters. Empty strings get
/// filtered out so trailing commas or whitespace-only segments
/// don't produce phantom parameters.
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

/// Same depth-tracking trick as extract_params_string but for
/// commas — `Dict(String, Int)` has a comma that isn't a
/// parameter separator. We only split on commas at depth 0 so
/// generic types stay intact.
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

/// Gleam parameters can be `name: Type` or just `name` without
/// a type annotation. We handle both — typed params let the
/// route compiler match on `Request`, `Context`, or `Data`,
/// while untyped ones are treated as route params by default.
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

/// When we finally hit the `pub fn`, all the annotations above
/// it are ready to be assembled into a route. We generate the
/// main route plus any redirect routes that point at this
/// handler's path.
///
fn create_routes_from_state(
  state: AnnotationState,
  fn_name: String,
  params: List(FunctionParam),
) -> List(ParsedRoute) {
  case state.method, state.path {
    Some(method), Some(path) -> {
      let main_route = ParsedRoute(method:, path:, handler: fn_name, params:)

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

/// Glob gives us filesystem paths like
/// `src/app/controllers/users.gleam` but Gleam imports use
/// module paths like `app/controllers/users`. This strips the
/// `src/` prefix and `.gleam` extension to bridge the gap.
///
pub fn module_from_path(path: String) -> Result(String, Nil) {
  path
  |> string.replace(".gleam", "")
  |> string.split_once("src/")
  |> result.map(fn(parts) { parts.1 })
}

/// Gleam lets you import types with or without the `type`
/// keyword — both `import mod.{type Foo}` and `import
/// mod.{Foo}` work. We check for both patterns so the import
/// detection doesn't give false negatives when developers use
/// either style.
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

/// Controllers can define a `pub fn middleware()` function to
/// apply middleware to all their routes. The route compiler
/// detects this and wraps every handler call with
/// `middleware.apply(controller.middleware(), ctx)`.
///
fn check_middleware_fn(content: String) -> Bool {
  content
  |> string.split("\n")
  |> list.any(fn(line) {
    let trimmed = string.trim(line)
    string.starts_with(trimmed, "pub fn middleware(")
  })
}

/// If a handler takes a `Context` parameter but the controller
/// didn't import it from glimr/http/context, we want to catch
/// it early with a clear message rather than letting the Gleam
/// compiler blame the generated dispatch code.
///
fn check_context_import(content: String) -> Bool {
  content
  |> string.split("\n")
  |> list.any(fn(line) {
    let trimmed = string.trim(line)
    case string.starts_with(trimmed, "//") {
      True -> False
      False ->
        string.starts_with(trimmed, "import glimr/http/context.{")
        && import_contains_type(trimmed, "Context")
    }
  })
}
