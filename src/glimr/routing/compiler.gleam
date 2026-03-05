//// Route Compiler
////
//// Turns controller annotations into a generated Gleam file
//// with a big case expression that dispatches requests by path
//// and method. This is the heart of the route system — it
//// validates everything (middleware exists, validators have
//// the right exports, handler params match route segments)
//// before generating code, so developers get clear errors at
//// compile time instead of runtime crashes.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option
import gleam/order
import gleam/regexp
import gleam/result
import gleam/string
import glimr/routing/annotation_parser.{
  type FunctionParam, type ParseResult, type ParsedRoute, ParsedRedirect,
  ParsedRoute,
}
import shellout
import simplifile

// ------------------------------------------------------------- Private Constants

/// `@validator "login"` in an annotation becomes
/// `app/http/validators/login` in the generated import.
///
const validator_base_path = "app/http/validators/"

// ------------------------------------------------------------- Public Types

/// Everything write_compiled_file needs to assemble the final
/// .gleam file — imports, the case expression body, which HTTP
/// methods are used (for the http import), and a line-to-route
/// mapping so compile errors can point at the original route
/// annotation instead of the generated code.
///
pub type CompileResult {
  CompileResult(
    imports: List(String),
    routes_code: String,
    used_methods: List(String),
    uses_middleware: Bool,
    uses_validator: Bool,
    uses_ctx: Bool,
    line_to_route: Dict(Int, String),
  )
}

// ------------------------------------------------------------- Private Types

/// Validation can fail in two distinct ways — the module
/// doesn't exist at all, or it exists but forgot to export the
/// required function. The error messages need to be different
/// for each case so developers know whether to create a file or
/// add a function.
///
type ModuleError {
  ModuleNotFound(name: String)
  ModuleMissingFunction(name: String, function: String)
}

// ------------------------------------------------------------- Public Functions

/// The main entry point — takes every controller's parsed
/// annotations and produces the Gleam code that dispatches
/// requests. Runs all validation first (paths, validators,
/// handler params) so developers get clear errors before any
/// code is generated.
///
pub fn compile_routes(
  controller_results: List(#(String, ParseResult)),
) -> Result(CompileResult, String) {
  let controller_routes =
    list.map(controller_results, fn(entry) {
      let #(module_path, result) = entry
      #(module_path, result.routes)
    })

  let routes =
    controller_routes
    |> list.flat_map(fn(entry) {
      let #(module_path, routes) = entry
      list.map(routes, fn(route) { prefix_handler(route, module_path) })
    })

  use _ <- result.try(validate_path_format(routes))
  use _ <- result.try(validate_path_params(routes))
  use _ <- result.try(validate_validators(controller_results))
  use _ <- result.try(validate_handler_params(routes))
  use _ <- result.try(validate_type_imports(controller_results))

  // Build list of controller aliases that export a middleware() function
  let middleware_controllers =
    controller_results
    |> list.filter_map(fn(entry) {
      let #(module_path, result) = entry
      case result.has_middleware_fn {
        True -> Ok(controller_alias(module_path))
        False -> Error(Nil)
      }
    })

  let imports = generate_imports(controller_routes, routes)
  let used_methods = collect_used_methods(routes)
  let uses_middleware = check_uses_middleware(controller_results)
  let uses_validator = check_uses_validator(routes)
  let uses_ctx = check_uses_context(routes)

  let #(routes_code, line_to_route) =
    generate_code(
      routes,
      list.length(imports),
      used_methods != [],
      uses_middleware,
      middleware_controllers,
    )

  Ok(CompileResult(
    imports:,
    routes_code:,
    used_methods:,
    uses_middleware:,
    uses_validator:,
    uses_ctx:,
    line_to_route:,
  ))
}

/// Takes the compiled result and writes the actual .gleam file.
/// Runs `gleam check` before formatting so that if the
/// generated code has type errors, the line numbers in the
/// error still match our line-to-route mapping — formatting
/// would shift everything around. If check fails, the previous
/// file content is restored so a bad compile never leaves a
/// broken file on disk.
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

  let redirect_import = case
    string.contains(compile_result.routes_code, "redirect.to(")
    || string.contains(compile_result.routes_code, "redirect.permanent(")
  {
    True -> "\nimport glimr/response/redirect"
    False -> ""
  }

  // Middleware and validators need ctx even if handlers don't use it directly
  let needs_ctx =
    compile_result.uses_ctx
    || compile_result.uses_middleware
    || compile_result.uses_validator
  let ctx_arg = case needs_ctx {
    True -> "ctx"
    False -> "_ctx"
  }
  let fn_args = case has_routes {
    True -> "path, method, " <> ctx_arg
    False -> "path, _method, _ctx"
  }

  let generated_comment =
    "//// This file was generated by Glimr ✨
//// https://github.com/glimr-org/glimr?tab=readme-ov-file#routes
////
//// DO NOT EDIT THIS FILE. If you would like to use plain
//// pattern matching over this compiled route approach, take a
//// look at the docs link below detailing how to do so.
////
//// https://github.com/glimr-org/glimr?tab=readme-ov-file#direct-pattern-matching
////

"

  let content =
    generated_comment
    <> imports_str
    <> http_import
    <> middleware_import
    <> redirect_import
    <> "\nimport glimr/response/response"
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

// ------------------------------------------------------------- Private Functions

/// `@validator "login"` in an annotation becomes the full
/// import path so the generated code can call the validator's
/// functions.
///
fn expand_validator_path(name: String) -> String {
  validator_base_path <> name
}

/// Gleam uses the last path segment as the module name in code
/// — `import app/http/validators/user` lets you call
/// `user.validate()`. Several places need just that last
/// segment for generating function calls and aliases.
///
fn last_segment(path: String) -> String {
  case string.split(path, "/") |> list.last {
    Ok(name) -> name
    Error(_) -> path
  }
}

/// The generated code calls `user.validate(ctx)`, not
/// `app/http/validators/user.validate(ctx)` — Gleam imports use
/// the last path segment as the module name. This pulls that
/// segment out so we can build the right function call.
///
fn validator_module_name(validator: String) -> String {
  last_segment(validator)
}

/// Two controllers can share a module name — `web/user` and
/// `api/user` both end in `user`. Gleam would choke on
/// duplicate imports, so we build a unique alias from the full
/// path after `controllers/`: `api/user` becomes `api_user`,
/// avoiding collisions.
///
fn controller_alias(module_path: String) -> String {
  case string.split_once(module_path, "controllers/") {
    Ok(#(_, after)) -> string.replace(after, "/", "_")
    Error(_) -> last_segment(module_path)
  }
}

/// The parsed annotation just says `handler: "index"` — but the
/// generated code needs `api_home_controller.index` to call the
/// right module. This qualifies handler names with their
/// controller alias so the dispatch code resolves correctly.
/// Redirects skip this since they don't call handlers.
///
fn prefix_handler(route: ParsedRoute, module_path: String) -> ParsedRoute {
  let alias = controller_alias(module_path)

  case route {
    ParsedRoute(method:, path:, handler:, validator:, params:) ->
      ParsedRoute(
        method:,
        path:,
        handler: alias <> "." <> handler,
        validator:,
        params:,
      )
    ParsedRedirect(..) -> route
  }
}

/// The generated file needs to import every module it
/// references — controllers and validators. Nested controllers
/// like `api/user` get an `as` alias (since Gleam would
/// otherwise use just the last segment) and duplicates are
/// filtered out so each module appears exactly once.
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
      let default_name = last_segment(module_path)
      // Use `as` alias when the alias differs from the default module name
      case alias == default_name {
        True -> "import " <> module_path
        False -> "import " <> module_path <> " as " <> alias
      }
    })

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

  list.flatten([controller_imports, validator_imports])
  |> list.unique
}

/// Including `import glimr/http/middleware` when no controller
/// has a `middleware()` function triggers an unused-import
/// warning from the Gleam compiler. This check lets us
/// conditionally add the import only when it's actually needed.
///
fn check_uses_middleware(
  controller_results: List(#(String, ParseResult)),
) -> Bool {
  list.any(controller_results, fn(entry) { { entry.1 }.has_middleware_fn })
}

/// Gleam warns on unused variables, so the generated function
/// signature uses `_ctx` when no handler needs the context.
/// This scan figures out whether any handler actually accepts a
/// Context param so we emit the right variable name.
///
fn check_uses_context(routes: List(ParsedRoute)) -> Bool {
  list.any(routes, fn(r) {
    case r {
      ParsedRoute(params:, ..) -> list.any(params, is_context_param)
      ParsedRedirect(..) -> False
    }
  })
}

/// Validators call `validate(ctx)` internally, so even if no
/// handler directly accepts a Context param, the generated
/// function still needs `ctx` (not `_ctx`) when validators are
/// in play. This check catches that indirect dependency.
///
fn check_uses_validator(routes: List(ParsedRoute)) -> Bool {
  list.any(routes, fn(r) {
    case r {
      ParsedRoute(validator: option.Some(_), ..) -> True
      _ -> False
    }
  })
}

/// Catching malformed paths here — like `/users/@all` or paths
/// with spaces — gives a clear "invalid route" error pointing
/// at the annotation, rather than a cryptic Gleam parse error
/// in the generated file that's hard to trace back.
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

/// The regex enforces that each segment is either a static name
/// (`users`, `api-v2`) or a colon-prefixed param (`:id`,
/// `:user_id`). Anything else would generate invalid Gleam
/// identifiers in the pattern match.
///
fn is_valid_path(path: String) -> Bool {
  // Static segment: [a-zA-Z0-9_-]+
  // Param segment: :[a-zA-Z_][a-zA-Z0-9_]*
  let assert Ok(re) =
    regexp.from_string("^(/([a-zA-Z0-9_-]+|:[a-zA-Z_][a-zA-Z0-9_]*))+$|^/$")
  regexp.check(re, path)
}

/// Route params become variable bindings in the generated code,
/// but `ctx` and `req` are already used by the dispatch
/// function signature. A route like `/users/:ctx` would shadow
/// the context variable and cause subtle bugs, so we reject
/// reserved names upfront.
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

/// Tests use fixture middleware and validators under
/// `test/fixtures/` rather than the real `src/` modules.
/// Without checking both locations, the compiler would reject
/// perfectly valid test setups as "module not found".
///
fn find_module_file(base_path: String, name: String) -> Result(String, Nil) {
  let full_path = base_path <> name
  let src_path = "src/" <> full_path <> ".gleam"
  let test_path = "test/fixtures/" <> full_path <> ".gleam"

  case simplifile.is_file(src_path) {
    Ok(True) -> Ok(src_path)
    _ ->
      case simplifile.is_file(test_path) {
        Ok(True) -> Ok(test_path)
        _ -> Error(Nil)
      }
  }
}

/// A typo in a `@validator` annotation would generate code that
/// imports a nonexistent module — the Gleam compiler error
/// would point at the generated file, not the annotation. By
/// checking the filesystem first, we can say exactly which
/// annotation has the problem.
///
fn check_module(
  name: String,
  base_path: String,
  required_function: String,
) -> Result(Nil, ModuleError) {
  case find_module_file(base_path, name) {
    Ok(path) ->
      case simplifile.read(path) {
        Ok(content) ->
          case string.contains(content, "pub fn " <> required_function <> "(") {
            True -> Ok(Nil)
            False -> Error(ModuleMissingFunction(name, required_function))
          }
        Error(_) -> Error(ModuleNotFound(name))
      }
    Error(_) -> Error(ModuleNotFound(name))
  }
}

/// Same idea as check_module but specifically for validators —
/// the generated code calls `user.validate(ctx)`, so the module
/// needs to exist and export that function. Checking early
/// avoids a confusing generated-code error.
///
fn check_validator(name: String) -> Result(Nil, ModuleError) {
  check_module(name, validator_base_path, "validate")
}

/// Mirror of format_middleware_error for validators —
/// "validator user doesn't exist" tells you to create the file,
/// "doesn't have a validate function" tells you to add the
/// export.
///
fn format_validator_error(err: ModuleError) -> String {
  case err {
    ModuleNotFound(name) -> "Validator \"" <> name <> "\" doesn't exist"
    ModuleMissingFunction(name, _) ->
      "Validator \"" <> name <> "\" doesn't have a public \"validate\" function"
  }
}

/// Walks every route that has a `@validator` annotation and
/// checks that the referenced module exists with a `validate`
/// export. Reports the specific route so the developer knows
/// exactly which annotation to fix.
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

/// Gleam requires explicit imports for unqualified types —
/// writing `ctx: Context(App)` without importing `Context`
/// compiles the generated file fine but then errors in the
/// controller. Catching this here lets us suggest the exact
/// import line the developer needs to add.
///
fn validate_type_imports(
  controller_results: List(#(String, ParseResult)),
) -> Result(Nil, String) {
  let errors =
    controller_results
    |> list.flat_map(fn(entry) {
      let #(module_path, result) = entry
      let alias = controller_alias(module_path)

      result.routes
      |> list.filter_map(fn(r) {
        case r {
          ParsedRoute(path:, handler:, params:, validator:, ..) -> {
            // Check for unqualified Context type without proper import
            let has_unqualified_context =
              list.any(params, fn(p) { p.param_type == "Context" })
            let context_error = case
              has_unqualified_context && !result.has_context_import
            {
              True ->
                option.Some(#(
                  path,
                  alias <> "." <> handler,
                  "Context type requires import: `import glimr/http/context.{type Context}`\n"
                    <> "Or use fully qualified type: `ctx: context.Context(App)`",
                ))
              False -> option.None
            }

            // Check for unqualified Data type without proper import
            let data_error = case validator {
              option.Some(v) -> {
                let validator_name = validator_module_name(v)
                let has_unqualified_data =
                  list.any(params, fn(p) { p.param_type == "Data" })
                let has_valid_import =
                  list.contains(result.validator_data_imports, validator_name)
                case has_unqualified_data && !has_valid_import {
                  True ->
                    option.Some(#(
                      path,
                      alias <> "." <> handler,
                      "Data type requires import: `import app/http/validators/"
                        <> validator_name
                        <> ".{type Data}`\n"
                        <> "Or use fully qualified type: `data: "
                        <> validator_name
                        <> ".Data`",
                    ))
                  False -> option.None
                }
              }
              option.None -> option.None
            }

            case context_error, data_error {
              option.Some(err), _ -> Ok(err)
              _, option.Some(err) -> Ok(err)
              option.None, option.None -> Error(Nil)
            }
          }
          ParsedRedirect(..) -> Error(Nil)
        }
      })
    })

  case errors {
    [] -> Ok(Nil)
    [#(path, handler, msg), ..] ->
      Error("Invalid route '" <> path <> "' for " <> handler <> "\n" <> msg)
  }
}

/// If a route defines `:id` but the handler doesn't accept an
/// `id` parameter, the generated code would pass an argument
/// the function doesn't expect. Catching mismatches here —
/// missing params, extra params, missing Data for validators —
/// produces much better errors than letting the Gleam compiler
/// figure it out from generated code.
///
fn validate_handler_params(routes: List(ParsedRoute)) -> Result(Nil, String) {
  let errors =
    routes
    |> list.filter_map(fn(r) {
      case r {
        ParsedRoute(path:, handler:, validator:, params:, ..) -> {
          let route_params = extract_params_from_path(path)
          check_handler_params(path, handler, route_params, params, validator)
        }
        ParsedRedirect(..) -> Error(Nil)
      }
    })

  case errors {
    [] -> Ok(Nil)
    [err, ..] -> Error(err)
  }
}

/// The actual validation logic for one handler — separated from
/// validate_handler_params so the error can include the
/// specific route path and handler name. Checks cascade:
/// untyped ctx params first, then missing route params, then
/// extra params, then missing validator Data.
///
fn check_handler_params(
  path: String,
  handler: String,
  route_params: List(String),
  fn_params: List(FunctionParam),
  validator: option.Option(String),
) -> Result(String, Nil) {
  // First check for ctx params without type annotations
  let untyped_ctx =
    list.find(fn_params, fn(p) {
      let name = string.lowercase(p.name)
      let is_ctx = name == "ctx" || name == "_ctx"
      is_ctx && p.param_type == ""
    })

  case untyped_ctx {
    Ok(param) -> {
      Ok(
        "Handler "
        <> handler
        <> " has parameter '"
        <> param.name
        <> "' without a type annotation\n"
        <> "Please specify the type: "
        <> param.name
        <> ": Context(App)",
      )
    }
    Error(_) -> {
      // Get non-special params (not Context or Data)
      let handler_route_params = get_route_param_names(fn_params, validator)

      // Check for missing route params (route has :id but handler doesn't have id)
      let missing =
        list.filter(route_params, fn(rp) {
          !list.contains(handler_route_params, rp)
        })

      case missing {
        [param, ..] ->
          Ok(
            "Route '"
            <> path
            <> "' defines :"
            <> param
            <> " but handler "
            <> handler
            <> " has no matching parameter",
          )
        [] -> {
          // Check for extra handler params (handler has 'name' but route has no :name)
          let extra_params =
            list.filter(fn_params, fn(p) {
              let clean_name = case string.starts_with(p.name, "_") {
                True -> string.drop_start(p.name, 1)
                False -> p.name
              }
              !is_context_param(p)
              && !is_validator_data_param(p, validator)
              && !list.contains(route_params, clean_name)
            })

          case extra_params {
            [param, ..] -> {
              let clean_name = case string.starts_with(param.name, "_") {
                True -> string.drop_start(param.name, 1)
                False -> param.name
              }
              let base_error =
                "Handler "
                <> handler
                <> " has parameter '"
                <> clean_name
                <> "' but route '"
                <> path
                <> "' has no matching segment"

              // Add hint for validated/data params
              let lower_name = string.lowercase(clean_name)
              let hint = case lower_name, validator {
                "validated", option.Some(v) | "data", option.Some(v) ->
                  "\nIf this is meant to be validated data, specify the type: "
                  <> param.name
                  <> ": "
                  <> validator_module_name(v)
                  <> ".Data"
                _, _ -> ""
              }

              Ok(base_error <> hint)
            }
            [] -> {
              // Check for missing validator Data param
              case validator {
                option.Some(v) -> {
                  case has_validator_data_param(fn_params, v) {
                    True -> Error(Nil)
                    False ->
                      Ok(
                        "Route '"
                        <> path
                        <> "' uses @validator \""
                        <> v
                        <> "\" but handler "
                        <> handler
                        <> " has no Data parameter",
                      )
                  }
                }
                option.None -> Error(Nil)
              }
            }
          }
        }
      }
    }
  }
}

/// When comparing handler params to route params, we only care
/// about the path-segment params (like `id` from `:id`).
/// Context and validator Data are framework-injected and don't
/// correspond to URL segments, so they're filtered out before
/// the comparison.
///
fn get_route_param_names(
  fn_params: List(FunctionParam),
  validator: option.Option(String),
) -> List(String) {
  fn_params
  |> list.filter(fn(p) {
    !is_context_param(p) && !is_validator_data_param(p, validator)
  })
  |> list.map(fn(p) {
    // Strip leading underscore if present
    case string.starts_with(p.name, "_") {
      True -> string.drop_start(p.name, 1)
      False -> p.name
    }
  })
}

/// A `@validator` annotation means the generated code calls
/// `validate(ctx)` and passes the result to the handler. If the
/// handler doesn't have a Data param to receive it, the
/// validated data goes nowhere — almost certainly a bug the
/// developer should know about immediately.
///
fn has_validator_data_param(
  fn_params: List(FunctionParam),
  validator: String,
) -> Bool {
  list.any(fn_params, fn(p) {
    is_validator_data_param(p, option.Some(validator))
  })
}

/// The generated file imports HTTP methods individually —
/// `import gleam/http.{Get, Post}` — rather than importing the
/// whole module. This collects which methods actually appear so
/// the import only includes what's used.
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

/// Orchestrates code generation — groups routes by path (so
/// `GET /users` and `POST /users` share one case clause), sorts
/// them (static paths before parameterized ones for correct
/// matching), and tracks line numbers so compile errors can be
/// mapped back to the original annotations.
///
fn generate_code(
  routes: List(ParsedRoute),
  import_count: Int,
  uses_methods: Bool,
  uses_middleware: Bool,
  middleware_controllers: List(String),
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
    generate_path_cases_with_lines(
      sorted_routes,
      start_line,
      [],
      dict.new(),
      middleware_controllers,
    )

  let code =
    "  case path {\n" <> cases <> "\n\n    _ -> response.not_found()\n  }"
  #(code, line_to_route)
}

/// Builds the case clauses one route at a time, counting lines
/// as it goes. The line mapping is the key to good error
/// messages — when `gleam check` reports an error on line 47 of
/// the generated file, we can look up which route annotation
/// caused it.
///
fn generate_path_cases_with_lines(
  routes: List(#(String, List(ParsedRoute))),
  current_line: Int,
  acc_cases: List(String),
  acc_mapping: Dict(Int, String),
  middleware_controllers: List(String),
) -> #(String, Dict(Int, String)) {
  case routes {
    [] -> #(string.join(list.reverse(acc_cases), "\n\n"), acc_mapping)
    [entry, ..rest] -> {
      let #(path, _) = entry
      let case_code = generate_path_case(entry, middleware_controllers)
      let case_lines = string.split(case_code, "\n") |> list.length

      // Map all lines of this case to the route path
      let new_mapping =
        int.range(
          from: current_line,
          to: current_line + case_lines,
          with: acc_mapping,
          run: fn(m, line) { dict.insert(m, line, path) },
        )

      generate_path_cases_with_lines(
        rest,
        current_line + case_lines + 1,
        // +1 for blank line between cases
        [case_code, ..acc_cases],
        new_mapping,
        middleware_controllers,
      )
    }
  }
}

/// `/users/profile` must appear before `/users/:id` in the case
/// expression, otherwise `:id` would match "profile" as a
/// parameter value. Static paths sort first to prevent params
/// from greedily swallowing literal segments.
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

/// `GET /users` and `POST /users` should share one case clause
/// with an inner method match, not produce two separate path
/// patterns. Grouping by path first makes the generated code
/// cleaner and avoids redundant pattern matches.
///
fn group_routes_by_path(
  routes: List(ParsedRoute),
) -> Dict(String, List(ParsedRoute)) {
  // Accumulate with prepending for O(1) insertion
  let grouped =
    list.fold(routes, dict.new(), fn(acc, route) {
      let path = case route {
        ParsedRoute(path:, ..) -> path
        ParsedRedirect(from:, ..) -> from
      }

      case dict.get(acc, path) {
        Ok(existing) -> dict.insert(acc, path, [route, ..existing])
        Error(_) -> dict.insert(acc, path, [route])
      }
    })

  // Reverse each group to restore original order
  dict.map_values(grouped, fn(_, routes) { list.reverse(routes) })
}

/// Each path gets one case clause — the pattern matches the URL
/// segments (with params bound to variables) and the body
/// dispatches by HTTP method. This produces one complete
/// `["users", id] -> ...` block.
///
fn generate_path_case(
  entry: #(String, List(ParsedRoute)),
  middleware_controllers: List(String),
) -> String {
  let #(path, routes) = entry
  let pattern = path_to_pattern(path)
  let body = generate_method_cases(routes, middleware_controllers)

  "    " <> pattern <> " ->\n" <> body
}

/// Turns `/users/:id` into `["users", id]` — static segments
/// get quoted as string literals and parameter segments become
/// bare variable names that Gleam binds during pattern
/// matching.
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

/// Splitting `/users/:id/` produces `["", "users", ":id", ""]`
/// — the empty strings from leading/trailing slashes need
/// filtering out so the pattern match doesn't include
/// empty-string literals.
///
fn path_to_segments(path: String) -> List(String) {
  path
  |> trim_slashes
  |> string.split("/")
  |> list.filter(fn(s) { s != "" })
}

/// The colon prefix is the annotation convention for dynamic
/// segments — `:id` means "bind whatever appears here to a
/// variable called id". Static segments like `users` have no
/// colon and become string literals instead.
///
fn is_param_segment(segment: String) -> Bool {
  string.starts_with(segment, ":")
}

/// The colon is just annotation syntax — the generated Gleam
/// code needs bare variable names like `id`, not `:id`.
/// Stripping the prefix here keeps the rest of the code
/// generation from having to worry about it.
///
fn extract_param_name(segment: String) -> String {
  string.drop_start(segment, 1)
}

/// Inside each path's case clause, we need to dispatch by HTTP
/// method. Redirects fire regardless of method, single routes
/// get a simple match, and multi-method paths get a full `case
/// method { Get -> ... Post -> ... }` block with a 405 fallback
/// listing the allowed methods.
///
fn generate_method_cases(
  routes: List(ParsedRoute),
  middleware_controllers: List(String),
) -> String {
  let first = list.first(routes)

  case first {
    Ok(ParsedRedirect(to:, status:, ..)) -> {
      let redirect_fn = case status {
        308 -> "redirect.permanent"
        _ -> "redirect.to"
      }
      "      " <> redirect_fn <> "(\"" <> to <> "\")"
    }
    _ -> {
      let method_routes =
        list.filter_map(routes, fn(r) {
          case r {
            ParsedRoute(method:, path:, handler:, validator:, params:) ->
              Ok(#(method, path, handler, validator, params))
            ParsedRedirect(..) -> Error(Nil)
          }
        })

      case method_routes {
        [] -> "      response.not_found()"
        [#(method, path, handler, validator, fn_params)] -> {
          let method_upper = string.capitalise(method)
          let route_params = extract_params_from_path(path)
          let controller_mw =
            resolve_controller_middleware(handler, middleware_controllers)
          let handler_call =
            generate_handler_call(
              handler,
              route_params,
              fn_params,
              controller_mw,
              validator,
            )
          "      case method {\n        "
          <> method_upper
          <> " -> "
          <> handler_call
          <> "\n        _ -> response.method_not_allowed(["
          <> method_upper
          <> "])\n      }"
        }
        _ -> {
          let methods =
            list.map(method_routes, fn(r) { string.capitalise(r.0) })
          let methods_list = string.join(methods, ", ")

          let cases =
            list.map(method_routes, fn(r) {
              let #(method, path, handler, validator, fn_params) = r
              let method_upper = string.capitalise(method)
              let route_params = extract_params_from_path(path)
              let controller_mw =
                resolve_controller_middleware(handler, middleware_controllers)
              let handler_call =
                generate_handler_call(
                  handler,
                  route_params,
                  fn_params,
                  controller_mw,
                  validator,
                )
              "        " <> method_upper <> " -> " <> handler_call
            })
            |> string.join("\n")

          "      case method {\n"
          <> cases
          <> "\n        _ -> response.method_not_allowed(["
          <> methods_list
          <> "])\n      }"
        }
      }
    }
  }
}

/// The handler call needs to pass route params as arguments —
/// `user_controller.show(ctx, id)` for a route like
/// `/users/:id`. This pulls out just the param names so we know
/// what variables the pattern match will bind.
///
fn extract_params_from_path(path: String) -> List(String) {
  path_to_segments(path)
  |> list.filter(is_param_segment)
  |> list.map(extract_param_name)
}

/// The most complex part of code generation — the handler call
/// might be bare (`controller.index(ctx)`), wrapped in a
/// validator (`use validated <- user.validate(ctx)`), or
/// wrapped in controller middleware (`use ctx <-
/// middleware.apply(controller.middleware(), ctx)`) or both.
/// The layers nest outward: middleware wraps validator wraps
/// handler.
///
fn generate_handler_call(
  handler: String,
  route_params: List(String),
  fn_params: List(FunctionParam),
  controller_middleware: option.Option(String),
  validator: option.Option(String),
) -> String {
  // Build args based on function signature order
  let args = build_handler_args(fn_params, route_params, validator)
  let call = handler <> "(" <> string.join(args, ", ") <> ")"

  // Wrap with validator if present
  let with_validator = case validator {
    option.Some(v) ->
      "{\n          use validated <- "
      <> validator_module_name(v)
      <> ".validate(ctx)\n          "
      <> call
      <> "\n        }"
    option.None -> call
  }

  // Wrap with controller middleware if present
  case controller_middleware {
    option.None -> with_validator
    option.Some(alias) -> {
      // Check if handler or validator uses ctx to avoid unused warnings
      let handler_uses_ctx = list.any(fn_params, is_context_param)
      let has_validator = option.is_some(validator)
      let mw_ctx = case handler_uses_ctx || has_validator {
        True -> "ctx"
        False -> "_ctx"
      }
      "{\n          use "
      <> mw_ctx
      <> " <- middleware.apply("
      <> alias
      <> ".middleware(), ctx)\n          "
      <> with_validator
      <> "\n        }"
    }
  }
}

/// Arguments must appear in the same order as the handler's
/// function signature — not the order they appear in the route
/// path. Walking the parsed params in declaration order and
/// mapping each one to `ctx`, `validated`, or the route param
/// variable ensures the generated call matches.
///
fn build_handler_args(
  fn_params: List(FunctionParam),
  route_params: List(String),
  validator: option.Option(String),
) -> List(String) {
  list.map(fn_params, fn(param) { param_to_arg(param, route_params, validator) })
}

/// Each param type maps to a different source: Context params
/// get `ctx` (from the dispatch function), Data params get
/// `validated` (from the validator's `use` callback), and
/// everything else is a route param bound by the pattern match.
/// The type annotation drives this decision, not the param
/// name.
///
fn param_to_arg(
  param: FunctionParam,
  _route_params: List(String),
  validator: option.Option(String),
) -> String {
  case is_context_param(param) {
    True -> "ctx"
    False ->
      case is_validator_data_param(param, validator) {
        True -> "validated"
        False ->
          case string.starts_with(param.name, "_") {
            True -> string.drop_start(param.name, 1)
            False -> param.name
          }
      }
  }
}

/// Developers can write the context type as either
/// `Context(App)` (with an import) or `context.Context(App)`
/// (fully qualified). Both mean the same thing, and both should
/// get `ctx` passed to them in the generated call.
///
fn is_context_param(param: FunctionParam) -> Bool {
  string.starts_with(param.param_type, "Context(")
  || string.starts_with(param.param_type, "context.Context(")
}

/// Like context params, validator data can be written as bare
/// `Data` (with an import) or `login.Data` (qualified). Either
/// way, the generated code should pass `validated` — the value
/// produced by the `use validated <-` callback.
///
fn is_validator_data_param(
  param: FunctionParam,
  validator: option.Option(String),
) -> Bool {
  case validator {
    option.Some(v) -> {
      let validator_name = validator_module_name(v)
      param.param_type == "Data"
      || param.param_type == validator_name <> ".Data"
    }
    option.None -> False
  }
}

/// Extracts the controller alias from a qualified handler name
/// like `admin_controller.dashboard` and checks whether that
/// controller has a `middleware()` function. Returns
/// Some(alias) if it does, None otherwise.
///
fn resolve_controller_middleware(
  handler: String,
  middleware_controllers: List(String),
) -> option.Option(String) {
  let alias = case string.split(handler, ".") {
    [a, _] -> a
    _ -> handler
  }
  case list.contains(middleware_controllers, alias) {
    True -> option.Some(alias)
    False -> option.None
  }
}

/// Paths come from annotations in various forms — `/users/`,
/// `/users`, `users/` — and need to be normalized before
/// splitting. Stripping both ends ensures splitting on `/`
/// produces only meaningful segments.
///
fn trim_slashes(s: String) -> String {
  s
  |> trim_start_char("/")
  |> trim_end_char("/")
}

/// Gleam's stdlib doesn't have a trim_start that takes a
/// character, so this does it manually. Recursive because paths
/// could theoretically have `//` (double slashes) that need
/// stripping.
///
fn trim_start_char(s: String, char: String) -> String {
  case string.starts_with(s, char) {
    True -> trim_start_char(string.drop_start(s, 1), char)
    False -> s
  }
}

/// Same as trim_start_char but for trailing characters.
/// Together with trim_start_char, handles all the edge cases of
/// how developers might write path prefixes in their
/// annotations.
///
fn trim_end_char(s: String, char: String) -> String {
  case string.ends_with(s, char) {
    True -> trim_end_char(string.drop_end(s, 1), char)
    False -> s
  }
}

/// When the generated code fails `gleam check`, the error
/// points at a line in the generated file — useless to the
/// developer. This maps that line back to the original route
/// path so the error says "(route: /users/:id)" instead of just
/// "line 47".
///
fn find_route_from_error(
  error_msg: String,
  file_path: String,
  line_to_route: Dict(Int, String),
) -> Result(String, Nil) {
  extract_line_number_for_file(error_msg, file_path)
  |> result.try(fn(line) { dict.get(line_to_route, line) })
}

/// Gleam errors use `file.gleam:12:7` format. We only care
/// about errors in our specific generated file (not other files
/// that might also have issues), so we look for our file path
/// first, then pull the line number.
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

/// Gleam's error messages are great but generic — "Expected 2
/// arguments" doesn't tell you it's because your handler
/// signature doesn't match the route. By pattern-matching on
/// the error text, we can add framework-specific hints that
/// guide developers to the actual fix.
///
fn get_specific_error_hint(msg: String) -> String {
  let is_arity_error =
    string.contains(msg, "Expected") && string.contains(msg, "arguments")
  let is_unknown_var = string.contains(msg, "Unknown variable")
  let is_unknown_module = string.contains(msg, "Unknown module")
  let is_not_function = string.contains(msg, "not a function")
  let is_middleware_type_mismatch =
    string.contains(msg, "Type mismatch") && string.contains(msg, "middleware")
  let is_wrong_return =
    string.contains(msg, "Response")
    && string.contains(msg, "Type mismatch")
    && !is_middleware_type_mismatch
  let is_validated_type_mismatch =
    string.contains(msg, "Type mismatch")
    && string.contains(msg, "validated")
    && !is_wrong_return
    && !is_middleware_type_mismatch

  case Nil {
    _ if is_middleware_type_mismatch ->
      "Controller middleware() must return List(Middleware(Context(App))).\n"
      <> "Each item should be a middleware function"
    _ if is_validated_type_mismatch ->
      "Type mismatch with validated data.\n"
      <> "Ensure your Data type is imported from the validator module."
    _ if is_arity_error ->
      "Handler function has incorrect number of parameters.\n"
      <> "Expected signature: fn(ctx: Context(App)) or fn(ctx: Context(App), ...path_params)"
    _ if is_wrong_return ->
      "Handler function must return a glimr/http/http.Response.\n"
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
