import gleam/list
import gleam/string
import gleeunit/should
import glimr/routing/annotation_parser.{ParsedRedirect}
import glimr/routing/compiler

// Standard imports for test sources
const standard_imports = "
import glimr/http/context.{type Context}
import app/app.{type App}
"

// Helper to compile controller routes with standard imports
fn compile_controller(
  module: String,
  source: String,
) -> Result(compiler.CompileResult, String) {
  let result = annotation_parser.parse(standard_imports <> source)
  compiler.compile_routes([#(module, result)])
}

// ------------------------------------------------------------- Basic Route Tests

pub fn parse_get_route_test() {
  let source =
    "
/// @get \"/\"
///
pub fn show(_ctx: Context(App)) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/home_controller", source)

  result.routes_code
  |> string.contains("[] ->")
  |> should.be_true

  result.routes_code
  |> string.contains("Get -> home_controller.show(ctx)")
  |> should.be_true

  result.used_methods
  |> should.equal(["Get"])
}

pub fn parse_post_route_test() {
  let source =
    "
/// @post \"/users\"
///
pub fn store(_ctx: Context(App)) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  result.routes_code
  |> string.contains("[\"users\"] ->")
  |> should.be_true

  result.routes_code
  |> string.contains("Post -> user_controller.store(ctx)")
  |> should.be_true

  result.used_methods
  |> should.equal(["Post"])
}

pub fn parse_multiple_methods_same_path_test() {
  let source =
    "
/// @get \"/users\"
///
pub fn index(_ctx: Context(App)) {
  wisp.ok()
}

/// @post \"/users\"
///
pub fn store(_ctx: Context(App)) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  result.routes_code
  |> string.contains("Get -> user_controller.index(ctx)")
  |> should.be_true

  result.routes_code
  |> string.contains("Post -> user_controller.store(ctx)")
  |> should.be_true

  result.used_methods
  |> list.sort(string.compare)
  |> should.equal(["Get", "Post"])
}

// ------------------------------------------------------------- Redirect Tests

pub fn parse_redirect_test() {
  let source =
    "
/// @redirect \"/old\"
/// @get \"/new\"
///
pub fn show(_ctx: Context(App)) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/page_controller", source)

  result.routes_code
  |> string.contains("[\"old\"] ->")
  |> should.be_true

  result.routes_code
  |> string.contains("redirect.to(\"/new\")")
  |> should.be_true
}

pub fn parse_permanent_redirect_test() {
  let source =
    "
/// @redirect_permanent \"/old-path\"
/// @get \"/new-path\"
///
pub fn show(_ctx: Context(App)) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/page_controller", source)

  result.routes_code
  |> string.contains("[\"old-path\"] ->")
  |> should.be_true

  result.routes_code
  |> string.contains("redirect.permanent(\"/new-path\")")
  |> should.be_true
}

// ------------------------------------------------------------- Controller Middleware Tests

pub fn controller_with_middleware_fn_wraps_handlers_test() {
  let source = standard_imports <> "
pub fn middleware() {
  [auth.run]
}

/// @get \"/dashboard\"
///
pub fn dashboard(_ctx: Context(App)) {
  wisp.ok()
}

/// @get \"/settings\"
///
pub fn settings(_ctx: Context(App)) {
  wisp.ok()
}
"

  let result = annotation_parser.parse(source)
  let assert Ok(compiled) =
    compiler.compile_routes([
      #("app/http/controllers/admin_controller", result),
    ])

  compiled.uses_middleware
  |> should.be_true

  // Both handlers should be wrapped with controller middleware
  compiled.routes_code
  |> string.contains("middleware.apply(admin_controller.middleware(), ctx)")
  |> should.be_true

  compiled.routes_code
  |> string.contains("[\"dashboard\"] ->")
  |> should.be_true

  compiled.routes_code
  |> string.contains("[\"settings\"] ->")
  |> should.be_true
}

pub fn controller_without_middleware_fn_no_wrapping_test() {
  let source =
    "
/// @get \"/users\"
///
pub fn index(_ctx: Context(App)) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  result.uses_middleware
  |> should.be_false

  // Handler should NOT be wrapped with middleware
  result.routes_code
  |> string.contains("middleware.apply")
  |> should.be_false

  result.routes_code
  |> string.contains("user_controller.index(ctx)")
  |> should.be_true
}

// ------------------------------------------------------------- Import Generation Tests

pub fn generates_controller_imports_test() {
  let source1 = standard_imports <> "
/// @get \"/\"
///
pub fn show(_ctx: Context(App)) {
  wisp.ok()
}
"

  let source2 = standard_imports <> "
/// @get \"/users\"
///
pub fn index(_ctx: Context(App)) {
  wisp.ok()
}
"

  let result1 = annotation_parser.parse(source1)
  let result2 = annotation_parser.parse(source2)

  let assert Ok(compiled) =
    compiler.compile_routes([
      #("app/http/controllers/home_controller", result1),
      #("app/http/controllers/user_controller", result2),
    ])

  compiled.imports
  |> list.any(fn(i) { string.contains(i, "home_controller") })
  |> should.be_true

  compiled.imports
  |> list.any(fn(i) { string.contains(i, "user_controller") })
  |> should.be_true
}

pub fn nested_controller_uses_unique_alias_test() {
  let source1 = standard_imports <> "
/// @get \"/\"
///
pub fn index(_ctx: Context(App)) {
  wisp.ok()
}
"

  let source2 = standard_imports <> "
/// @get \"/api\"
///
pub fn index(_ctx: Context(App)) {
  wisp.ok()
}
"

  let result1 = annotation_parser.parse(source1)
  let result2 = annotation_parser.parse(source2)

  let assert Ok(compiled) =
    compiler.compile_routes([
      #("app/http/controllers/welcome_controller", result1),
      #("app/http/controllers/api/welcome_controller", result2),
    ])

  // Nested controller should use `as` alias
  compiled.imports
  |> list.any(fn(i) {
    string.contains(i, "api/welcome_controller as api_welcome_controller")
  })
  |> should.be_true

  // Non-nested controller should not use `as`
  compiled.imports
  |> list.any(fn(i) { i == "import app/http/controllers/welcome_controller" })
  |> should.be_true

  // Routes should use the correct aliases
  compiled.routes_code
  |> string.contains("welcome_controller.index")
  |> should.be_true

  compiled.routes_code
  |> string.contains("api_welcome_controller.index")
  |> should.be_true
}

// ------------------------------------------------------------- Route Parameters Tests

pub fn parse_route_with_parameter_test() {
  let source =
    "
/// @get \"/users/:id\"
///
pub fn show(_ctx: Context(App), id: String) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  result.routes_code
  |> string.contains("[\"users\", id] ->")
  |> should.be_true

  result.routes_code
  |> string.contains("user_controller.show(ctx, id)")
  |> should.be_true
}

pub fn parse_route_with_multiple_parameters_test() {
  let source =
    "
/// @get \"/posts/:post_id/comments/:comment_id\"
///
pub fn show(_ctx: Context(App), post_id: String, comment_id: String) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/comment_controller", source)

  result.routes_code
  |> string.contains("[\"posts\", post_id, \"comments\", comment_id] ->")
  |> should.be_true

  result.routes_code
  |> string.contains("comment_controller.show(ctx, post_id, comment_id)")
  |> should.be_true
}

// ------------------------------------------------------------- 404 Fallback Test

pub fn generates_404_fallback_test() {
  let source =
    "
/// @get \"/\"
///
pub fn show(_ctx: Context(App)) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/home_controller", source)

  result.routes_code
  |> string.contains("_ -> response.not_found()")
  |> should.be_true
}

// ------------------------------------------------------------- Method Not Allowed Test

pub fn generates_method_not_allowed_test() {
  let source =
    "
/// @get \"/users\"
///
pub fn index(_ctx: Context(App)) {
  wisp.ok()
}

/// @post \"/users\"
///
pub fn store(_ctx: Context(App)) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  result.routes_code
  |> string.contains("response.method_not_allowed([Get, Post])")
  |> should.be_true
}

// ------------------------------------------------------------- Annotation Parser Unit Tests

pub fn annotation_parser_extracts_routes_test() {
  let source =
    "
/// @get \"/users\"
///
pub fn index(_ctx: Context(App)) {
  wisp.ok()
}

/// @post \"/users\"
///
pub fn store(_ctx: Context(App)) {
  wisp.ok()
}
"

  let result = annotation_parser.parse(source)

  result.routes
  |> list.length
  |> should.equal(2)
}

pub fn annotation_parser_detects_middleware_fn_test() {
  let source =
    "
pub fn middleware() {
  [auth.run]
}

/// @get \"/admin\"
///
pub fn index(_ctx: Context(App)) {
  wisp.ok()
}
"

  let result = annotation_parser.parse(source)

  result.has_middleware_fn
  |> should.be_true
}

pub fn annotation_parser_no_middleware_fn_test() {
  let source =
    "
/// @get \"/users\"
///
pub fn index(_ctx: Context(App)) {
  wisp.ok()
}
"

  let result = annotation_parser.parse(source)

  result.has_middleware_fn
  |> should.be_false
}

pub fn annotation_parser_extracts_redirects_test() {
  let source =
    "
/// @redirect \"/old\"
/// @get \"/new\"
///
pub fn show(_ctx: Context(App)) {
  wisp.ok()
}
"

  let result = annotation_parser.parse(source)

  result.routes
  |> list.length
  |> should.equal(2)

  result.routes
  |> list.any(fn(r) {
    case r {
      ParsedRedirect(from: "/old", to: "/new", status: 303) -> True
      _ -> False
    }
  })
  |> should.be_true
}

pub fn annotation_parser_module_from_path_test() {
  let assert Ok(module) =
    annotation_parser.module_from_path(
      "src/app/http/controllers/user_controller.gleam",
    )

  module
  |> should.equal("app/http/controllers/user_controller")
}

// ------------------------------------------------------------- Reserved Path Parameter Names Tests

pub fn rejects_reserved_path_param_req_test() {
  let source =
    "
/// @get \"/users/:req\"
///
pub fn show(_ctx: Context(App), req: String) {
  wisp.ok()
}
"

  let result =
    compile_controller("app/http/controllers/user_controller", source)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("Reserved path parameter")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn rejects_reserved_path_param_ctx_test() {
  let source =
    "
/// @get \"/users/:ctx\"
///
pub fn show(_ctx: Context(App), ctx: String) {
  wisp.ok()
}
"

  let result =
    compile_controller("app/http/controllers/user_controller", source)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("Reserved path parameter")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

// ------------------------------------------------------------- Flexible Parameter Ordering Tests

pub fn flexible_params_id_before_ctx_test() {
  let source =
    "
/// @get \"/users/:id\"
///
pub fn show(id: String, ctx: Context(App)) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  // Args should be in the function's param order
  result.routes_code
  |> string.contains("user_controller.show(id, ctx)")
  |> should.be_true
}

pub fn flexible_params_ctx_first_test() {
  let source =
    "
/// @get \"/users/:id\"
///
pub fn show(ctx: Context(App), id: String) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  // Args should match function signature order
  result.routes_code
  |> string.contains("user_controller.show(ctx, id)")
  |> should.be_true
}

pub fn flexible_params_only_route_params_test() {
  let source =
    "
/// @get \"/posts/:post_id/comments/:id\"
///
pub fn show(id: String, post_id: String) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/comment_controller", source)

  // Only route params, no ctx
  result.routes_code
  |> string.contains("comment_controller.show(id, post_id)")
  |> should.be_true
}

pub fn flexible_params_no_params_test() {
  let source =
    "
/// @get \"/health\"
///
pub fn check() {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/health_controller", source)

  // No params at all
  result.routes_code
  |> string.contains("health_controller.check()")
  |> should.be_true
}

pub fn rejects_missing_route_param_test() {
  let source =
    "
/// @get \"/users/:id\"
///
pub fn show(ctx: Context(App)) {
  wisp.ok()
}
"

  let result =
    compile_controller("app/http/controllers/user_controller", source)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("defines :id but handler")
      |> should.be_true

      msg
      |> string.contains("has no matching parameter")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn rejects_extra_handler_param_test() {
  let source =
    "
/// @get \"/users/:id\"
///
pub fn show(ctx: Context(App), id: String, name: String) {
  wisp.ok()
}
"

  let result =
    compile_controller("app/http/controllers/user_controller", source)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("has parameter 'name' but route")
      |> should.be_true

      msg
      |> string.contains("has no matching segment")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn rejects_untyped_ctx_param_test() {
  let source =
    "
/// @get \"/users\"
///
pub fn index(ctx) {
  wisp.ok()
}
"

  let result =
    compile_controller("app/http/controllers/user_controller", source)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("has parameter 'ctx' without a type annotation")
      |> should.be_true

      msg
      |> string.contains("Please specify the type: ctx: Context(App)")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn uses_underscore_for_unused_ctx_test() {
  let source =
    "
/// @get \"/health\"
///
pub fn check() {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/health_controller", source)

  // When no handler uses ctx, it should be marked as unused
  result.uses_ctx
  |> should.be_false
}

pub fn detects_ctx_usage_test() {
  let source =
    "
/// @get \"/users\"
///
pub fn index(ctx: Context(App)) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  result.uses_ctx
  |> should.be_true
}
