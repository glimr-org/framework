import gleam/list
import gleam/option
import gleam/string
import gleeunit/should
import glimr/routing/annotation_parser.{ParsedRedirect, ParsedRoute}
import glimr/routing/compiler

// Standard imports for test sources
const standard_imports = "
import wisp.{type Request}
import app/http/context/ctx.{type Context}
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
pub fn show(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/home_controller", source)

  result.routes_code
  |> string.contains("[] ->")
  |> should.be_true

  result.routes_code
  |> string.contains("Get -> home_controller.show(req, ctx)")
  |> should.be_true

  result.used_methods
  |> should.equal(["Get"])
}

pub fn parse_post_route_test() {
  let source =
    "
/// @post \"/users\"
///
pub fn store(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  result.routes_code
  |> string.contains("[\"users\"] ->")
  |> should.be_true

  result.routes_code
  |> string.contains("Post -> user_controller.store(req, ctx)")
  |> should.be_true

  result.used_methods
  |> should.equal(["Post"])
}

pub fn parse_multiple_methods_same_path_test() {
  let source =
    "
/// @get \"/users\"
///
pub fn index(_req: Request, _ctx: Context) {
  wisp.ok()
}

/// @post \"/users\"
///
pub fn store(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  result.routes_code
  |> string.contains("Get -> user_controller.index(req, ctx)")
  |> should.be_true

  result.routes_code
  |> string.contains("Post -> user_controller.store(req, ctx)")
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
pub fn show(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/page_controller", source)

  result.routes_code
  |> string.contains("[\"old\"] ->")
  |> should.be_true

  result.routes_code
  |> string.contains("wisp.redirect(\"/new\")")
  |> should.be_true
}

pub fn parse_permanent_redirect_test() {
  let source =
    "
/// @redirect_permanent \"/old-path\"
/// @get \"/new-path\"
///
pub fn show(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/page_controller", source)

  result.routes_code
  |> string.contains("[\"old-path\"] ->")
  |> should.be_true

  result.routes_code
  |> string.contains("wisp.permanent_redirect(\"/new-path\")")
  |> should.be_true
}

// ------------------------------------------------------------- Middleware Tests

pub fn parse_route_with_middleware_test() {
  let source =
    "
/// @get \"/admin\"
/// @middleware \"auth\"
///
pub fn index(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/admin_controller", source)

  result.uses_middleware
  |> should.be_true

  result.routes_code
  |> string.contains("middleware.apply([auth.run], req, ctx)")
  |> should.be_true
}

pub fn parse_route_with_multiple_middleware_test() {
  let source =
    "
/// @get \"/admin\"
/// @middleware \"auth\"
/// @middleware \"logging\"
///
pub fn index(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/admin_controller", source)

  result.routes_code
  |> string.contains("auth.run")
  |> should.be_true

  result.routes_code
  |> string.contains("logging.run")
  |> should.be_true
}

// ------------------------------------------------------------- Group Middleware Tests

pub fn parse_group_middleware_test() {
  let source =
    "
// @group_middleware \"auth\"

/// @get \"/dashboard\"
///
pub fn dashboard(_req: Request, _ctx: Context) {
  wisp.ok()
}

/// @get \"/settings\"
///
pub fn settings(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/admin_controller", source)

  result.uses_middleware
  |> should.be_true

  result.routes_code
  |> string.contains("[\"dashboard\"] ->")
  |> should.be_true

  result.routes_code
  |> string.contains("[\"settings\"] ->")
  |> should.be_true
}

pub fn parse_group_middleware_combined_with_route_middleware_test() {
  let source =
    "
// @group_middleware \"auth\"

/// @get \"/dashboard\"
/// @middleware \"logging\"
///
pub fn dashboard(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/admin_controller", source)

  result.routes_code
  |> string.contains("auth.run")
  |> should.be_true

  result.routes_code
  |> string.contains("logging.run")
  |> should.be_true
}

// ------------------------------------------------------------- Import Generation Tests

pub fn generates_controller_imports_test() {
  let source1 = standard_imports <> "
/// @get \"/\"
///
pub fn show(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let source2 = standard_imports <> "
/// @get \"/users\"
///
pub fn index(_req: Request, _ctx: Context) {
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
pub fn index(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let source2 = standard_imports <> "
/// @get \"/api\"
///
pub fn index(_req: Request, _ctx: Context) {
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
pub fn show(_req: Request, _ctx: Context, id: String) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  result.routes_code
  |> string.contains("[\"users\", id] ->")
  |> should.be_true

  result.routes_code
  |> string.contains("user_controller.show(req, ctx, id)")
  |> should.be_true
}

pub fn parse_route_with_multiple_parameters_test() {
  let source =
    "
/// @get \"/posts/:post_id/comments/:comment_id\"
///
pub fn show(_req: Request, _ctx: Context, post_id: String, comment_id: String) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/comment_controller", source)

  result.routes_code
  |> string.contains("[\"posts\", post_id, \"comments\", comment_id] ->")
  |> should.be_true

  result.routes_code
  |> string.contains("comment_controller.show(req, ctx, post_id, comment_id)")
  |> should.be_true
}

// ------------------------------------------------------------- 404 Fallback Test

pub fn generates_404_fallback_test() {
  let source =
    "
/// @get \"/\"
///
pub fn show(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/home_controller", source)

  result.routes_code
  |> string.contains("_ -> wisp.not_found()")
  |> should.be_true
}

// ------------------------------------------------------------- Method Not Allowed Test

pub fn generates_method_not_allowed_test() {
  let source =
    "
/// @get \"/users\"
///
pub fn index(_req: Request, _ctx: Context) {
  wisp.ok()
}

/// @post \"/users\"
///
pub fn store(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  result.routes_code
  |> string.contains("wisp.method_not_allowed([Get, Post])")
  |> should.be_true
}

// ------------------------------------------------------------- Annotation Parser Unit Tests

pub fn annotation_parser_extracts_routes_test() {
  let source =
    "
/// @get \"/users\"
///
pub fn index(_req: Request, _ctx: Context) {
  wisp.ok()
}

/// @post \"/users\"
///
pub fn store(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let result = annotation_parser.parse(source)

  result.routes
  |> list.length
  |> should.equal(2)
}

pub fn annotation_parser_extracts_group_middleware_test() {
  let source =
    "
// @group_middleware \"auth\"
// @group_middleware \"logging\"

/// @get \"/admin\"
///
pub fn index(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let result = annotation_parser.parse(source)

  result.group_middleware
  |> should.equal(["auth", "logging"])
}

pub fn annotation_parser_extracts_route_middleware_test() {
  let source =
    "
/// @get \"/admin\"
/// @middleware \"auth\"
///
pub fn index(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let result = annotation_parser.parse(source)

  case result.routes {
    [ParsedRoute(middleware: mw, ..)] -> {
      mw |> should.equal(["auth"])
    }
    _ -> should.fail()
  }
}

pub fn annotation_parser_combines_group_and_route_middleware_test() {
  let source =
    "
// @group_middleware \"group\"

/// @get \"/admin\"
/// @middleware \"route\"
///
pub fn index(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let result = annotation_parser.parse(source)

  case result.routes {
    [ParsedRoute(middleware: mw, ..)] -> {
      mw
      |> should.equal(["group", "route"])
    }
    _ -> should.fail()
  }
}

pub fn annotation_parser_extracts_redirects_test() {
  let source =
    "
/// @redirect \"/old\"
/// @get \"/new\"
///
pub fn show(_req: Request, _ctx: Context) {
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
pub fn show(_req: Request, _ctx: Context, req: String) {
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
pub fn show(_req: Request, _ctx: Context, ctx: String) {
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

// ------------------------------------------------------------- Middleware Validation Error Tests

pub fn rejects_invalid_group_middleware_test() {
  let source =
    "
// @group_middleware \"nonexistent\"

/// @get \"/users\"
///
pub fn index(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let result =
    compile_controller("app/http/controllers/user_controller", source)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("Invalid group middleware for user_controller")
      |> should.be_true

      msg
      |> string.contains("Middleware \"nonexistent\" doesn't exist")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn rejects_invalid_route_middleware_test() {
  let source =
    "
/// @get \"/users\"
/// @middleware \"nonexistent\"
///
pub fn index(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let result =
    compile_controller("app/http/controllers/user_controller", source)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("Invalid route '/users' for user_controller.index")
      |> should.be_true

      msg
      |> string.contains("Middleware \"nonexistent\" doesn't exist")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn rejects_group_middleware_without_run_test() {
  let source =
    "
// @group_middleware \"no_handle\"

/// @get \"/users\"
///
pub fn index(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let result =
    compile_controller("app/http/controllers/user_controller", source)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("Invalid group middleware for user_controller")
      |> should.be_true

      msg
      |> string.contains("doesn't have a public \"run\" function")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn rejects_route_middleware_without_run_test() {
  let source =
    "
/// @get \"/users\"
/// @middleware \"no_handle\"
///
pub fn index(_req: Request, _ctx: Context) {
  wisp.ok()
}
"

  let result =
    compile_controller("app/http/controllers/user_controller", source)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("Invalid route '/users' for user_controller.index")
      |> should.be_true

      msg
      |> string.contains("doesn't have a public \"run\" function")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

// ------------------------------------------------------------- Validator Tests

pub fn parse_route_with_validator_test() {
  let source =
    "
import app/http/validators/user_validator.{type Data}

/// @post \"/users\"
/// @validator \"user_validator\"
///
pub fn store(_req: Request, _ctx: Context, validated: Data) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  result.routes_code
  |> string.contains("user_validator.validate(req, ctx)")
  |> should.be_true

  result.routes_code
  |> string.contains("user_controller.store(req, ctx, validated)")
  |> should.be_true
}

pub fn parse_route_with_validator_and_path_params_test() {
  let source =
    "
import app/http/validators/user_validator.{type Data}

/// @post \"/users/:id\"
/// @validator \"user_validator\"
///
pub fn update(_req: Request, _ctx: Context, id: String, validated: Data) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  result.routes_code
  |> string.contains("user_validator.validate(req, ctx)")
  |> should.be_true

  // Path params come before validated
  result.routes_code
  |> string.contains("user_controller.update(req, ctx, id, validated)")
  |> should.be_true
}

pub fn parse_route_with_validator_and_middleware_test() {
  let source =
    "
import app/http/validators/user_validator.{type Data}

/// @post \"/users\"
/// @middleware \"auth\"
/// @validator \"user_validator\"
///
pub fn store(_req: Request, _ctx: Context, validated: Data) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  // Both middleware and validator should be present
  result.routes_code
  |> string.contains("middleware.apply")
  |> should.be_true

  result.routes_code
  |> string.contains("user_validator.validate(req, ctx)")
  |> should.be_true
}

pub fn rejects_invalid_validator_test() {
  let source =
    "
/// @post \"/users\"
/// @validator \"nonexistent\"
///
pub fn store(_req: Request, _ctx: Context, validated: Data) {
  wisp.ok()
}
"

  let result =
    compile_controller("app/http/controllers/user_controller", source)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("Invalid route '/users' for user_controller.store")
      |> should.be_true

      msg
      |> string.contains("Validator \"nonexistent\" doesn't exist")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn annotation_parser_extracts_validator_test() {
  let source =
    "
/// @post \"/users\"
/// @validator \"user_validator\"
///
pub fn store(_req: Request, _ctx: Context, validated: Data) {
  wisp.ok()
}
"

  let result = annotation_parser.parse(source)

  case result.routes {
    [ParsedRoute(validator: option.Some(v), ..)] -> {
      v |> should.equal("user_validator")
    }
    _ -> should.fail()
  }
}

// ------------------------------------------------------------- Flexible Parameter Ordering Tests

pub fn flexible_params_validated_first_test() {
  let source =
    "
/// @post \"/users/:id\"
/// @validator \"user_validator\"
///
pub fn update(validated: user_validator.Data, id: String, req: Request, ctx: Context) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  // Args should be in the function's param order
  result.routes_code
  |> string.contains("user_controller.update(validated, id, req, ctx)")
  |> should.be_true
}

pub fn flexible_params_ctx_before_req_test() {
  let source =
    "
/// @get \"/users/:id\"
///
pub fn show(ctx: Context, req: Request, id: String) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  // Args should match function signature order
  result.routes_code
  |> string.contains("user_controller.show(ctx, req, id)")
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

  // Only route params, no req/ctx
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
pub fn show(req: Request, ctx: Context) {
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
pub fn show(req: Request, ctx: Context, id: String, name: String) {
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

pub fn rejects_missing_validator_data_param_test() {
  let source =
    "
/// @post \"/users\"
/// @validator \"user_validator\"
///
pub fn store(req: Request, ctx: Context) {
  wisp.ok()
}
"

  let result =
    compile_controller("app/http/controllers/user_controller", source)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("uses @validator \"user_validator\"")
      |> should.be_true

      msg
      |> string.contains("has no Data parameter")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn rejects_untyped_req_param_test() {
  let source =
    "
/// @get \"/users\"
///
pub fn index(req, ctx: Context) {
  wisp.ok()
}
"

  let result =
    compile_controller("app/http/controllers/user_controller", source)

  case result {
    Error(msg) -> {
      msg
      |> string.contains("has parameter 'req' without a type annotation")
      |> should.be_true

      msg
      |> string.contains("Please specify the type: req: Request")
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
pub fn index(req: Request, ctx) {
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
      |> string.contains("Please specify the type: ctx: Context")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn hints_about_data_type_for_validated_param_test() {
  let source =
    "
/// @post \"/users\"
/// @validator \"user_validator\"
///
pub fn store(req: Request, ctx: Context, validated) {
  wisp.ok()
}
"

  let result =
    compile_controller("app/http/controllers/user_controller", source)

  case result {
    Error(msg) -> {
      // Should show the extra param error with a hint
      msg
      |> string.contains("has parameter 'validated' but route")
      |> should.be_true

      msg
      |> string.contains("If this is meant to be validated data")
      |> should.be_true

      msg
      |> string.contains("validated: user_validator.Data")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn hints_about_data_type_for_data_param_test() {
  let source =
    "
/// @post \"/users\"
/// @validator \"user_validator\"
///
pub fn store(req: Request, ctx: Context, data) {
  wisp.ok()
}
"

  let result =
    compile_controller("app/http/controllers/user_controller", source)

  case result {
    Error(msg) -> {
      // Should show the extra param error with a hint
      msg
      |> string.contains("has parameter 'data' but route")
      |> should.be_true

      msg
      |> string.contains("If this is meant to be validated data")
      |> should.be_true

      msg
      |> string.contains("data: user_validator.Data")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn uses_underscore_for_unused_req_ctx_test() {
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

  // When no handler uses req/ctx, they should be marked as unused
  result.uses_req
  |> should.be_false

  result.uses_ctx
  |> should.be_false
}

pub fn detects_req_usage_test() {
  let source =
    "
/// @get \"/users\"
///
pub fn index(req: Request) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  result.uses_req
  |> should.be_true

  result.uses_ctx
  |> should.be_false
}

pub fn detects_ctx_usage_test() {
  let source =
    "
/// @get \"/users\"
///
pub fn index(ctx: Context) {
  wisp.ok()
}
"

  let assert Ok(result) =
    compile_controller("app/http/controllers/user_controller", source)

  result.uses_req
  |> should.be_false

  result.uses_ctx
  |> should.be_true
}
