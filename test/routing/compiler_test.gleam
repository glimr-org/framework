import gleam/list
import gleam/string
import gleeunit/should
import glimr/routing/compiler
import simplifile

// Helper to create a temp route file and compile it
fn compile_route_source(source: String) {
  let temp_path = "test/routing/_temp_route.gleam"
  let assert Ok(_) = simplifile.write(temp_path, source)
  let result = compiler.compile_file(temp_path)
  let _ = simplifile.delete(temp_path)
  result
}

pub fn parse_get_route_test() {
  let source =
    "
import app/http/controllers/home_controller
import glimr/routing/route

pub fn routes() {
  [
    route.get(\"/\", home_controller.show),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

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
import app/http/controllers/user_controller
import glimr/routing/route

pub fn routes() {
  [
    route.post(\"/users\", user_controller.store),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

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
import app/http/controllers/user_controller
import glimr/routing/route

pub fn routes() {
  [
    route.get(\"/users\", user_controller.index),
    route.post(\"/users\", user_controller.store),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

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

// Prefix Tests

pub fn parse_prefix_test() {
  let source =
    "
import app/http/controllers/api_controller
import glimr/routing/route

pub fn routes() {
  [
    route.prefix(\"/api\", [
      route.get(\"/users\", api_controller.users),
    ]),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

  result.routes_code
  |> string.contains("[\"api\", \"users\"] ->")
  |> should.be_true
}

pub fn parse_nested_prefix_test() {
  let source =
    "
import app/http/controllers/admin_controller
import glimr/routing/route

pub fn routes() {
  [
    route.prefix(\"/api\", [
      route.prefix(\"/v1\", [
        route.get(\"/users\", admin_controller.users),
      ]),
    ]),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

  result.routes_code
  |> string.contains("[\"api\", \"v1\", \"users\"] ->")
  |> should.be_true
}

// Redirect Tests

pub fn parse_redirect_test() {
  let source =
    "
import glimr/routing/route

pub fn routes() {
  [
    route.redirect(\"/old\", \"/new\"),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

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
import glimr/routing/route

pub fn routes() {
  [
    route.redirect_permanent(\"/old-path\", \"/new-path\"),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

  result.routes_code
  |> string.contains("[\"old-path\"] ->")
  |> should.be_true

  result.routes_code
  |> string.contains("wisp.moved_permanently(\"/new-path\")")
  |> should.be_true
}

// Middleware Tests

pub fn parse_route_with_middleware_test() {
  let source =
    "
import app/http/controllers/admin_controller
import app/http/middleware/auth
import glimr/routing/route

pub fn routes() {
  [
    route.get(\"/admin\", admin_controller.index)
      |> route.middleware([auth.handle]),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

  result.uses_middleware
  |> should.be_true

  result.routes_code
  |> string.contains(
    "use req, ctx <- middleware.apply([auth.handle], req, ctx)",
  )
  |> should.be_true
}

pub fn parse_route_with_multiple_middleware_test() {
  let source =
    "
import app/http/controllers/admin_controller
import app/http/middleware/auth
import app/http/middleware/logging
import glimr/routing/route

pub fn routes() {
  [
    route.get(\"/admin\", admin_controller.index)
      |> route.middleware([auth.handle, logging.handle]),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

  result.routes_code
  |> string.contains(
    "middleware.apply([auth.handle, logging.handle], req, ctx)",
  )
  |> should.be_true
}

// Group Middleware Tests

pub fn parse_group_middleware_test() {
  let source =
    "
import app/http/controllers/admin_controller
import app/http/middleware/auth
import glimr/routing/route

pub fn routes() {
  [
    route.group_middleware([auth.handle], [
      route.get(\"/dashboard\", admin_controller.dashboard),
      route.get(\"/settings\", admin_controller.settings),
    ]),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

  result.uses_middleware
  |> should.be_true

  // Both routes should have the middleware applied
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
import app/http/controllers/admin_controller
import app/http/middleware/auth
import app/http/middleware/logging
import glimr/routing/route

pub fn routes() {
  [
    route.group_middleware([auth.handle], [
      route.get(\"/dashboard\", admin_controller.dashboard)
        |> route.middleware([logging.handle]),
    ]),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

  // Should have both middleware - group middleware first, then route middleware
  result.routes_code
  |> string.contains("auth.handle")
  |> should.be_true

  result.routes_code
  |> string.contains("logging.handle")
  |> should.be_true
}

pub fn parse_prefix_with_group_middleware_test() {
  let source =
    "
import app/http/controllers/admin_controller
import app/http/middleware/auth
import glimr/routing/route

pub fn routes() {
  [
    route.prefix(\"/admin\", [
      route.group_middleware([auth.handle], [
        route.get(\"/users\", admin_controller.users),
      ]),
    ]),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

  result.routes_code
  |> string.contains("[\"admin\", \"users\"] ->")
  |> should.be_true

  result.routes_code
  |> string.contains("middleware.apply([auth.handle], req, ctx)")
  |> should.be_true
}

// Import Extraction Tests

pub fn extracts_controller_imports_test() {
  let source =
    "
import app/http/controllers/home_controller
import app/http/controllers/user_controller
import glimr/routing/route

pub fn routes() {
  [
    route.get(\"/\", home_controller.show),
    route.get(\"/users\", user_controller.index),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

  result.imports
  |> list.any(fn(i) { string.contains(i, "home_controller") })
  |> should.be_true

  result.imports
  |> list.any(fn(i) { string.contains(i, "user_controller") })
  |> should.be_true

  // Should not include route import
  result.imports
  |> list.any(fn(i) { string.contains(i, "glimr/routing/route") })
  |> should.be_false
}

pub fn extracts_middleware_imports_test() {
  let source =
    "
import app/http/controllers/admin_controller
import app/http/middleware/auth
import glimr/routing/route

pub fn routes() {
  [
    route.get(\"/admin\", admin_controller.index)
      |> route.middleware([auth.handle]),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

  result.imports
  |> list.any(fn(i) { string.contains(i, "middleware/auth") })
  |> should.be_true
}

// Route Parameters Tests

pub fn parse_route_with_parameter_test() {
  let source =
    "
import app/http/controllers/user_controller
import glimr/routing/route

pub fn routes() {
  [
    route.get(\"/users/{id}\", user_controller.show),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

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
import app/http/controllers/comment_controller
import glimr/routing/route

pub fn routes() {
  [
    route.get(\"/posts/{post_id}/comments/{comment_id}\", comment_controller.show),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

  result.routes_code
  |> string.contains("[\"posts\", post_id, \"comments\", comment_id] ->")
  |> should.be_true

  result.routes_code
  |> string.contains("comment_controller.show(req, ctx, post_id, comment_id)")
  |> should.be_true
}

// 404 Fallback Test

pub fn generates_404_fallback_test() {
  let source =
    "
import app/http/controllers/home_controller
import glimr/routing/route

pub fn routes() {
  [
    route.get(\"/\", home_controller.show),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

  result.routes_code
  |> string.contains("_ -> wisp.not_found()")
  |> should.be_true
}

// Method Not Allowed Test

pub fn generates_method_not_allowed_test() {
  let source =
    "
import app/http/controllers/user_controller
import glimr/routing/route

pub fn routes() {
  [
    route.get(\"/users\", user_controller.index),
    route.post(\"/users\", user_controller.store),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

  result.routes_code
  |> string.contains("wisp.method_not_allowed([Get, Post])")
  |> should.be_true
}

// Complex Route File Test

pub fn parse_complex_route_file_test() {
  let source =
    "
import app/http/controllers/home_controller
import app/http/controllers/user_controller
import app/http/controllers/admin_controller
import app/http/middleware/auth
import app/http/middleware/logging
import glimr/routing/route

pub fn routes() {
  [
    route.redirect(\"/\", \"/home\"),
    route.get(\"/home\", home_controller.show),

    route.prefix(\"/users\", [
      route.get(\"/\", user_controller.index),
      route.post(\"/\", user_controller.store),
      route.get(\"/{id}\", user_controller.show),
    ]),

    route.prefix(\"/admin\", [
      route.group_middleware([auth.handle], [
        route.get(\"/dashboard\", admin_controller.dashboard)
          |> route.middleware([logging.handle]),
        route.get(\"/settings\", admin_controller.settings),
      ]),
    ]),
  ]
}
"

  let assert Ok(result) = compile_route_source(source)

  // Check redirect exists
  result.routes_code
  |> string.contains("wisp.redirect")
  |> should.be_true

  // Check home route
  result.routes_code
  |> string.contains("home")
  |> should.be_true

  // Check user routes with prefix
  result.routes_code
  |> string.contains("users")
  |> should.be_true

  // Check admin routes with prefix
  result.routes_code
  |> string.contains("admin")
  |> should.be_true

  // Check middleware is applied
  result.uses_middleware
  |> should.be_true
}
