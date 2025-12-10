import gleam/http
import gleam/http/request
import gleeunit/should
import glimr/http/kernel
import glimr/routing/router
import routing/helpers
import wisp

// Prefix Matching Tests

pub fn prefix_match_exact_test() {
  let groups = [
    router.RouteGroup(
      prefix: "/api",
      middleware_group: kernel.Api,
      routes: fn(_path, _method, _req, _ctx) {
        // This shouldn't be called
        wisp.response(500)
      },
    ),
    router.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn(_path, _method, _req, _ctx) { wisp.response(200) },
    ),
  ]

  let req = helpers.make_request() |> request.set_path("/home")

  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  response.status
  |> should.equal(200)
}

pub fn prefix_match_api_route_test() {
  let groups = [
    router.RouteGroup(
      prefix: "/api",
      middleware_group: kernel.Api,
      routes: fn(_path, _method, _req, _ctx) { wisp.response(201) },
    ),
    router.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn(_path, _method, _req, _ctx) { wisp.response(200) },
    ),
  ]

  let req = helpers.make_request() |> request.set_path("/api/users")

  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  response.status
  |> should.equal(201)
}

pub fn prefix_match_nested_prefix_test() {
  let groups = [
    router.RouteGroup(
      prefix: "/api/v1",
      middleware_group: kernel.Api,
      routes: fn(_path, _method, _req, _ctx) { wisp.response(202) },
    ),
    router.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn(_path, _method, _req, _ctx) { wisp.response(200) },
    ),
  ]

  let req = helpers.make_request() |> request.set_path("/api/v1/users")

  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  response.status
  |> should.equal(202)
}

pub fn prefix_match_first_wins_test() {
  // Both groups could match "/api/users", but first should win
  let groups = [
    router.RouteGroup(
      prefix: "/api",
      middleware_group: kernel.Api,
      routes: fn(_path, _method, _req, _ctx) { wisp.response(201) },
    ),
    router.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn(_path, _method, _req, _ctx) { wisp.response(200) },
    ),
  ]

  let req = helpers.make_request() |> request.set_path("/api/users")

  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  // API handler should be called (201), not web (200)
  response.status
  |> should.equal(201)
}

pub fn prefix_match_catch_all_test() {
  // Empty prefix matches everything
  let groups = [
    router.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn(_path, _method, _req, _ctx) { wisp.response(200) },
    ),
  ]

  let req = helpers.make_request() |> request.set_path("/any/path/here")

  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  response.status
  |> should.equal(200)
}

// Path Stripping Tests

pub fn path_stripping_removes_prefix_test() {
  let groups = [
    router.RouteGroup(
      prefix: "/api",
      middleware_group: kernel.Api,
      routes: fn(path, _method, _req, _ctx) {
        // Path should be stripped of "/api"
        case path {
          ["users", "123"] -> wisp.response(201)
          _ -> wisp.response(500)
        }
      },
    ),
    router.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn(_path, _method, _req, _ctx) { wisp.response(200) },
    ),
  ]

  let req = helpers.make_request() |> request.set_path("/api/users/123")

  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  response.status
  |> should.equal(201)
}

pub fn path_stripping_empty_prefix_no_change_test() {
  let groups = [
    router.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn(path, _method, _req, _ctx) {
        // Path should include all segments
        case path {
          ["users", "123"] -> wisp.response(200)
          _ -> wisp.response(500)
        }
      },
    ),
  ]

  let req = helpers.make_request() |> request.set_path("/users/123")

  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  response.status
  |> should.equal(200)
}

pub fn path_stripping_nested_prefix_test() {
  let groups = [
    router.RouteGroup(
      prefix: "/api/v1",
      middleware_group: kernel.Api,
      routes: fn(path, _method, _req, _ctx) {
        // Path should be stripped of "/api/v1"
        case path {
          ["users"] -> wisp.response(201)
          _ -> wisp.response(500)
        }
      },
    ),
  ]

  let req = helpers.make_request() |> request.set_path("/api/v1/users")

  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  response.status
  |> should.equal(201)
}

pub fn path_stripping_root_path_test() {
  let groups = [
    router.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn(path, _method, _req, _ctx) {
        // Root path becomes empty list
        case path {
          [] -> wisp.response(200)
          _ -> wisp.response(500)
        }
      },
    ),
  ]

  let req = helpers.make_request() |> request.set_path("/")

  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  response.status
  |> should.equal(200)
}

// Pattern Matching in Handlers Tests

pub fn handler_pattern_matching_exact_test() {
  let groups = [
    router.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn(path, method, _req, _ctx) {
        case path, method {
          ["users"], http.Get -> wisp.response(200)
          _, _ -> wisp.response(404)
        }
      },
    ),
  ]

  let req = helpers.make_request() |> request.set_path("/users")

  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  response.status
  |> should.equal(200)
}

pub fn handler_pattern_matching_with_id_test() {
  let groups = [
    router.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn(path, method, _req, _ctx) {
        case path, method {
          ["users", id], http.Get -> {
            // Type-safe id extraction!
            case id {
              "123" -> wisp.response(200)
              _ -> wisp.response(404)
            }
          }
          _, _ -> wisp.response(404)
        }
      },
    ),
  ]

  let req = helpers.make_request() |> request.set_path("/users/123")

  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  response.status
  |> should.equal(200)
}

pub fn handler_pattern_matching_method_test() {
  let groups = [
    router.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn(path, method, _req, _ctx) {
        case path, method {
          ["users"], http.Get -> wisp.response(200)
          ["users"], http.Post -> wisp.response(201)
          _, _ -> wisp.response(404)
        }
      },
    ),
  ]

  // Test GET
  let req =
    helpers.make_request()
    |> request.set_path("/users")
    |> request.set_method(http.Get)

  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  response.status
  |> should.equal(200)

  // Test POST
  let req =
    helpers.make_request()
    |> request.set_path("/users")
    |> request.set_method(http.Post)

  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  response.status
  |> should.equal(201)
}

pub fn handler_pattern_matching_catch_all_test() {
  let groups = [
    router.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn(path, method, _req, _ctx) {
        case path, method {
          ["users"], http.Get -> wisp.response(200)
          _, _ -> wisp.response(404)
        }
      },
    ),
  ]

  let req = helpers.make_request() |> request.set_path("/posts")

  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  response.status
  |> should.equal(404)
}

// Middleware Group Tests

pub fn middleware_group_api_test() {
  let groups = [
    router.RouteGroup(
      prefix: "/api",
      middleware_group: kernel.Api,
      routes: fn(_path, _method, _req, _ctx) { wisp.response(200) },
    ),
    router.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn(_path, _method, _req, _ctx) { wisp.response(200) },
    ),
  ]

  let req = helpers.make_request() |> request.set_path("/api/users")

  // Track which middleware group was used
  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, middleware_group, next) {
        case middleware_group {
          kernel.Api -> {
            let resp = next(req)
            wisp.set_header(resp, "x-middleware", "api")
          }
          kernel.Web -> {
            let resp = next(req)
            wisp.set_header(resp, "x-middleware", "web")
          }
          kernel.Custom(_) -> {
            let resp = next(req)
            wisp.set_header(resp, "x-middleware", "custom")
          }
        }
      },
    )

  response.headers
  |> should.equal([#("x-middleware", "api")])
}

pub fn middleware_group_web_test() {
  let groups = [
    router.RouteGroup(
      prefix: "/api",
      middleware_group: kernel.Api,
      routes: fn(_path, _method, _req, _ctx) { wisp.response(200) },
    ),
    router.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn(_path, _method, _req, _ctx) { wisp.response(200) },
    ),
  ]

  let req = helpers.make_request() |> request.set_path("/home")

  // Track which middleware group was used
  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, middleware_group, next) {
        case middleware_group {
          kernel.Api -> {
            let resp = next(req)
            wisp.set_header(resp, "x-middleware", "api")
          }
          kernel.Web -> {
            let resp = next(req)
            wisp.set_header(resp, "x-middleware", "web")
          }
          kernel.Custom(_) -> {
            let resp = next(req)
            wisp.set_header(resp, "x-middleware", "custom")
          }
        }
      },
    )

  response.headers
  |> should.equal([#("x-middleware", "web")])
}

// Error Cases Tests

pub fn no_matching_group_returns_404_test() {
  // No groups registered at all
  let groups = []

  let req = helpers.make_request() |> request.set_path("/users")

  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  response.status
  |> should.equal(404)
}

pub fn handler_404_passes_through_test() {
  let groups = [
    router.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn(path, method, _req, _ctx) {
        case path, method {
          ["users"], http.Get -> wisp.response(200)
          _, _ -> wisp.response(404)
        }
      },
    ),
  ]

  let req = helpers.make_request() |> request.set_path("/posts")

  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  response.status
  |> should.equal(404)
}

// Context Tests

pub fn context_passed_to_handler_test() {
  let groups = [
    router.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn(_path, _method, _req, ctx) {
        // Handler can access context
        case ctx {
          helpers.TestContext("secret") -> wisp.response(200)
          _ -> wisp.response(500)
        }
      },
    ),
  ]

  let req = helpers.make_request() |> request.set_path("/")

  let response =
    router.handle(
      req,
      helpers.TestContext("secret"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  response.status
  |> should.equal(200)
}

pub fn request_passed_to_handler_test() {
  let groups = [
    router.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn(_path, _method, req, _ctx) {
        // Handler can access request
        case req.method {
          http.Post -> wisp.response(201)
          _ -> wisp.response(200)
        }
      },
    ),
  ]

  let req =
    helpers.make_request()
    |> request.set_path("/")
    |> request.set_method(http.Post)

  let response =
    router.handle(
      req,
      helpers.TestContext("test"),
      groups,
      fn(req, _ctx, _middleware_group, next) { next(req) },
    )

  response.status
  |> should.equal(201)
}
