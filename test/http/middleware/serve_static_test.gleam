import gleam/http
import gleam/http/request
import gleeunit/should
import glimr/http/middleware
import glimr/http/middleware/handle_head
import glimr/http/middleware/html_errors
import glimr/http/middleware/json_errors
import glimr/http/middleware/log_request
import glimr/http/middleware/method_override
import glimr/http/middleware/rescue_crashes
import glimr/http/middleware/serve_static
import wisp

pub type TestContext {
  TestContext(value: String)
}

@external(erlang, "erlang", "make_ref")
fn stub_connection() -> wisp.Connection

fn make_request() -> wisp.Request {
  request.new()
  |> request.set_method(http.Get)
  |> request.set_path("/")
  |> request.set_body(stub_connection())
}

// ------------------------------------------------------------- Conformity Tests

pub fn serve_static_conforms_to_middleware_type_test() {
  let req = make_request()
  let ctx = TestContext("test")

  let response =
    middleware.apply([serve_static.run], req, ctx, fn(_req, _ctx) {
      wisp.response(200)
    })

  response.status
  |> should.equal(200)
}

pub fn method_override_conforms_to_middleware_type_test() {
  let req = make_request()
  let ctx = TestContext("test")

  let response =
    middleware.apply([method_override.run], req, ctx, fn(_req, _ctx) {
      wisp.response(200)
    })

  response.status
  |> should.equal(200)
}

pub fn log_request_conforms_to_middleware_type_test() {
  let req = make_request()
  let ctx = TestContext("test")

  let response =
    middleware.apply([log_request.run], req, ctx, fn(_req, _ctx) {
      wisp.response(200)
    })

  response.status
  |> should.equal(200)
}

pub fn rescue_crashes_conforms_to_middleware_type_test() {
  let req = make_request()
  let ctx = TestContext("test")

  let response =
    middleware.apply([rescue_crashes.run], req, ctx, fn(_req, _ctx) {
      wisp.response(200)
    })

  response.status
  |> should.equal(200)
}

pub fn handle_head_conforms_to_middleware_type_test() {
  let req = make_request()
  let ctx = TestContext("test")

  let response =
    middleware.apply([handle_head.run], req, ctx, fn(_req, _ctx) {
      wisp.response(200)
    })

  response.status
  |> should.equal(200)
}

pub fn html_errors_conforms_to_middleware_type_test() {
  let req = make_request()
  let ctx = TestContext("test")

  let response =
    middleware.apply([html_errors.run], req, ctx, fn(_req, _ctx) {
      wisp.response(200)
    })

  response.status
  |> should.equal(200)
}

pub fn json_errors_conforms_to_middleware_type_test() {
  let req = make_request()
  let ctx = TestContext("test")

  let response =
    middleware.apply([json_errors.run], req, ctx, fn(_req, _ctx) {
      wisp.response(200)
    })

  response.status
  |> should.equal(200)
}

// ------------------------------------------------------------- Passthrough Tests

pub fn method_override_passes_context_through_test() {
  let req = make_request()
  let ctx = TestContext("preserved")

  let response =
    middleware.apply([method_override.run], req, ctx, fn(_req, ctx) {
      case ctx {
        TestContext("preserved") -> wisp.response(200)
        _ -> wisp.response(500)
      }
    })

  response.status
  |> should.equal(200)
}

pub fn log_request_passes_request_and_context_through_test() {
  let req =
    make_request()
    |> request.set_path("/test-path")
  let ctx = TestContext("preserved")

  let response =
    middleware.apply([log_request.run], req, ctx, fn(req, ctx) {
      let path = wisp.path_segments(req)
      case path, ctx {
        ["test-path"], TestContext("preserved") -> wisp.response(200)
        _, _ -> wisp.response(500)
      }
    })

  response.status
  |> should.equal(200)
}

pub fn rescue_crashes_passes_request_and_context_through_test() {
  let req =
    make_request()
    |> request.set_path("/safe")
  let ctx = TestContext("preserved")

  let response =
    middleware.apply([rescue_crashes.run], req, ctx, fn(req, ctx) {
      let path = wisp.path_segments(req)
      case path, ctx {
        ["safe"], TestContext("preserved") -> wisp.response(200)
        _, _ -> wisp.response(500)
      }
    })

  response.status
  |> should.equal(200)
}

// ------------------------------------------------------------- Behavior Tests

pub fn serve_static_passes_through_for_non_static_requests_test() {
  let req =
    make_request()
    |> request.set_path("/users")
  let ctx = TestContext("test")

  let response =
    middleware.apply([serve_static.run], req, ctx, fn(_req, _ctx) {
      wisp.response(200)
    })

  response.status
  |> should.equal(200)
}

pub fn handle_head_converts_head_to_get_test() {
  let req =
    make_request()
    |> request.set_method(http.Head)
  let ctx = TestContext("test")

  let response =
    middleware.apply([handle_head.run], req, ctx, fn(req, _ctx) {
      // handle_head converts HEAD to GET for the handler
      case req.method {
        http.Get -> wisp.response(200)
        _ -> wisp.response(500)
      }
    })

  response.status
  |> should.equal(200)
}

pub fn html_errors_replaces_error_body_test() {
  let req = make_request()
  let ctx = TestContext("test")

  let response =
    middleware.apply([html_errors.run], req, ctx, fn(_req, _ctx) {
      wisp.response(404)
    })

  response.status
  |> should.equal(404)

  // Should have an HTML error body instead of empty
  case response.body {
    wisp.Text(body) -> {
      body
      |> should.not_equal("")
    }
    _ -> should.fail()
  }
}

pub fn json_errors_replaces_error_body_test() {
  let req = make_request()
  let ctx = TestContext("test")

  let response =
    middleware.apply([json_errors.run], req, ctx, fn(_req, _ctx) {
      wisp.response(404)
    })

  response.status
  |> should.equal(404)

  // Should have a JSON error body instead of empty
  case response.body {
    wisp.Text(body) -> {
      body
      |> should.not_equal("")
    }
    _ -> should.fail()
  }
}

// ------------------------------------------------------------- Pipeline Tests

pub fn multiple_wrappers_compose_in_pipeline_test() {
  let req = make_request()
  let ctx = TestContext("test")

  let response =
    middleware.apply(
      [
        method_override.run,
        log_request.run,
        rescue_crashes.run,
        handle_head.run,
      ],
      req,
      ctx,
      fn(_req, _ctx) { wisp.response(200) },
    )

  response.status
  |> should.equal(200)
}

pub fn full_web_pipeline_test() {
  let req =
    make_request()
    |> request.set_path("/users")
  let ctx = TestContext("test")

  let response =
    middleware.apply(
      [
        serve_static.run,
        method_override.run,
        log_request.run,
        html_errors.run,
        rescue_crashes.run,
        handle_head.run,
      ],
      req,
      ctx,
      fn(_req, _ctx) { wisp.response(200) },
    )

  response.status
  |> should.equal(200)
}

pub fn full_api_pipeline_test() {
  let req =
    make_request()
    |> request.set_path("/api/users")
  let ctx = TestContext("test")

  let response =
    middleware.apply(
      [
        method_override.run,
        log_request.run,
        json_errors.run,
        rescue_crashes.run,
        handle_head.run,
      ],
      req,
      ctx,
      fn(_req, _ctx) { wisp.response(200) },
    )

  response.status
  |> should.equal(200)
}
