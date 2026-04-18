import gleam/http
import gleam/http/request
import gleeunit/should
import glimr/config
import glimr/http/context.{type Context}
import glimr/http/middleware
import glimr/http/middleware/expects_html
import glimr/http/middleware/expects_json
import glimr/http/middleware/handle_head
import glimr/http/middleware/log_request
import glimr/http/middleware/method_override
import glimr/http/middleware/rescue_crashes
import glimr/http/middleware/serve_static
import glimr/http/response
import routing/helpers.{TestApp}
import simplifile
import wisp

const config_dir = "config"

const config_file = "config/app.toml"

@external(erlang, "erlang", "make_ref")
fn stub_connection() -> wisp.Connection

fn ensure_app_config() -> Nil {
  let _ = simplifile.create_directory_all(config_dir)
  let _ = simplifile.write(config_file, "[static]\ndirectory = \"/static\"\n")
  clear_config_cache()
  config.load()
  Nil
}

fn cleanup_app_config() -> Nil {
  let _ = simplifile.delete(config_file)
  let _ = simplifile.delete(config_dir)
  clear_config_cache()
  Nil
}

fn make_request() -> wisp.Request {
  request.new()
  |> request.set_method(http.Get)
  |> request.set_path("/")
  |> request.set_body(stub_connection())
}

// ------------------------------------------------------------- Conformity Tests

pub fn serve_static_conforms_to_middleware_type_test() {
  ensure_app_config()
  let req = make_request()
  let ctx = context.new(req, TestApp("test"))

  let resp =
    middleware.apply([serve_static.run], ctx, fn(_ctx) { response.empty(200) })

  resp.status
  |> should.equal(200)

  cleanup_app_config()
}

pub fn method_override_conforms_to_middleware_type_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test"))

  let resp =
    middleware.apply([method_override.run], ctx, fn(_ctx) {
      response.empty(200)
    })

  resp.status
  |> should.equal(200)
}

pub fn log_request_conforms_to_middleware_type_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test"))

  let resp =
    middleware.apply([log_request.run], ctx, fn(_ctx) { response.empty(200) })

  resp.status
  |> should.equal(200)
}

pub fn rescue_crashes_conforms_to_middleware_type_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test"))

  let resp =
    middleware.apply([rescue_crashes.run], ctx, fn(_ctx) { response.empty(200) })

  resp.status
  |> should.equal(200)
}

pub fn expects_html_conforms_to_middleware_type_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test"))

  let resp =
    middleware.apply([expects_html.run], ctx, fn(_ctx) { response.empty(200) })

  resp.status
  |> should.equal(200)
}

pub fn expects_json_conforms_to_middleware_type_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test"))

  let resp =
    middleware.apply([expects_json.run], ctx, fn(_ctx) { response.empty(200) })

  resp.status
  |> should.equal(200)
}

pub fn handle_head_conforms_to_middleware_type_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test"))

  let resp =
    middleware.apply([handle_head.run], ctx, fn(_ctx) { response.empty(200) })

  resp.status
  |> should.equal(200)
}

// ------------------------------------------------------------- Passthrough Tests

pub fn method_override_passes_context_through_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("preserved"))

  let resp =
    middleware.apply(
      [method_override.run],
      ctx,
      fn(ctx: Context(helpers.TestApp)) {
        case ctx.app {
          TestApp("preserved") -> response.empty(200)
          _ -> response.empty(500)
        }
      },
    )

  resp.status
  |> should.equal(200)
}

pub fn log_request_passes_request_and_context_through_test() {
  let req =
    make_request()
    |> request.set_path("/test-path")
  let ctx = context.new(req, TestApp("preserved"))

  let resp =
    middleware.apply([log_request.run], ctx, fn(ctx: Context(helpers.TestApp)) {
      let path = wisp.path_segments(ctx.req)
      case path, ctx.app {
        ["test-path"], TestApp("preserved") -> response.empty(200)
        _, _ -> response.empty(500)
      }
    })

  resp.status
  |> should.equal(200)
}

pub fn rescue_crashes_passes_request_and_context_through_test() {
  let req =
    make_request()
    |> request.set_path("/safe")
  let ctx = context.new(req, TestApp("preserved"))

  let resp =
    middleware.apply(
      [rescue_crashes.run],
      ctx,
      fn(ctx: Context(helpers.TestApp)) {
        let path = wisp.path_segments(ctx.req)
        case path, ctx.app {
          ["safe"], TestApp("preserved") -> response.empty(200)
          _, _ -> response.empty(500)
        }
      },
    )

  resp.status
  |> should.equal(200)
}

// ------------------------------------------------------------- Behavior Tests

pub fn serve_static_passes_through_for_non_static_requests_test() {
  ensure_app_config()
  let req =
    make_request()
    |> request.set_path("/users")
  let ctx = context.new(req, TestApp("test"))

  let resp =
    middleware.apply([serve_static.run], ctx, fn(_ctx) { response.empty(200) })

  resp.status
  |> should.equal(200)

  cleanup_app_config()
}

pub fn handle_head_converts_head_to_get_test() {
  let req =
    make_request()
    |> request.set_method(http.Head)
  let ctx = context.new(req, TestApp("test"))

  let resp =
    middleware.apply([handle_head.run], ctx, fn(ctx: Context(helpers.TestApp)) {
      // handle_head converts HEAD to GET for the handler
      case ctx.req.method {
        http.Get -> response.empty(200)
        _ -> response.empty(500)
      }
    })

  resp.status
  |> should.equal(200)
}

// ------------------------------------------------------------- Pipeline Tests

pub fn multiple_wrappers_compose_in_pipeline_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test"))

  let resp =
    middleware.apply(
      [
        method_override.run,
        log_request.run,
        rescue_crashes.run,
        handle_head.run,
      ],
      ctx,
      fn(_ctx) { response.empty(200) },
    )

  resp.status
  |> should.equal(200)
}

pub fn full_web_pipeline_test() {
  ensure_app_config()
  let req =
    make_request()
    |> request.set_path("/users")
  let ctx = context.new(req, TestApp("test"))

  let resp =
    middleware.apply(
      [
        expects_html.run,
        serve_static.run,
        method_override.run,
        log_request.run,
        rescue_crashes.run,
        handle_head.run,
      ],
      ctx,
      fn(_ctx) { response.empty(200) },
    )

  resp.status
  |> should.equal(200)

  cleanup_app_config()
}

pub fn full_api_pipeline_test() {
  let req =
    make_request()
    |> request.set_path("/api/users")
  let ctx = context.new(req, TestApp("test"))

  let resp =
    middleware.apply(
      [
        expects_json.run,
        method_override.run,
        log_request.run,
        rescue_crashes.run,
        handle_head.run,
      ],
      ctx,
      fn(_ctx) { response.empty(200) },
    )

  resp.status
  |> should.equal(200)
}

// ------------------------------------------------------------- FFI Helpers

@external(erlang, "glimr_app_config_test_ffi", "clear_config_cache")
fn clear_config_cache() -> Nil
