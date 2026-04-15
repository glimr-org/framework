import gleam/http
import gleam/http/request
import gleam/option.{type Option, None, Some}
import gleeunit/should
import glimr/http/context.{type Context, Context}
import glimr/http/middleware
import glimr/http/response
import wisp

pub type TestApp {
  TestApp(value: String, user: Option(String))
}

@external(erlang, "erlang", "make_ref")
fn stub_connection() -> wisp.Connection

pub fn make_request() -> wisp.Request {
  request.new()
  |> request.set_method(http.Get)
  |> request.set_path("/")
  |> request.set_body(stub_connection())
}

// ------------------------------------------------------------- No Middleware Tests

pub fn apply_no_middleware_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test", None))

  let response = middleware.apply([], ctx, fn(_ctx) { response.empty(200) })

  response.status
  |> should.equal(200)
}

pub fn apply_no_middleware_calls_handler_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test", None))

  // Handler should be called directly
  let response =
    middleware.apply([], ctx, fn(ctx: Context(TestApp)) {
      // Verify request is passed through
      case ctx.req.method {
        http.Get -> response.empty(200)
        _ -> response.empty(500)
      }
    })

  response.status
  |> should.equal(200)
}

// ------------------------------------------------------------- Single Middleware Tests

pub fn apply_single_middleware_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test", None))

  let middleware1 = fn(ctx: Context(TestApp), next) {
    let resp = next(ctx)
    wisp.set_header(resp, "x-middleware", "applied")
  }

  let response =
    middleware.apply([middleware1], ctx, fn(_ctx) { response.empty(200) })

  response.status
  |> should.equal(200)

  response.headers
  |> should.equal([#("x-middleware", "applied")])
}

pub fn apply_single_middleware_modifies_request_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test", None))

  let middleware1 = fn(ctx: Context(TestApp), next) {
    // Modify request before passing to next
    let new_req = request.set_path(ctx.req, "/modified")
    next(Context(..ctx, req: new_req))
  }

  let response =
    middleware.apply([middleware1], ctx, fn(ctx: Context(TestApp)) {
      let path = wisp.path_segments(ctx.req)
      case path {
        ["modified"] -> response.empty(200)
        _ -> response.empty(500)
      }
    })

  response.status
  |> should.equal(200)
}

pub fn apply_single_middleware_accesses_context_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("secret", None))

  let middleware1 = fn(ctx: Context(TestApp), next) {
    case ctx.app {
      TestApp("secret", _) -> {
        let resp = next(ctx)
        wisp.set_header(resp, "x-auth", "valid")
      }
      _ -> {
        let resp = next(ctx)
        wisp.set_header(resp, "x-auth", "invalid")
      }
    }
  }

  let response =
    middleware.apply([middleware1], ctx, fn(_ctx) { response.empty(200) })

  response.headers
  |> should.equal([#("x-auth", "valid")])
}

// ------------------------------------------------------------- Multiple Middleware Tests

pub fn apply_multiple_middleware_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test", None))

  let middleware1 = fn(ctx: Context(TestApp), next) {
    let resp = next(ctx)
    wisp.set_header(resp, "x-first", "1")
  }

  let middleware2 = fn(ctx: Context(TestApp), next) {
    let resp = next(ctx)
    wisp.set_header(resp, "x-second", "2")
  }

  let response =
    middleware.apply([middleware1, middleware2], ctx, fn(_ctx) {
      response.empty(200)
    })

  response.status
  |> should.equal(200)

  // Both headers should be present
  let headers = response.headers
  let assert Ok(first) =
    headers
    |> list_find(fn(h) { h.0 == "x-first" })
  let assert Ok(second) =
    headers
    |> list_find(fn(h) { h.0 == "x-second" })

  first.1
  |> should.equal("1")

  second.1
  |> should.equal("2")
}

pub fn apply_middleware_execution_order_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test", None))

  // Middleware execute in order: first wraps second wraps third
  // So if they all set the same header, the outermost (first) has final say
  let middleware1 = fn(ctx: Context(TestApp), next) {
    let resp = next(ctx)
    wisp.set_header(resp, "x-order", "first")
  }

  let middleware2 = fn(ctx: Context(TestApp), next) {
    let resp = next(ctx)
    wisp.set_header(resp, "x-order", "second")
  }

  let middleware3 = fn(ctx: Context(TestApp), next) {
    let resp = next(ctx)
    wisp.set_header(resp, "x-order", "third")
  }

  let response =
    middleware.apply([middleware1, middleware2, middleware3], ctx, fn(_ctx) {
      response.empty(200)
    })

  // First middleware (outermost) has final say
  response.headers
  |> should.equal([#("x-order", "first")])
}

pub fn apply_middleware_chain_modifies_request_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test", None))

  let middleware1 = fn(ctx: Context(TestApp), next) {
    // First middleware sets path to /step1
    let new_req = request.set_path(ctx.req, "/step1")
    next(Context(..ctx, req: new_req))
  }

  let middleware2 = fn(ctx: Context(TestApp), next) {
    // Second middleware should see /step1 and change to /step2
    let path = wisp.path_segments(ctx.req)
    case path {
      ["step1"] -> {
        let new_req = request.set_path(ctx.req, "/step2")
        next(Context(..ctx, req: new_req))
      }
      _ -> next(ctx)
    }
  }

  let response =
    middleware.apply([middleware1, middleware2], ctx, fn(ctx: Context(TestApp)) {
      // Handler should see final path
      let path = wisp.path_segments(ctx.req)
      case path {
        ["step2"] -> response.empty(200)
        _ -> response.empty(500)
      }
    })

  response.status
  |> should.equal(200)
}

// ------------------------------------------------------------- Short-circuit Tests

pub fn apply_middleware_can_short_circuit_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("unauthorized", None))

  let auth_middleware = fn(ctx: Context(TestApp), next) {
    case ctx.app {
      TestApp("authorized", _) -> next(ctx)
      // Short-circuit - don't call next
      _ -> response.empty(401)
    }
  }

  let response =
    middleware.apply([auth_middleware], ctx, fn(_ctx) {
      // This handler should never be called
      response.empty(200)
    })

  response.status
  |> should.equal(401)
}

pub fn apply_middleware_early_return_stops_chain_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test", None))

  let middleware1 = fn(_ctx: Context(TestApp), _next) {
    // First middleware returns early without calling next
    response.empty(403)
  }

  let middleware2 = fn(ctx: Context(TestApp), next) {
    // This should never be called
    let resp = next(ctx)
    wisp.set_header(resp, "x-unreachable", "true")
  }

  let response =
    middleware.apply([middleware1, middleware2], ctx, fn(_ctx) {
      // Handler should never be called either
      response.empty(200)
    })

  response.status
  |> should.equal(403)

  // Second middleware's header should not be present
  response.headers
  |> should.equal([])
}

// ------------------------------------------------------------- Response Modification Tests

pub fn apply_middleware_modifies_response_body_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test", None))

  let middleware1 = fn(ctx: Context(TestApp), next) {
    let resp = next(ctx)
    // Modify response body
    wisp.html_body(resp, "<html>wrapped</html>")
  }

  let response =
    middleware.apply([middleware1], ctx, fn(_ctx) {
      wisp.html_response("original", 200)
    })

  case response.body {
    wisp.Text(content) -> {
      content
      |> should.equal("<html>wrapped</html>")
    }
    _ -> should.fail()
  }
}

pub fn apply_middleware_modifies_status_code_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test", None))

  let middleware1 = fn(ctx: Context(TestApp), next) {
    let _resp = next(ctx)
    // Change status code (create new response)
    response.empty(201)
  }

  let response =
    middleware.apply([middleware1], ctx, fn(_ctx) { response.empty(200) })

  response.status
  |> should.equal(201)
}

// ------------------------------------------------------------- Complex Scenarios

pub fn apply_middleware_realistic_auth_flow_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("admin", None))

  let auth_middleware = fn(ctx: Context(TestApp), next) {
    case ctx.app {
      TestApp("admin", _) -> {
        // Add user info to response header
        let resp = next(ctx)
        wisp.set_header(resp, "x-user", "admin")
      }
      _ -> response.empty(401)
    }
  }

  let logging_middleware = fn(ctx: Context(TestApp), next) {
    // Log the request (in real app would use io)
    let resp = next(ctx)
    wisp.set_header(resp, "x-logged", "true")
  }

  let response =
    middleware.apply([logging_middleware, auth_middleware], ctx, fn(_ctx) {
      response.empty(200)
    })

  response.status
  |> should.equal(200)

  let assert Ok(user_header) =
    response.headers
    |> list_find(fn(h) { h.0 == "x-user" })

  let assert Ok(logged_header) =
    response.headers
    |> list_find(fn(h) { h.0 == "x-logged" })

  user_header.1
  |> should.equal("admin")

  logged_header.1
  |> should.equal("true")
}

pub fn apply_middleware_modifies_context_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test", None))

  let auth_middleware = fn(ctx: Context(TestApp), next) {
    // Simulate authenticating a user and adding to context
    let updated_ctx =
      Context(..ctx, app: TestApp(..ctx.app, user: Some("alice")))
    next(updated_ctx)
  }

  let response =
    middleware.apply([auth_middleware], ctx, fn(ctx: Context(TestApp)) {
      // Handler should see the modified context
      case ctx.app.user {
        Some(username) -> {
          let resp = response.empty(200)
          wisp.set_header(resp, "x-username", username)
        }
        None -> response.empty(401)
      }
    })

  response.status
  |> should.equal(200)

  let assert Ok(username_header) =
    response.headers
    |> list_find(fn(h) { h.0 == "x-username" })

  username_header.1
  |> should.equal("alice")
}

pub fn apply_middleware_chain_modifies_context_test() {
  let req = make_request()
  let ctx = context.new(req, TestApp("test", None))

  let auth_middleware = fn(ctx: Context(TestApp), next) {
    // First middleware adds user to context
    let updated_ctx = Context(..ctx, app: TestApp(..ctx.app, user: Some("bob")))
    next(updated_ctx)
  }

  let validation_middleware = fn(ctx: Context(TestApp), next) {
    // Second middleware can read the user added by first middleware
    case ctx.app.user {
      Some("bob") -> {
        // User is valid, continue
        next(ctx)
      }
      _ -> response.empty(403)
    }
  }

  let response =
    middleware.apply(
      [auth_middleware, validation_middleware],
      ctx,
      fn(ctx: Context(TestApp)) {
        // Handler should see the modified context from auth middleware
        case ctx.app.user {
          Some(username) -> {
            let resp = response.empty(200)
            wisp.set_header(resp, "x-validated-user", username)
          }
          None -> response.empty(500)
        }
      },
    )

  response.status
  |> should.equal(200)

  let assert Ok(user_header) =
    response.headers
    |> list_find(fn(h) { h.0 == "x-validated-user" })

  user_header.1
  |> should.equal("bob")
}

// Helper function to find in list
fn list_find(list: List(a), predicate: fn(a) -> Bool) -> Result(a, Nil) {
  case list {
    [] -> Error(Nil)
    [first, ..rest] ->
      case predicate(first) {
        True -> Ok(first)
        False -> list_find(rest, predicate)
      }
  }
}
