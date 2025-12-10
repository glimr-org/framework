import gleam/http
import gleam/http/request
import gleeunit/should
import glimr/http/middleware
import wisp

pub type TestContext {
  TestContext(value: String)
}

@external(erlang, "erlang", "make_ref")
fn stub_connection() -> wisp.Connection

pub fn make_request() -> wisp.Request {
  request.new()
  |> request.set_method(http.Get)
  |> request.set_path("/")
  |> request.set_body(stub_connection())
}

// No Middleware Tests

pub fn apply_no_middleware_test() {
  let req = make_request()
  let ctx = TestContext("test")

  let response = middleware.apply([], req, ctx, fn(_req) { wisp.response(200) })

  response.status
  |> should.equal(200)
}

pub fn apply_no_middleware_calls_handler_test() {
  let req = make_request()
  let ctx = TestContext("test")

  // Handler should be called directly
  let response =
    middleware.apply([], req, ctx, fn(req) {
      // Verify request is passed through
      case req.method {
        http.Get -> wisp.response(200)
        _ -> wisp.response(500)
      }
    })

  response.status
  |> should.equal(200)
}

// Single Middleware Tests

pub fn apply_single_middleware_test() {
  let req = make_request()
  let ctx = TestContext("test")

  let middleware1 = fn(req, _ctx, next) {
    let resp = next(req)
    wisp.set_header(resp, "x-middleware", "applied")
  }

  let response =
    middleware.apply([middleware1], req, ctx, fn(_req) { wisp.response(200) })

  response.status
  |> should.equal(200)

  response.headers
  |> should.equal([#("x-middleware", "applied")])
}

pub fn apply_single_middleware_modifies_request_test() {
  let req = make_request()
  let ctx = TestContext("test")

  let middleware1 = fn(req, _ctx, next) {
    // Modify request before passing to next
    let req = request.set_path(req, "/modified")
    next(req)
  }

  let response =
    middleware.apply([middleware1], req, ctx, fn(req) {
      let path = wisp.path_segments(req)
      case path {
        ["modified"] -> wisp.response(200)
        _ -> wisp.response(500)
      }
    })

  response.status
  |> should.equal(200)
}

pub fn apply_single_middleware_accesses_context_test() {
  let req = make_request()
  let ctx = TestContext("secret")

  let middleware1 = fn(req, ctx, next) {
    case ctx {
      TestContext("secret") -> {
        let resp = next(req)
        wisp.set_header(resp, "x-auth", "valid")
      }
      _ -> {
        let resp = next(req)
        wisp.set_header(resp, "x-auth", "invalid")
      }
    }
  }

  let response =
    middleware.apply([middleware1], req, ctx, fn(_req) { wisp.response(200) })

  response.headers
  |> should.equal([#("x-auth", "valid")])
}

// Multiple Middleware Tests

pub fn apply_multiple_middleware_test() {
  let req = make_request()
  let ctx = TestContext("test")

  let middleware1 = fn(req, _ctx, next) {
    let resp = next(req)
    wisp.set_header(resp, "x-first", "1")
  }

  let middleware2 = fn(req, _ctx, next) {
    let resp = next(req)
    wisp.set_header(resp, "x-second", "2")
  }

  let response =
    middleware.apply([middleware1, middleware2], req, ctx, fn(_req) {
      wisp.response(200)
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
  let ctx = TestContext("test")

  // Middleware execute in order: first wraps second wraps third
  // So if they all set the same header, the outermost (first) has final say
  let middleware1 = fn(req, _ctx, next) {
    let resp = next(req)
    wisp.set_header(resp, "x-order", "first")
  }

  let middleware2 = fn(req, _ctx, next) {
    let resp = next(req)
    wisp.set_header(resp, "x-order", "second")
  }

  let middleware3 = fn(req, _ctx, next) {
    let resp = next(req)
    wisp.set_header(resp, "x-order", "third")
  }

  let response =
    middleware.apply(
      [middleware1, middleware2, middleware3],
      req,
      ctx,
      fn(_req) { wisp.response(200) },
    )

  // First middleware (outermost) has final say
  response.headers
  |> should.equal([#("x-order", "first")])
}

pub fn apply_middleware_chain_modifies_request_test() {
  let req = make_request()
  let ctx = TestContext("test")

  let middleware1 = fn(req, _ctx, next) {
    // First middleware sets path to /step1
    let req = request.set_path(req, "/step1")
    next(req)
  }

  let middleware2 = fn(req, _ctx, next) {
    // Second middleware should see /step1 and change to /step2
    let path = wisp.path_segments(req)
    case path {
      ["step1"] -> {
        let req = request.set_path(req, "/step2")
        next(req)
      }
      _ -> next(req)
    }
  }

  let response =
    middleware.apply([middleware1, middleware2], req, ctx, fn(req) {
      // Handler should see final path
      let path = wisp.path_segments(req)
      case path {
        ["step2"] -> wisp.response(200)
        _ -> wisp.response(500)
      }
    })

  response.status
  |> should.equal(200)
}

// Short-circuit Tests

pub fn apply_middleware_can_short_circuit_test() {
  let req = make_request()
  let ctx = TestContext("unauthorized")

  let auth_middleware = fn(req, ctx, next) {
    case ctx {
      TestContext("authorized") -> next(req)
      // Short-circuit - don't call next
      _ -> wisp.response(401)
    }
  }

  let response =
    middleware.apply([auth_middleware], req, ctx, fn(_req) {
      // This handler should never be called
      wisp.response(200)
    })

  response.status
  |> should.equal(401)
}

pub fn apply_middleware_early_return_stops_chain_test() {
  let req = make_request()
  let ctx = TestContext("test")

  let middleware1 = fn(_req, _ctx, _next) {
    // First middleware returns early without calling next
    wisp.response(403)
  }

  let middleware2 = fn(req, _ctx, next) {
    // This should never be called
    let resp = next(req)
    wisp.set_header(resp, "x-unreachable", "true")
  }

  let response =
    middleware.apply([middleware1, middleware2], req, ctx, fn(_req) {
      // Handler should never be called either
      wisp.response(200)
    })

  response.status
  |> should.equal(403)

  // Second middleware's header should not be present
  response.headers
  |> should.equal([])
}

// Response Modification Tests

pub fn apply_middleware_modifies_response_body_test() {
  let req = make_request()
  let ctx = TestContext("test")

  let middleware1 = fn(req, _ctx, next) {
    let resp = next(req)
    // Modify response body
    wisp.html_body(resp, "<html>wrapped</html>")
  }

  let response =
    middleware.apply([middleware1], req, ctx, fn(_req) {
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
  let ctx = TestContext("test")

  let middleware1 = fn(req, _ctx, next) {
    let _resp = next(req)
    // Change status code (create new response)
    wisp.response(201)
  }

  let response =
    middleware.apply([middleware1], req, ctx, fn(_req) { wisp.response(200) })

  response.status
  |> should.equal(201)
}

// Complex Scenarios

pub fn apply_middleware_realistic_auth_flow_test() {
  let req = make_request()
  let ctx = TestContext("admin")

  let auth_middleware = fn(req, ctx, next) {
    case ctx {
      TestContext("admin") -> {
        // Add user info to response header
        let resp = next(req)
        wisp.set_header(resp, "x-user", "admin")
      }
      _ -> wisp.response(401)
    }
  }

  let logging_middleware = fn(req, _ctx, next) {
    // Log the request (in real app would use io)
    let resp = next(req)
    wisp.set_header(resp, "x-logged", "true")
  }

  let response =
    middleware.apply([logging_middleware, auth_middleware], req, ctx, fn(_req) {
      wisp.response(200)
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
