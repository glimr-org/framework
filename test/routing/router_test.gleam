import gleam/dict
import gleam/http
import gleam/http/response
import gleam/list
import gleeunit/should
import glimr/http/kernel
import glimr/routing/route
import glimr/routing/router
import routing/helpers
import wisp

// Path Matching Tests

pub fn matches_path_exact_match_test() {
  router.matches_path("/users", "/users")
  |> should.be_true()
}

pub fn matches_path_no_match_test() {
  router.matches_path("/users", "/posts")
  |> should.be_false()
}

pub fn matches_path_with_single_param_test() {
  router.matches_path("/users/{id}", "/users/123")
  |> should.be_true()
}

pub fn matches_path_with_multiple_params_test() {
  router.matches_path("/users/{id}/posts/{post_id}", "/users/123/posts/456")
  |> should.be_true()
}

pub fn matches_path_wrong_length_test() {
  router.matches_path("/users/{id}", "/users")
  |> should.be_false()
}

pub fn matches_path_mixed_static_and_dynamic_test() {
  router.matches_path("/users/{id}/edit", "/users/123/edit")
  |> should.be_true()
}

pub fn matches_path_mixed_mismatch_test() {
  router.matches_path("/users/{id}/edit", "/users/123/delete")
  |> should.be_false()
}

pub fn matches_path_multiple_segments_test() {
  router.matches_path("/api/v1/users/{id}", "/api/v1/users/42")
  |> should.be_true()
}

pub fn matches_path_root_test() {
  router.matches_path("/", "/")
  |> should.be_true()
}

// Find Matching Route Tests

pub fn find_matching_route_exact_match_test() {
  let routes = [route.get("/users", helpers.test_handler)]
  let groups = [
    route.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn() { routes },
    ),
  ]

  router.find_matching_route_in_groups(groups, "/users", http.Get)
  |> should.be_ok()
}

pub fn find_matching_route_with_params_test() {
  let routes = [route.get("/users/{id}", helpers.test_handler)]
  let groups = [
    route.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn() { routes },
    ),
  ]

  let result =
    router.find_matching_route_in_groups(groups, "/users/123", http.Get)

  case result {
    Ok(#(_route, params, _group)) -> {
      dict.get(params, "id")
      |> should.be_ok()
      |> should.equal("123")
    }
    Error(_) -> should.fail()
  }
}

pub fn find_matching_route_multiple_params_test() {
  let routes = [
    route.get("/users/{user_id}/posts/{post_id}", helpers.test_handler),
  ]
  let groups = [
    route.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn() { routes },
    ),
  ]

  let result =
    router.find_matching_route_in_groups(groups, "/users/42/posts/99", http.Get)

  case result {
    Ok(#(_route, params, _group)) -> {
      dict.get(params, "user_id")
      |> should.be_ok()
      |> should.equal("42")

      dict.get(params, "post_id")
      |> should.be_ok()
      |> should.equal("99")
    }
    Error(_) -> should.fail()
  }
}

pub fn find_matching_route_no_match_test() {
  let routes = [route.get("/users", helpers.test_handler)]
  let groups = [
    route.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn() { routes },
    ),
  ]

  router.find_matching_route_in_groups(groups, "/posts", http.Get)
  |> should.equal(Error(router.NoRouteFound))
}

pub fn find_matching_route_wrong_method_test() {
  let routes = [route.get("/users", helpers.test_handler)]
  let groups = [
    route.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn() { routes },
    ),
  ]

  router.find_matching_route_in_groups(groups, "/users", http.Post)
  |> should.equal(Error(router.MethodNotAllowed))
}

pub fn find_matching_route_multiple_routes_test() {
  let routes = [
    route.get("/users", helpers.test_handler),
    route.get("/posts", helpers.test_handler),
    route.post("/users", helpers.test_handler),
  ]
  let groups = [
    route.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn() { routes },
    ),
  ]

  router.find_matching_route_in_groups(groups, "/posts", http.Get)
  |> should.be_ok()
}

pub fn find_matching_route_multiple_groups_test() {
  let web_routes = [route.get("/", helpers.test_handler)]
  let api_routes = [route.get("/api/users", helpers.test_handler)]

  let groups = [
    route.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn() { web_routes },
    ),
    route.RouteGroup(
      prefix: "/api",
      middleware_group: kernel.Api,
      routes: fn() { api_routes },
    ),
  ]

  let result =
    router.find_matching_route_in_groups(groups, "/api/users", http.Get)

  case result {
    Ok(#(_route, _params, group)) -> {
      group
      |> should.equal(kernel.Api)
    }
    Error(_) -> should.fail()
  }
}

pub fn find_matching_route_first_group_wins_test() {
  let web_routes = [route.get("/users", helpers.test_handler)]
  let api_routes = [route.get("/users", helpers.test_handler)]

  let groups = [
    route.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn() { web_routes },
    ),
    route.RouteGroup(
      prefix: "/api",
      middleware_group: kernel.Api,
      routes: fn() { api_routes },
    ),
  ]

  let result = router.find_matching_route_in_groups(groups, "/users", http.Get)

  case result {
    Ok(#(_route, _params, group)) -> {
      // First group should match
      group
      |> should.equal(kernel.Web)
    }
    Error(_) -> should.fail()
  }
}

// Get All Routes Tests

pub fn get_all_routes_single_group_test() {
  let routes = [
    route.get("/users", helpers.test_handler),
    route.get("/posts", helpers.test_handler),
  ]
  let groups = [
    route.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn() { routes },
    ),
  ]

  let all_routes = router.get_all_routes(groups)

  all_routes
  |> list.length()
  |> should.equal(2)
}

pub fn get_all_routes_multiple_groups_test() {
  let web_routes = [route.get("/", helpers.test_handler)]
  let api_routes = [
    route.get("/api/users", helpers.test_handler),
    route.get("/api/posts", helpers.test_handler),
  ]

  let groups = [
    route.RouteGroup(
      prefix: "",
      middleware_group: kernel.Web,
      routes: fn() { web_routes },
    ),
    route.RouteGroup(
      prefix: "/api",
      middleware_group: kernel.Api,
      routes: fn() { api_routes },
    ),
  ]

  let all_routes = router.get_all_routes(groups)

  all_routes
  |> list.length()
  |> should.equal(3)
}

pub fn get_all_routes_empty_groups_test() {
  let groups = []

  let all_routes = router.get_all_routes(groups)

  all_routes
  |> list.length()
  |> should.equal(0)
}

// Apply Middleware Tests

pub fn apply_middleware_no_middleware_test() {
  let ctx = helpers.TestContext("test")
  let req =
    route.RouteRequest(request: helpers.make_request(), params: dict.new())
  let handler = fn(_req, _ctx) { wisp.response(200) }

  let response = router.apply_middleware(req, ctx, [], handler)

  response.status
  |> should.equal(200)
}

pub fn apply_middleware_single_middleware_test() {
  let ctx = helpers.TestContext("test")
  let req =
    route.RouteRequest(request: helpers.make_request(), params: dict.new())

  let middleware = fn(wisp_req, _ctx, next) {
    let resp = next(wisp_req)
    wisp.set_header(resp, "x-custom", "applied")
  }

  let handler = fn(_req, _ctx) { wisp.response(200) }

  let response = router.apply_middleware(req, ctx, [middleware], handler)

  response.status
  |> should.equal(200)

  response.get_header(response, "x-custom")
  |> should.be_ok()
  |> should.equal("applied")
}

pub fn apply_middleware_multiple_middleware_test() {
  let ctx = helpers.TestContext("test")
  let req =
    route.RouteRequest(request: helpers.make_request(), params: dict.new())

  let middleware1 = fn(wisp_req, _ctx, next) {
    let resp = next(wisp_req)
    wisp.set_header(resp, "x-first", "1")
  }

  let middleware2 = fn(wisp_req, _ctx, next) {
    let resp = next(wisp_req)
    wisp.set_header(resp, "x-second", "2")
  }

  let handler = fn(_req, _ctx) { wisp.response(200) }

  let response =
    router.apply_middleware(req, ctx, [middleware1, middleware2], handler)

  response.status
  |> should.equal(200)

  response.get_header(response, "x-first")
  |> should.be_ok()
  |> should.equal("1")

  response.get_header(response, "x-second")
  |> should.be_ok()
  |> should.equal("2")
}

pub fn apply_middleware_order_test() {
  let ctx = helpers.TestContext("test")
  let req =
    route.RouteRequest(request: helpers.make_request(), params: dict.new())

  // Middleware executes in order but wraps the response in reverse
  // [middleware1, middleware2] means middleware1 wraps middleware2
  let middleware1 = fn(wisp_req, _ctx, next) {
    let resp = next(wisp_req)
    wisp.set_header(resp, "x-order", "first")
  }

  let middleware2 = fn(wisp_req, _ctx, next) {
    let resp = next(wisp_req)
    wisp.set_header(resp, "x-order", "second")
  }

  let handler = fn(_req, _ctx) { wisp.response(200) }

  let response =
    router.apply_middleware(req, ctx, [middleware1, middleware2], handler)

  // The first middleware (outermost) has the final say
  response.get_header(response, "x-order")
  |> should.be_ok()
  |> should.equal("first")
}
