import gleam/dict
import gleam/http
import gleeunit/should
import glimr/routing/route
import routing/helpers

// Route Creation Tests

pub fn get_route_test() {
  let r = route.get("/users", helpers.test_handler)

  r.method
  |> should.equal(http.Get)

  r.path
  |> should.equal("/users")
}

pub fn post_route_test() {
  let r = route.post("/users", helpers.test_handler)

  r.method
  |> should.equal(http.Post)

  r.path
  |> should.equal("/users")
}

pub fn put_route_test() {
  let r = route.put("/users/1", helpers.test_handler)

  r.method
  |> should.equal(http.Put)

  r.path
  |> should.equal("/users/1")
}

pub fn delete_route_test() {
  let r = route.delete("/users/1", helpers.test_handler)

  r.method
  |> should.equal(http.Delete)

  r.path
  |> should.equal("/users/1")
}

// Path Normalization Tests

pub fn normalize_path_adds_leading_slash_test() {
  let r = route.get("users", helpers.test_handler)

  r.path
  |> should.equal("/users")
}

pub fn normalize_path_removes_trailing_slash_test() {
  let r = route.get("/users/", helpers.test_handler)

  r.path
  |> should.equal("/users")
}

pub fn normalize_path_root_unchanged_test() {
  let r = route.get("/", helpers.test_handler)

  r.path
  |> should.equal("/")
}

pub fn normalize_path_both_slash_issues_test() {
  let r = route.get("users/", helpers.test_handler)

  r.path
  |> should.equal("/users")
}

// Route Modifier Tests

pub fn route_name_test() {
  let r =
    route.get("/users", helpers.test_handler)
    |> route.name("users.index")

  r.name
  |> should.equal("users.index")
}

pub fn route_name_empty_by_default_test() {
  let r = route.get("/users", helpers.test_handler)

  r.name
  |> should.equal("")
}

pub fn route_middleware_test() {
  let middleware = fn(req, _ctx, next) { next(req) }

  let r =
    route.get("/users", helpers.test_handler)
    |> route.middleware([middleware])

  // Middleware list should not be empty
  case r.middleware {
    [] -> should.fail()
    _ -> True |> should.be_true()
  }
}

pub fn route_middleware_empty_by_default_test() {
  let r = route.get("/users", helpers.test_handler)

  r.middleware
  |> should.equal([])
}

// Route Parameter Tests

pub fn get_param_success_test() {
  let params = dict.from_list([#("id", "123"), #("name", "alice")])
  let req = route.RouteRequest(request: helpers.make_request(), params: params)

  route.get_param(req, "id")
  |> should.be_ok()
  |> should.equal("123")

  route.get_param(req, "name")
  |> should.be_ok()
  |> should.equal("alice")
}

pub fn get_param_not_found_test() {
  let params = dict.new()
  let req = route.RouteRequest(request: helpers.make_request(), params: params)

  route.get_param(req, "id")
  |> should.be_error()
}

pub fn get_param_or_with_value_test() {
  let params = dict.from_list([#("id", "123")])
  let req = route.RouteRequest(request: helpers.make_request(), params: params)

  route.get_param_or(req, "id", "default")
  |> should.equal("123")
}

pub fn get_param_or_with_default_test() {
  let params = dict.new()
  let req = route.RouteRequest(request: helpers.make_request(), params: params)

  route.get_param_or(req, "id", "default")
  |> should.equal("default")
}

// Route Grouping Tests

pub fn group_middleware_single_route_test() {
  let middleware = fn(req, _ctx, next) { next(req) }

  let routes =
    route.group_middleware([middleware], [
      [route.get("/users", helpers.test_handler)],
    ])

  case routes {
    [first] -> {
      // Middleware should be applied
      case first.middleware {
        [] -> should.fail()
        _ -> True |> should.be_true()
      }
    }
    _ -> should.fail()
  }
}

pub fn group_middleware_multiple_routes_test() {
  let middleware = fn(req, _ctx, next) { next(req) }

  let routes =
    route.group_middleware([middleware], [
      [
        route.get("/users", helpers.test_handler),
        route.post("/users", helpers.test_handler),
      ],
    ])

  case routes {
    [first, second] -> {
      // Both should have middleware
      case first.middleware {
        [] -> should.fail()
        _ -> True |> should.be_true()
      }

      case second.middleware {
        [] -> should.fail()
        _ -> True |> should.be_true()
      }
    }
    _ -> should.fail()
  }
}

pub fn group_middleware_appends_to_existing_test() {
  let middleware1 = fn(req, _ctx, next) { next(req) }
  let middleware2 = fn(req, _ctx, next) { next(req) }

  let routes =
    route.group_middleware([middleware1], [
      [
        route.get("/users", helpers.test_handler)
        |> route.middleware([middleware2]),
      ],
    ])

  case routes {
    [first] -> {
      // Should have both middleware (group + route)
      case first.middleware {
        [_, _] -> True |> should.be_true()
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn group_path_prefix_test() {
  let routes =
    route.group_path_prefix("/api", [
      [route.get("/users", helpers.test_handler)],
    ])

  case routes {
    [first] -> {
      first.path
      |> should.equal("/api/users")
    }
    _ -> should.fail()
  }
}

pub fn group_path_prefix_normalization_test() {
  let routes =
    route.group_path_prefix("api", [[route.get("users", helpers.test_handler)]])

  case routes {
    [first] -> {
      first.path
      |> should.equal("/api/users")
    }
    _ -> should.fail()
  }
}

pub fn group_path_prefix_multiple_routes_test() {
  let routes =
    route.group_path_prefix("/api", [
      [
        route.get("/users", helpers.test_handler),
        route.get("/posts", helpers.test_handler),
      ],
    ])

  case routes {
    [first, second] -> {
      first.path
      |> should.equal("/api/users")

      second.path
      |> should.equal("/api/posts")
    }
    _ -> should.fail()
  }
}

pub fn group_name_prefix_test() {
  let routes =
    route.group_name_prefix("api.", [
      [
        route.get("/users", helpers.test_handler)
        |> route.name("users.index"),
      ],
    ])

  case routes {
    [first] -> {
      first.name
      |> should.equal("api.users.index")
    }
    _ -> should.fail()
  }
}

pub fn group_name_prefix_empty_name_test() {
  let routes =
    route.group_name_prefix("api.", [
      [route.get("/users", helpers.test_handler)],
    ])

  case routes {
    [first] -> {
      first.name
      |> should.equal("api.")
    }
    _ -> should.fail()
  }
}

pub fn group_name_prefix_multiple_routes_test() {
  let routes =
    route.group_name_prefix("admin.", [
      [
        route.get("/users", helpers.test_handler)
          |> route.name("users"),
        route.get("/posts", helpers.test_handler)
          |> route.name("posts"),
      ],
    ])

  case routes {
    [first, second] -> {
      first.name
      |> should.equal("admin.users")

      second.name
      |> should.equal("admin.posts")
    }
    _ -> should.fail()
  }
}

pub fn nested_route_lists_test() {
  let routes =
    route.group_path_prefix("/api", [
      [route.get("/users", helpers.test_handler)],
      [route.get("/posts", helpers.test_handler)],
    ])

  case routes {
    [first, second] -> {
      first.path
      |> should.equal("/api/users")

      second.path
      |> should.equal("/api/posts")
    }
    _ -> should.fail()
  }
}
