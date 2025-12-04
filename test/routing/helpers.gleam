import gleam/http
import gleam/http/request
import glimr/routing/route
import wisp

pub type TestContext {
  TestContext(value: String)
}

pub fn test_handler(
  _req: route.RouteRequest,
  _ctx: TestContext,
) -> wisp.Response {
  wisp.response(200)
}

@external(erlang, "erlang", "make_ref")
fn stub_connection() -> wisp.Connection

pub fn make_request() -> wisp.Request {
  request.new()
  |> request.set_method(http.Get)
  |> request.set_path("/")
  |> request.set_body(stub_connection())
}
