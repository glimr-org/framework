//// Development Proxy
////
//// A reverse proxy for development that forwards requests to
//// the application server. Enables hot reloading by retrying
//// connections while the app restarts.
////

import gleam/bit_array
import gleam/bytes_tree
import gleam/erlang/process
import gleam/http
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/httpc
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glimr/console/console
import mist.{type Connection, type ResponseData}

// ------------------------------------------------------------- Public Functions

/// Starts the development proxy server. Listens on the given
/// port and forwards all requests to the target port where
/// the application server runs.
///
pub fn start(listen_port: Int, target_port: Int) -> Nil {
  console.output()
  |> console.line_success(
    "Dev proxy started on port "
    <> int.to_string(listen_port)
    <> " → "
    <> int.to_string(target_port),
  )
  |> console.print()

  let assert Ok(_) =
    fn(req: Request(Connection)) -> Response(ResponseData) {
      handle_request(req, target_port)
    }
    |> mist.new()
    |> mist.port(listen_port)
    |> mist.start()

  Nil
}

// ------------------------------------------------------------- Private Functions

/// Handles an incoming proxy request. Reads the body, builds
/// a forwarded request to the target server, and returns the
/// response or a 502 error if connection fails.
///
fn handle_request(
  req: Request(Connection),
  target_port: Int,
) -> Response(ResponseData) {
  let body = read_body(req)

  let path_with_query = case req.query {
    Some(q) -> req.path <> "?" <> q
    None -> req.path
  }

  let target_req =
    request.new()
    |> request.set_scheme(http.Http)
    |> request.set_method(req.method)
    |> request.set_host("localhost")
    |> request.set_port(target_port)
    |> request.set_path(path_with_query)
    |> request.set_body(body)
    |> copy_headers(req.headers)

  case forward_with_retry(target_req, 200, 50) {
    Ok(resp) -> to_mist_response(resp)
    Error(_) ->
      response.new(502)
      |> response.set_body(
        mist.Bytes(bytes_tree.from_string("Dev proxy: failed to connect to app")),
      )
  }
}

/// Forwards a request with retry logic. Attempts to send the
/// request up to max_attempts times with a 50ms delay between
/// retries to handle app restarts during hot reload.
///
fn forward_with_retry(
  req: Request(String),
  max_attempts: Int,
  attempt: Int,
) -> Result(Response(String), Nil) {
  case httpc.send(req) {
    Ok(resp) -> Ok(resp)
    Error(_) if attempt < max_attempts -> {
      process.sleep(50)
      forward_with_retry(req, max_attempts, attempt + 1)
    }
    Error(_) -> Error(Nil)
  }
}

/// Reads the request body from a mist connection. Limits body
/// size to 10MB and returns an empty string if reading fails
/// or the body cannot be decoded as UTF-8.
///
fn read_body(req: Request(Connection)) -> String {
  case mist.read_body(req, 10_000_000) {
    Ok(req_with_body) ->
      req_with_body.body
      |> bit_array.to_string()
      |> result.unwrap("")
    Error(_) -> ""
  }
}

/// Copies headers from the original request to the forwarded
/// request. Excludes host, content-length, and transfer-encoding
/// headers which are set by the HTTP client.
///
fn copy_headers(
  req: Request(String),
  headers: List(#(String, String)),
) -> Request(String) {
  list.fold(headers, req, fn(r, h) {
    let #(name, value) = h
    let lower_name = string.lowercase(name)
    case lower_name {
      "host" | "content-length" | "transfer-encoding" -> r
      _ -> request.set_header(r, name, value)
    }
  })
}

/// Converts an httpc response to a mist response. Transforms
/// the string body to bytes and copies headers for sending
/// back to the client.
///
fn to_mist_response(resp: Response(String)) -> Response(ResponseData) {
  let body = mist.Bytes(bytes_tree.from_string(resp.body))

  response.new(resp.status)
  |> response.set_body(body)
  |> copy_response_headers(resp.headers)
}

/// Copies headers from the upstream response to the client
/// response. Excludes transfer-encoding and content-length
/// headers which mist manages automatically.
///
fn copy_response_headers(
  resp: Response(ResponseData),
  headers: List(#(String, String)),
) -> Response(ResponseData) {
  list.fold(headers, resp, fn(r, h) {
    let #(name, value) = h
    let lower_name = string.lowercase(name)
    case lower_name {
      "transfer-encoding" | "content-length" -> r
      _ -> response.set_header(r, name, value)
    }
  })
}
