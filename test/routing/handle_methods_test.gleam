import gleam/http
import gleeunit/should
import glimr/routing/router
import wisp

// Method Matching Tests

pub fn match_matches_get_test() {
  let result =
    router.match(http.Get, [
      #(http.Get, fn() { wisp.response(200) }),
      #(http.Post, fn() { wisp.response(201) }),
    ])

  result.status
  |> should.equal(200)
}

pub fn match_matches_post_test() {
  let result =
    router.match(http.Post, [
      #(http.Get, fn() { wisp.response(200) }),
      #(http.Post, fn() { wisp.response(201) }),
    ])

  result.status
  |> should.equal(201)
}

pub fn match_returns_405_for_unmatched_method_test() {
  let result =
    router.match(http.Delete, [
      #(http.Get, fn() { wisp.response(200) }),
      #(http.Post, fn() { wisp.response(201) }),
    ])

  result.status
  |> should.equal(405)
}

pub fn match_single_method_test() {
  let result =
    router.match(http.Get, [#(http.Get, fn() { wisp.response(200) })])

  result.status
  |> should.equal(200)
}

pub fn match_single_method_wrong_method_test() {
  let result =
    router.match(http.Post, [#(http.Get, fn() { wisp.response(200) })])

  result.status
  |> should.equal(405)
}

// Lazy Evaluation Tests

pub fn match_only_executes_matched_handler_test() {
  // This test verifies lazy evaluation by ensuring only the matched
  // handler function is called, not all of them

  let result =
    router.match(http.Get, [
      #(http.Get, fn() { wisp.html_response("GET executed", 200) }),
      #(http.Post, fn() {
        // This should NOT be executed
        // If it were, we'd see "POST executed" in the body
        wisp.html_response("POST executed", 201)
      }),
    ])

  result.status
  |> should.equal(200)

  case result.body {
    wisp.Text(content) -> {
      content
      |> should.equal("GET executed")
    }
    _ -> should.fail()
  }
}

// Multiple Methods Tests

pub fn match_multiple_methods_test() {
  // Test PUT
  let result =
    router.match(http.Put, [
      #(http.Get, fn() { wisp.response(200) }),
      #(http.Post, fn() { wisp.response(201) }),
      #(http.Put, fn() { wisp.response(202) }),
      #(http.Delete, fn() { wisp.response(204) }),
    ])

  result.status
  |> should.equal(202)

  // Test DELETE
  let result =
    router.match(http.Delete, [
      #(http.Get, fn() { wisp.response(200) }),
      #(http.Post, fn() { wisp.response(201) }),
      #(http.Put, fn() { wisp.response(202) }),
      #(http.Delete, fn() { wisp.response(204) }),
    ])

  result.status
  |> should.equal(204)
}

// Empty List Tests

pub fn match_empty_list_returns_405_test() {
  let result = router.match(http.Get, [])

  result.status
  |> should.equal(405)
}
