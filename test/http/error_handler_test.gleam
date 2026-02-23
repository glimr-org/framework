import gleam/string
import gleeunit/should
import glimr/http/error_handler
import glimr/response/response
import wisp

// ------------------------------------------------------------- HTML Response Tests

pub fn html_200_unchanged_test() {
  let resp = wisp.response(200)

  let result = error_handler.default_responses(response.HTML, fn() { resp })

  result.status
  |> should.equal(200)
}

pub fn html_404_test() {
  let resp = wisp.response(404)

  let result = error_handler.default_responses(response.HTML, fn() { resp })

  result.status
  |> should.equal(404)

  case result.body {
    wisp.Text(body) -> {
      body
      |> string.contains("Not Found")
      |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn html_405_test() {
  let resp = wisp.response(405)

  let result = error_handler.default_responses(response.HTML, fn() { resp })

  result.status
  |> should.equal(405)

  case result.body {
    wisp.Text(body) -> {
      body
      |> string.contains("Method Not Allowed")
      |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn html_400_test() {
  let resp = wisp.response(400)

  let result = error_handler.default_responses(response.HTML, fn() { resp })

  result.status
  |> should.equal(400)

  case result.body {
    wisp.Text(body) -> {
      body
      |> string.contains("Bad Request")
      |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn html_413_test() {
  let resp = wisp.response(413)

  let result = error_handler.default_responses(response.HTML, fn() { resp })

  result.status
  |> should.equal(413)

  case result.body {
    wisp.Text(body) -> {
      body
      |> string.contains("Request Entity Too Large")
      |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn html_500_test() {
  let resp = wisp.response(500)

  let result = error_handler.default_responses(response.HTML, fn() { resp })

  result.status
  |> should.equal(500)

  case result.body {
    wisp.Text(body) -> {
      body
      |> string.contains("Internal Server Error")
      |> should.be_true()
    }
    _ -> should.fail()
  }
}

// ------------------------------------------------------------- JSON Response Tests

pub fn json_200_unchanged_test() {
  let resp = wisp.response(200)

  let result = error_handler.default_responses(response.JSON, fn() { resp })

  result.status
  |> should.equal(200)
}

pub fn json_404_test() {
  let resp = wisp.response(404)

  let result = error_handler.default_responses(response.JSON, fn() { resp })

  result.status
  |> should.equal(404)

  case result.body {
    wisp.Text(body) -> {
      body
      |> string.contains("Not Found")
      |> should.be_true()

      // Verify it's JSON format
      body
      |> string.starts_with("{")
      |> should.be_true()

      body
      |> string.contains("\"error\"")
      |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn json_405_test() {
  let resp = wisp.response(405)

  let result = error_handler.default_responses(response.JSON, fn() { resp })

  result.status
  |> should.equal(405)

  case result.body {
    wisp.Text(body) -> {
      body
      |> string.contains("Method Not Allowed")
      |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn json_400_test() {
  let resp = wisp.response(400)

  let result = error_handler.default_responses(response.JSON, fn() { resp })

  result.status
  |> should.equal(400)

  case result.body {
    wisp.Text(body) -> {
      body
      |> string.contains("Bad Request")
      |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn json_422_test() {
  let resp = wisp.response(422)

  let result = error_handler.default_responses(response.JSON, fn() { resp })

  result.status
  |> should.equal(422)

  case result.body {
    wisp.Text(body) -> {
      body
      |> string.contains("Bad Request")
      |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn json_413_test() {
  let resp = wisp.response(413)

  let result = error_handler.default_responses(response.JSON, fn() { resp })

  result.status
  |> should.equal(413)

  case result.body {
    wisp.Text(body) -> {
      body
      |> string.contains("Request Entity Too Large")
      |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn json_500_test() {
  let resp = wisp.response(500)

  let result = error_handler.default_responses(response.JSON, fn() { resp })

  result.status
  |> should.equal(500)

  case result.body {
    wisp.Text(body) -> {
      body
      |> string.contains("Internal Server Error")
      |> should.be_true()
    }
    _ -> should.fail()
  }
}
