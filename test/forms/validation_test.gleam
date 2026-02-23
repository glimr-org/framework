import gleam/bit_array
import gleam/int
import gleam/list
import gleam/string
import gleeunit/should
import glimr/forms/validator
import simplifile
import wisp

const ctx = Nil

// ------------------------------------------------------------- String Rule Tests

pub fn for_required_pass_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("name", "John")], files: []))

  validator.start([validator.for("name", [validator.Required])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_required_fail_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("name", "")], files: []))

  case
    validator.start(
      [validator.for("name", [validator.Required])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(name: field_name, messages: msgs)]) -> {
      field_name
      |> should.equal("name")

      msgs
      |> should.equal(["Name is required"])
    }
    _ -> should.fail()
  }
}

pub fn for_email_pass_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("email", "test@example.com")], files: []))

  validator.start([validator.for("email", [validator.Email])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_email_fail_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("email", "invalid")], files: []))

  case
    validator.start([validator.for("email", [validator.Email])], form_data, ctx)
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["Email must be a valid email address"])
    }
    _ -> should.fail()
  }
}

pub fn for_email_fail_at_dot_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("email", "@.")], files: []))

  validator.start([validator.for("email", [validator.Email])], form_data, ctx)
  |> should.be_error()
}

pub fn for_email_fail_dot_at_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("email", ".@")], files: []))

  validator.start([validator.for("email", [validator.Email])], form_data, ctx)
  |> should.be_error()
}

pub fn for_email_fail_no_domain_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("email", "user@")], files: []))

  validator.start([validator.for("email", [validator.Email])], form_data, ctx)
  |> should.be_error()
}

pub fn for_email_fail_no_local_part_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("email", "@example.com")], files: []))

  validator.start([validator.for("email", [validator.Email])], form_data, ctx)
  |> should.be_error()
}

pub fn for_email_fail_spaces_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("email", "user @example.com")], files: []))

  validator.start([validator.for("email", [validator.Email])], form_data, ctx)
  |> should.be_error()
}

pub fn for_min_length_pass_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("name", "John")], files: []))

  validator.start(
    [validator.for("name", [validator.MinLength(3)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_min_length_fail_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("name", "Jo")], files: []))

  case
    validator.start(
      [validator.for("name", [validator.MinLength(3)])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["Name must be at least 3 characters long"])
    }
    _ -> should.fail()
  }
}

pub fn for_max_length_pass_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("name", "John")], files: []))

  validator.start(
    [validator.for("name", [validator.MaxLength(10)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_max_length_fail_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("name", "VeryLongName")], files: []))

  case
    validator.start(
      [validator.for("name", [validator.MaxLength(5)])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["Name must be no more than 5 characters long"])
    }
    _ -> should.fail()
  }
}

// ------------------------------------------------------------- Numeric Rule Tests

pub fn for_numeric_pass_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("age", "25")], files: []))

  validator.start([validator.for("age", [validator.Numeric])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_numeric_fail_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("age", "abc")], files: []))

  case
    validator.start([validator.for("age", [validator.Numeric])], form_data, ctx)
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["Age must be a valid number"])
    }
    _ -> should.fail()
  }
}

pub fn for_numeric_pass_negative_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("age", "-5")], files: []))

  validator.start([validator.for("age", [validator.Numeric])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_numeric_fail_decimal_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("age", "12.5")], files: []))

  validator.start([validator.for("age", [validator.Numeric])], form_data, ctx)
  |> should.be_error()
}

pub fn for_numeric_fail_empty_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("age", "")], files: []))

  validator.start([validator.for("age", [validator.Numeric])], form_data, ctx)
  |> should.be_error()
}

pub fn for_min_pass_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("age", "25")], files: []))

  validator.start([validator.for("age", [validator.Min(18)])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_min_fail_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("age", "16")], files: []))

  case
    validator.start([validator.for("age", [validator.Min(18)])], form_data, ctx)
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["Age must be at least 18"])
    }
    _ -> should.fail()
  }
}

pub fn for_max_pass_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("age", "30")], files: []))

  validator.start([validator.for("age", [validator.Max(100)])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_max_fail_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("age", "150")], files: []))

  case
    validator.start(
      [validator.for("age", [validator.Max(100)])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["Age must be no more than 100"])
    }
    _ -> should.fail()
  }
}

// ------------------------------------------------------------- Format Rule Tests

pub fn for_url_pass_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("website", "https://example.com")], files: []))

  validator.start([validator.for("website", [validator.Url])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_url_fail_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("website", "not-a-url")], files: []))

  case
    validator.start([validator.for("website", [validator.Url])], form_data, ctx)
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["Website must be a valid URL"])
    }
    _ -> should.fail()
  }
}

pub fn for_url_pass_http_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("website", "http://example.com")], files: []))

  validator.start([validator.for("website", [validator.Url])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_url_fail_scheme_only_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("website", "http://")], files: []))

  validator.start([validator.for("website", [validator.Url])], form_data, ctx)
  |> should.be_error()
}

pub fn for_url_fail_no_scheme_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("website", "example.com")], files: []))

  validator.start([validator.for("website", [validator.Url])], form_data, ctx)
  |> should.be_error()
}

pub fn for_url_fail_spaces_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("website", "https://exam ple.com")], files: []))

  validator.start([validator.for("website", [validator.Url])], form_data, ctx)
  |> should.be_error()
}

pub fn for_digits_pass_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("code", "12")], files: []))

  validator.start(
    [validator.for("code", [validator.Digits(2)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_digits_fail_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("code", "123")], files: []))

  case
    validator.start(
      [validator.for("code", [validator.Digits(2)])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["Code must have exactly 2 digits"])
    }
    _ -> should.fail()
  }
}

pub fn for_min_digits_pass_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("code", "123")], files: []))

  validator.start(
    [validator.for("code", [validator.MinDigits(2)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_min_digits_fail_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("code", "1")], files: []))

  case
    validator.start(
      [validator.for("code", [validator.MinDigits(2)])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["Code must have at least 2 digits"])
    }
    _ -> should.fail()
  }
}

pub fn for_max_digits_pass_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("code", "12")], files: []))

  validator.start(
    [validator.for("code", [validator.MaxDigits(3)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_max_digits_fail_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("code", "1234")], files: []))

  case
    validator.start(
      [validator.for("code", [validator.MaxDigits(3)])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["Code must have no more than 3 digits"])
    }
    _ -> should.fail()
  }
}

pub fn for_digits_pass_leading_zeros_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("code", "0042")], files: []))

  validator.start(
    [validator.for("code", [validator.Digits(4)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_digits_fail_negative_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("code", "-12")], files: []))

  validator.start(
    [validator.for("code", [validator.Digits(2)])],
    form_data,
    ctx,
  )
  |> should.be_error()
}

pub fn for_digits_fail_non_numeric_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("code", "ab")], files: []))

  validator.start(
    [validator.for("code", [validator.Digits(2)])],
    form_data,
    ctx,
  )
  |> should.be_error()
}

pub fn for_min_digits_pass_leading_zeros_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("code", "007")], files: []))

  validator.start(
    [validator.for("code", [validator.MinDigits(2)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_min_digits_fail_negative_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("code", "-123")], files: []))

  validator.start(
    [validator.for("code", [validator.MinDigits(2)])],
    form_data,
    ctx,
  )
  |> should.be_error()
}

pub fn for_max_digits_pass_leading_zeros_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("code", "007")], files: []))

  validator.start(
    [validator.for("code", [validator.MaxDigits(3)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_max_digits_fail_negative_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("code", "-12")], files: []))

  validator.start(
    [validator.for("code", [validator.MaxDigits(3)])],
    form_data,
    ctx,
  )
  |> should.be_error()
}

// ------------------------------------------------------------- Multiple Rules Tests

pub fn for_multiple_rules_all_pass_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("email", "test@example.com")], files: []))

  validator.start(
    [
      validator.for("email", [
        validator.Required,
        validator.Email,
        validator.MinLength(5),
      ]),
    ],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_multiple_rules_some_fail_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("name", "Jo")], files: []))

  case
    validator.start(
      [
        validator.for("name", [
          validator.Required,
          validator.MinLength(3),
          validator.MaxLength(10),
        ]),
      ],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(name: field_name, messages: msgs)]) -> {
      field_name
      |> should.equal("name")

      msgs
      |> should.equal(["Name must be at least 3 characters long"])
    }
    _ -> should.fail()
  }
}

pub fn start_all_pass_test() {
  let form_data =
    validator.form_data(wisp.FormData(
      values: [#("name", "John"), #("email", "john@example.com")],
      files: [],
    ))

  validator.start(
    [
      validator.for("name", [validator.Required]),
      validator.for("email", [validator.Required, validator.Email]),
    ],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn start_some_fail_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("name", ""), #("email", "invalid")], files: []))

  case
    validator.start(
      [
        validator.for("name", [validator.Required]),
        validator.for("email", [validator.Email]),
      ],
      form_data,
      ctx,
    )
  {
    Error(errors) -> {
      errors
      |> list.length
      |> should.equal(2)
    }
    Ok(_) -> should.fail()
  }
}

// ------------------------------------------------------------- File Validation Tests

pub fn for_file_required_pass_test() {
  let uploaded_file =
    wisp.UploadedFile(file_name: "test.jpg", path: "/tmp/test")
  let form_data = validator.form_data(wisp.FormData(values: [], files: [#("avatar", uploaded_file)]))

  validator.start(
    [validator.for_file("avatar", [validator.FileRequired])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_file_required_fail_empty_filename_test() {
  let uploaded_file = wisp.UploadedFile(file_name: "", path: "/tmp/test")
  let form_data = validator.form_data(wisp.FormData(values: [], files: [#("avatar", uploaded_file)]))

  case
    validator.start(
      [validator.for_file("avatar", [validator.FileRequired])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["Avatar is required"])
    }
    _ -> should.fail()
  }
}

pub fn for_file_required_fail_missing_test() {
  let form_data = validator.form_data(wisp.FormData(values: [], files: []))

  case
    validator.start(
      [validator.for_file("avatar", [validator.FileRequired])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["Avatar is required"])
    }
    _ -> should.fail()
  }
}

pub fn for_file_extension_pass_test() {
  let uploaded_file =
    wisp.UploadedFile(file_name: "test.jpg", path: "/tmp/test")
  let form_data = validator.form_data(wisp.FormData(values: [], files: [#("avatar", uploaded_file)]))

  validator.start(
    [
      validator.for_file("avatar", [validator.FileExtension(["jpg", "png"])]),
    ],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_file_extension_fail_test() {
  let uploaded_file =
    wisp.UploadedFile(file_name: "test.pdf", path: "/tmp/test")
  let form_data = validator.form_data(wisp.FormData(values: [], files: [#("avatar", uploaded_file)]))

  case
    validator.start(
      [
        validator.for_file("avatar", [
          validator.FileExtension(["jpg", "png"]),
        ]),
      ],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal([
        "Avatar must have one of the following extensions: jpg, png",
      ])
    }
    _ -> should.fail()
  }
}

pub fn for_file_extension_case_insensitive_test() {
  let uploaded_file =
    wisp.UploadedFile(file_name: "test.JPG", path: "/tmp/test")
  let form_data = validator.form_data(wisp.FormData(values: [], files: [#("avatar", uploaded_file)]))

  validator.start(
    [
      validator.for_file("avatar", [validator.FileExtension(["jpg", "png"])]),
    ],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

// ------------------------------------------------------------- File Size Tests

pub fn for_file_min_size_pass_test() {
  let test_path = "/tmp/glimr_test_min_size_pass.txt"
  let content = bit_array.from_string("a" <> string.repeat("x", 2047))
  let assert Ok(_) = simplifile.write_bits(test_path, content)

  let uploaded_file = wisp.UploadedFile(file_name: "test.txt", path: test_path)
  let form_data = validator.form_data(wisp.FormData(values: [], files: [#("file", uploaded_file)]))

  let result =
    validator.start(
      [validator.for_file("file", [validator.FileMinSize(2)])],
      form_data,
      ctx,
    )

  let assert Ok(_) = simplifile.delete(test_path)

  result
  |> should.be_ok()
}

pub fn for_file_min_size_fail_test() {
  let test_path = "/tmp/glimr_test_min_size_fail.txt"
  let content = bit_array.from_string("small")
  let assert Ok(_) = simplifile.write_bits(test_path, content)

  let uploaded_file = wisp.UploadedFile(file_name: "test.txt", path: test_path)
  let form_data = validator.form_data(wisp.FormData(values: [], files: [#("file", uploaded_file)]))

  let result =
    validator.start(
      [validator.for_file("file", [validator.FileMinSize(10)])],
      form_data,
      ctx,
    )

  let assert Ok(_) = simplifile.delete(test_path)

  case result {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["File must be at least 10 KB in size"])
    }
    _ -> should.fail()
  }
}

pub fn for_file_max_size_pass_test() {
  let test_path = "/tmp/glimr_test_max_size_pass.txt"
  let content = bit_array.from_string("small content")
  let assert Ok(_) = simplifile.write_bits(test_path, content)

  let uploaded_file = wisp.UploadedFile(file_name: "test.txt", path: test_path)
  let form_data = validator.form_data(wisp.FormData(values: [], files: [#("file", uploaded_file)]))

  let result =
    validator.start(
      [validator.for_file("file", [validator.FileMaxSize(10)])],
      form_data,
      ctx,
    )

  let assert Ok(_) = simplifile.delete(test_path)

  result
  |> should.be_ok()
}

pub fn for_file_max_size_fail_test() {
  let test_path = "/tmp/glimr_test_max_size_fail.txt"
  let content = bit_array.from_string("a" <> string.repeat("x", 5120))
  let assert Ok(_) = simplifile.write_bits(test_path, content)

  let uploaded_file = wisp.UploadedFile(file_name: "test.txt", path: test_path)
  let form_data = validator.form_data(wisp.FormData(values: [], files: [#("file", uploaded_file)]))

  let result =
    validator.start(
      [validator.for_file("file", [validator.FileMaxSize(2)])],
      form_data,
      ctx,
    )

  let assert Ok(_) = simplifile.delete(test_path)

  case result {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["File must be no more than 2 KB in size"])
    }
    _ -> should.fail()
  }
}

// ------------------------------------------------------------- Custom Validation Tests

pub fn for_custom_pass_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("username", "alice123")], files: []))

  let custom_rule = fn(value: String, _data: validator.FormData, _ctx: Nil) -> Result(Nil, String) {
    case string.contains(value, "alice") {
      True -> Ok(Nil)
      False -> Error("must contain 'alice'")
    }
  }

  validator.start(
    [validator.for("username", [validator.Custom(custom_rule)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_custom_fail_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("username", "bob123")], files: []))

  let custom_rule = fn(value: String, _data: validator.FormData, _ctx: Nil) -> Result(Nil, String) {
    case string.contains(value, "alice") {
      True -> Ok(Nil)
      False -> Error("must contain 'alice'")
    }
  }

  case
    validator.start(
      [validator.for("username", [validator.Custom(custom_rule)])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["Username must contain 'alice'"])
    }
    _ -> should.fail()
  }
}

pub fn for_custom_with_other_rules_test() {
  let form_data = validator.form_data(wisp.FormData(values: [#("password", "Test123")], files: []))

  let has_number = fn(value: String, _data: validator.FormData, _ctx: Nil) -> Result(Nil, String) {
    case
      string.to_graphemes(value)
      |> list.any(fn(c) {
        case int.parse(c) {
          Ok(_) -> True
          Error(_) -> False
        }
      })
    {
      True -> Ok(Nil)
      False -> Error("must contain at least one number")
    }
  }

  validator.start(
    [
      validator.for("password", [
        validator.Required,
        validator.MinLength(6),
        validator.Custom(has_number),
      ]),
    ],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_file_custom_pass_test() {
  let test_path = "/tmp/glimr_test_custom_pass.txt"
  let content = bit_array.from_string("valid content")
  let assert Ok(_) = simplifile.write_bits(test_path, content)

  let uploaded_file = wisp.UploadedFile(file_name: "test.txt", path: test_path)
  let form_data = validator.form_data(wisp.FormData(values: [], files: [#("file", uploaded_file)]))

  let custom_file_rule = fn(file: wisp.UploadedFile, _data: validator.FormData, _ctx: Nil) -> Result(
    Nil,
    String,
  ) {
    case string.ends_with(file.file_name, ".txt") {
      True -> Ok(Nil)
      False -> Error("must be a text file")
    }
  }

  let result =
    validator.start(
      [
        validator.for_file("file", [validator.FileCustom(custom_file_rule)]),
      ],
      form_data,
      ctx,
    )

  let assert Ok(_) = simplifile.delete(test_path)

  result
  |> should.be_ok()
}

pub fn for_file_custom_fail_test() {
  let test_path = "/tmp/glimr_test_custom_fail.jpg"
  let content = bit_array.from_string("image content")
  let assert Ok(_) = simplifile.write_bits(test_path, content)

  let uploaded_file = wisp.UploadedFile(file_name: "test.jpg", path: test_path)
  let form_data = validator.form_data(wisp.FormData(values: [], files: [#("file", uploaded_file)]))

  let custom_file_rule = fn(file: wisp.UploadedFile, _data: validator.FormData, _ctx: Nil) -> Result(
    Nil,
    String,
  ) {
    case string.ends_with(file.file_name, ".txt") {
      True -> Ok(Nil)
      False -> Error("must be a text file")
    }
  }

  let result =
    validator.start(
      [
        validator.for_file("file", [validator.FileCustom(custom_file_rule)]),
      ],
      form_data,
      ctx,
    )

  let assert Ok(_) = simplifile.delete(test_path)

  case result {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["File must be a text file"])
    }
    _ -> should.fail()
  }
}

pub fn for_file_custom_with_other_rules_test() {
  let test_path = "/tmp/glimr_test_custom_combined.txt"
  let content = bit_array.from_string("test content")
  let assert Ok(_) = simplifile.write_bits(test_path, content)

  let uploaded_file = wisp.UploadedFile(file_name: "test.txt", path: test_path)
  let form_data = validator.form_data(wisp.FormData(values: [], files: [#("file", uploaded_file)]))

  let custom_file_rule = fn(file: wisp.UploadedFile, _data: validator.FormData, _ctx: Nil) -> Result(
    Nil,
    String,
  ) {
    case string.contains(file.file_name, "test") {
      True -> Ok(Nil)
      False -> Error("filename must contain 'test'")
    }
  }

  let result =
    validator.start(
      [
        validator.for_file("file", [
          validator.FileRequired,
          validator.FileExtension(["txt", "md"]),
          validator.FileCustom(custom_file_rule),
        ]),
      ],
      form_data,
      ctx,
    )

  let assert Ok(_) = simplifile.delete(test_path)

  result
  |> should.be_ok()
}
