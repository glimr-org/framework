import gleam/bit_array
import gleam/list
import gleam/string
import gleeunit/should
import glimr/forms/validator
import simplifile
import wisp

pub fn for_required_pass_test() {
  let form_data = wisp.FormData(values: [#("name", "John")], files: [])

  form_data
  |> validator.for("name", [validator.Required])
  |> should.be_ok()
}

pub fn for_required_fail_test() {
  let form_data = wisp.FormData(values: [#("name", "")], files: [])

  case form_data |> validator.for("name", [validator.Required]) {
    Error(validator.ValidationError(name: field_name, messages: msgs)) -> {
      field_name
      |> should.equal("name")

      msgs
      |> should.equal(["name is required"])
    }
    Ok(_) -> should.fail()
  }
}

pub fn for_email_pass_test() {
  let form_data =
    wisp.FormData(values: [#("email", "test@example.com")], files: [])

  form_data
  |> validator.for("email", [validator.Email])
  |> should.be_ok()
}

pub fn for_email_fail_test() {
  let form_data = wisp.FormData(values: [#("email", "invalid")], files: [])

  case form_data |> validator.for("email", [validator.Email]) {
    Error(validator.ValidationError(messages: msgs, ..)) -> {
      msgs
      |> should.equal(["email must be a valid email address"])
    }
    Ok(_) -> should.fail()
  }
}

pub fn for_min_length_pass_test() {
  let form_data = wisp.FormData(values: [#("name", "John")], files: [])

  form_data
  |> validator.for("name", [validator.MinLength(3)])
  |> should.be_ok()
}

pub fn for_min_length_fail_test() {
  let form_data = wisp.FormData(values: [#("name", "Jo")], files: [])

  case form_data |> validator.for("name", [validator.MinLength(3)]) {
    Error(validator.ValidationError(messages: msgs, ..)) -> {
      msgs
      |> should.equal(["name must be at least 3 characters long"])
    }
    Ok(_) -> should.fail()
  }
}

pub fn for_max_length_pass_test() {
  let form_data = wisp.FormData(values: [#("name", "John")], files: [])

  form_data
  |> validator.for("name", [validator.MaxLength(10)])
  |> should.be_ok()
}

pub fn for_max_length_fail_test() {
  let form_data = wisp.FormData(values: [#("name", "VeryLongName")], files: [])

  case form_data |> validator.for("name", [validator.MaxLength(5)]) {
    Error(validator.ValidationError(messages: msgs, ..)) -> {
      msgs
      |> should.equal(["name must be no more than 5 characters long"])
    }
    Ok(_) -> should.fail()
  }
}

pub fn for_numeric_pass_test() {
  let form_data = wisp.FormData(values: [#("age", "25")], files: [])

  form_data
  |> validator.for("age", [validator.Numeric])
  |> should.be_ok()
}

pub fn for_numeric_fail_test() {
  let form_data = wisp.FormData(values: [#("age", "abc")], files: [])

  case form_data |> validator.for("age", [validator.Numeric]) {
    Error(validator.ValidationError(messages: msgs, ..)) -> {
      msgs
      |> should.equal(["age must be a valid number"])
    }
    Ok(_) -> should.fail()
  }
}

pub fn for_min_pass_test() {
  let form_data = wisp.FormData(values: [#("age", "25")], files: [])

  form_data
  |> validator.for("age", [validator.Min(18)])
  |> should.be_ok()
}

pub fn for_min_fail_test() {
  let form_data = wisp.FormData(values: [#("age", "16")], files: [])

  case form_data |> validator.for("age", [validator.Min(18)]) {
    Error(validator.ValidationError(messages: msgs, ..)) -> {
      msgs
      |> should.equal(["age must be at least 18"])
    }
    Ok(_) -> should.fail()
  }
}

pub fn for_max_pass_test() {
  let form_data = wisp.FormData(values: [#("age", "30")], files: [])

  form_data
  |> validator.for("age", [validator.Max(100)])
  |> should.be_ok()
}

pub fn for_max_fail_test() {
  let form_data = wisp.FormData(values: [#("age", "150")], files: [])

  case form_data |> validator.for("age", [validator.Max(100)]) {
    Error(validator.ValidationError(messages: msgs, ..)) -> {
      msgs
      |> should.equal(["age must be no more than 100"])
    }
    Ok(_) -> should.fail()
  }
}

pub fn for_url_pass_test() {
  let form_data =
    wisp.FormData(values: [#("website", "https://example.com")], files: [])

  form_data
  |> validator.for("website", [validator.Url])
  |> should.be_ok()
}

pub fn for_url_fail_test() {
  let form_data = wisp.FormData(values: [#("website", "not-a-url")], files: [])

  case form_data |> validator.for("website", [validator.Url]) {
    Error(validator.ValidationError(messages: msgs, ..)) -> {
      msgs
      |> should.equal(["website must be a valid URL"])
    }
    Ok(_) -> should.fail()
  }
}

pub fn for_digits_pass_test() {
  let form_data = wisp.FormData(values: [#("code", "12")], files: [])

  form_data
  |> validator.for("code", [validator.Digits(2)])
  |> should.be_ok()
}

pub fn for_digits_fail_test() {
  let form_data = wisp.FormData(values: [#("code", "123")], files: [])

  case form_data |> validator.for("code", [validator.Digits(2)]) {
    Error(validator.ValidationError(messages: msgs, ..)) -> {
      msgs
      |> should.equal(["code must have exactly 2 digits"])
    }
    Ok(_) -> should.fail()
  }
}

pub fn for_min_digits_pass_test() {
  let form_data = wisp.FormData(values: [#("code", "123")], files: [])

  form_data
  |> validator.for("code", [validator.MinDigits(2)])
  |> should.be_ok()
}

pub fn for_min_digits_fail_test() {
  let form_data = wisp.FormData(values: [#("code", "1")], files: [])

  case form_data |> validator.for("code", [validator.MinDigits(2)]) {
    Error(validator.ValidationError(messages: msgs, ..)) -> {
      msgs
      |> should.equal(["code must have at least 2 digits"])
    }
    Ok(_) -> should.fail()
  }
}

pub fn for_max_digits_pass_test() {
  let form_data = wisp.FormData(values: [#("code", "12")], files: [])

  form_data
  |> validator.for("code", [validator.MaxDigits(3)])
  |> should.be_ok()
}

pub fn for_max_digits_fail_test() {
  let form_data = wisp.FormData(values: [#("code", "1234")], files: [])

  case form_data |> validator.for("code", [validator.MaxDigits(3)]) {
    Error(validator.ValidationError(messages: msgs, ..)) -> {
      msgs
      |> should.equal(["code must have no more than 3 digits"])
    }
    Ok(_) -> should.fail()
  }
}

pub fn for_multiple_rules_all_pass_test() {
  let form_data =
    wisp.FormData(values: [#("email", "test@example.com")], files: [])

  form_data
  |> validator.for("email", [
    validator.Required,
    validator.Email,
    validator.MinLength(5),
  ])
  |> should.be_ok()
}

pub fn for_multiple_rules_some_fail_test() {
  let form_data = wisp.FormData(values: [#("name", "Jo")], files: [])

  case
    form_data
    |> validator.for("name", [
      validator.Required,
      validator.MinLength(3),
      validator.MaxLength(10),
    ])
  {
    Error(validator.ValidationError(name: field_name, messages: msgs)) -> {
      field_name
      |> should.equal("name")

      msgs
      |> should.equal(["name must be at least 3 characters long"])
    }
    Ok(_) -> should.fail()
  }
}

pub fn start_all_pass_test() {
  let form_data =
    wisp.FormData(
      values: [#("name", "John"), #("email", "john@example.com")],
      files: [],
    )

  validator.start([
    form_data |> validator.for("name", [validator.Required]),
    form_data
      |> validator.for("email", [validator.Required, validator.Email]),
  ])
  |> should.be_ok()
}

pub fn start_some_fail_test() {
  let form_data =
    wisp.FormData(values: [#("name", ""), #("email", "invalid")], files: [])

  case
    validator.start([
      form_data |> validator.for("name", [validator.Required]),
      form_data |> validator.for("email", [validator.Email]),
    ])
  {
    Error(errors) -> {
      errors
      |> list.length
      |> should.equal(2)
    }
    Ok(_) -> should.fail()
  }
}

pub fn for_file_required_pass_test() {
  let uploaded_file =
    wisp.UploadedFile(file_name: "test.jpg", path: "/tmp/test")
  let form_data = wisp.FormData(values: [], files: [#("avatar", uploaded_file)])

  form_data
  |> validator.for_file("avatar", [validator.FileRequired])
  |> should.be_ok()
}

pub fn for_file_required_fail_empty_filename_test() {
  let uploaded_file = wisp.UploadedFile(file_name: "", path: "/tmp/test")
  let form_data = wisp.FormData(values: [], files: [#("avatar", uploaded_file)])

  case form_data |> validator.for_file("avatar", [validator.FileRequired]) {
    Error(validator.ValidationError(messages: msgs, ..)) -> {
      msgs
      |> should.equal(["avatar is required"])
    }
    Ok(_) -> should.fail()
  }
}

pub fn for_file_required_fail_missing_test() {
  let form_data = wisp.FormData(values: [], files: [])

  case form_data |> validator.for_file("avatar", [validator.FileRequired]) {
    Error(validator.ValidationError(messages: msgs, ..)) -> {
      msgs
      |> should.equal(["avatar is required"])
    }
    Ok(_) -> should.fail()
  }
}

pub fn for_file_extension_pass_test() {
  let uploaded_file =
    wisp.UploadedFile(file_name: "test.jpg", path: "/tmp/test")
  let form_data = wisp.FormData(values: [], files: [#("avatar", uploaded_file)])

  form_data
  |> validator.for_file("avatar", [validator.FileExtension(["jpg", "png"])])
  |> should.be_ok()
}

pub fn for_file_extension_fail_test() {
  let uploaded_file =
    wisp.UploadedFile(file_name: "test.pdf", path: "/tmp/test")
  let form_data = wisp.FormData(values: [], files: [#("avatar", uploaded_file)])

  case
    form_data
    |> validator.for_file("avatar", [validator.FileExtension(["jpg", "png"])])
  {
    Error(validator.ValidationError(messages: msgs, ..)) -> {
      msgs
      |> should.equal([
        "avatar must have one of the following extensions: jpg, png",
      ])
    }
    Ok(_) -> should.fail()
  }
}

pub fn for_file_extension_case_insensitive_test() {
  let uploaded_file =
    wisp.UploadedFile(file_name: "test.JPG", path: "/tmp/test")
  let form_data = wisp.FormData(values: [], files: [#("avatar", uploaded_file)])

  form_data
  |> validator.for_file("avatar", [validator.FileExtension(["jpg", "png"])])
  |> should.be_ok()
}

pub fn for_file_min_size_pass_test() {
  let test_path = "/tmp/glimr_test_min_size_pass.txt"
  let content = bit_array.from_string("a" <> string.repeat("x", 2047))
  let assert Ok(_) = simplifile.write_bits(test_path, content)

  let uploaded_file = wisp.UploadedFile(file_name: "test.txt", path: test_path)
  let form_data = wisp.FormData(values: [], files: [#("file", uploaded_file)])

  let result =
    form_data
    |> validator.for_file("file", [validator.FileMinSize(2)])

  let assert Ok(_) = simplifile.delete(test_path)

  result
  |> should.be_ok()
}

pub fn for_file_min_size_fail_test() {
  let test_path = "/tmp/glimr_test_min_size_fail.txt"
  let content = bit_array.from_string("small")
  let assert Ok(_) = simplifile.write_bits(test_path, content)

  let uploaded_file = wisp.UploadedFile(file_name: "test.txt", path: test_path)
  let form_data = wisp.FormData(values: [], files: [#("file", uploaded_file)])

  let result =
    form_data
    |> validator.for_file("file", [validator.FileMinSize(10)])

  let assert Ok(_) = simplifile.delete(test_path)

  case result {
    Error(validator.ValidationError(messages: msgs, ..)) -> {
      msgs
      |> should.equal(["file must be at least 10 KB in size"])
    }
    Ok(_) -> should.fail()
  }
}

pub fn for_file_max_size_pass_test() {
  let test_path = "/tmp/glimr_test_max_size_pass.txt"
  let content = bit_array.from_string("small content")
  let assert Ok(_) = simplifile.write_bits(test_path, content)

  let uploaded_file = wisp.UploadedFile(file_name: "test.txt", path: test_path)
  let form_data = wisp.FormData(values: [], files: [#("file", uploaded_file)])

  let result =
    form_data
    |> validator.for_file("file", [validator.FileMaxSize(10)])

  let assert Ok(_) = simplifile.delete(test_path)

  result
  |> should.be_ok()
}

pub fn for_file_max_size_fail_test() {
  let test_path = "/tmp/glimr_test_max_size_fail.txt"
  let content = bit_array.from_string("a" <> string.repeat("x", 5120))
  let assert Ok(_) = simplifile.write_bits(test_path, content)

  let uploaded_file = wisp.UploadedFile(file_name: "test.txt", path: test_path)
  let form_data = wisp.FormData(values: [], files: [#("file", uploaded_file)])

  let result =
    form_data
    |> validator.for_file("file", [validator.FileMaxSize(2)])

  let assert Ok(_) = simplifile.delete(test_path)

  case result {
    Error(validator.ValidationError(messages: msgs, ..)) -> {
      msgs
      |> should.equal(["file must be no more than 2 KB in size"])
    }
    Ok(_) -> should.fail()
  }
}
