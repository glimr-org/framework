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
  let form_data =
    validator.form_data(wisp.FormData(values: [#("name", "John")], files: []))

  validator.start([validator.for("name", [validator.Required])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_required_fail_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("name", "")], files: []))

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
      |> should.equal(["name is required"])
    }
    _ -> should.fail()
  }
}

pub fn for_email_pass_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("email", "test@example.com")], files: []),
    )

  validator.start([validator.for("email", [validator.Email])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_email_fail_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("email", "invalid")], files: []),
    )

  case
    validator.start([validator.for("email", [validator.Email])], form_data, ctx)
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["email must be a valid email address"])
    }
    _ -> should.fail()
  }
}

pub fn for_email_fail_at_dot_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("email", "@.")], files: []))

  validator.start([validator.for("email", [validator.Email])], form_data, ctx)
  |> should.be_error()
}

pub fn for_email_fail_dot_at_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("email", ".@")], files: []))

  validator.start([validator.for("email", [validator.Email])], form_data, ctx)
  |> should.be_error()
}

pub fn for_email_fail_no_domain_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("email", "user@")], files: []))

  validator.start([validator.for("email", [validator.Email])], form_data, ctx)
  |> should.be_error()
}

pub fn for_email_fail_no_local_part_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("email", "@example.com")], files: []),
    )

  validator.start([validator.for("email", [validator.Email])], form_data, ctx)
  |> should.be_error()
}

pub fn for_email_fail_spaces_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("email", "user @example.com")], files: []),
    )

  validator.start([validator.for("email", [validator.Email])], form_data, ctx)
  |> should.be_error()
}

pub fn for_min_length_pass_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("name", "John")], files: []))

  validator.start(
    [validator.for("name", [validator.MinLength(3)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_min_length_fail_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("name", "Jo")], files: []))

  case
    validator.start(
      [validator.for("name", [validator.MinLength(3)])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["name must be at least 3 characters long"])
    }
    _ -> should.fail()
  }
}

pub fn for_max_length_pass_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("name", "John")], files: []))

  validator.start(
    [validator.for("name", [validator.MaxLength(10)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_max_length_fail_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("name", "VeryLongName")], files: []),
    )

  case
    validator.start(
      [validator.for("name", [validator.MaxLength(5)])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["name must be no more than 5 characters long"])
    }
    _ -> should.fail()
  }
}

// ------------------------------------------------------------- Numeric Rule Tests

pub fn for_numeric_pass_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("age", "25")], files: []))

  validator.start([validator.for("age", [validator.Numeric])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_numeric_fail_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("age", "abc")], files: []))

  case
    validator.start([validator.for("age", [validator.Numeric])], form_data, ctx)
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["age must be a valid number"])
    }
    _ -> should.fail()
  }
}

pub fn for_numeric_pass_negative_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("age", "-5")], files: []))

  validator.start([validator.for("age", [validator.Numeric])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_numeric_fail_decimal_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("age", "12.5")], files: []))

  validator.start([validator.for("age", [validator.Numeric])], form_data, ctx)
  |> should.be_error()
}

pub fn for_numeric_fail_empty_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("age", "")], files: []))

  validator.start([validator.for("age", [validator.Numeric])], form_data, ctx)
  |> should.be_error()
}

pub fn for_min_pass_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("age", "25")], files: []))

  validator.start([validator.for("age", [validator.Min(18)])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_min_fail_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("age", "16")], files: []))

  case
    validator.start([validator.for("age", [validator.Min(18)])], form_data, ctx)
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["age must be at least 18"])
    }
    _ -> should.fail()
  }
}

pub fn for_max_pass_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("age", "30")], files: []))

  validator.start([validator.for("age", [validator.Max(100)])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_max_fail_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("age", "150")], files: []))

  case
    validator.start(
      [validator.for("age", [validator.Max(100)])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["age must be no more than 100"])
    }
    _ -> should.fail()
  }
}

// ------------------------------------------------------------- Format Rule Tests

pub fn for_url_pass_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("website", "https://example.com")], files: []),
    )

  validator.start([validator.for("website", [validator.Url])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_url_fail_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("website", "not-a-url")], files: []),
    )

  case
    validator.start([validator.for("website", [validator.Url])], form_data, ctx)
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["website must be a valid URL"])
    }
    _ -> should.fail()
  }
}

pub fn for_url_pass_http_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("website", "http://example.com")], files: []),
    )

  validator.start([validator.for("website", [validator.Url])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_url_fail_scheme_only_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("website", "http://")], files: []),
    )

  validator.start([validator.for("website", [validator.Url])], form_data, ctx)
  |> should.be_error()
}

pub fn for_url_fail_no_scheme_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("website", "example.com")], files: []),
    )

  validator.start([validator.for("website", [validator.Url])], form_data, ctx)
  |> should.be_error()
}

pub fn for_url_fail_spaces_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("website", "https://exam ple.com")], files: []),
    )

  validator.start([validator.for("website", [validator.Url])], form_data, ctx)
  |> should.be_error()
}

pub fn for_digits_pass_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("code", "12")], files: []))

  validator.start(
    [validator.for("code", [validator.Digits(2)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_digits_fail_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("code", "123")], files: []))

  case
    validator.start(
      [validator.for("code", [validator.Digits(2)])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["code must have exactly 2 digits"])
    }
    _ -> should.fail()
  }
}

pub fn for_min_digits_pass_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("code", "123")], files: []))

  validator.start(
    [validator.for("code", [validator.MinDigits(2)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_min_digits_fail_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("code", "1")], files: []))

  case
    validator.start(
      [validator.for("code", [validator.MinDigits(2)])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["code must have at least 2 digits"])
    }
    _ -> should.fail()
  }
}

pub fn for_max_digits_pass_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("code", "12")], files: []))

  validator.start(
    [validator.for("code", [validator.MaxDigits(3)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_max_digits_fail_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("code", "1234")], files: []))

  case
    validator.start(
      [validator.for("code", [validator.MaxDigits(3)])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["code must have no more than 3 digits"])
    }
    _ -> should.fail()
  }
}

pub fn for_digits_pass_leading_zeros_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("code", "0042")], files: []))

  validator.start(
    [validator.for("code", [validator.Digits(4)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_digits_fail_negative_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("code", "-12")], files: []))

  validator.start(
    [validator.for("code", [validator.Digits(2)])],
    form_data,
    ctx,
  )
  |> should.be_error()
}

pub fn for_digits_fail_non_numeric_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("code", "ab")], files: []))

  validator.start(
    [validator.for("code", [validator.Digits(2)])],
    form_data,
    ctx,
  )
  |> should.be_error()
}

pub fn for_min_digits_pass_leading_zeros_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("code", "007")], files: []))

  validator.start(
    [validator.for("code", [validator.MinDigits(2)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_min_digits_fail_negative_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("code", "-123")], files: []))

  validator.start(
    [validator.for("code", [validator.MinDigits(2)])],
    form_data,
    ctx,
  )
  |> should.be_error()
}

pub fn for_max_digits_pass_leading_zeros_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("code", "007")], files: []))

  validator.start(
    [validator.for("code", [validator.MaxDigits(3)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_max_digits_fail_negative_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("code", "-12")], files: []))

  validator.start(
    [validator.for("code", [validator.MaxDigits(3)])],
    form_data,
    ctx,
  )
  |> should.be_error()
}

// ------------------------------------------------------------- Confirmed Rule Tests

pub fn for_confirmed_pass_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(
        values: [
          #("password", "secret123"),
          #("password_confirmation", "secret123"),
        ],
        files: [],
      ),
    )

  validator.start(
    [validator.for("password", [validator.Confirmed("password_confirmation")])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_confirmed_fail_mismatch_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(
        values: [
          #("password", "secret123"),
          #("password_confirmation", "different"),
        ],
        files: [],
      ),
    )

  case
    validator.start(
      [
        validator.for("password", [validator.Confirmed("password_confirmation")]),
      ],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(name: field_name, messages: msgs)]) -> {
      field_name
      |> should.equal("password")

      msgs
      |> should.equal(["password does not match password_confirmation"])
    }
    _ -> should.fail()
  }
}

pub fn for_confirmed_fail_missing_confirmation_field_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("password", "secret123")], files: []),
    )

  case
    validator.start(
      [
        validator.for("password", [validator.Confirmed("password_confirmation")]),
      ],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(name: field_name, messages: msgs)]) -> {
      field_name
      |> should.equal("password")

      msgs
      |> should.equal(["password does not match password_confirmation"])
    }
    _ -> should.fail()
  }
}

pub fn for_confirmed_skip_empty_value_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(
        values: [#("password", ""), #("password_confirmation", "something")],
        files: [],
      ),
    )

  validator.start(
    [validator.for("password", [validator.Confirmed("password_confirmation")])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

// ------------------------------------------------------------- Regex Rule Tests

pub fn for_regex_pass_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("code", "ABC-123")], files: []),
    )

  validator.start(
    [validator.for("code", [validator.Regex("^[A-Z]+-\\d+$")])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_regex_fail_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("code", "abc")], files: []))

  case
    validator.start(
      [validator.for("code", [validator.Regex("^[A-Z]+-\\d+$")])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["code format is invalid"])
    }
    _ -> should.fail()
  }
}

pub fn for_regex_skip_empty_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("code", "")], files: []))

  validator.start(
    [validator.for("code", [validator.Regex("^[A-Z]+$")])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

// ------------------------------------------------------------- RequiredIf Rule Tests

pub fn for_required_if_triggered_pass_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(
        values: [#("type", "business"), #("company_name", "Acme")],
        files: [],
      ),
    )

  validator.start(
    [validator.for("company_name", [validator.RequiredIf("type", "business")])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_required_if_triggered_fail_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(
        values: [#("type", "business"), #("company_name", "")],
        files: [],
      ),
    )

  case
    validator.start(
      [
        validator.for("company_name", [
          validator.RequiredIf("type", "business"),
        ]),
      ],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal([
        "company_name is required when type is business",
      ])
    }
    _ -> should.fail()
  }
}

pub fn for_required_if_not_triggered_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(
        values: [#("type", "personal"), #("company_name", "")],
        files: [],
      ),
    )

  validator.start(
    [validator.for("company_name", [validator.RequiredIf("type", "business")])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

// ------------------------------------------------------------- RequiredUnless Rule Tests

pub fn for_required_unless_pass_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("role", "admin"), #("reason", "")], files: []),
    )

  validator.start(
    [validator.for("reason", [validator.RequiredUnless("role", "admin")])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_required_unless_fail_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("role", "user"), #("reason", "")], files: []),
    )

  case
    validator.start(
      [validator.for("reason", [validator.RequiredUnless("role", "admin")])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["reason is required unless role is admin"])
    }
    _ -> should.fail()
  }
}

pub fn for_required_unless_has_value_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(
        values: [#("role", "user"), #("reason", "needs access")],
        files: [],
      ),
    )

  validator.start(
    [validator.for("reason", [validator.RequiredUnless("role", "admin")])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

// ------------------------------------------------------------- In Rule Tests

pub fn for_in_pass_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("color", "red")], files: []))

  validator.start(
    [validator.for("color", [validator.In(["red", "green", "blue"])])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_in_fail_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("color", "purple")], files: []),
    )

  case
    validator.start(
      [validator.for("color", [validator.In(["red", "green", "blue"])])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["color must be one of: red, green, blue"])
    }
    _ -> should.fail()
  }
}

pub fn for_in_skip_empty_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("color", "")], files: []))

  validator.start(
    [validator.for("color", [validator.In(["red", "green", "blue"])])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

// ------------------------------------------------------------- NotIn Rule Tests

pub fn for_not_in_pass_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("username", "alice")], files: []),
    )

  validator.start(
    [validator.for("username", [validator.NotIn(["admin", "root"])])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_not_in_fail_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("username", "admin")], files: []),
    )

  case
    validator.start(
      [validator.for("username", [validator.NotIn(["admin", "root"])])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["username must not be one of: admin, root"])
    }
    _ -> should.fail()
  }
}

pub fn for_not_in_skip_empty_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("username", "")], files: []))

  validator.start(
    [validator.for("username", [validator.NotIn(["admin", "root"])])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

// ------------------------------------------------------------- Alpha Rule Tests

pub fn for_alpha_pass_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("name", "John")], files: []))

  validator.start([validator.for("name", [validator.Alpha])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_alpha_fail_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("name", "John123")], files: []),
    )

  case
    validator.start([validator.for("name", [validator.Alpha])], form_data, ctx)
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["name must contain only letters"])
    }
    _ -> should.fail()
  }
}

pub fn for_alpha_fail_spaces_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("name", "John Doe")], files: []),
    )

  validator.start([validator.for("name", [validator.Alpha])], form_data, ctx)
  |> should.be_error()
}

pub fn for_alpha_skip_empty_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("name", "")], files: []))

  validator.start([validator.for("name", [validator.Alpha])], form_data, ctx)
  |> should.be_ok()
}

// ------------------------------------------------------------- AlphaNumeric Rule Tests

pub fn for_alpha_numeric_pass_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("username", "john123")], files: []),
    )

  validator.start(
    [validator.for("username", [validator.AlphaNumeric])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_alpha_numeric_fail_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("username", "john-123")], files: []),
    )

  case
    validator.start(
      [validator.for("username", [validator.AlphaNumeric])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["username must contain only letters and numbers"])
    }
    _ -> should.fail()
  }
}

pub fn for_alpha_numeric_skip_empty_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("username", "")], files: []))

  validator.start(
    [validator.for("username", [validator.AlphaNumeric])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

// ------------------------------------------------------------- StartsWith Rule Tests

pub fn for_starts_with_pass_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("code", "PRE-123")], files: []),
    )

  validator.start(
    [validator.for("code", [validator.StartsWith("PRE-")])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_starts_with_fail_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("code", "POST-123")], files: []),
    )

  case
    validator.start(
      [validator.for("code", [validator.StartsWith("PRE-")])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["code must start with PRE-"])
    }
    _ -> should.fail()
  }
}

pub fn for_starts_with_skip_empty_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("code", "")], files: []))

  validator.start(
    [validator.for("code", [validator.StartsWith("PRE-")])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

// ------------------------------------------------------------- EndsWith Rule Tests

pub fn for_ends_with_pass_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("email", "user@example.com")], files: []),
    )

  validator.start(
    [validator.for("email", [validator.EndsWith(".com")])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_ends_with_fail_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("email", "user@example.org")], files: []),
    )

  case
    validator.start(
      [validator.for("email", [validator.EndsWith(".com")])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["email must end with .com"])
    }
    _ -> should.fail()
  }
}

pub fn for_ends_with_skip_empty_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("email", "")], files: []))

  validator.start(
    [validator.for("email", [validator.EndsWith(".com")])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

// ------------------------------------------------------------- Between Rule Tests

pub fn for_between_pass_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("age", "25")], files: []))

  validator.start(
    [validator.for("age", [validator.Between(18, 65)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_between_pass_at_min_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("age", "18")], files: []))

  validator.start(
    [validator.for("age", [validator.Between(18, 65)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_between_pass_at_max_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("age", "65")], files: []))

  validator.start(
    [validator.for("age", [validator.Between(18, 65)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_between_fail_below_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("age", "10")], files: []))

  case
    validator.start(
      [validator.for("age", [validator.Between(18, 65)])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["age must be between 18 and 65"])
    }
    _ -> should.fail()
  }
}

pub fn for_between_fail_above_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("age", "70")], files: []))

  validator.start(
    [validator.for("age", [validator.Between(18, 65)])],
    form_data,
    ctx,
  )
  |> should.be_error()
}

pub fn for_between_fail_not_numeric_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("age", "abc")], files: []))

  case
    validator.start(
      [validator.for("age", [validator.Between(18, 65)])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["age must be a valid number"])
    }
    _ -> should.fail()
  }
}

pub fn for_between_skip_empty_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("age", "")], files: []))

  validator.start(
    [validator.for("age", [validator.Between(18, 65)])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

// ------------------------------------------------------------- Date Rule Tests

pub fn for_date_pass_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("birthday", "2000-01-15")], files: []),
    )

  validator.start([validator.for("birthday", [validator.Date])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_date_fail_format_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("birthday", "01/15/2000")], files: []),
    )

  case
    validator.start(
      [validator.for("birthday", [validator.Date])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["birthday must be a valid date (YYYY-MM-DD)"])
    }
    _ -> should.fail()
  }
}

pub fn for_date_fail_invalid_month_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("birthday", "2000-13-01")], files: []),
    )

  validator.start([validator.for("birthday", [validator.Date])], form_data, ctx)
  |> should.be_error()
}

pub fn for_date_fail_invalid_day_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("birthday", "2000-01-32")], files: []),
    )

  validator.start([validator.for("birthday", [validator.Date])], form_data, ctx)
  |> should.be_error()
}

pub fn for_date_skip_empty_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("birthday", "")], files: []))

  validator.start([validator.for("birthday", [validator.Date])], form_data, ctx)
  |> should.be_ok()
}

// ------------------------------------------------------------- Uuid Rule Tests

pub fn for_uuid_pass_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(
        values: [#("id", "550e8400-e29b-41d4-a716-446655440000")],
        files: [],
      ),
    )

  validator.start([validator.for("id", [validator.Uuid])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_uuid_fail_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("id", "not-a-uuid")], files: []),
    )

  case
    validator.start([validator.for("id", [validator.Uuid])], form_data, ctx)
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["id must be a valid UUID"])
    }
    _ -> should.fail()
  }
}

pub fn for_uuid_fail_wrong_length_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("id", "550e8400-e29b-41d4-a716")], files: []),
    )

  validator.start([validator.for("id", [validator.Uuid])], form_data, ctx)
  |> should.be_error()
}

pub fn for_uuid_skip_empty_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("id", "")], files: []))

  validator.start([validator.for("id", [validator.Uuid])], form_data, ctx)
  |> should.be_ok()
}

// ------------------------------------------------------------- Ip Rule Tests

pub fn for_ip_pass_ipv4_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("server", "192.168.1.1")], files: []),
    )

  validator.start([validator.for("server", [validator.Ip])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_ip_pass_ipv6_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(
        values: [#("server", "2001:0db8:85a3:0000:0000:8a2e:0370:7334")],
        files: [],
      ),
    )

  validator.start([validator.for("server", [validator.Ip])], form_data, ctx)
  |> should.be_ok()
}

pub fn for_ip_fail_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("server", "999.999.999.999")], files: []),
    )

  case
    validator.start([validator.for("server", [validator.Ip])], form_data, ctx)
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["server must be a valid IP address"])
    }
    _ -> should.fail()
  }
}

pub fn for_ip_fail_text_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("server", "not-an-ip")], files: []),
    )

  validator.start([validator.for("server", [validator.Ip])], form_data, ctx)
  |> should.be_error()
}

pub fn for_ip_skip_empty_test() {
  let form_data =
    validator.form_data(wisp.FormData(values: [#("server", "")], files: []))

  validator.start([validator.for("server", [validator.Ip])], form_data, ctx)
  |> should.be_ok()
}

// ------------------------------------------------------------- Multiple Rules Tests

pub fn for_multiple_rules_all_pass_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("email", "test@example.com")], files: []),
    )

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
  let form_data =
    validator.form_data(wisp.FormData(values: [#("name", "Jo")], files: []))

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
      |> should.equal(["name must be at least 3 characters long"])
    }
    _ -> should.fail()
  }
}

pub fn start_all_pass_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(
        values: [#("name", "John"), #("email", "john@example.com")],
        files: [],
      ),
    )

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
    validator.form_data(
      wisp.FormData(values: [#("name", ""), #("email", "invalid")], files: []),
    )

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
  let form_data =
    validator.form_data(
      wisp.FormData(values: [], files: [#("avatar", uploaded_file)]),
    )

  validator.start(
    [validator.for_file("avatar", [validator.FileRequired])],
    form_data,
    ctx,
  )
  |> should.be_ok()
}

pub fn for_file_required_fail_empty_filename_test() {
  let uploaded_file = wisp.UploadedFile(file_name: "", path: "/tmp/test")
  let form_data =
    validator.form_data(
      wisp.FormData(values: [], files: [#("avatar", uploaded_file)]),
    )

  case
    validator.start(
      [validator.for_file("avatar", [validator.FileRequired])],
      form_data,
      ctx,
    )
  {
    Error([validator.ValidationError(messages: msgs, ..)]) -> {
      msgs
      |> should.equal(["avatar is required"])
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
      |> should.equal(["avatar is required"])
    }
    _ -> should.fail()
  }
}

pub fn for_file_extension_pass_test() {
  let uploaded_file =
    wisp.UploadedFile(file_name: "test.jpg", path: "/tmp/test")
  let form_data =
    validator.form_data(
      wisp.FormData(values: [], files: [#("avatar", uploaded_file)]),
    )

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
  let form_data =
    validator.form_data(
      wisp.FormData(values: [], files: [#("avatar", uploaded_file)]),
    )

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
        "avatar must have one of the following extensions: jpg, png",
      ])
    }
    _ -> should.fail()
  }
}

pub fn for_file_extension_case_insensitive_test() {
  let uploaded_file =
    wisp.UploadedFile(file_name: "test.JPG", path: "/tmp/test")
  let form_data =
    validator.form_data(
      wisp.FormData(values: [], files: [#("avatar", uploaded_file)]),
    )

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
  let form_data =
    validator.form_data(
      wisp.FormData(values: [], files: [#("file", uploaded_file)]),
    )

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
  let form_data =
    validator.form_data(
      wisp.FormData(values: [], files: [#("file", uploaded_file)]),
    )

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
      |> should.equal(["file must be at least 10 KB in size"])
    }
    _ -> should.fail()
  }
}

pub fn for_file_max_size_pass_test() {
  let test_path = "/tmp/glimr_test_max_size_pass.txt"
  let content = bit_array.from_string("small content")
  let assert Ok(_) = simplifile.write_bits(test_path, content)

  let uploaded_file = wisp.UploadedFile(file_name: "test.txt", path: test_path)
  let form_data =
    validator.form_data(
      wisp.FormData(values: [], files: [#("file", uploaded_file)]),
    )

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
  let form_data =
    validator.form_data(
      wisp.FormData(values: [], files: [#("file", uploaded_file)]),
    )

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
      |> should.equal(["file must be no more than 2 KB in size"])
    }
    _ -> should.fail()
  }
}

// ------------------------------------------------------------- Custom Validation Tests

pub fn for_custom_pass_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("username", "alice123")], files: []),
    )

  let custom_rule = fn(
    field: String,
    value: String,
    _data: validator.FormData,
    _ctx: Nil,
  ) -> Result(Nil, String) {
    case string.contains(value, "alice") {
      True -> Ok(Nil)
      False -> Error(field <> " must contain 'alice'")
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
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("username", "bob123")], files: []),
    )

  let custom_rule = fn(
    field: String,
    value: String,
    _data: validator.FormData,
    _ctx: Nil,
  ) -> Result(Nil, String) {
    case string.contains(value, "alice") {
      True -> Ok(Nil)
      False -> Error(field <> " must contain 'alice'")
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
      |> should.equal(["username must contain 'alice'"])
    }
    _ -> should.fail()
  }
}

pub fn for_custom_with_other_rules_test() {
  let form_data =
    validator.form_data(
      wisp.FormData(values: [#("password", "Test123")], files: []),
    )

  let has_number = fn(
    field: String,
    value: String,
    _data: validator.FormData,
    _ctx: Nil,
  ) -> Result(Nil, String) {
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
      False -> Error(field <> " must contain at least one number")
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
  let form_data =
    validator.form_data(
      wisp.FormData(values: [], files: [#("file", uploaded_file)]),
    )

  let custom_file_rule = fn(
    field: String,
    file: wisp.UploadedFile,
    _data: validator.FormData,
    _ctx: Nil,
  ) -> Result(Nil, String) {
    case string.ends_with(file.file_name, ".txt") {
      True -> Ok(Nil)
      False -> Error(field <> " must be a text file")
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
  let form_data =
    validator.form_data(
      wisp.FormData(values: [], files: [#("file", uploaded_file)]),
    )

  let custom_file_rule = fn(
    field: String,
    file: wisp.UploadedFile,
    _data: validator.FormData,
    _ctx: Nil,
  ) -> Result(Nil, String) {
    case string.ends_with(file.file_name, ".txt") {
      True -> Ok(Nil)
      False -> Error(field <> " must be a text file")
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
      |> should.equal(["file must be a text file"])
    }
    _ -> should.fail()
  }
}

pub fn for_file_custom_with_other_rules_test() {
  let test_path = "/tmp/glimr_test_custom_combined.txt"
  let content = bit_array.from_string("test content")
  let assert Ok(_) = simplifile.write_bits(test_path, content)

  let uploaded_file = wisp.UploadedFile(file_name: "test.txt", path: test_path)
  let form_data =
    validator.form_data(
      wisp.FormData(values: [], files: [#("file", uploaded_file)]),
    )

  let custom_file_rule = fn(
    _field: String,
    file: wisp.UploadedFile,
    _data: validator.FormData,
    _ctx: Nil,
  ) -> Result(Nil, String) {
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
