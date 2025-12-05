//// ------------------------------------------------------------
//// Validation Helpers
//// ------------------------------------------------------------
////
//// Form validation utilities with built-in rules for common
//// validation patterns including required fields, email format,
//// length constraints, numeric ranges, and URL validation.
////

import gleam/int
import gleam/list
import gleam/string
import glimr/helpers/form
import glimr/routing/route
import wisp

/// ------------------------------------------------------------
/// ValidationError Type
/// ------------------------------------------------------------
///
/// Represents a validation error for a specific field, that 
/// contains the field name and a list of error messages. 
/// Multiple rules can fail for a single field, generating 
/// multiple messages.
///
pub type ValidationError {
  ValidationError(name: String, messages: List(String))
}

/// ------------------------------------------------------------
/// Rule Type
/// ------------------------------------------------------------
///
/// Defines validation rules that can be applied to form fields.
/// Rules include required field checks, format validation, 
/// length constraints, and numeric range validation.
///
pub type Rule {
  Required
  Email
  MinLength(Int)
  MaxLength(Int)
  Min(Int)
  Max(Int)
  Numeric
  Url
}

/// ------------------------------------------------------------
/// Handle Form Validation
/// ------------------------------------------------------------
///
/// Validates form data and executes a callback on success.
/// Automatically extracts form data from the request, runs
/// validation rules, and returns a 422 error response on
/// failure with formatted error messages.
///
pub fn handle(
  rules: fn(wisp.FormData) -> Result(Nil, List(ValidationError)),
  req: route.RouteRequest,
  on_valid: fn(wisp.FormData) -> wisp.Response,
) -> wisp.Response {
  use form <- wisp.require_form(req.request)

  case rules(form) {
    Ok(_) -> on_valid(form)
    Error(errors) -> {
      let error_html =
        "<h1>Validation Errors:</h1><ul>"
        <> string.join(
          list.map(errors, fn(err) {
            string.join(
              list.map(err.messages, fn(msg) { "<li>" <> msg <> "</li>" }),
              "",
            )
          }),
          "",
        )
        <> "</ul>"
      wisp.html_response(error_html, 422)
    }
  }
}

/// ------------------------------------------------------------
/// Start Validation
/// ------------------------------------------------------------
///
/// Collects validation results from multiple field validations
/// and returns a combined result. Returns Ok(Nil) if all rules
/// pass, or Error with all validation errors if any fail.
///
/// ------------------------------------------------------------
/// Example:
/// ```gleam
/// validation.start([
///   form |> validation.for("email", [Required, Email]),
///   form |> validation.for("age", [Required, Numeric, Min(18)]),
/// ])
/// ```
///
pub fn start(rules) -> Result(Nil, List(ValidationError)) {
  let errors =
    rules
    |> list.filter_map(fn(result) {
      case result {
        Ok(_) -> Error(Nil)
        Error(err) -> Ok(err)
      }
    })

  response(errors)
}

/// ------------------------------------------------------------
/// Validate Field
/// ------------------------------------------------------------
///
/// Validates a single form field against a list of rules.
/// Returns Ok(Nil) if all rules pass, or Error with the
/// validation error containing all failed rule messages.
///
/// ------------------------------------------------------------
/// Example:
/// ```gleam
/// form |> validation.for("email", [Required, Email])
/// ```
///
pub fn for(
  form: wisp.FormData,
  field_name: String,
  rules: List(Rule),
) -> Result(Nil, ValidationError) {
  let value = form |> form.get_field_value(field_name)

  let messages =
    rules
    |> list.filter_map(fn(rule) {
      case apply_rule(value, rule) {
        Ok(_) -> Error(Nil)
        Error(message) -> Ok(field_name <> " " <> message)
      }
    })

  case messages {
    [] -> Ok(Nil)
    msgs -> Error(ValidationError(name: field_name, messages: msgs))
  }
}

/// ------------------------------------------------------------
/// Convert Errors to Result
/// ------------------------------------------------------------
///
/// Converts a list of validation errors into a Result type.
/// Returns Ok(Nil) if the error list is empty, or Error with
/// the errors if any exist. Used internally by start function.
///
pub fn response(
  errors: List(ValidationError),
) -> Result(Nil, List(ValidationError)) {
  case errors {
    [] -> Ok(Nil)
    _ -> Error(errors)
  }
}

/// ------------------------------------------------------------
/// Apply Rule
/// ------------------------------------------------------------
///
/// Applies a single validation rule to a field value. Returns
/// Ok(Nil) if the rule passes, or Error with an error message
/// if validation fails. Used internally by the for function.
///
fn apply_rule(value: String, rule: Rule) -> Result(Nil, String) {
  case rule {
    Required -> validate_required(value)
    Email -> validate_email(value)
    MinLength(min) -> validate_min_length(value, min)
    MaxLength(max) -> validate_max_length(value, max)
    Min(min) -> validate_min(value, min)
    Max(max) -> validate_max(value, max)
    Numeric -> validate_numeric(value)
    Url -> validate_url(value)
  }
}

/// ------------------------------------------------------------
/// Validate Required Field
/// ------------------------------------------------------------
///
/// Validates that a field has a non-empty value after trimming
/// whitespace. Returns an error if the field is empty.
///
fn validate_required(value: String) -> Result(Nil, String) {
  case string.trim(value) {
    "" -> Error("is required")
    _ -> Ok(Nil)
  }
}

/// ------------------------------------------------------------
/// Validate Email Format
/// ------------------------------------------------------------
///
/// Validates that a field contains a basic email format with
/// both "@" and "." characters. This is a simple check, not
/// a comprehensive RFC-compliant email validator.
///
fn validate_email(value: String) -> Result(Nil, String) {
  let trimmed = string.trim(value)
  case
    trimmed != ""
    && string.contains(trimmed, "@")
    && string.contains(trimmed, ".")
  {
    True -> Ok(Nil)
    False -> Error("must be a valid email address")
  }
}

/// ------------------------------------------------------------
/// Validate Minimum Length
/// ------------------------------------------------------------
///
/// Validates that a field's length meets or exceeds the
/// specified minimum character count.
///
fn validate_min_length(value: String, min: Int) -> Result(Nil, String) {
  case string.length(value) >= min {
    True -> Ok(Nil)
    False ->
      Error("must be at least " <> int.to_string(min) <> " characters long")
  }
}

/// ------------------------------------------------------------
/// Validate Maximum Length
/// ------------------------------------------------------------
///
/// Validates that a field's length does not exceed the
/// specified maximum character count.
///
fn validate_max_length(value: String, max: Int) -> Result(Nil, String) {
  case string.length(value) <= max {
    True -> Ok(Nil)
    False ->
      Error("must be no more than " <> int.to_string(max) <> " characters long")
  }
}

/// ------------------------------------------------------------
/// Validate Minimum Numeric Value
/// ------------------------------------------------------------
///
/// Validates that a numeric field meets or exceeds the
/// specified minimum value. Returns an error if the field
/// is not a valid number.
///
fn validate_min(value: String, min: Int) -> Result(Nil, String) {
  case int.parse(value) {
    Ok(n) if n >= min -> Ok(Nil)
    Ok(_) -> Error("must be at least " <> int.to_string(min))
    Error(_) -> Error("must be a valid number")
  }
}

/// ------------------------------------------------------------
/// Validate Maximum Numeric Value
/// ------------------------------------------------------------
///
/// Validates that a numeric field does not exceed the
/// specified maximum value. Returns an error if the field
/// is not a valid number.
///
fn validate_max(value: String, max: Int) -> Result(Nil, String) {
  case int.parse(value) {
    Ok(n) if n <= max -> Ok(Nil)
    Ok(_) -> Error("must be no more than " <> int.to_string(max))
    Error(_) -> Error("must be a valid number")
  }
}

/// ------------------------------------------------------------
/// Validate Numeric Format
/// ------------------------------------------------------------
///
/// Validates that a field contains a valid numeric value
/// that can be parsed as an integer.
///
fn validate_numeric(value: String) -> Result(Nil, String) {
  case int.parse(value) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("must be a valid number")
  }
}

/// ------------------------------------------------------------
/// Validate URL Format
/// ------------------------------------------------------------
///
/// Validates that a field contains a valid URL starting with
/// http:// or https://. This is a basic check for URL format.
///
fn validate_url(value: String) -> Result(Nil, String) {
  let trimmed = string.trim(value)
  case
    string.starts_with(trimmed, "http://")
    || string.starts_with(trimmed, "https://")
  {
    True -> Ok(Nil)
    False -> Error("must be a valid URL")
  }
}
