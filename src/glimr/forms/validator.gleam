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
import gleam/result
import gleam/string
import glimr/forms/form
import simplifile
import wisp.{type FormData, type Request, type Response, type UploadedFile}

// ------------------------------------------------------------- Public Types

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
  Digits(Int)
  MinDigits(Int)
  MaxDigits(Int)
  Custom(CustomValidation)
}

/// ------------------------------------------------------------
/// File Rule Type
/// ------------------------------------------------------------
///
/// Defines validation rules that can be applied to file upload
/// fields. Rules include required file checks, file size
/// constraints (in KB), and allowed file extension validation.
///
pub type FileRule {
  FileRequired
  FileMinSize(Int)
  FileMaxSize(Int)
  FileExtension(List(String))
  FileCustom(CustomFileValidation)
}

// ------------------------------------------------------------- Private Types

/// ------------------------------------------------------------
/// CustomValidation Type
/// ------------------------------------------------------------
///
/// A function type for custom text field validation. Takes a
/// string value and returns Ok(Nil) if valid, or Error with
/// an error message if validation fails.
///
type CustomValidation =
  fn(String) -> Result(Nil, String)

/// ------------------------------------------------------------
/// CustomFileValidation Type
/// ------------------------------------------------------------
///
/// A function type for custom file upload validation. Takes
/// an UploadedFile and returns Ok(Nil) if valid, or Error
/// with an error message if validation fails.
///
type CustomFileValidation =
  fn(UploadedFile) -> Result(Nil, String)

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Handle Form Validation
/// ------------------------------------------------------------
///
/// Validates form data, transforms it using an extractor 
/// function, and executes a callback on success. Automatically 
/// extracts form data from the request, runs validation rules, 
/// transforms the validated data to a typed structure, and 
/// returns a 422 error response on failure with formatted error 
/// messages.
///
/// ------------------------------------------------------------
///
/// *Example:*
///
/// In your request module (e.g., `app/http/requests/contact_store.gleam`):
///
/// ```gleam
/// pub type Data {
///   Data(name: String, email: String, avatar: UploadedFile)
/// }
///
/// pub fn rules(form) {
///   [
///     form |> validator.for("name", [Required, MinLength(2)]),
///     form |> validator.for("email", [Required, Email]),
///     form |> validator.for_file("avatar", [RequiredFile, FileMaxSize(5000)]),
///   ]
/// }
///
/// pub fn data(form) -> Data {
///   Data(
///     name: form.get(form, "name"),
///     email: form.get(form, "email"),
///     avatar: form.get_file(form, "avatar"),
///   )
/// }
/// ```
///
/// In your controller:
///
/// ```gleam
/// pub fn store(req: Request, ctx: Context) -> Response {
///   use validated <- validator.run(req, contact_store.rules, contact_store.data)
///
///   // validated is now your Data with typed fields!
///   // validated.name: String
///   // validated.email: String
///   // validated.avatar: UploadedFile
///
///   redirect.to("/success")
/// }
/// ```
///
pub fn run(
  req: Request,
  rules: fn(FormData) -> List(Result(a, ValidationError)),
  data: fn(FormData) -> typed_form,
  on_valid: fn(typed_form) -> Response,
) -> Response {
  use form <- wisp.require_form(req)

  case start(rules(form)) {
    Ok(_) -> on_valid(data(form))
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
pub fn start(
  rules: List(Result(a, ValidationError)),
) -> Result(Nil, List(ValidationError)) {
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
///
/// *Example:*
/// 
/// ```gleam
/// form |> validation.for("email", [Required, Email])
/// ```
///
pub fn for(
  form: FormData,
  field_name: String,
  rules: List(Rule),
) -> Result(Nil, ValidationError) {
  let value = form |> form.get(field_name)

  let messages =
    rules
    |> list.filter_map(fn(rule) {
      case apply_rule(value, rule) {
        Ok(_) -> Error(Nil)
        Error(message) -> Ok(format_error_message(field_name, message))
      }
    })

  case messages {
    [] -> Ok(Nil)
    msgs -> Error(ValidationError(name: field_name, messages: msgs))
  }
}

/// ------------------------------------------------------------
/// Validate File Field
/// ------------------------------------------------------------
///
/// Validates a single file upload field against a list of rules.
/// Returns Ok(Nil) if all rules pass, or Error with the
/// validation error containing all failed rule messages.
///
/// ------------------------------------------------------------
///
/// *Example:*
/// 
/// ```gleam
/// form |> validation.for_file("avatar", [FileRequired, FileMaxSize(2048)])
/// ```
///
pub fn for_file(
  form: FormData,
  field_name: String,
  rules: List(FileRule),
) -> Result(Nil, ValidationError) {
  let file = form |> form.get_file_result(field_name)

  let messages =
    rules
    |> list.filter_map(fn(rule) {
      case apply_file_rule(file, rule) {
        Ok(_) -> Error(Nil)
        Error(message) -> Ok(format_error_message(field_name, message))
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

// ------------------------------------------------------------- Private Functions

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
    Digits(count) -> validate_digits(value, count)
    MinDigits(min) -> validate_min_digits(value, min)
    MaxDigits(max) -> validate_max_digits(value, max)
    Custom(custom_validation) -> validate_custom(custom_validation, value)
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

/// ------------------------------------------------------------
/// Validate Exact Digit Count
/// ------------------------------------------------------------
///
/// Validates that a numeric field has exactly the specified
/// number of digits. Returns an error if the field is not a
/// valid number or doesn't have the exact digit count.
///
fn validate_digits(value: String, count: Int) -> Result(Nil, String) {
  case int.parse(value) {
    Ok(n) -> {
      let digit_count =
        int.absolute_value(n)
        |> int.to_string
        |> string.length

      case digit_count == count {
        True -> Ok(Nil)
        False ->
          Error("must have exactly " <> int.to_string(count) <> " digits")
      }
    }
    Error(_) -> Error("must be a valid number")
  }
}

/// ------------------------------------------------------------
/// Validate Minimum Digit Count
/// ------------------------------------------------------------
///
/// Validates that a numeric field has at least the specified
/// number of digits. Returns an error if the field is not a
/// valid number or has fewer digits than required.
///
fn validate_min_digits(value: String, min: Int) -> Result(Nil, String) {
  case int.parse(value) {
    Ok(n) -> {
      let digit_count =
        int.absolute_value(n)
        |> int.to_string
        |> string.length

      case digit_count >= min {
        True -> Ok(Nil)
        False -> Error("must have at least " <> int.to_string(min) <> " digits")
      }
    }
    Error(_) -> Error("must be a valid number")
  }
}

/// ------------------------------------------------------------
/// Validate Maximum Digit Count
/// ------------------------------------------------------------
///
/// Validates that a numeric field has at most the specified
/// number of digits. Returns an error if the field is not a
/// valid number or has more digits than allowed.
///
fn validate_max_digits(value: String, max: Int) -> Result(Nil, String) {
  case int.parse(value) {
    Ok(n) -> {
      let digit_count =
        int.absolute_value(n)
        |> int.to_string
        |> string.length

      case digit_count <= max {
        True -> Ok(Nil)
        False ->
          Error("must have no more than " <> int.to_string(max) <> " digits")
      }
    }
    Error(_) -> Error("must be a valid number")
  }
}

/// ------------------------------------------------------------
/// Validate Custom Rule
/// ------------------------------------------------------------
///
/// Applies a custom validation function to a field value.
/// Returns Ok(Nil) if the custom validation passes, or Error
/// with the custom error message if validation fails.
///
fn validate_custom(
  custom_validation: CustomValidation,
  value: String,
) -> Result(Nil, String) {
  custom_validation(value)
}

/// ------------------------------------------------------------
/// Apply File Rule
/// ------------------------------------------------------------
///
/// Applies a single validation rule to an uploaded file. Returns
/// Ok(Nil) if the rule passes, or Error with an error message
/// if validation fails. Used internally by the for_file function.
///
fn apply_file_rule(
  file: Result(UploadedFile, Nil),
  rule: FileRule,
) -> Result(Nil, String) {
  case rule {
    FileRequired -> validate_file_required(file)
    FileMinSize(min) -> validate_file_min_size(file, min)
    FileMaxSize(max) -> validate_file_max_size(file, max)
    FileExtension(extensions) -> validate_file_extension(file, extensions)
    FileCustom(custom_validation) ->
      validate_file_custom(custom_validation, file)
  }
}

/// ------------------------------------------------------------
/// Validate File Required
/// ------------------------------------------------------------
///
/// Validates that a file has been uploaded. Returns an error
/// if no file is present in the form data.
///
fn validate_file_required(
  file: Result(UploadedFile, Nil),
) -> Result(Nil, String) {
  case file {
    Ok(uploaded_file) -> {
      case string.trim(uploaded_file.file_name) {
        "" -> Error("is required")
        _ -> Ok(Nil)
      }
    }
    Error(_) -> Error("is required")
  }
}

/// ------------------------------------------------------------
/// Validate File Minimum Size
/// ------------------------------------------------------------
///
/// Validates that an uploaded file meets or exceeds the
/// specified minimum size in kilobytes (KB).
///
fn validate_file_min_size(
  file: Result(UploadedFile, Nil),
  min_kb: Int,
) -> Result(Nil, String) {
  case file {
    Error(_) -> Ok(Nil)
    Ok(uploaded_file) -> {
      case simplifile.file_info(uploaded_file.path) {
        Ok(info) -> {
          let size_kb = info.size / 1024
          case size_kb >= min_kb {
            True -> Ok(Nil)
            False ->
              Error(
                "must be at least " <> int.to_string(min_kb) <> " KB in size",
              )
          }
        }
        Error(_) -> Error("could not read file information")
      }
    }
  }
}

/// ------------------------------------------------------------
/// Validate File Maximum Size
/// ------------------------------------------------------------
///
/// Validates that an uploaded file does not exceed the
/// specified maximum size in kilobytes (KB).
///
fn validate_file_max_size(
  file: Result(UploadedFile, Nil),
  max_kb: Int,
) -> Result(Nil, String) {
  case file {
    Error(_) -> Ok(Nil)
    Ok(uploaded_file) -> {
      case simplifile.file_info(uploaded_file.path) {
        Ok(info) -> {
          let size_kb = info.size / 1024
          case size_kb <= max_kb {
            True -> Ok(Nil)
            False ->
              Error(
                "must be no more than "
                <> int.to_string(max_kb)
                <> " KB in size",
              )
          }
        }
        Error(_) -> Error("could not read file information")
      }
    }
  }
}

/// ------------------------------------------------------------
/// Validate File Extension
/// ------------------------------------------------------------
///
/// Validates that an uploaded file has one of the allowed
/// extensions. Extensions should be provided without dots
/// (e.g., ["jpg", "png", "pdf"]).
///
fn validate_file_extension(
  file: Result(UploadedFile, Nil),
  allowed_extensions: List(String),
) -> Result(Nil, String) {
  case file {
    Error(_) -> Ok(Nil)
    Ok(uploaded_file) -> {
      let file_extension =
        uploaded_file.file_name
        |> string.split(".")
        |> list.last
        |> result.unwrap("")
        |> string.lowercase

      case list.contains(allowed_extensions, file_extension) {
        True -> Ok(Nil)
        False -> {
          let allowed = string.join(allowed_extensions, ", ")
          Error("must have one of the following extensions: " <> allowed)
        }
      }
    }
  }
}

/// ------------------------------------------------------------
/// Validate Custom File Rule
/// ------------------------------------------------------------
///
/// Applies a custom validation function to an uploaded file.
/// Returns Ok(Nil) if the custom validation passes, or Error
/// with the custom error message if validation fails. Returns
/// Ok(Nil) if no file is present.
///
fn validate_file_custom(
  custom_validation: CustomFileValidation,
  file: Result(UploadedFile, Nil),
) -> Result(Nil, String) {
  case file {
    Error(_) -> Ok(Nil)
    Ok(uploaded_file) -> custom_validation(uploaded_file)
  }
}

/// ------------------------------------------------------------
/// Format Error Message
/// ------------------------------------------------------------
///
/// Formats a validation error message by normalizing the field
/// name and combining it with the error message. Converts
/// underscores and dashes to spaces, capitalizes the result.
/// For example: "user_name" becomes "User name is required".
///
fn format_error_message(field_name: String, message: String) -> String {
  let normalized_name =
    field_name
    |> string.split("_")
    |> list.intersperse(" ")
    |> string.concat
    |> string.split("-")
    |> list.intersperse(" ")
    |> string.concat

  string.capitalise(normalized_name) <> " " <> message
}
