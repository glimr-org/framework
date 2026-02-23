//// Validation Helpers
////
//// Sprinkling ad-hoc validation logic across controllers leads
//// to inconsistent error messages and duplicated checks. This
//// module provides a declarative rule system so validators are
//// defined once per form, collected into a list, and executed
//// together — producing a uniform error structure that templates
//// can render without special-casing each field. The ctx type
//// parameter lets custom rules reach into app state for checks
//// like uniqueness without coupling the validator to a specific
//// database or config.

import gleam/int
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import glimr/forms/form
import glimr/response/redirect
import glimr/response/response.{type ResponseFormat, HTML, JSON}
import glimr/session/session.{type Session}
import simplifile
import wisp.{type FormData, type Request, type Response, type UploadedFile}

// ------------------------------------------------------------- Public Types

/// Grouping messages by field name lets templates render errors
/// next to the relevant input rather than in a flat list at the
/// top of the page. A list of messages per field means all
/// failures show at once so users can fix everything in one
/// submission instead of discovering errors one at a time.
///
pub type ValidationError {
  ValidationError(name: String, messages: List(String))
}

/// An enum of built-in rules covers the most common validation
/// needs so validators stay declarative — no inline functions
/// for simple checks like Required or MinLength. The Custom
/// variant is the escape hatch for app-specific logic like
/// uniqueness checks that need database access via ctx.
///
pub type StringRule(ctx) {
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
  Custom(CustomValidation(ctx))
}

/// File uploads need different validation than text fields —
/// size limits, extension checks, and presence checks all
/// operate on the uploaded file metadata rather than a string
/// value. A separate rule type keeps the type system from
/// allowing string rules on file fields or vice versa.
///
pub type FileRule(ctx) {
  FileRequired
  FileMinSize(Int)
  FileMaxSize(Int)
  FileExtension(List(String))
  FileCustom(CustomFileValidation(ctx))
}

/// Deferring execution lets validators be defined at module
/// level as pure data — no form data or context needed until
/// start() runs them. The opaque type prevents callers from
/// pattern matching on internals, so the FieldRule vs
/// FileFieldRule distinction stays an implementation detail.
///
pub opaque type Rule(ctx) {
  FieldRule(field_name: String, rules: List(StringRule(ctx)))
  FileFieldRule(field_name: String, rules: List(FileRule(ctx)))
}

/// Wrapping form access in getter functions rather than passing
/// raw FormData ensures the data extractor can only read fields
/// that were already validated. This prevents accidentally using
/// unvalidated values in the typed output struct.
///
pub type Validated {
  Validated(get: fn(String) -> String, get_file: fn(String) -> UploadedFile)
}

// ------------------------------------------------------------- Private Types

/// A type alias keeps the Custom variant readable — without it
/// the full fn signature would clutter the StringRule enum.
/// Accepting ctx lets custom rules do database lookups like
/// uniqueness checks without hardcoding a connection type into
/// the validator module.
///
type CustomValidation(ctx) =
  fn(String, ctx) -> Result(Nil, String)

/// Mirrors CustomValidation but for file uploads — the input
/// is an UploadedFile instead of a String because file rules
/// need access to path, size, and filename metadata that a
/// string value can't represent.
///
type CustomFileValidation(ctx) =
  fn(UploadedFile, ctx) -> Result(Nil, String)

// ------------------------------------------------------------- Public Functions

/// Controllers shouldn't mix validation logic with request
/// handling — that makes both harder to test and read. The use
/// callback pattern lets controllers declare validation inline
/// and only handle the happy path, while error responses are
/// generated automatically based on the response format:
///
/// - HTML: flashes the first error per field into the session
///   as "errors.<field>" and redirects back to the form.
/// - JSON: returns a 422 response with structured errors.
///
/// *Example:*
///
/// In your validator module (e.g., `app/http/validators/contact.gleam`):
///
/// ```gleam
/// pub type Data {
///   Data(name: String, email: String)
/// }
///
/// fn rules() {
///   [
///     validator.for("name", [Required, MinLength(2)]),
///     validator.for("email", [Required, Email]),
///   ]
/// }
///
/// fn data(v: Validated) -> Data {
///   Data(
///     name: v.get("name"),
///     email: v.get("email"),
///   )
/// }
///
/// pub fn validate(req: Request, ctx: Context, next: fn(Data) -> Response) {
///   use validated <- validator.run(
///     req, 
///     ctx, 
///     ctx.response_format, 
///     ctx.session, 
///     rules, 
///     data
///   )
///
///   next(validated)
/// }
/// ```
///
pub fn run(
  req: Request,
  ctx: ctx,
  response_format: ResponseFormat,
  session: Session,
  rules: fn() -> List(Rule(ctx)),
  data: fn(Validated) -> typed_form,
  on_valid: fn(typed_form) -> Response,
) -> Response {
  use form_data <- wisp.require_form(req)

  case start(rules(), form_data, ctx) {
    Ok(_) -> {
      let validated =
        Validated(
          get: fn(field) { form_data |> form.get(field) },
          get_file: fn(field) { form_data |> form.get_file(field) },
        )
      on_valid(data(validated))
    }
    Error(errors) -> {
      case response_format {
        HTML -> {
          list.each(errors, fn(err) {
            case err.messages {
              [first, ..] ->
                session.flash(session, "errors." <> err.name, first)
              [] -> Nil
            }
          })
          redirect.back(req)
        }
        JSON -> {
          json.object([
            #(
              "errors",
              json.array(errors, fn(err) {
                json.object([
                  #("field", json.string(err.name)),
                  #("messages", json.array(err.messages, json.string)),
                ])
              }),
            ),
          ])
          |> response.json(422)
        }
      }
    }
  }
}

/// Separated from run() so callers that need custom error
/// handling can execute validation without the automatic 422
/// response. Collecting all errors before returning ensures
/// users see every failure at once rather than fixing them
/// one by one across multiple submissions.
///
pub fn start(
  pending: List(Rule(ctx)),
  form: FormData,
  ctx: ctx,
) -> Result(Nil, List(ValidationError)) {
  let errors =
    pending
    |> list.filter_map(fn(p) {
      case execute(p, form, ctx) {
        Ok(_) -> Error(Nil)
        Error(err) -> Ok(err)
      }
    })

  response(errors)
}

/// Binding a field name to its rules in a single call keeps
/// the validator definition readable — without this, callers
/// would need to construct FieldRule variants directly and
/// the opaque type would need to be exposed.
///
/// *Example:*
///
/// ```gleam
/// validator.for("email", [Required, Email])
/// ```
///
pub fn for(field_name: String, rules: List(StringRule(ctx))) -> Rule(ctx) {
  FieldRule(field_name:, rules:)
}

/// Mirrors for() but accepts FileRule variants so the type
/// system prevents accidentally mixing string rules with file
/// fields. The same deferred execution model applies — rules
/// are data until start() runs them.
///
/// *Example:*
///
/// ```gleam
/// validator.for_file("avatar", [FileRequired, FileMaxSize(2048)])
/// ```
///
pub fn for_file(field_name: String, rules: List(FileRule(ctx))) -> Rule(ctx) {
  FileFieldRule(field_name:, rules:)
}

/// Exposed publicly so custom validation flows that bypass
/// start() can still produce the same Result shape. Converting
/// an empty error list to Ok(Nil) avoids forcing callers to
/// check list length before deciding success or failure.
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

/// Dispatches between text field and file field validation so
/// start() doesn't need to know which variant it's dealing
/// with. Extracting the value from form data here rather than
/// in each rule keeps individual rules pure — they only see
/// the value and ctx, not the full form.
///
fn execute(
  pending: Rule(ctx),
  form_data: FormData,
  ctx: ctx,
) -> Result(Nil, ValidationError) {
  case pending {
    FieldRule(field_name:, rules:) -> {
      let value = form_data |> form.get(field_name)
      let messages =
        rules
        |> list.filter_map(fn(rule) {
          case apply_rule(value, ctx, rule) {
            Ok(_) -> Error(Nil)
            Error(message) -> Ok(format_error_message(field_name, message))
          }
        })

      case messages {
        [] -> Ok(Nil)
        msgs -> Error(ValidationError(name: field_name, messages: msgs))
      }
    }
    FileFieldRule(field_name:, rules:) -> {
      let file = form_data |> form.get_file_result(field_name)
      let messages =
        rules
        |> list.filter_map(fn(rule) {
          case apply_file_rule(file, ctx, rule) {
            Ok(_) -> Error(Nil)
            Error(message) -> Ok(format_error_message(field_name, message))
          }
        })

      case messages {
        [] -> Ok(Nil)
        msgs -> Error(ValidationError(name: field_name, messages: msgs))
      }
    }
  }
}

/// Pattern matching on the StringRule enum dispatches to the
/// correct validation function. Each variant maps to exactly
/// one function so adding a new built-in rule only requires
/// adding a variant and its corresponding validate_ function.
///
fn apply_rule(
  value: String,
  ctx: ctx,
  rule: StringRule(ctx),
) -> Result(Nil, String) {
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
    Custom(custom_validation) -> validate_custom(custom_validation, value, ctx)
  }
}

/// Trimming before checking prevents whitespace-only values
/// from passing as "present" — a form field filled with spaces
/// is effectively empty and should fail the required check
/// just like a blank field would.
///
fn validate_required(value: String) -> Result(Nil, String) {
  case string.trim(value) {
    "" -> Error("is required")
    _ -> Ok(Nil)
  }
}

/// A lightweight check for @ and . catches the most common
/// typos without rejecting valid edge-case addresses that a
/// strict RFC parser might accept. Full email validation is
/// best done by sending a confirmation link anyway.
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

/// Enforces minimum length for fields like passwords or
/// usernames where very short values are meaningless. The
/// error message includes the threshold so users know exactly
/// how many characters are needed.
///
fn validate_min_length(value: String, min: Int) -> Result(Nil, String) {
  case string.length(value) >= min {
    True -> Ok(Nil)
    False ->
      Error("must be at least " <> int.to_string(min) <> " characters long")
  }
}

/// Catches overly long input before it hits the database,
/// where a column length violation would produce a cryptic
/// error. Validating early gives users a clear message about
/// the limit.
///
fn validate_max_length(value: String, max: Int) -> Result(Nil, String) {
  case string.length(value) <= max {
    True -> Ok(Nil)
    False ->
      Error("must be no more than " <> int.to_string(max) <> " characters long")
  }
}

/// Range checks on numeric fields like age or quantity prevent
/// nonsensical values from reaching business logic. Parsing
/// the string to int first means a non-numeric value gets a
/// clear "must be a number" error instead of a confusing
/// range message.
///
fn validate_min(value: String, min: Int) -> Result(Nil, String) {
  case int.parse(value) {
    Ok(n) if n >= min -> Ok(Nil)
    Ok(_) -> Error("must be at least " <> int.to_string(min))
    Error(_) -> Error("must be a valid number")
  }
}

/// Upper-bound checks prevent values that would overflow
/// database columns or break business rules like maximum
/// order quantities. Same parse-first strategy as validate_min
/// so the error message matches the actual problem.
///
fn validate_max(value: String, max: Int) -> Result(Nil, String) {
  case int.parse(value) {
    Ok(n) if n <= max -> Ok(Nil)
    Ok(_) -> Error("must be no more than " <> int.to_string(max))
    Error(_) -> Error("must be a valid number")
  }
}

/// Catches non-numeric input early so downstream code can
/// safely parse the value without a second round of error
/// handling. Useful for fields like zip codes that must be
/// digits but don't need range validation.
///
fn validate_numeric(value: String) -> Result(Nil, String) {
  case int.parse(value) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("must be a valid number")
  }
}

/// Requiring a scheme prefix catches common mistakes like
/// entering "example.com" without the protocol. A full URL
/// parser would be overkill since the value will be used in
/// links where the scheme is mandatory anyway.
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

/// Fixed-length numeric codes like PINs or verification codes
/// need exact digit counts. Using absolute_value before
/// counting ensures negative signs don't inflate the count
/// and give a false pass.
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

/// Variable-length numeric identifiers like phone numbers or
/// account codes need a minimum digit count to be meaningful.
/// Same absolute_value strategy as validate_digits to ignore
/// negative signs in the count.
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

/// Caps digit count for fields like zip codes or short numeric
/// identifiers where too many digits indicate bad input. Same
/// absolute_value strategy as the other digit validators to
/// handle negative signs consistently.
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

/// Delegates to the user-provided function without wrapping
/// the result — the custom validator already returns the same
/// Result(Nil, String) shape. This thin wrapper exists so
/// apply_rule can dispatch uniformly via pattern matching.
///
fn validate_custom(
  custom_validation: CustomValidation(ctx),
  value: String,
  ctx: ctx,
) -> Result(Nil, String) {
  custom_validation(value, ctx)
}

/// Mirrors apply_rule but for file uploads — pattern matching
/// on FileRule dispatches to the correct file validation
/// function. Keeping file and string dispatch separate avoids
/// a tangled match expression that mixes unrelated rule types.
///
fn apply_file_rule(
  file: Result(UploadedFile, Nil),
  ctx: ctx,
  rule: FileRule(ctx),
) -> Result(Nil, String) {
  case rule {
    FileRequired -> validate_file_required(file)
    FileMinSize(min) -> validate_file_min_size(file, min)
    FileMaxSize(max) -> validate_file_max_size(file, max)
    FileExtension(extensions) -> validate_file_extension(file, extensions)
    FileCustom(custom_validation) ->
      validate_file_custom(custom_validation, file, ctx)
  }
}

/// Browsers may submit an empty file input with a blank
/// filename rather than omitting the field entirely. Checking
/// both the Result and the trimmed filename catches both cases
/// so "required" means a real file was actually selected.
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

/// Minimum size catches corrupt or truncated uploads that are
/// technically present but too small to be valid — like a
/// 0-byte image. Returning Ok when no file is present lets
/// this rule compose with FileRequired independently.
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

/// Catching oversized files at validation time gives users a
/// clear error message before the file hits storage or
/// processing. Returning Ok when no file is present lets this
/// rule compose with FileRequired independently.
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

/// Restricting extensions prevents users from uploading
/// unexpected file types that the app can't process — like
/// a .exe on an image upload form. Lowercasing the extracted
/// extension ensures ".JPG" matches "jpg" in the allow list.
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

/// Delegates to the user-provided function only when a file
/// is present — skipping absent files lets custom rules focus
/// on validating content without null-checking. FileRequired
/// handles the "must be present" concern separately.
///
fn validate_file_custom(
  custom_validation: CustomFileValidation(ctx),
  file: Result(UploadedFile, Nil),
  ctx: ctx,
) -> Result(Nil, String) {
  case file {
    Error(_) -> Ok(Nil)
    Ok(uploaded_file) -> custom_validation(uploaded_file, ctx)
  }
}

/// HTML form field names use snake_case or kebab-case but error
/// messages should read naturally — "User name is required" not
/// "user_name is required". Normalizing here keeps individual
/// rule functions from needing to know about field name format.
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
