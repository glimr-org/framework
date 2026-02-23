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
import gleam/regexp
import gleam/result
import gleam/string
import glimr/forms/form
import glimr/response/redirect
import glimr/response/response.{type ResponseFormat, HTML, JSON}
import glimr/session/session.{type Session}
import simplifile
import wisp.{
  type FormData as WispFormData, type Request, type Response, type UploadedFile,
}

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
  Confirmed(String)
  Regex(String)
  RequiredIf(String, String)
  RequiredUnless(String, String)
  In(List(String))
  NotIn(List(String))
  Alpha
  AlphaNumeric
  StartsWith(String)
  EndsWith(String)
  Between(Int, Int)
  Date
  Uuid
  Ip
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

/// Wraps form access in getter functions so validation rules
/// and data extractors can read field values without coupling
/// to wisp's internal FormData type. Built once from the raw
/// form data and passed through the entire validation chain,
/// then reused by the data extractor after validation passes.
///
pub type FormData {
  FormData(
    get: fn(String) -> String,
    get_file: fn(String) -> UploadedFile,
    get_file_result: fn(String) -> Result(UploadedFile, Nil),
  )
}

// ------------------------------------------------------------- Private Types

/// A type alias keeps the Custom variant readable — without it
/// the full fn signature would clutter the StringRule enum.
/// Accepting FormData lets custom rules access other fields
/// for cross-field validation (e.g., comparing two dates),
/// and ctx lets them do database lookups like uniqueness
/// checks without hardcoding a connection type into the
/// validator module.
///
type CustomValidation(ctx) =
  fn(String, String, FormData, ctx) -> Result(Nil, String)

/// Mirrors CustomValidation but for file uploads — the input
/// is an UploadedFile instead of a String because file rules
/// need access to path, size, and filename metadata that a
/// string value can't represent.
///
type CustomFileValidation(ctx) =
  fn(String, UploadedFile, FormData, ctx) -> Result(Nil, String)

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
/// fn data(data: FormData) -> Data {
///   Data(
///     name: data.get("name"),
///     email: data.get("email"),
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
  data_fn: fn(FormData) -> typed_form,
  on_valid: fn(typed_form) -> Response,
) -> Response {
  use wisp_form_data <- wisp.require_form(req)

  let data = form_data(wisp_form_data)

  case start(rules(), data, ctx) {
    Ok(_) -> {
      on_valid(data_fn(data))
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
  data: FormData,
  ctx: ctx,
) -> Result(Nil, List(ValidationError)) {
  let errors =
    pending
    |> list.filter_map(fn(p) {
      case execute(p, data, ctx) {
        Ok(_) -> Error(Nil)
        Error(err) -> Ok(err)
      }
    })

  response(errors)
}

/// Bridges wisp's raw form data into the FormData abstraction
/// so callers using start() directly don't need to build the
/// getter closures themselves. Marked @internal because app
/// code should go through run() — this exists for tests and
/// custom validation flows within the framework.
///
@internal
pub fn form_data(wisp_form_data: WispFormData) -> FormData {
  FormData(
    get: fn(field) { wisp_form_data |> form.get(field) },
    get_file: fn(field) { wisp_form_data |> form.get_file(field) },
    get_file_result: fn(field) { wisp_form_data |> form.get_file_result(field) },
  )
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
/// the value and data, not the full form.
///
fn execute(
  pending: Rule(ctx),
  data: FormData,
  ctx: ctx,
) -> Result(Nil, ValidationError) {
  case pending {
    FieldRule(field_name:, rules:) -> {
      let value = data.get(field_name)
      let messages =
        rules
        |> list.filter_map(fn(rule) {
          case apply_rule(field_name, value, data, ctx, rule) {
            Ok(_) -> Error(Nil)
            Error(message) -> Ok(message)
          }
        })

      case messages {
        [] -> Ok(Nil)
        msgs -> Error(ValidationError(name: field_name, messages: msgs))
      }
    }
    FileFieldRule(field_name:, rules:) -> {
      let file = data.get_file_result(field_name)
      let messages =
        rules
        |> list.filter_map(fn(rule) {
          case apply_file_rule(field_name, file, data, ctx, rule) {
            Ok(_) -> Error(Nil)
            Error(message) -> Ok(message)
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
  field: String,
  value: String,
  data: FormData,
  ctx: ctx,
  rule: StringRule(ctx),
) -> Result(Nil, String) {
  case rule {
    Required -> validate_required(field, value)
    Email -> validate_email(field, value)
    MinLength(min) -> validate_min_length(field, value, min)
    MaxLength(max) -> validate_max_length(field, value, max)
    Min(min) -> validate_min(field, value, min)
    Max(max) -> validate_max(field, value, max)
    Numeric -> validate_numeric(field, value)
    Url -> validate_url(field, value)
    Digits(count) -> validate_digits(field, value, count)
    MinDigits(min) -> validate_min_digits(field, value, min)
    MaxDigits(max) -> validate_max_digits(field, value, max)
    Confirmed(confirmation_field) ->
      validate_confirmed(field, value, data, confirmation_field)
    Regex(pattern) -> validate_regex(field, value, pattern)
    RequiredIf(other_field, other_value) ->
      validate_required_if(field, value, data, other_field, other_value)
    RequiredUnless(other_field, other_value) ->
      validate_required_unless(field, value, data, other_field, other_value)
    In(allowed) -> validate_in(field, value, allowed)
    NotIn(disallowed) -> validate_not_in(field, value, disallowed)
    Alpha -> validate_alpha(field, value)
    AlphaNumeric -> validate_alpha_numeric(field, value)
    StartsWith(prefix) -> validate_starts_with(field, value, prefix)
    EndsWith(suffix) -> validate_ends_with(field, value, suffix)
    Between(min, max) -> validate_between(field, value, min, max)
    Date -> validate_date(field, value)
    Uuid -> validate_uuid(field, value)
    Ip -> validate_ip(field, value)
    Custom(custom_validation) ->
      validate_custom(custom_validation, field, value, data, ctx)
  }
}

/// Trimming before checking prevents whitespace-only values
/// from passing as "present" — a form field filled with spaces
/// is effectively empty and should fail the required check
/// just like a blank field would.
///
fn validate_required(field: String, value: String) -> Result(Nil, String) {
  case string.trim(value) {
    "" -> Error(field <> " is required")
    _ -> Ok(Nil)
  }
}

/// Validates email format using a regex that checks for a
/// non-empty local part, an @ symbol, a non-empty domain,
/// and at least one dot in the domain. Full email validation
/// is best done by sending a confirmation link anyway.
///
fn validate_email(field: String, value: String) -> Result(Nil, String) {
  case value {
    "" -> Ok(Nil)
    _ -> {
      let assert Ok(re) = regexp.from_string("^[^\\s@]+@[^\\s@]+\\.[^\\s@]+$")

      case regexp.check(re, string.trim(value)) {
        True -> Ok(Nil)
        False -> Error(field <> " must be a valid email address")
      }
    }
  }
}

/// Enforces minimum length for fields like passwords or
/// usernames where very short values are meaningless. The
/// error message includes the threshold so users know exactly
/// how many characters are needed.
///
fn validate_min_length(
  field: String,
  value: String,
  min: Int,
) -> Result(Nil, String) {
  case string.length(value) >= min {
    True -> Ok(Nil)
    False ->
      Error(
        field
        <> " must be at least "
        <> int.to_string(min)
        <> " characters long",
      )
  }
}

/// Catches overly long input before it hits the database,
/// where a column length violation would produce a cryptic
/// error. Validating early gives users a clear message about
/// the limit.
///
fn validate_max_length(
  field: String,
  value: String,
  max: Int,
) -> Result(Nil, String) {
  case string.length(value) <= max {
    True -> Ok(Nil)
    False ->
      Error(
        field
        <> " must be no more than "
        <> int.to_string(max)
        <> " characters long",
      )
  }
}

/// Range checks on numeric fields like age or quantity prevent
/// nonsensical values from reaching business logic. Parsing
/// the string to int first means a non-numeric value gets a
/// clear "must be a number" error instead of a confusing
/// range message.
///
fn validate_min(field: String, value: String, min: Int) -> Result(Nil, String) {
  case int.parse(value) {
    Ok(n) if n >= min -> Ok(Nil)
    Ok(_) -> Error(field <> " must be at least " <> int.to_string(min))
    Error(_) -> Error(field <> " must be a valid number")
  }
}

/// Upper-bound checks prevent values that would overflow
/// database columns or break business rules like maximum
/// order quantities. Same parse-first strategy as validate_min
/// so the error message matches the actual problem.
///
fn validate_max(field: String, value: String, max: Int) -> Result(Nil, String) {
  case int.parse(value) {
    Ok(n) if n <= max -> Ok(Nil)
    Ok(_) -> Error(field <> " must be no more than " <> int.to_string(max))
    Error(_) -> Error(field <> " must be a valid number")
  }
}

/// Catches non-numeric input early so downstream code can
/// safely parse the value without a second round of error
/// handling. Uses a regex to accept integers and negative
/// numbers while rejecting everything else.
///
fn validate_numeric(field: String, value: String) -> Result(Nil, String) {
  case value {
    "" -> Ok(Nil)
    _ -> {
      let assert Ok(re) = regexp.from_string("^-?\\d+$")

      case regexp.check(re, string.trim(value)) {
        True -> Ok(Nil)
        False -> Error(field <> " must be a valid number")
      }
    }
  }
}

/// Validates URL format using a regex that requires an http
/// or https scheme followed by a domain with at least one
/// dot. Catches missing schemes and bare domain entries
/// without a full URL parser.
///
fn validate_url(field: String, value: String) -> Result(Nil, String) {
  case value {
    "" -> Ok(Nil)
    _ -> {
      let assert Ok(re) = regexp.from_string("^https?://[^\\s/$.?#].[^\\s]*$")

      case regexp.check(re, string.trim(value)) {
        True -> Ok(Nil)
        False -> Error(field <> " must be a valid URL")
      }
    }
  }
}

/// Fixed-length numeric codes like PINs or verification codes
/// need exact digit counts. Uses a regex to match exactly N
/// digits, correctly preserving leading zeros that int.parse
/// would strip.
///
fn validate_digits(
  field: String,
  value: String,
  count: Int,
) -> Result(Nil, String) {
  let assert Ok(re) = {
    regexp.from_string("^\\d{" <> int.to_string(count) <> "}$")
  }

  case regexp.check(re, string.trim(value)) {
    True -> Ok(Nil)
    False ->
      Error(field <> " must have exactly " <> int.to_string(count) <> " digits")
  }
}

/// Variable-length numeric identifiers like phone numbers or
/// account codes need a minimum digit count to be meaningful.
/// Uses a regex to require at least N digits, correctly
/// preserving leading zeros.
///
fn validate_min_digits(
  field: String,
  value: String,
  min: Int,
) -> Result(Nil, String) {
  let assert Ok(re) = regexp.from_string("^\\d{" <> int.to_string(min) <> ",}$")

  case regexp.check(re, string.trim(value)) {
    True -> Ok(Nil)
    False ->
      Error(field <> " must have at least " <> int.to_string(min) <> " digits")
  }
}

/// Caps digit count for fields like zip codes or short numeric
/// identifiers where too many digits indicate bad input. Uses
/// a regex to allow 1 to N digits, correctly preserving
/// leading zeros.
///
fn validate_max_digits(
  field: String,
  value: String,
  max: Int,
) -> Result(Nil, String) {
  let assert Ok(re) = {
    regexp.from_string("^\\d{1," <> int.to_string(max) <> "}$")
  }

  case regexp.check(re, string.trim(value)) {
    True -> Ok(Nil)
    False ->
      Error(
        field <> " must have no more than " <> int.to_string(max) <> " digits",
      )
  }
}

/// Password and email confirmation fields need to match their
/// counterpart before submission is accepted. Accessing the
/// other field through FormData keeps this cross-field check
/// declarative rather than requiring custom validation logic.
///
fn validate_confirmed(
  field: String,
  value: String,
  data: FormData,
  confirmation_field: String,
) -> Result(Nil, String) {
  case value {
    "" -> Ok(Nil)
    _ ->
      case data.get(confirmation_field) == value {
        True -> Ok(Nil)
        False -> Error(field <> " does not match " <> confirmation_field)
      }
  }
}

/// Covers format constraints that don't warrant a dedicated
/// rule variant — like postal codes or product SKUs with
/// project-specific patterns. Skips empty values so Required
/// handles presence independently.
///
fn validate_regex(
  field: String,
  value: String,
  pattern: String,
) -> Result(Nil, String) {
  case value {
    "" -> Ok(Nil)
    _ -> {
      let assert Ok(re) = regexp.from_string(pattern)
      case regexp.check(re, value) {
        True -> Ok(Nil)
        False -> Error(field <> " format is invalid")
      }
    }
  }
}

/// Forms with conditional sections need fields that are only
/// required in certain states — like "company_name" when the
/// account type is "business". Without this, the logic would
/// need a Custom rule or pre-processing in the controller.
///
fn validate_required_if(
  field: String,
  value: String,
  data: FormData,
  other_field: String,
  other_value: String,
) -> Result(Nil, String) {
  case data.get(other_field) == other_value {
    True ->
      case string.trim(value) {
        "" ->
          Error(
            field
            <> " is required when "
            <> other_field
            <> " is "
            <> other_value,
          )
        _ -> Ok(Nil)
      }
    False -> Ok(Nil)
  }
}

/// The inverse of RequiredIf — the field is required by
/// default but becomes optional when a specific condition is
/// met. Useful for forms where selecting one option makes
/// another field irrelevant.
///
fn validate_required_unless(
  field: String,
  value: String,
  data: FormData,
  other_field: String,
  other_value: String,
) -> Result(Nil, String) {
  case data.get(other_field) == other_value {
    True -> Ok(Nil)
    False ->
      case string.trim(value) {
        "" ->
          Error(
            field
            <> " is required unless "
            <> other_field
            <> " is "
            <> other_value,
          )
        _ -> Ok(Nil)
      }
  }
}

/// Select fields and radio buttons should only accept values
/// from their option list — accepting arbitrary input would
/// bypass the intended choices and could cause downstream
/// errors when the value doesn't match any enum variant.
///
fn validate_in(
  field: String,
  value: String,
  allowed: List(String),
) -> Result(Nil, String) {
  case value {
    "" -> Ok(Nil)
    _ ->
      case list.contains(allowed, value) {
        True -> Ok(Nil)
        False -> {
          let options = string.join(allowed, ", ")
          Error(field <> " must be one of: " <> options)
        }
      }
  }
}

/// Some fields accept free-form input but need to block a
/// small set of reserved or offensive values — like usernames
/// that conflict with URL routes. Easier than enumerating
/// every valid option with In.
///
fn validate_not_in(
  field: String,
  value: String,
  disallowed: List(String),
) -> Result(Nil, String) {
  case value {
    "" -> Ok(Nil)
    _ ->
      case list.contains(disallowed, value) {
        True ->
          Error(
            field <> " must not be one of: " <> string.join(disallowed, ", "),
          )
        False -> Ok(Nil)
      }
  }
}

/// Fields like country codes or currency identifiers must be
/// purely alphabetic — digits or special characters would be
/// invalid. A dedicated rule is clearer than asking users to
/// supply a regex pattern for this common constraint.
///
fn validate_alpha(field: String, value: String) -> Result(Nil, String) {
  case value {
    "" -> Ok(Nil)
    _ -> {
      let assert Ok(re) = regexp.from_string("^[a-zA-Z]+$")
      case regexp.check(re, value) {
        True -> Ok(Nil)
        False -> Error(field <> " must contain only letters")
      }
    }
  }
}

/// Usernames and slugs often need to be alphanumeric to avoid
/// issues with URL routing, database queries, or display
/// formatting. A dedicated rule avoids repeating the same
/// regex pattern across multiple validators.
///
fn validate_alpha_numeric(field: String, value: String) -> Result(Nil, String) {
  case value {
    "" -> Ok(Nil)
    _ -> {
      let assert Ok(re) = regexp.from_string("^[a-zA-Z0-9]+$")
      case regexp.check(re, value) {
        True -> Ok(Nil)
        False -> Error(field <> " must contain only letters and numbers")
      }
    }
  }
}

/// Fields like phone numbers with country codes or URLs with
/// required schemes need a specific prefix to be valid.
/// Skips empty values so Required handles presence
/// independently.
///
fn validate_starts_with(
  field: String,
  value: String,
  prefix: String,
) -> Result(Nil, String) {
  case value {
    "" -> Ok(Nil)
    _ ->
      case string.starts_with(value, prefix) {
        True -> Ok(Nil)
        False -> Error(field <> " must start with " <> prefix)
      }
  }
}

/// Fields like email domains or file paths that must end with
/// a specific suffix — for example requiring "@company.com"
/// for corporate email fields. Skips empty values so Required
/// handles presence independently.
///
fn validate_ends_with(
  field: String,
  value: String,
  suffix: String,
) -> Result(Nil, String) {
  case value {
    "" -> Ok(Nil)
    _ ->
      case string.ends_with(value, suffix) {
        True -> Ok(Nil)
        False -> Error(field <> " must end with " <> suffix)
      }
  }
}

/// Combines Min and Max into a single rule for fields where
/// both bounds are always specified together — like rating
/// scores or age ranges. Avoids cluttering the rule list with
/// two separate entries for one logical constraint.
///
fn validate_between(
  field: String,
  value: String,
  min: Int,
  max: Int,
) -> Result(Nil, String) {
  case value {
    "" -> Ok(Nil)
    _ ->
      case int.parse(value) {
        Ok(n) if n >= min && n <= max -> Ok(Nil)
        Ok(_) ->
          Error(
            field
            <> " must be between "
            <> int.to_string(min)
            <> " and "
            <> int.to_string(max),
          )
        Error(_) -> Error(field <> " must be a valid number")
      }
  }
}

/// Date inputs arrive as strings that need structural and
/// semantic validation — the regex catches malformed formats
/// while the range check catches impossible dates like month
/// 13. Both checks are needed because the regex alone would
/// accept "2024-99-99".
///
fn validate_date(field: String, value: String) -> Result(Nil, String) {
  case value {
    "" -> Ok(Nil)
    _ -> {
      let assert Ok(re) = regexp.from_string("^\\d{4}-\\d{2}-\\d{2}$")
      case regexp.check(re, value) {
        False -> Error(field <> " must be a valid date (YYYY-MM-DD)")
        True -> {
          let parts = string.split(value, "-")
          case parts {
            [_, m, d] -> {
              let month = result.unwrap(int.parse(m), 0)
              let day = result.unwrap(int.parse(d), 0)
              case month >= 1 && month <= 12 && day >= 1 && day <= 31 {
                True -> Ok(Nil)
                False -> Error(field <> " must be a valid date (YYYY-MM-DD)")
              }
            }
            _ -> Error(field <> " must be a valid date (YYYY-MM-DD)")
          }
        }
      }
    }
  }
}

/// API endpoints that accept resource IDs should reject
/// malformed UUIDs at the validation layer rather than letting
/// them reach the database where the error message would be
/// less helpful. Supports versions 1-5 per RFC 4122.
///
fn validate_uuid(field: String, value: String) -> Result(Nil, String) {
  case value {
    "" -> Ok(Nil)
    _ -> {
      let assert Ok(re) =
        regexp.from_string(
          "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[1-5][0-9a-fA-F]{3}-[89abAB][0-9a-fA-F]{3}-[0-9a-fA-F]{12}$",
        )
      case regexp.check(re, value) {
        True -> Ok(Nil)
        False -> Error(field <> " must be a valid UUID")
      }
    }
  }
}

/// Network configuration forms need to reject malformed IP
/// addresses before they reach system calls that would fail
/// with cryptic errors. Supporting both v4 and v6 avoids
/// needing separate rules for each protocol version.
///
fn validate_ip(field: String, value: String) -> Result(Nil, String) {
  case value {
    "" -> Ok(Nil)
    _ -> {
      let assert Ok(ipv4_re) =
        regexp.from_string(
          "^(25[0-5]|2[0-4]\\d|[01]?\\d\\d?)\\.(25[0-5]|2[0-4]\\d|[01]?\\d\\d?)\\.(25[0-5]|2[0-4]\\d|[01]?\\d\\d?)\\.(25[0-5]|2[0-4]\\d|[01]?\\d\\d?)$",
        )
      let assert Ok(ipv6_re) =
        regexp.from_string("^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$")
      case regexp.check(ipv4_re, value) || regexp.check(ipv6_re, value) {
        True -> Ok(Nil)
        False -> Error(field <> " must be a valid IP address")
      }
    }
  }
}

/// Delegates to the user-provided function, passing the
/// current field value, form data for cross-field access,
/// and the app context. This thin wrapper exists so
/// apply_rule can dispatch uniformly via pattern matching.
///
fn validate_custom(
  custom_validation: CustomValidation(ctx),
  field: String,
  value: String,
  data: FormData,
  ctx: ctx,
) -> Result(Nil, String) {
  custom_validation(field, value, data, ctx)
}

/// Mirrors apply_rule but for file uploads — pattern matching
/// on FileRule dispatches to the correct file validation
/// function. Keeping file and string dispatch separate avoids
/// a tangled match expression that mixes unrelated rule types.
///
fn apply_file_rule(
  field: String,
  file: Result(UploadedFile, Nil),
  data: FormData,
  ctx: ctx,
  rule: FileRule(ctx),
) -> Result(Nil, String) {
  case rule {
    FileRequired -> validate_file_required(field, file)
    FileMinSize(min) -> validate_file_min_size(field, file, min)
    FileMaxSize(max) -> validate_file_max_size(field, file, max)
    FileExtension(extensions) ->
      validate_file_extension(field, file, extensions)
    FileCustom(custom_validation) ->
      validate_file_custom(custom_validation, field, file, data, ctx)
  }
}

/// Browsers may submit an empty file input with a blank
/// filename rather than omitting the field entirely. Checking
/// both the Result and the trimmed filename catches both cases
/// so "required" means a real file was actually selected.
///
fn validate_file_required(
  field: String,
  file: Result(UploadedFile, Nil),
) -> Result(Nil, String) {
  case file {
    Ok(uploaded_file) -> {
      case string.trim(uploaded_file.file_name) {
        "" -> Error(field <> " is required")
        _ -> Ok(Nil)
      }
    }
    Error(_) -> Error(field <> " is required")
  }
}

/// Minimum size catches corrupt or truncated uploads that are
/// technically present but too small to be valid — like a
/// 0-byte image. Returning Ok when no file is present lets
/// this rule compose with FileRequired independently.
///
fn validate_file_min_size(
  field: String,
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
                field
                <> " must be at least "
                <> int.to_string(min_kb)
                <> " KB in size",
              )
          }
        }
        Error(_) -> Error(field <> " could not read file information")
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
  field: String,
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
                field
                <> " must be no more than "
                <> int.to_string(max_kb)
                <> " KB in size",
              )
          }
        }
        Error(_) -> Error(field <> " could not read file information")
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
  field: String,
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
          Error(
            field <> " must have one of the following extensions: " <> allowed,
          )
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
  field: String,
  file: Result(UploadedFile, Nil),
  data: FormData,
  ctx: ctx,
) -> Result(Nil, String) {
  case file {
    Error(_) -> Ok(Nil)
    Ok(uploaded_file) -> custom_validation(field, uploaded_file, data, ctx)
  }
}
