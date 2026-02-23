//// Form Helpers
////
//// Wisp's FormData stores values and files as key-value pair
//// lists, but list.key_find returns a Result that controllers
//// would need to unwrap on every field access — noisy and
//// repetitive when a form has many fields. These helpers
//// provide ergonomic accessors that match common use cases:
//// default to empty string for optional fields, Bool checks
//// for conditional logic, and panic vs Result variants for
//// file uploads depending on whether absence is a bug or an
//// expected case.
////

import gleam/list
import gleam/result
import wisp.{type FormData, type UploadedFile}

// ------------------------------------------------------------- Public Functions

/// Defaulting to an empty string on missing fields avoids
/// forcing callers to handle a Result for every optional
/// input. Most form fields like name or bio are safe to treat
/// as empty when absent, so the common case stays clean.
///
/// *Example:*
/// 
/// ```gleam
/// let email = form |> form.get("email")
/// ```
///
pub fn get(form: FormData, field: String) -> String {
  list.key_find(form.values, field) |> result.unwrap("")
}

/// Sometimes controllers need to know if a field was submitted
/// at all — checkboxes and optional sections omit the field
/// entirely rather than sending an empty value. A Bool check
/// reads more clearly than matching on a Result in guards.
///
/// *Example:*
/// 
/// ```gleam
/// case form |> form.has("email") {
///   True -> process_email(form_data)
///   False -> panic as "I'm literally panicking rn"
/// }
/// ```
///
pub fn has(form: FormData, field: String) -> Bool {
  case list.key_find(form.values, field) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// When the form markup guarantees a file input exists, a
/// missing file means the request is malformed and continuing
/// would be a bug. The assert panic surfaces this immediately
/// rather than propagating a silent default downstream.
///
/// *Example:*
/// 
/// ```gleam
/// form.get_file("avatar")
/// ```
///
pub fn get_file(form: FormData, field: String) -> UploadedFile {
  let assert Ok(file) = list.key_find(form.files, field)
  file
}

/// Unlike get_file, this returns a Result so callers can handle
/// missing uploads gracefully — useful for forms where the file
/// field is optional, like an avatar that defaults to a
/// placeholder when not provided.
///
/// *Example:*
/// 
/// ```gleam
/// case form |> form.get_file("avatar") {
///   Ok(file) -> save_upload(file)
///   Error(_) -> panic as "I'm literally panicking rn"
/// }
/// ```
///
pub fn get_file_result(
  form: FormData,
  field: String,
) -> Result(UploadedFile, Nil) {
  list.key_find(form.files, field)
}

/// A Bool check for file presence is cleaner than matching on
/// a Result when the controller just needs to branch — for
/// example, deciding whether to update an avatar or keep the
/// existing one without unwrapping the file itself.
///
/// *Example:*
/// 
/// ```gleam
/// case form_data |> form.has_file("avatar") {
///   True -> process_upload(form_data)
///   False -> use_default_avatar()
/// }
/// ```
///
pub fn has_file(form: FormData, field: String) -> Bool {
  case get_file_result(form, field) {
    Ok(_) -> True
    Error(_) -> False
  }
}
