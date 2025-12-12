//// ------------------------------------------------------------
//// Form Helpers
//// ------------------------------------------------------------
////
//// Utilities for working with form data submissions including
//// field value extraction, file upload handling, and validation
//// helpers for HTML form processing.
////

import gleam/list
import gleam/result
import wisp.{type FormData, type UploadedFile}

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Get Form Field Value
/// ------------------------------------------------------------
///
/// Extracts a field value from submitted form data by name.
/// Returns the field value as a string, or an empty string
/// if the field doesn't exist. Use for optional form fields.
///
/// ------------------------------------------------------------
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

/// ------------------------------------------------------------
/// Check if Form Has Field
/// ------------------------------------------------------------
///
/// Checks whether a field exists in the submitted form data.
/// Returns True if the field is present, False otherwise.
/// Use to validate required fields before processing.
///
/// ------------------------------------------------------------
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

/// ------------------------------------------------------------
/// Get Uploaded File or Fail
/// ------------------------------------------------------------
///
/// Extracts an uploaded file from the form data by field name.
/// Returns UploadedFile if found, or panics if not found
///
/// ------------------------------------------------------------
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

/// ------------------------------------------------------------
/// Get Uploaded File
/// ------------------------------------------------------------
///
/// Extracts an uploaded file from the form data by field name.
/// Returns Ok(UploadedFile) if found, or Error(Nil) if the
/// file field doesn't exist. Use for file upload processing.
///
/// ------------------------------------------------------------
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

/// ------------------------------------------------------------
/// Check if Form Has File
/// ------------------------------------------------------------
///
/// Checks whether a file upload field exists in the submitted
/// form data. Returns True if present, False otherwise. Use
/// to validate file uploads before processing.
///
/// ------------------------------------------------------------
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
