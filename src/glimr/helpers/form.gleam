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
import wisp

/// ------------------------------------------------------------
/// Get Form Field Value
/// ------------------------------------------------------------
///
/// Extracts a field value from submitted form data by name.
/// Returns the field value as a string, or an empty string
/// if the field doesn't exist. Use for optional form fields.
///
/// ------------------------------------------------------------
/// Example:
/// ```gleam
/// let email = form.get_field_value(form_data, "email")
/// ```
///
pub fn get_field_value(form_data: wisp.FormData, field_name: String) -> String {
  list.key_find(form_data.values, field_name) |> result.unwrap("")
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
/// Example:
/// ```gleam
/// case form.has_field(form_data, "email") {
///   True -> process_email(form_data)
///   False -> validation_error("Email is required")
/// }
/// ```
///
pub fn has_field(form_data: wisp.FormData, field_name: String) -> Bool {
  case list.key_find(form_data.values, field_name) {
    Ok(_) -> True
    Error(_) -> False
  }
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
/// Example:
/// ```gleam
/// case form.get_file(form_data, "avatar") {
///   Ok(file) -> save_upload(file)
///   Error(_) -> no_file_error()
/// }
/// ```
///
pub fn get_file(
  form_data: wisp.FormData,
  field_name: String,
) -> Result(wisp.UploadedFile, Nil) {
  list.key_find(form_data.files, field_name)
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
/// Example:
/// ```gleam
/// case form.has_file(form_data, "avatar") {
///   True -> process_upload(form_data)
///   False -> use_default_avatar()
/// }
/// ```
///
pub fn has_file(form_data: wisp.FormData, field_name: String) -> Bool {
  case get_file(form_data, field_name) {
    Ok(_) -> True
    Error(_) -> False
  }
}
