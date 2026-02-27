//// Glimr Helper Functions
////
//// This module contains helper functions to get data specific
//// to the Glimr framework, like getting the glimr app's
//// version number for example.

// ------------------------------------------------------------- Public Functions

/// Get the current version number of the Glimr Framework. This
/// function is used internally to display the version number in
/// our console help message, but can of course be used to
/// display the current version wherever you need.
///
pub fn get_version() -> String {
  case get_glimr_version() {
    Ok(version) -> version
    Error(_) -> "unknown"
  }
}

// ------------------------------------------------------------- FFI Bindings

/// Uses erlang to get the current version number for the Glimr
/// Framework. This function is used to display the version
/// number in our console helper message for example.
///
@external(erlang, "glimr_ffi", "get_version")
fn get_glimr_version() -> Result(String, Nil)
