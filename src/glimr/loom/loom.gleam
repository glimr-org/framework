import gleam/list
import gleam/result
import gleam/string
import glimr/response/response
import simplifile

// ------------------------------------------------------------- Public Constants

/// Returns the base path for loom view files
pub const views_path = response.views_path

/// Returns the base path for compiled loom files
pub const output_path = "src/compiled/loom/"

/// Returns the base path for loom gleam files where gleam 
/// functionality is specified for a specific loom view
/// or loom component
///
pub const app_path = "src/app/loom/"

// ------------------------------------------------------------- Public Functions

/// Returns if the given path is a valid loom file. This is 
/// normally used to validate that it's valid before trying to
/// compile it into a gleam file.
///
pub fn is_views_path(path: String) -> Bool {
  string.starts_with(path, views_path)
}

/// Returns if the given path is a valid loom gleam file. This 
/// is normally used to validate that it's valid before trying 
/// to compile the equivalent loom html file into gleam code.
///
pub fn is_app_path(path: String) -> Bool {
  string.starts_with(path, app_path)
}

/// Finds all loom template files in a directory. Recursively
/// searches for files ending in .loom.html and returns their
/// full paths so that they can be compiled.
///
pub fn find_files() -> List(String) {
  simplifile.get_files(views_path)
  |> result.unwrap([])
  |> list.filter(string.ends_with(_, ".loom.html"))
}

/// Finds all loom component files in a directory. Recursively
/// searches for files ending in .loom.html that live in the 
/// components/ directory, and returns their full paths so 
/// that they can be compiled.
///
pub fn find_components(files: List(String)) -> List(String) {
  list.filter(files, string.contains(_, "components/"))
}

/// Finds all non loom component files in a directory. 
/// Recursively searches for files ending in .loom.html that do 
/// not live in the components/ directory, and returns their 
/// full paths so that they can be compiled.
///
pub fn find_non_components(files: List(String)) -> List(String) {
  use file <- list.filter(files)
  !string.contains(file, "components/")
}
