//// Filesystem
////
//// This module contains helper functions to read and handle
//// stub file data, create files from it, and also find
//// and replace variable data within files.

import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/string
import simplifile.{type FileError}
import wisp

// ------------------------------------------------------------- Public Functions

/// Helper function to Create all parent directories for a 
/// given file path if the directories provided do not 
/// already exist in the users application.
///
pub fn ensure_directory_exists(
  file_path: String,
) -> Result(Nil, simplifile.FileError) {
  case get_directory_path(file_path) {
    "" -> Ok(Nil)
    dir -> simplifile.create_directory_all(dir)
  }
}

/// Checks if a file exists at the given path. Returns Ok(True)
/// if the file exists, Ok(False) if it doesn't, or Error if
/// there was an issue checking the filesystem.
///
pub fn file_exists(path: String) -> Result(Bool, FileError) {
  simplifile.is_file(path)
}

/// Checks if a directory exists at the given path. Returns
/// Ok(True) if the directory exists, Ok(False) if it doesn't,
/// or Error if there was an issue checking the filesystem.
///
pub fn directory_exists(path: String) -> Result(Bool, FileError) {
  simplifile.is_directory(path)
}

// ------------------------------------------------------------- Internal Public Functions

/// Reads a stub template file from the framework's priv
/// directory. These stub files are normally used in
/// Glimr console commands like make:controller.
///
@internal
pub fn read_stub(stub_path: String) -> Result(String, Nil) {
  case wisp.priv_directory("glimr") {
    Ok(priv_dir) -> {
      let full_path = priv_dir <> "/stubs/" <> stub_path
      simplifile.read(full_path)
      |> result.replace_error(Nil)
    }
    Error(_) -> Error(Nil)
  }
}

/// Reads a stub file and writes it to the destination path.
/// This function will also creates parent directories if 
/// they do not already exist.
///
@internal
pub fn write_from_stub(stub_path: String, dest_path: String) -> Result(Nil, Nil) {
  case read_stub(stub_path) {
    Ok(content) -> {
      let _ = ensure_directory_exists(dest_path)
      simplifile.write(dest_path, content)
      |> result.replace_error(Nil)
    }
    Error(_) -> Error(Nil)
  }
}

/// Reads a stub file and writes it to the destination path.
/// This function will also creates parent directories if 
/// they do not already exist.
///
@internal
pub fn write_from_stub_with_variables(
  stub_path stub_path: String,
  dest_path dest_path: String,
  variables variables: List(#(String, String)),
) -> Result(Nil, Nil) {
  case read_stub(stub_path) {
    Ok(content) -> {
      let _ = ensure_directory_exists(dest_path)

      let content = replace_variables(dict.from_list(variables), content)

      simplifile.write(dest_path, content)
      |> result.replace_error(Nil)
    }
    Error(_) -> Error(Nil)
  }
}

/// Escapes HTML special characters to prevent XSS attacks.
/// Converts <, >, &, ", and ' to their HTML entity equivalents.
///
@internal
pub fn escape_html(value: String) -> String {
  value
  |> string.replace("&", "&amp;")
  |> string.replace("<", "&lt;")
  |> string.replace(">", "&gt;")
  |> string.replace("\"", "&quot;")
  |> string.replace("'", "&#x27;")
}

/// Escapes template syntax ({{ and }}) to prevent template injection
/// when content is inserted into layouts. This prevents content from
/// being processed multiple times.
///
@internal
pub fn escape_template_syntax(content: String) -> String {
  content
  |> string.replace("{{", "&#123;&#123;")
  |> string.replace("}}", "&#125;&#125;")
}

/// Replaces all {{ key }} patterns in the HTML with their values
/// from the data dictionary. Supports both {{key}} and {{ key }}
/// syntax (with or without spaces). All values are automatically
/// HTML-escaped to prevent XSS attacks. Strips any unused variables
/// that weren't provided.
///
@internal
pub fn replace_variables(data: Dict(String, String), content: String) -> String {
  let html =
    dict.fold(data, content, fn(acc, key, value) {
      let escaped_value = escape_html(value)
      acc
      |> string.replace("{{" <> key <> "}}", escaped_value)
      |> string.replace("{{ " <> key <> " }}", escaped_value)
      |> string.replace("{{ " <> key <> "}}", escaped_value)
      |> string.replace("{{" <> key <> " }}", escaped_value)
    })

  strip_unused_variables(html)
}

/// Recursively removes all {{variable}} patterns that weren't
/// replaced by template data. This prevents showing placeholder
/// text in the rendered output.
///
@internal
pub fn strip_unused_variables(content: String) -> String {
  case string.split_once(content, "{{") {
    Ok(#(before, after)) -> {
      case string.split_once(after, "}}") {
        Ok(#(_, rest)) -> before <> strip_unused_variables(rest)
        Error(_) -> content
      }
    }
    Error(_) -> content
  }
}

// ------------------------------------------------------------- Private Functions

/// Extracts the directory portion of a file path by splitting
/// on slashes, removing the last segment (filename), and
/// rejoining. Returns empty string if no directory component.
///
fn get_directory_path(file_path: String) -> String {
  case string.split(file_path, "/") {
    [] -> ""
    parts -> {
      parts
      |> list.reverse
      |> list.drop(1)
      |> list.reverse
      |> string.join("/")
    }
  }
}
