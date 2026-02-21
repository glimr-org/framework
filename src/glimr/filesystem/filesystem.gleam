//// Filesystem
////
//// Console commands like make:controller and make:migration
//// need to scaffold files from templates, but the file I/O
//// boilerplate (read stub, create directories, replace
//// variables, write output) would be duplicated across every
//// command. This module centralizes that workflow so commands
//// just specify which stub to use, where to write it, and
//// what variables to substitute.
////

import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/string
import simplifile.{type FileError}
import wisp

// ------------------------------------------------------------- Public Functions

/// Scaffold commands write files into directories that may not
/// exist yet (e.g., src/controllers/admin/). Creating parents
/// automatically avoids forcing users to mkdir before every
/// make: command, and simplifile.create_directory_all is a
/// no-op when the directory already exists.
///
pub fn ensure_directory_exists(
  file_path: String,
) -> Result(Nil, simplifile.FileError) {
  case get_directory_path(file_path) {
    "" -> Ok(Nil)
    dir -> simplifile.create_directory_all(dir)
  }
}

/// Wraps simplifile.is_file so callers don't need to import
/// simplifile directly. Commands use this to warn before
/// overwriting an existing file — scaffolding should never
/// silently clobber user code.
///
pub fn file_exists(path: String) -> Result(Bool, FileError) {
  simplifile.is_file(path)
}

/// Wraps simplifile.is_directory so callers don't need to
/// import simplifile directly. Used by commands that need to
/// verify a project structure exists before generating files
/// into it.
///
pub fn directory_exists(path: String) -> Result(Bool, FileError) {
  simplifile.is_directory(path)
}

/// Stubs live in each package's priv/stubs/ directory so they
/// ship with the compiled package and are available at runtime
/// without hardcoding file paths relative to the source tree.
/// wisp.priv_directory resolves the correct path regardless of
/// how or where the package is installed.
///
pub fn read_stub(package: String, stub_path: String) -> Result(String, Nil) {
  case wisp.priv_directory(package) {
    Ok(priv_dir) -> {
      let full_path = priv_dir <> "/stubs/" <> stub_path
      simplifile.read(full_path)
      |> result.replace_error(Nil)
    }
    Error(_) -> Error(Nil)
  }
}

/// Combines read_stub, ensure_directory_exists, and write into
/// a single call for the common case where the stub content is
/// used as-is without variable substitution. Commands that
/// generate static files (like .gitkeep or config templates)
/// use this instead of the _with_variables variant.
///
pub fn write_from_stub(
  package: String,
  stub_path: String,
  dest_path: String,
) -> Result(Nil, Nil) {
  case read_stub(package, stub_path) {
    Ok(content) -> {
      let _ = ensure_directory_exists(dest_path)
      simplifile.write(dest_path, content)
      |> result.replace_error(Nil)
    }
    Error(_) -> Error(Nil)
  }
}

/// Most scaffold commands need to inject a module name, table
/// name, or route path into the stub template before writing.
/// Labeled arguments make the call site self-documenting and
/// hard to get wrong — without labels, four string arguments
/// in a row would be easy to accidentally swap.
///
pub fn write_from_stub_with_variables(
  package package: String,
  stub_path stub_path: String,
  dest_path dest_path: String,
  variables variables: List(#(String, String)),
) -> Result(Nil, Nil) {
  case read_stub(package, stub_path) {
    Ok(content) -> {
      let _ = ensure_directory_exists(dest_path)

      let content = replace_variables(dict.from_list(variables), content)

      simplifile.write(dest_path, content)
      |> result.replace_error(Nil)
    }
    Error(_) -> Error(Nil)
  }
}

// ------------------------------------------------------------- Internal Public Functions

/// The @{{ }} syntax is distinct from Gleam's string
/// interpolation and HTML template tags, so stubs can contain
/// real Gleam code without the variable markers being
/// misinterpreted. Handling all four spacing variants
/// (with/without inner spaces) means stub authors don't need
/// to worry about formatting consistency. Unused variables are
/// stripped so leftover placeholders never appear in output.
///
@internal
pub fn replace_variables(data: Dict(String, String), content: String) -> String {
  let html =
    dict.fold(data, content, fn(acc, key, value) {
      acc
      |> string.replace("@{{" <> key <> "}}", value)
      |> string.replace("@{{ " <> key <> " }}", value)
      |> string.replace("@{{ " <> key <> "}}", value)
      |> string.replace("@{{" <> key <> " }}", value)
    })

  strip_unused_variables(html)
}

/// After variable substitution, any @{{ }} markers left in the
/// content are undefined variables. Stripping them recursively
/// ensures generated files never contain raw placeholder syntax
/// that would cause compile errors or confuse users. Recursion
/// handles nested or adjacent markers in a single pass.
///
@internal
pub fn strip_unused_variables(content: String) -> String {
  case string.split_once(content, "@{{") {
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

/// ensure_directory_exists receives a full file path but needs
/// to create only the parent directories, not a directory named
/// after the file. Splitting off the last segment extracts the
/// directory portion without depending on a path manipulation
/// library. Empty string for bare filenames signals that no
/// directory creation is needed.
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
