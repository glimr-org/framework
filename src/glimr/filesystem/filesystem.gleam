//// Filesystem
////
//// Console commands like make:controller and make:migration
//// need to scaffold files from templates, but the file I/O
//// boilerplate — read stub, create directories, replace
//// variables, write output — would be duplicated across every
//// command. This module centralizes that workflow so commands
//// just specify which stub to use, where to write it, and what
//// variables to substitute.
////

import gleam/dict.{type Dict}
import gleam/result
import gleam/string
import simplifile.{type FileError}
import wisp

// ------------------------------------------------------------- Public Functions

/// When you run `make:controller admin/users`, the target
/// directory `src/controllers/admin/` probably doesn't exist
/// yet. Nobody wants to manually mkdir before every scaffold
/// command, so this creates the full directory tree
/// automatically. It's a no-op if the path already exists, so
/// it's safe to call every time.
///
pub fn ensure_directory_exists(
  file_path: String,
) -> Result(Nil, simplifile.FileError) {
  case get_directory_path(file_path) {
    "" -> Ok(Nil)
    dir -> simplifile.create_directory_all(dir)
  }
}

/// Scaffold commands check this before writing so they can warn
/// developers instead of silently overwriting existing code.
/// Accidentally clobbering a controller someone has spent hours
/// customizing would be a terrible experience — better to ask
/// for confirmation first.
///
pub fn file_exists(path: String) -> Result(Bool, FileError) {
  simplifile.is_file(path)
}

/// Some commands need to verify that a project directory exists
/// before generating files into it — for example, checking that
/// src/controllers/ is there before creating a new controller.
/// Without this check, generated files could end up in
/// unexpected locations.
///
pub fn directory_exists(path: String) -> Result(Bool, FileError) {
  simplifile.is_directory(path)
}

/// Stubs live in each package's priv/stubs/ directory so they
/// ship with the compiled package and are available at runtime.
/// Using wisp.priv_directory means the path resolves correctly
/// regardless of how or where the package is installed — no
/// hardcoded paths that break when someone installs from Hex.
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

/// For static files like .gitkeep or boilerplate config that
/// don't need any variable substitution, this handles the
/// entire read-stub-create-dirs-write workflow in one call.
/// Most scaffold commands use the _with_variables variant
/// instead, but this keeps the simple cases simple.
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

/// The workhorse of file scaffolding — reads a stub template,
/// replaces `@{{ variable }}` markers with real values (module
/// name, table name, route path, etc.), and writes the result.
/// Labeled arguments are essential here because four string
/// parameters in a row would be a nightmare to get in the right
/// order.
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

/// The `@{{ }}` syntax was chosen to be distinct from both
/// Gleam's string interpolation and HTML template tags, so
/// stubs can contain real Gleam code without the variable
/// markers being misinterpreted. All four spacing variants
/// (with/without inner spaces) are handled so stub authors
/// don't need to worry about formatting consistency — and any
/// leftover placeholders get stripped so they never appear in
/// generated output.
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

/// After variable substitution, any `@{{ }}` markers still left
/// in the content are undefined variables — maybe the stub was
/// updated but the command wasn't. Stripping them prevents
/// generated files from having raw placeholder syntax that
/// would cause compile errors or confuse developers. The
/// recursion handles cases where multiple markers are adjacent
/// or scattered through the content.
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

/// This function receives a full file path like
/// `src/controllers/admin/users_controller.gleam` but
/// ensure_directory_exists only needs to create the parent
/// directories, not a directory named after the file itself.
/// The reverse-split-reverse trick extracts just the directory
/// portion without needing a path library. Returns empty string
/// for bare filenames, signaling that no directory creation is
/// needed.
///
fn get_directory_path(file_path: String) -> String {
  // Strip the filename by reversing, splitting at the first
  // "/", and reversing back
  case string.split_once(string.reverse(file_path), "/") {
    Ok(#(_, dir)) -> string.reverse(dir)
    Error(_) -> ""
  }
}
