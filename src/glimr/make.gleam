//// ------------------------------------------------------------
//// Make Commands
//// ------------------------------------------------------------
////
//// Laravel-style CLI commands for generating boilerplate code.
//// Provides commands to scaffold controllers, middleware, form
//// requests, and validation rules. Automatically creates nested
//// directories if they don't exist.
////

import gleam/erlang/charlist
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile
import wisp

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Main Entry Point
/// ------------------------------------------------------------
///
/// The main entry point for the Glimr make command system.
/// Parses command-line arguments and delegates to the
/// appropriate generator function. Use via the `./glimr` 
/// wrapper script for a Laravel-style CLI experience.
///
pub fn main() {
  let args = get_args()

  case args {
    ["controller", name] -> make_controller(name, resource: False)
    ["controller", name, "--resource"] -> make_controller(name, resource: True)
    ["middleware", name] -> make_middleware(name)
    ["request", name] -> make_request(name)
    ["rule", name] -> make_rule(name)
    _ -> {
      io.println("Glimr Make Commands:")
      io.println("")
      io.println(
        "  ./glimr make:controller <name>              Create a new controller",
      )
      io.println(
        "  ./glimr make:controller <name> --resource   Create a resource controller",
      )
      io.println(
        "  ./glimr make:middleware <name>              Create a new middleware",
      )
      io.println(
        "  ./glimr make:request <name>                 Create a form request",
      )
      io.println(
        "  ./glimr make:rule <name>                    Create a validation rule",
      )
      io.println("")
      io.println("Examples:")
      io.println("  ./glimr make:controller user")
      io.println("  ./glimr make:controller post --resource")
      io.println("  ./glimr make:middleware auth")
      io.println("  ./glimr make:request login")
      io.println("  ./glimr make:rule username_available")
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// ------------------------------------------------------------
/// Make Controller
/// ------------------------------------------------------------
///
/// Creates a controller file from a stub template. Supports
/// both basic controllers (index method only) and resource
/// controllers (full CRUD: index, show, store, edit, update,
/// delete). Automatically creates nested directories if they
/// don't exist.
///
fn make_controller(name: String, resource resource: Bool) {
  let module_name = to_snake_case(name) <> "_controller"
  let file_path = "src/app/http/controllers/" <> module_name <> ".gleam"

  let stub_name = case resource {
    True -> "controller-resource.stub"
    False -> "controller.stub"
  }

  case read_stub("http/" <> stub_name) {
    Ok(content) -> {
      let _ = ensure_directory_exists(file_path)
      case simplifile.write(file_path, content) {
        Ok(_) -> {
          io.println("Controller created successfully!")
          io.println("  " <> file_path)
        }
        Error(_) -> io.println("Error: Could not write file " <> file_path)
      }
    }
    Error(_) -> io.println("Error: Could not read stub file")
  }
}

/// ------------------------------------------------------------
/// Make Middleware
/// ------------------------------------------------------------
///
/// Creates a middleware file from a stub template. The
/// generated middleware includes a handle function with
/// Request, Context, and Next parameters. Automatically
/// creates nested directories if they don't exist.
///
fn make_middleware(name: String) {
  let module_name = to_snake_case(name)
  let file_path = "src/app/http/middleware/" <> module_name <> ".gleam"

  case read_stub("http/middleware.stub") {
    Ok(content) -> {
      let _ = ensure_directory_exists(file_path)
      case simplifile.write(file_path, content) {
        Ok(_) -> {
          io.println("Middleware created successfully!")
          io.println("  " <> file_path)
        }
        Error(_) -> io.println("Error: Could not write file " <> file_path)
      }
    }
    Error(_) -> io.println("Error: Could not read stub file")
  }
}

/// ------------------------------------------------------------
/// Make Request
/// ------------------------------------------------------------
///
/// Creates a form request validation file from a stub template.
/// The generated file includes rules() and data() functions for
/// validating and extracting form data. Automatically creates
/// nested directories if they don't exist.
///
fn make_request(name: String) {
  let module_name = to_snake_case(name)
  let file_path = "src/app/http/requests/" <> module_name <> ".gleam"

  case read_stub("http/request.stub") {
    Ok(content) -> {
      let _ = ensure_directory_exists(file_path)
      case simplifile.write(file_path, content) {
        Ok(_) -> {
          io.println("Request created successfully!")
          io.println("  " <> file_path)
        }
        Error(_) -> io.println("Error: Could not write file " <> file_path)
      }
    }
    Error(_) -> io.println("Error: Could not read stub file")
  }
}

/// ------------------------------------------------------------
/// Make Rule
/// ------------------------------------------------------------
///
/// Creates a custom validation rule file from a stub template.
/// The generated file includes a validate() function that
/// returns a custom validation function. Automatically creates
/// nested directories if they don't exist.
///
fn make_rule(name: String) {
  let module_name = to_snake_case(name)
  let file_path = "src/app/http/rules/" <> module_name <> ".gleam"

  case read_stub("http/rule.stub") {
    Ok(content) -> {
      let _ = ensure_directory_exists(file_path)
      case simplifile.write(file_path, content) {
        Ok(_) -> {
          io.println("Rule created successfully!")
          io.println("  " <> file_path)
        }
        Error(_) -> io.println("Error: Could not write file " <> file_path)
      }
    }
    Error(_) -> io.println("Error: Could not read stub file")
  }
}

/// ------------------------------------------------------------
/// Read Stub
/// ------------------------------------------------------------
///
/// Reads a stub template file from the framework's priv
/// directory. The stub path is relative to priv/stubs/ and
/// returns an error if the file doesn't exist or can't be read.
///
fn read_stub(stub_path: String) -> Result(String, Nil) {
  case wisp.priv_directory("glimr") {
    Ok(priv_dir) -> {
      let full_path = priv_dir <> "/stubs/" <> stub_path
      simplifile.read(full_path)
      |> result.replace_error(Nil)
    }
    Error(_) -> Error(Nil)
  }
}

/// ------------------------------------------------------------
/// Ensure Directory Exists
/// ------------------------------------------------------------
///
/// Creates all parent directories for a given file path if they
/// don't already exist. Extracts the directory path from the
/// file path and uses create_directory_all to create the entire
/// directory tree.
///
fn ensure_directory_exists(
  file_path: String,
) -> Result(Nil, simplifile.FileError) {
  case get_directory_path(file_path) {
    "" -> Ok(Nil)
    dir -> simplifile.create_directory_all(dir)
  }
}

/// ------------------------------------------------------------
/// Get Directory Path
/// ------------------------------------------------------------
///
/// Extracts the directory portion of a file path by splitting
/// on slashes, removing the last segment (filename), and
/// rejoining the remaining parts. Returns an empty string if
/// there's no directory component.
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

/// ------------------------------------------------------------
/// To Snake Case
/// ------------------------------------------------------------
///
/// Converts a string to snake_case by converting all characters
/// to lowercase. Used to normalize controller, middleware,
/// request, and rule names to follow Gleam naming conventions.
///
fn to_snake_case(name: String) -> String {
  name
  |> string.lowercase
}

/// ------------------------------------------------------------
/// Erlang Get Args
/// ------------------------------------------------------------
///
/// External call to Erlang's init:get_plain_arguments/0 which
/// returns command-line arguments as a list of charlists. Used
/// internally by get_args() to retrieve CLI arguments.
///
@external(erlang, "init", "get_plain_arguments")
fn erlang_get_args() -> List(charlist.Charlist)

/// ------------------------------------------------------------
/// Get Args
/// ------------------------------------------------------------
///
/// Retrieves command-line arguments as Gleam strings. Calls
/// the Erlang function to get raw charlists and converts them
/// to Gleam strings for easier processing.
///
fn get_args() -> List(String) {
  erlang_get_args()
  |> list.map(charlist.to_string)
}
