//// Route Compiler
////
//// Compiles route definition files to executable Gleam code.
//// Discovers route files in src/routes and generates compiled
//// versions in bootstrap/gen/routes.
////

import gleam/list
import gleam/result
import gleam/string
import glimr/console/console
import glimr/routing/compiler
import simplifile

/// Compiles all route files in src/routes directory. Discovers
/// .gleam files and compiles each one to the bootstrap/gen/routes
/// directory.
///
pub fn run(verbose: Bool) -> Result(Nil, String) {
  case verbose {
    True -> {
      console.output()
      |> console.line_warning("Compiling routes...")
      |> console.print()
    }
    False -> Nil
  }

  // Ensure the output directory exists
  let _ = simplifile.create_directory_all("src/bootstrap/gen/routes")

  let route_files = discover_route_files("src/routes")
  compile_route_files(route_files, verbose)
}

/// Compiles a single route file by path. Creates the output
/// directory if needed and generates the compiled route in
/// the bootstrap/gen/routes directory.
///
pub fn run_path(path: String, verbose: Bool) -> Result(Nil, String) {
  case verbose {
    True -> {
      console.output()
      |> console.line_warning("Compiling routes...")
      |> console.print()
    }
    False -> Nil
  }

  let _ = simplifile.create_directory_all("src/bootstrap/gen/routes")

  let filename =
    path
    |> string.replace("src/routes/", "")
  let dest = "src/bootstrap/gen/routes/" <> filename

  case compile_route_file(path, dest, verbose) {
    Ok(_) -> {
      case verbose {
        True -> Nil
        False -> {
          console.output()
          |> console.unpadded()
          |> console.line_success("Compiled route file: " <> filename)
          |> console.print()
        }
      }
      Ok(Nil)
    }
    Error(e) -> Error(e)
  }
}

/// Finds all .gleam files in the routes directory and maps
/// them to source/destination pairs. Destination is the
/// bootstrap/gen/routes directory with the same filename.
///
fn discover_route_files(dir: String) -> List(#(String, String)) {
  simplifile.read_directory(dir)
  |> result.unwrap([])
  |> list.filter(fn(f) { string.ends_with(f, ".gleam") })
  |> list.map(fn(f) {
    let source = dir <> "/" <> f
    let dest = "src/bootstrap/gen/routes/" <> f
    #(source, dest)
  })
}

/// Recursively compiles a list of route files. Stops and
/// returns an error if any file fails to compile, otherwise
/// returns Ok when all files are processed.
///
fn compile_route_files(
  files: List(#(String, String)),
  verbose: Bool,
) -> Result(Nil, String) {
  case files {
    [] -> {
      case verbose {
        True -> Nil
        False -> {
          console.output()
          |> console.unpadded()
          |> console.line_success("Compiled route files...")
          |> console.print()
        }
      }

      Ok(Nil)
    }
    [#(source, dest), ..rest] -> {
      case compile_route_file(source, dest, verbose) {
        Ok(_) -> compile_route_files(rest, verbose)
        Error(e) -> Error(e)
      }
    }
  }
}

/// Compiles a single route file from source to destination.
/// Uses the routing compiler to parse and generate code, then
/// writes the result to the destination path.
///
fn compile_route_file(
  source: String,
  dest: String,
  verbose: Bool,
) -> Result(Nil, String) {
  case compiler.compile_file(source) {
    Ok(result) -> {
      case compiler.write_compiled_file(result, dest) {
        Ok(_) -> {
          case verbose {
            True -> {
              console.output()
              |> console.unpadded()
              |> console.line("  " <> source <> " -> " <> console.success(dest))
              |> console.print()
            }
            False -> Nil
          }

          Ok(Nil)
        }
        Error(err) -> {
          console.output()
          |> console.unpadded()
          |> console.line_error(source <> " failed")
          |> console.blank_line(1)
          |> console.print()
          Error(err)
        }
      }
    }
    Error(err) -> Error(err)
  }
}
