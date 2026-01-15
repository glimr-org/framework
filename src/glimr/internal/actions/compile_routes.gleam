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
pub fn run() -> Result(Nil, String) {
  console.output()
  |> console.unpadded()
  |> console.blank_line(1)
  |> console.line_warning("Compiling routes...")
  |> console.print()

  // Ensure the output directory exists
  let _ = simplifile.create_directory_all("src/bootstrap/gen/routes")

  let route_files = discover_route_files("src/routes")
  compile_route_files(route_files)
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
fn compile_route_files(files: List(#(String, String))) -> Result(Nil, String) {
  case files {
    [] -> Ok(Nil)
    [#(source, dest), ..rest] -> {
      case compile_route_file(source, dest) {
        Ok(_) -> compile_route_files(rest)
        Error(e) -> Error(e)
      }
    }
  }
}

/// Compiles a single route file from source to destination.
/// Uses the routing compiler to parse and generate code, then
/// writes the result to the destination path.
///
fn compile_route_file(source: String, dest: String) -> Result(Nil, String) {
  case compiler.compile_file(source) {
    Ok(result) -> {
      case compiler.write_compiled_file(result, dest) {
        Ok(_) -> {
          console.output()
          |> console.unpadded()
          |> console.line("  " <> source <> " -> " <> console.success(dest))
          |> console.print()
          Ok(Nil)
        }
        Error(err) -> {
          console.output()
          |> console.unpadded()
          |> console.line_error("  " <> source <> " failed")
          |> console.blank_line(1)
          |> console.print()
          Error(err)
        }
      }
    }
    Error(_) -> Ok(Nil)
  }
}
