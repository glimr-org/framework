//// Make Auth Service
////
//// Scaffolds authentication for a given model name. Generates
//// the model (schema + queries), load middleware, auth guard
//// middleware, and patches the kernel and context files. All
//// file I/O and string manipulation lives here so the command
//// handler stays thin and each operation can be tested
//// independently.
////
//// The driver-specific commands (postgres_make_auth,
//// sqlite_make_auth) call into this service for the shared
//// parts, then handle migration and repository generation
//// themselves.
////

import gleam/list
import gleam/string
import glimr/console/console
import glimr/filesystem/filesystem
import glimr/utils/string as glimr_string
import shellout
import simplifile

// ------------------------------------------------------------- Public Functions

/// Sets up the database side of auth — schema with email and
/// hashed_password columns, plus standard CRUD queries. Skips
/// if the model directory already exists so running the command
/// twice doesn't blow away customizations.
///
pub fn create_model(model_name: String, connection: String) -> Nil {
  let table_name = glimr_string.pluralize(model_name)
  let model_dir = "src/database/" <> connection <> "/models/" <> model_name
  let queries_dir = model_dir <> "/queries"

  let assert Ok(dir_exists) = filesystem.directory_exists(model_dir)

  case dir_exists {
    True -> {
      console.output()
      |> console.line_warning(
        "Skipped: " <> model_dir <> " (model already exists)",
      )
      |> console.print()
    }
    False -> {
      let schema_path = model_dir <> "/" <> model_name <> "_schema.gleam"
      let variables = [#("table_name", table_name)]

      let assert Ok(_) =
        filesystem.write_from_stub_with_variables(
          package: "glimr",
          stub_path: "auth/schema.stub",
          dest_path: schema_path,
          variables: variables,
        )

      let query_stubs = auth_query_stubs()
      list.each(query_stubs, fn(query_name) {
        let query_path = queries_dir <> "/" <> query_name <> ".sql"
        let assert Ok(_) =
          filesystem.write_from_stub_with_variables(
            package: "glimr",
            stub_path: "auth/queries/" <> query_name <> ".stub",
            dest_path: query_path,
            variables: variables,
          )
      })

      console.output()
      |> console.line_success("Created: " <> model_dir)
      |> console.print()
    }
  }
}

/// Every request that needs the current user has to look it up
/// from the session. This middleware does that once, early in
/// the pipeline, so controllers can just read ctx.user instead
/// of repeating the same session-to-database lookup.
///
pub fn create_load_middleware(
  model_name: String,
  connection: String,
  ctx_db_name: String,
) -> Nil {
  let file_path = "src/app/http/middleware/load_" <> model_name <> ".gleam"
  let session_key = "_auth_" <> model_name <> "_id"

  scaffold_file(file_path, "auth/load_model.stub", [
    #("model", model_name),
    #("session_key", session_key),
    #("connection", connection),
    #("ctx_db_name", ctx_db_name),
  ])
}

/// Protected pages need to bounce unauthenticated visitors to
/// the login page. This middleware handles that redirect so
/// controllers don't need to check auth status themselves —
/// just attach `@middleware "auth/{model}"` to the route.
///
pub fn create_auth_middleware(model_name: String) -> Nil {
  let file_path = "src/app/http/middleware/auth/" <> model_name <> ".gleam"

  scaffold_file(file_path, "auth/auth_model.stub", [
    #("model", model_name),
  ])
}

/// Login and registration pages shouldn't be accessible to
/// already-authenticated users — showing them a login form when
/// they're already signed in is confusing. This middleware
/// redirects them away.
///
pub fn create_guest_middleware(model_name: String) -> Nil {
  let file_path = "src/app/http/middleware/guest/" <> model_name <> ".gleam"

  scaffold_file(file_path, "auth/guest_model.stub", [
    #("model", model_name),
  ])
}

/// The loader middleware needs to be registered in the HTTP
/// kernel so it runs on every request. Patching the file
/// automatically saves the developer from manually editing the
/// kernel and figuring out the right placement.
///
pub fn register_in_kernel(model_name: String) -> Nil {
  let kernel_path = "src/app/http/kernel.gleam"
  let middleware_name = "load_" <> model_name

  case simplifile.read(kernel_path) {
    Error(_) -> {
      console.output()
      |> console.line_error(
        "Could not read " <> kernel_path <> " — does it exist?",
      )
      |> console.print()
    }

    Ok(content) -> {
      case string.contains(content, middleware_name) {
        True -> {
          console.output()
          |> console.line_warning(
            "Skipped: "
            <> kernel_path
            <> " ("
            <> middleware_name
            <> " already registered)",
          )
          |> console.print()
        }
        False -> {
          let modified = inject_into_kernel(content, model_name)
          write_and_format(kernel_path, original: content, modified: modified)
        }
      }
    }
  }
}

/// Controllers access the current user via `ctx.user`, which
/// means the Context type needs a field for it. Adding the
/// field and its import automatically means the developer
/// doesn't have to manually wire up types across files.
///
pub fn register_in_context(model_name: String, connection: String) -> Nil {
  let ctx_path = "src/app/http/context/ctx.gleam"

  case simplifile.read(ctx_path) {
    Error(_) -> {
      console.output()
      |> console.line_error(
        "Could not read " <> ctx_path <> " — does it exist?",
      )
      |> console.print()
    }

    Ok(content) -> {
      case string.contains(content, model_name <> ":") {
        True -> {
          console.output()
          |> console.line_warning(
            "Skipped: "
            <> ctx_path
            <> " ("
            <> model_name
            <> " field already exists)",
          )
          |> console.print()
        }
        False -> {
          let modified = inject_into_context(content, model_name, connection)
          write_and_format(ctx_path, original: content, modified: modified)
        }
      }
    }
  }
}

/// The ctx_provider creates the initial Context value with all
/// fields set. Without this patch, the new auth field would be
/// missing from the constructor and every request would crash
/// with a missing field error.
///
pub fn register_in_ctx_provider(model_name: String) -> Nil {
  let provider_path = "src/app/providers/ctx_provider.gleam"

  case simplifile.read(provider_path) {
    Error(_) -> {
      console.output()
      |> console.line_error(
        "Could not read " <> provider_path <> " — does it exist?",
      )
      |> console.print()
    }

    Ok(content) -> {
      case string.contains(content, model_name <> ": ") {
        True -> {
          console.output()
          |> console.line_warning(
            "Skipped: "
            <> provider_path
            <> " ("
            <> model_name
            <> " already initialized)",
          )
          |> console.print()
        }
        False -> {
          let modified = inject_into_ctx_provider(content, model_name)
          write_and_format(provider_path, original: content, modified: modified)
        }
      }
    }
  }
}

/// Public so the test suite can verify the string
/// transformation without touching the filesystem. The actual
/// command reads the file, calls this, then writes back —
/// keeping I/O at the edges.
///
pub fn inject_into_kernel(content: String, model_name: String) -> String {
  let middleware_name = "load_" <> model_name
  let lines = string.split(content, "\n")

  let lines =
    insert_import(lines, "import app/http/middleware/" <> middleware_name)
  let lines = insert_middleware_entries(lines, middleware_name <> ".run,")

  string.join(lines, "\n")
}

/// Same pattern as inject_into_kernel — pure string transform
/// that the test suite can exercise without file I/O. Adds the
/// import for the generated repository module and inserts the
/// typed Option field into the Context constructor.
///
pub fn inject_into_context(
  content: String,
  model_name: String,
  connection: String,
) -> String {
  let lines = string.split(content, "\n")
  let model_type = pascal_case(model_name)
  let repo_module = model_name
  let repo_import =
    "import database/"
    <> connection
    <> "/models/"
    <> model_name
    <> "/gen/"
    <> repo_module

  // Ensure the Option import is present
  let lines = case has_import(lines, "gleam/option") {
    True -> lines
    False -> insert_import(lines, "import gleam/option.{type Option}")
  }

  // Add the repository import
  let lines = case has_import(lines, repo_module) {
    True -> lines
    False -> insert_import(lines, repo_import)
  }

  // Insert the field before the closing paren of the Context constructor
  let field_type = "Option(" <> repo_module <> "." <> model_type <> ")"
  let lines = insert_context_field(lines, model_name, field_type)

  string.join(lines, "\n")
}

/// Completes the trio of inject functions — this one handles
/// the ctx_provider where Context values are first constructed.
/// Inserts `{model}: option.None` so the new field has a valid
/// default value.
///
pub fn inject_into_ctx_provider(content: String, model_name: String) -> String {
  let lines = string.split(content, "\n")

  // Ensure the option import is present
  let lines = case has_import(lines, "gleam/option") {
    True -> lines
    False -> insert_import(lines, "import gleam/option")
  }

  // Insert {model}: option.None into the Context constructor
  let lines = insert_context_field(lines, model_name, "option.None")

  string.join(lines, "\n")
}

/// Public so the Postgres and SQLite make_auth commands can
/// check which queries were generated. Keeping the list here
/// instead of duplicating it in each driver command means
/// adding a new auth query only requires one change.
///
pub fn auth_query_stubs() -> List(String) {
  ["create", "delete", "find", "by_email", "list", "update"]
}

// ------------------------------------------------------------- Private Functions

/// Checking for existing files before writing prevents the
/// command from destroying customizations when a developer runs
/// it again (e.g. after adding a second auth model for admins
/// alongside users).
///
fn scaffold_file(
  file_path: String,
  stub_path: String,
  variables: List(#(String, String)),
) -> Nil {
  let assert Ok(file_exists) = filesystem.file_exists(file_path)

  case file_exists {
    True -> {
      console.output()
      |> console.line_warning("Skipped: " <> file_path <> " (already exists)")
      |> console.print()
    }
    False -> {
      case
        filesystem.write_from_stub_with_variables(
          package: "glimr",
          stub_path: stub_path,
          dest_path: file_path,
          variables: variables,
        )
      {
        Ok(_) -> {
          console.output()
          |> console.line_success("Created: " <> file_path)
          |> console.print()
        }
        Error(_) -> {
          console.output()
          |> console.line_error("Failed to create " <> file_path)
          |> console.print()
        }
      }
    }
  }
}

/// If gleam format fails (meaning the injected code broke the
/// syntax), we roll back to the original file contents rather
/// than leaving the developer with a broken file they have to
/// fix manually.
///
fn write_and_format(
  path: String,
  original original: String,
  modified modified: String,
) -> Nil {
  case simplifile.write(path, modified) {
    Error(_) -> {
      console.output()
      |> console.line_error("Failed to write " <> path)
      |> console.print()
    }
    Ok(_) -> {
      case shellout.command("gleam", ["format", path], in: ".", opt: []) {
        Ok(_) -> {
          console.output()
          |> console.line_success("Updated: " <> path)
          |> console.print()
        }
        Error(_) -> {
          let _ = simplifile.write(path, original)
          console.output()
          |> console.line_error(
            "Failed to format " <> path <> ", changes reverted",
          )
          |> console.print()
        }
      }
    }
  }
}

/// Running make_auth twice shouldn't produce duplicate imports.
/// Using string.contains rather than exact matching catches
/// variations like `import gleam/option.{type Option}` vs
/// `import gleam/option` — both mean the module is already
/// imported.
///
fn has_import(lines: List(String), module: String) -> Bool {
  list.any(lines, fn(line) {
    let trimmed = string.trim_start(line)
    string.starts_with(trimmed, "import ") && string.contains(trimmed, module)
  })
}

/// Placing the new import after the last existing import keeps
/// the import block contiguous. If no imports exist the new one
/// goes at the top so gleam format can sort it later.
///
fn insert_import(lines: List(String), import_line: String) -> List(String) {
  let last_import_index = find_last_import_index(lines, 0, -1)

  case last_import_index >= 0 {
    True -> insert_at(lines, last_import_index + 1, import_line)
    False -> [import_line, ..lines]
  }
}

/// Scans all lines to find the last import rather than stopping
/// at the first gap, because gleam format may insert blank
/// lines between import groups.
///
fn find_last_import_index(lines: List(String), current: Int, last: Int) -> Int {
  case lines {
    [] -> last
    [line, ..rest] -> {
      let trimmed = string.trim_start(line)
      case string.starts_with(trimmed, "import ") {
        True -> find_last_import_index(rest, current + 1, current)
        False -> find_last_import_index(rest, current + 1, last)
      }
    }
  }
}

/// Finds every middleware list in the kernel and appends the
/// new .run entry after the last .run entry in each list.
/// Inserting at the end means the loader runs after all other
/// middleware, which is correct since it depends on the
/// session.
///
fn insert_middleware_entries(lines: List(String), entry: String) -> List(String) {
  insert_middleware_entries_loop(lines, entry, -1, 0, [])
}

/// Tracks the index of the last .run entry seen so far. When a
/// closing bracket is hit, the new entry is spliced in after
/// that index, then scanning resumes.
///
fn insert_middleware_entries_loop(
  lines: List(String),
  entry: String,
  last_run_index: Int,
  current: Int,
  acc: List(String),
) -> List(String) {
  case lines {
    [] -> list.reverse(acc)
    [line, ..rest] -> {
      let trimmed = string.trim(line)
      case string.contains(trimmed, ".run,") {
        True ->
          insert_middleware_entries_loop(rest, entry, current, current + 1, [
            line,
            ..acc
          ])
        False ->
          case string.starts_with(trimmed, "]") && last_run_index >= 0 {
            True -> {
              let ordered = list.reverse(acc)
              let indent = get_indent(get_at(ordered, last_run_index))
              let new_line = indent <> entry
              let ordered = insert_at(ordered, last_run_index + 1, new_line)
              let acc = list.reverse(ordered)
              insert_middleware_entries_loop(rest, entry, -1, current + 2, [
                line,
                ..acc
              ])
            }
            False ->
              insert_middleware_entries_loop(
                rest,
                entry,
                last_run_index,
                current + 1,
                [line, ..acc],
              )
          }
      }
    }
  }
}

/// Some developers write their Context on one line, others
/// spread it across many. Normalizing to multi-line first means
/// the field scanner has a consistent format to work with, and
/// gleam format cleans up the style afterward.
///
fn insert_context_field(
  lines: List(String),
  model_name: String,
  field_type: String,
) -> List(String) {
  let field_line = "    " <> model_name <> ": " <> field_type <> ","
  let lines = expand_single_line_constructor(lines)
  insert_context_field_loop(lines, field_line, -1, 0, [])
}

/// Inserting after the last field rather than before `)` means
/// the new field gets a trailing comma naturally (we add one),
/// and the existing fields don't need their commas adjusted.
/// Field detection is heuristic — indented lines with `:` that
/// aren't comments or type declarations.
///
fn insert_context_field_loop(
  lines: List(String),
  field_line: String,
  last_field_index: Int,
  current: Int,
  acc: List(String),
) -> List(String) {
  case lines {
    [] -> list.reverse(acc)
    [line, ..rest] -> {
      let trimmed = string.trim(line)
      case trimmed == ")" && last_field_index >= 0 {
        True -> {
          // Insert the new field before the closing paren
          let ordered = list.reverse(acc)
          let ordered = insert_at(ordered, last_field_index + 1, field_line)
          let acc = list.reverse(ordered)
          // Continue with remaining lines, no more insertion needed
          insert_context_field_loop(rest, field_line, -1, current + 2, [
            line,
            ..acc
          ])
        }
        False -> {
          // Track lines that look like fields (indented with a colon)
          let is_field =
            string.contains(trimmed, ":")
            && string.starts_with(line, "  ")
            && !string.starts_with(trimmed, "//")
            && !string.starts_with(trimmed, "pub")
          let new_last = case is_field {
            True -> current
            False -> last_field_index
          }
          insert_context_field_loop(rest, field_line, new_last, current + 1, [
            line,
            ..acc
          ])
        }
      }
    }
  }
}

/// A single-line constructor like `Context(db: Pool, env: Env)`
/// would break the field scanner which expects one field per
/// line. Splitting it into multi-line first gives us a uniform
/// format. Already multi-line constructors pass through
/// unchanged.
///
fn expand_single_line_constructor(lines: List(String)) -> List(String) {
  list.flat_map(lines, fn(line) {
    let trimmed = string.trim(line)
    // Match a constructor line: starts with a capital letter,
    // contains "(", has ":" inside, and ends with ")"
    case
      string.contains(trimmed, "(")
      && string.contains(trimmed, ":")
      && string.ends_with(trimmed, ")")
      && !string.starts_with(trimmed, "//")
      && !string.starts_with(trimmed, "pub type")
    {
      False -> [line]
      True -> {
        // Split "  Constructor(field1: T1, field2: T2)" into parts
        case string.split_once(trimmed, "(") {
          Error(_) -> [line]
          Ok(#(constructor, rest)) -> {
            // Remove trailing ")"
            let fields_str = string.drop_end(rest, 1)
            let fields =
              string.split(fields_str, ",")
              |> list.map(string.trim)
              |> list.filter(fn(f) { f != "" })
            let indent = get_indent(line)
            let constructor_line = indent <> constructor <> "("
            let field_lines =
              list.map(fields, fn(f) { indent <> "  " <> f <> "," })
            let close_line = indent <> ")"
            [constructor_line, ..list.append(field_lines, [close_line])]
          }
        }
      }
    }
  })
}

/// Gleam types use PascalCase but model names are snake_case
/// throughout the config and filesystem. This bridges the two
/// so generated code references the correct type name without
/// the user needing to specify it separately.
///
fn pascal_case(name: String) -> String {
  name
  |> string.split("_")
  |> list.map(string.capitalise)
  |> string.join("")
}

/// Preserves the indentation of the surrounding .run entries so
/// the inserted line matches the existing code style. Without
/// this the injected line would start at column zero and gleam
/// format would need to fix it.
///
fn get_indent(line: String) -> String {
  let chars = string.to_graphemes(line)
  get_indent_loop(chars, "")
}

/// Gleam has no built-in "take while" for graphemes, so this
/// recurses manually to extract the leading whitespace. Without
/// a separate loop function the indent logic would need to be
/// inlined everywhere get_indent is called.
///
fn get_indent_loop(chars: List(String), acc: String) -> String {
  case chars {
    [] -> acc
    [" ", ..rest] -> get_indent_loop(rest, acc <> " ")
    ["\t", ..rest] -> get_indent_loop(rest, acc <> "\t")
    _ -> acc
  }
}

/// Gleam lists don't support random access, so this walks to
/// the target index. Used only to grab the indentation of a
/// known .run line, so the O(n) cost is negligible on the small
/// line counts of a kernel file.
///
fn get_at(lines: List(String), index: Int) -> String {
  case lines, index {
    [line, ..], 0 -> line
    [_, ..rest], n -> get_at(rest, n - 1)
    [], _ -> ""
  }
}

/// Gleam's immutable lists have no splice operation, so this
/// rebuilds the list with a new value at the target position.
/// Shared by import, middleware, and context field injection to
/// avoid duplicating the same accumulator-reversal pattern in
/// each caller.
///
fn insert_at(lines: List(String), index: Int, value: String) -> List(String) {
  insert_at_loop(lines, index, value, 0, [])
}

/// Separated from insert_at so the public interface hides the
/// accumulator and counter. Prepending the new value before the
/// current element at the target index ensures existing lines
/// are never lost or reordered during the rebuild.
///
fn insert_at_loop(
  lines: List(String),
  index: Int,
  value: String,
  current: Int,
  acc: List(String),
) -> List(String) {
  case current == index {
    True -> {
      let acc = [value, ..acc]
      case lines {
        [] -> list.reverse(acc)
        [line, ..rest] ->
          insert_at_loop(rest, index, value, current + 1, [line, ..acc])
      }
    }
    False ->
      case lines {
        [] -> list.reverse(acc)
        [line, ..rest] ->
          insert_at_loop(rest, index, value, current + 1, [line, ..acc])
      }
  }
}
