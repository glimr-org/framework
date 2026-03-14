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
/// controllers don't need to check auth status themselves — add
/// it to the controller's `middleware()` function or use
/// `middleware.apply` in individual routes.
///
pub fn create_auth_middleware(model_name: String, scoped: Bool) -> Nil {
  let file_path = "src/app/http/middleware/auth_" <> model_name <> ".gleam"

  let guest_redirect = case scoped {
    False -> "/login"
    True -> "/" <> model_name <> "/login"
  }

  scaffold_file(file_path, "auth/auth_model.stub", [
    #("model", model_name),
    #("guest_redirect", guest_redirect),
  ])
}

/// Login and registration pages shouldn't be accessible to
/// already-authenticated users — showing them a login form when
/// they're already signed in is confusing. This middleware
/// redirects them away.
///
pub fn create_guest_middleware(model_name: String, _scoped: Bool) -> Nil {
  let file_path = "src/app/http/middleware/guest_" <> model_name <> ".gleam"

  scaffold_file(file_path, "auth/guest_model.stub", [
    #("model", model_name),
  ])
}

/// A working login page needs both a GET route (show the form)
/// and a POST route (handle submission), plus guest middleware
/// to redirect already-logged-in users. Generating all of this
/// from a stub means `make_auth` produces a functional login
/// flow without the developer writing any controller code.
///
pub fn create_login_controller(
  model_name: String,
  connection: String,
  ctx_db_name: String,
  scoped: Bool,
) -> Nil {
  let file_path = case scoped {
    False -> "src/app/http/controllers/auth/login_controller.gleam"
    True ->
      "src/app/http/controllers/auth/"
      <> model_name
      <> "_login_controller.gleam"
  }

  let route_prefix = case scoped {
    False -> ""
    True -> "/" <> model_name
  }

  let guest_middleware = "guest_" <> model_name

  let validator = case scoped {
    False -> "store_login"
    True -> "store_" <> model_name <> "_login"
  }

  let view_path = case scoped {
    False -> "auth/login"
    True -> "auth/" <> model_name <> "/login"
  }

  scaffold_file(file_path, "auth/login_controller.stub", [
    #("model", model_name),
    #("route_prefix", route_prefix),
    #("connection", connection),
    #("ctx_db_name", ctx_db_name),
    #("guest_middleware", guest_middleware),
    #("validator", validator),
    #("view_path", view_path),
  ])
}

/// Logout needs to be POST-only (GET logout links are a CSRF
/// risk — any page could embed an image tag pointing at your
/// logout URL). The generated controller clears the session and
/// redirects, with auth middleware to ensure only logged-in
/// users can hit it.
///
pub fn create_logout_controller(model_name: String, scoped: Bool) -> Nil {
  let file_path = case scoped {
    False -> "src/app/http/controllers/auth/logout_controller.gleam"
    True ->
      "src/app/http/controllers/auth/"
      <> model_name
      <> "_logout_controller.gleam"
  }

  let route_prefix = case scoped {
    False -> ""
    True -> "/" <> model_name
  }

  let auth_middleware = "auth_" <> model_name

  scaffold_file(file_path, "auth/logout_controller.stub", [
    #("model", model_name),
    #("route_prefix", route_prefix),
    #("auth_middleware", auth_middleware),
  ])
}

/// Registration mirrors the login controller pattern — GET
/// shows the form, POST creates the account. The stub includes
/// password hashing and automatic login after successful
/// registration so the developer gets a complete signup flow
/// they can customise.
///
pub fn create_register_controller(
  model_name: String,
  connection: String,
  ctx_db_name: String,
  scoped: Bool,
) -> Nil {
  let file_path = case scoped {
    False -> "src/app/http/controllers/auth/register_controller.gleam"
    True ->
      "src/app/http/controllers/auth/"
      <> model_name
      <> "_register_controller.gleam"
  }

  let route_prefix = case scoped {
    False -> ""
    True -> "/" <> model_name
  }

  let guest_middleware = "guest_" <> model_name

  let validator = case scoped {
    False -> "store_register"
    True -> "store_" <> model_name <> "_register"
  }

  let view_path = case scoped {
    False -> "auth/register"
    True -> "auth/" <> model_name <> "/register"
  }

  scaffold_file(file_path, "auth/register_controller.stub", [
    #("model", model_name),
    #("route_prefix", route_prefix),
    #("connection", connection),
    #("ctx_db_name", ctx_db_name),
    #("guest_middleware", guest_middleware),
    #("validator", validator),
    #("view_path", view_path),
  ])
}

/// The login form needs to reject empty submissions before they
/// even hit the database. Generating a validator with Required
/// rules for email and password gives the login controller
/// immediate form validation without the developer having to
/// write boilerplate.
///
pub fn create_login_validator(scoped: Bool, model_name: String) -> Nil {
  let file_name = case scoped {
    False -> "store_login"
    True -> "store_" <> model_name <> "_login"
  }

  let file_path = "src/app/http/validators/" <> file_name <> ".gleam"

  scaffold_file(file_path, "auth/store_login_validator.stub", [])
}

/// The registration form needs password confirmation and
/// stricter rules than login. Generating a validator with
/// Required, MinLength, and Confirmed rules gives the register
/// controller immediate form validation.
///
pub fn create_register_validator(scoped: Bool, model_name: String) -> Nil {
  let file_name = case scoped {
    False -> "store_register"
    True -> "store_" <> model_name <> "_register"
  }

  let file_path = "src/app/http/validators/" <> file_name <> ".gleam"

  scaffold_file(file_path, "auth/store_register_validator.stub", [])
}

/// A generated controller isn't much use without pages to
/// render. This scaffolds the full set of loom templates
/// (login, register, dashboard) plus reusable form components
/// (input, button, link) so `make_auth` produces a working UI
/// out of the box. Scoped models get their own view
/// subdirectory so multiple auth models don't collide.
///
pub fn create_loom_views(
  scoped: Bool,
  model_name: String,
  connection: String,
) -> Nil {
  let route_prefix = case scoped {
    False -> ""
    True -> "/" <> model_name
  }

  let model_type = pascal_case(model_name)

  let auth_dir = case scoped {
    False -> "src/resources/views/auth/"
    True -> "src/resources/views/auth/" <> model_name <> "/"
  }

  let dashboard_dir = case scoped {
    False -> "src/resources/views/"
    True -> "src/resources/views/" <> model_name <> "/"
  }

  // Components (shared, no variables)
  scaffold_file(
    "src/resources/views/components/input.loom.html",
    "auth/views/components/input.loom.html.stub",
    [],
  )

  // Auth views (with route_prefix for form actions and links)
  scaffold_file(
    auth_dir <> "login.loom.html",
    "auth/views/login.loom.html.stub",
    [#("route_prefix", route_prefix)],
  )
  scaffold_file(
    auth_dir <> "register.loom.html",
    "auth/views/register.loom.html.stub",
    [#("route_prefix", route_prefix)],
  )

  // Dashboard view
  scaffold_file(
    dashboard_dir <> "dashboard.loom.html",
    "auth/views/dashboard.loom.html.stub",
    [
      #("model", model_name),
      #("model_type", model_type),
      #("connection", connection),
      #("route_prefix", route_prefix),
    ],
  )
}

/// The dashboard is the first page a user sees after logging
/// in. Generating it with auth middleware already applied means
/// `make_auth` produces a complete login-to-dashboard flow out
/// of the box.
///
pub fn create_dashboard_controller(model_name: String, scoped: Bool) -> Nil {
  let file_path = case scoped {
    False -> "src/app/http/controllers/dashboard_controller.gleam"
    True ->
      "src/app/http/controllers/" <> model_name <> "_dashboard_controller.gleam"
  }

  let route_prefix = case scoped {
    False -> ""
    True -> "/" <> model_name
  }

  let auth_middleware = "auth_" <> model_name

  let view_path = case scoped {
    False -> "dashboard"
    True -> model_name <> "/dashboard"
  }

  scaffold_file(file_path, "auth/dashboard_controller.stub", [
    #("model", model_name),
    #("route_prefix", route_prefix),
    #("auth_middleware", auth_middleware),
    #("view_path", view_path),
  ])
}

/// Running `make_auth admin` after already running `make_auth
/// user` (without --scoped) would silently overwrite the user
/// auth controllers since unscoped auth uses generic filenames
/// like `login_controller.gleam`. This check catches that
/// situation and warns the developer to use --scoped instead.
///
pub fn check_existing_unscoped_auth(model_name: String) -> Result(Nil, String) {
  let controller_path = "src/app/http/controllers/auth/login_controller.gleam"

  case simplifile.read(controller_path) {
    Error(_) -> Ok(Nil)
    Ok(content) -> {
      case extract_auth_model(content) {
        Error(_) -> Ok(Nil)
        Ok(existing_model) -> {
          case existing_model == model_name {
            True -> Ok(Nil)
            False -> Error(existing_model)
          }
        }
      }
    }
  }
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

/// Controllers access the current user via `app.user`, which
/// means the App type needs a field for it. Adding the field
/// and its import automatically means the developer doesn't
/// have to manually wire up types across files.
///
pub fn register_in_app(model_name: String, connection: String) -> Nil {
  let app_path = "src/app/app.gleam"

  case simplifile.read(app_path) {
    Error(_) -> {
      console.output()
      |> console.line_error(
        "Could not read " <> app_path <> " — does it exist?",
      )
      |> console.print()
    }

    Ok(content) -> {
      case string.contains(content, model_name <> ":") {
        True -> {
          console.output()
          |> console.line_warning(
            "Skipped: "
            <> app_path
            <> " ("
            <> model_name
            <> " field already exists)",
          )
          |> console.print()
        }
        False -> {
          let modified = inject_into_app(content, model_name, connection)
          write_and_format(app_path, original: content, modified: modified)
        }
      }
    }
  }
}

/// The bootstrap app module creates the initial App value with
/// all fields set. Without this patch, the new auth field would
/// be missing from the constructor and every request would
/// crash with a missing field error.
///
pub fn register_in_app_start(model_name: String) -> Nil {
  let bootstrap_path = "src/bootstrap/app.gleam"

  case simplifile.read(bootstrap_path) {
    Error(_) -> {
      console.output()
      |> console.line_error(
        "Could not read " <> bootstrap_path <> " — does it exist?",
      )
      |> console.print()
    }

    Ok(content) -> {
      case string.contains(content, model_name <> ": ") {
        True -> {
          console.output()
          |> console.line_warning(
            "Skipped: "
            <> bootstrap_path
            <> " ("
            <> model_name
            <> " already initialized)",
          )
          |> console.print()
        }
        False -> {
          let modified = inject_into_app_start(content, model_name)
          write_and_format(
            bootstrap_path,
            original: content,
            modified: modified,
          )
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
/// typed Option field into the App constructor.
///
pub fn inject_into_app(
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
/// the bootstrap app module where App values are first
/// constructed. Inserts `{model}: option.None` so the new field
/// has a valid default value.
///
pub fn inject_into_app_start(content: String, model_name: String) -> String {
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

/// The unscoped login controller references the model via
/// `ctx.app.user` (or whatever the model name is). Parsing that
/// out of the file lets check_existing_unscoped_auth figure out
/// which model currently "owns" the unscoped auth routes
/// without needing a manifest or config file.
///
fn extract_auth_model(content: String) -> Result(String, Nil) {
  case string.split_once(content, "ctx.app.") {
    Error(_) -> Error(Nil)
    Ok(#(_, after)) -> {
      let model =
        after
        |> string.to_graphemes()
        |> list.take_while(fn(c) { c != "," && c != ")" && c != "\n" })
        |> string.join("")
        |> string.trim()
      case model {
        "" -> Error(Nil)
        name -> Ok(name)
      }
    }
  }
}

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
