import gleam/list
import gleam/string
import gleeunit/should
import glimr/filesystem/filesystem
import glimr/internal/services/make_auth_service

// ------------------------------------------------------------- Model: Stub Files Exist

pub fn all_auth_query_stubs_exist_test() {
  let query_stubs = make_auth_service.auth_query_stubs()
  list.each(query_stubs, fn(name) {
    let stub_path = "auth/queries/" <> name <> ".stub"
    let result = filesystem.read_stub("glimr", stub_path)
    should.be_ok(result)
  })
}

pub fn auth_schema_stub_exists_test() {
  let result = filesystem.read_stub("glimr", "auth/schema.stub")
  should.be_ok(result)
}

// ------------------------------------------------------------- Controller: Stub Files Exist

pub fn login_controller_stub_exists_test() {
  let result = filesystem.read_stub("glimr", "auth/login_controller.stub")
  should.be_ok(result)
}

pub fn logout_controller_stub_exists_test() {
  let result = filesystem.read_stub("glimr", "auth/logout_controller.stub")
  should.be_ok(result)
}

pub fn register_controller_stub_exists_test() {
  let result = filesystem.read_stub("glimr", "auth/register_controller.stub")
  should.be_ok(result)
}

// ------------------------------------------------------------- Loom Views: Stub Files Exist

pub fn login_view_stub_exists_test() {
  let result = filesystem.read_stub("glimr", "auth/views/login.loom.html.stub")
  should.be_ok(result)
}

pub fn register_view_stub_exists_test() {
  let result =
    filesystem.read_stub("glimr", "auth/views/register.loom.html.stub")
  should.be_ok(result)
}

pub fn input_component_stub_exists_test() {
  let result =
    filesystem.read_stub("glimr", "auth/views/components/input.loom.html.stub")
  should.be_ok(result)
}

// ------------------------------------------------------------- Dashboard: Stub Files Exist

pub fn dashboard_controller_stub_exists_test() {
  let result = filesystem.read_stub("glimr", "auth/dashboard_controller.stub")
  should.be_ok(result)
}

pub fn dashboard_view_stub_exists_test() {
  let result =
    filesystem.read_stub("glimr", "auth/views/dashboard.loom.html.stub")
  should.be_ok(result)
}

// ------------------------------------------------------------- Overwrite Detection

pub fn check_existing_unscoped_auth_returns_ok_when_no_file_test() {
  let result = make_auth_service.check_existing_unscoped_auth("user")
  should.be_ok(result)
}

// ------------------------------------------------------------- Kernel: Import Injection

pub fn adds_import_after_last_import_line_test() {
  let input =
    "import app/http/context/ctx.{type Context}
import app/http/middleware/load_session
import glimr/http/kernel.{type MiddlewareGroup}
import wisp.{type Request, type Response}

pub fn handle() {
}"

  let result = make_auth_service.inject_into_kernel(input, "user")

  should.be_true(has_line(result, "import app/http/middleware/load_user"))
}

pub fn import_placed_after_last_import_test() {
  let input =
    "import app/http/context/ctx.{type Context}
import glimr/http/kernel.{type MiddlewareGroup}
import wisp.{type Request, type Response}

pub fn handle() {
}"

  let result = make_auth_service.inject_into_kernel(input, "user")
  let lines = string.split(result, "\n")

  // The load_user import should come right after the wisp import (last import)
  let wisp_idx = find_line_index(lines, "import wisp", 0)
  let load_user_idx =
    find_line_index(lines, "import app/http/middleware/load_user", 0)

  should.be_true(load_user_idx == wisp_idx + 1)
}

// ------------------------------------------------------------- Kernel: Middleware Entry Injection

pub fn adds_load_run_to_single_middleware_group_test() {
  let input =
    "import app/http/middleware/load_session
import wisp.{type Request, type Response}

pub fn handle() {
  case middleware_group {
    kernel.Web | _ -> {
      [
        serve_static.run,
        log_request.run,
        load_session.run,
        // ...
      ]
      |> middleware.apply(req, ctx, router)
    }
  }
}"

  let result = make_auth_service.inject_into_kernel(input, "user")

  should.be_true(has_line(result, "load_user.run,"))
}

pub fn adds_load_run_to_multiple_middleware_groups_test() {
  let input =
    "import app/http/middleware/load_session
import wisp.{type Request, type Response}

pub fn handle() {
  case middleware_group {
    kernel.Api -> {
      [
        json_errors.run,
        load_session.run,
        // ...
      ]
      |> middleware.apply(req, ctx, router)
    }
    kernel.Web | _ -> {
      [
        serve_static.run,
        load_session.run,
        // ...
      ]
      |> middleware.apply(req, ctx, router)
    }
  }
}"

  let result = make_auth_service.inject_into_kernel(input, "user")

  // Count occurrences of load_user.run
  let count = count_occurrences(result, "load_user.run,")
  should.equal(count, 2)
}

pub fn load_run_inserted_after_last_run_entry_test() {
  let input =
    "import app/http/middleware/load_session
import wisp.{type Request, type Response}

pub fn handle() {
  case middleware_group {
    kernel.Web | _ -> {
      [
        serve_static.run,
        log_request.run,
        load_session.run,
        // ...
      ]
      |> middleware.apply(req, ctx, router)
    }
  }
}"

  let result = make_auth_service.inject_into_kernel(input, "user")
  let lines = string.split(result, "\n")

  let load_session_idx = find_line_index(lines, "load_session.run,", 0)
  let load_user_idx = find_line_index(lines, "load_user.run,", 0)

  // load_user.run should come right after load_session.run
  should.be_true(load_user_idx == load_session_idx + 1)
}

// ------------------------------------------------------------- Kernel: Custom Model Name

pub fn kernel_injection_with_custom_model_name_test() {
  let input =
    "import app/http/middleware/load_session
import wisp.{type Request, type Response}

pub fn handle() {
  case middleware_group {
    kernel.Web | _ -> {
      [
        load_session.run,
        // ...
      ]
      |> middleware.apply(req, ctx, router)
    }
  }
}"

  let result = make_auth_service.inject_into_kernel(input, "customer")

  should.be_true(has_line(result, "import app/http/middleware/load_customer"))
  should.be_true(has_line(result, "load_customer.run,"))
}

// ------------------------------------------------------------- Kernel: Idempotency

pub fn kernel_injection_is_idempotent_test() {
  let input =
    "import app/http/middleware/load_session
import app/http/middleware/load_user
import wisp.{type Request, type Response}

pub fn handle() {
  case middleware_group {
    kernel.Web | _ -> {
      [
        load_session.run,
        load_user.run,
        // ...
      ]
      |> middleware.apply(req, ctx, router)
    }
  }
}"

  // inject_into_kernel should still add the import and entry since
  // idempotency is handled at the register_in_kernel level, but
  // the string transform itself is deterministic
  let result = make_auth_service.inject_into_kernel(input, "user")

  // Should have two load_user imports (the original + the injected one)
  // The register_in_kernel function handles idempotency before calling this
  let import_count =
    count_occurrences(result, "import app/http/middleware/load_user")
  should.equal(import_count, 2)
}

// ------------------------------------------------------------- App: Field Injection (Typed Model)

pub fn adds_typed_field_to_app_test() {
  let input =
    "import gleam/option.{type Option}

pub type App {
  App(
    db: DbPool,
  )
}"

  let result = make_auth_service.inject_into_app(input, "user", "main")

  should.be_true(has_line(result, "user: Option(user.User),"))
}

pub fn app_adds_repository_import_test() {
  let input =
    "import gleam/option.{type Option}

pub type App {
  App(
    db: DbPool,
  )
}"

  let result = make_auth_service.inject_into_app(input, "user", "main")

  should.be_true(has_line(result, "import database/main/models/user/gen/user"))
}

pub fn app_field_injection_with_custom_model_test() {
  let input =
    "import gleam/option.{type Option}

pub type App {
  App(
    db: DbPool,
  )
}"

  let result = make_auth_service.inject_into_app(input, "customer", "postgres")

  should.be_true(has_line(result, "customer: Option(customer.Customer),"))
  should.be_true(has_line(
    result,
    "import database/postgres/models/customer/gen/customer",
  ))
}

pub fn app_field_added_after_last_existing_field_test() {
  let input =
    "import gleam/option.{type Option}

pub type App {
  App(
    cache: CachePool,
    db: DbPool,
  )
}"

  let result = make_auth_service.inject_into_app(input, "user", "main")
  let lines = string.split(result, "\n")

  let db_idx = find_line_index(lines, "db: DbPool,", 0)
  let user_idx = find_line_index(lines, "user: Option(user.User),", 0)

  // user field should come right after db field
  should.be_true(user_idx == db_idx + 1)
}

pub fn app_adds_option_import_when_missing_test() {
  let input =
    "pub type App {
  App(
    db: DbPool,
  )
}"

  let result = make_auth_service.inject_into_app(input, "user", "main")

  should.be_true(has_line(result, "import gleam/option.{type Option}"))
  should.be_true(has_line(result, "user: Option(user.User),"))
}

pub fn app_preserves_existing_option_import_test() {
  let input =
    "import gleam/option.{type Option}

pub type App {
  App(
    db: DbPool,
  )
}"

  let result = make_auth_service.inject_into_app(input, "user", "main")

  // Should have exactly one Option import
  let count = count_occurrences(result, "import gleam/option")
  should.equal(count, 1)
}

// ------------------------------------------------------------- App: Single-Line Constructor

pub fn app_injection_works_on_single_line_constructor_test() {
  let input =
    "import gleam/option.{type Option}

pub type App {
  App(cache: CachePool, db: DbPool)
}"

  let result = make_auth_service.inject_into_app(input, "user", "main")

  should.be_true(has_line(result, "user: Option(user.User),"))
}

// ------------------------------------------------------------- App Start: Field Injection

pub fn adds_none_field_to_app_start_test() {
  let input =
    "import app/app

pub fn start() -> app.App {
  let db = postgres.start(\"main\")

  app.App(
    db: db,
    // ...
  )
}"

  let result = make_auth_service.inject_into_app_start(input, "user")

  should.be_true(has_line(result, "user: option.None,"))
}

pub fn app_start_adds_option_import_when_missing_test() {
  let input =
    "import app/app

pub fn start() -> app.App {
  app.App(
    db: db,
    // ...
  )
}"

  let result = make_auth_service.inject_into_app_start(input, "user")

  should.be_true(has_line(result, "import gleam/option"))
}

pub fn app_start_preserves_existing_option_import_test() {
  let input =
    "import app/app
import gleam/option

pub fn start() -> app.App {
  app.App(
    db: db,
    // ...
  )
}"

  let result = make_auth_service.inject_into_app_start(input, "user")

  let count = count_occurrences(result, "import gleam/option")
  should.equal(count, 1)
}

pub fn app_start_field_after_last_field_test() {
  let input =
    "import app/app

pub fn start() -> app.App {
  app.App(
    cache: cache,
    db: db,
    // ...
  )
}"

  let result = make_auth_service.inject_into_app_start(input, "user")
  let lines = string.split(result, "\n")

  let db_idx = find_line_index(lines, "db: db,", 0)
  let user_idx = find_line_index(lines, "user: option.None,", 0)

  should.be_true(user_idx == db_idx + 1)
}

pub fn app_start_works_on_single_line_constructor_test() {
  let input =
    "import app/app

pub fn start() -> app.App {
  let db = postgres.start(\"main\")
  app.App(cache: cache, db: db)
}"

  let result = make_auth_service.inject_into_app_start(input, "user")

  should.be_true(has_line(result, "user: option.None,"))
}

// ------------------------------------------------------------- Helpers

fn has_line(content: String, needle: String) -> Bool {
  find_line_index(string.split(content, "\n"), needle, 0) >= 0
}

fn find_line_index(lines: List(String), needle: String, current: Int) -> Int {
  case lines {
    [] -> -1
    [line, ..rest] ->
      case string.contains(line, needle) {
        True -> current
        False -> find_line_index(rest, needle, current + 1)
      }
  }
}

fn count_occurrences(haystack: String, needle: String) -> Int {
  case string.split_once(haystack, needle) {
    Ok(#(_, rest)) -> 1 + count_occurrences(rest, needle)
    Error(_) -> 0
  }
}
