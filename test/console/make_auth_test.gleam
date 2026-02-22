import gleam/string
import gleeunit/should
import glimr/internal/services/make_auth_service

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

// ------------------------------------------------------------- Context: Field Injection (Typed Model)

pub fn adds_typed_field_to_context_test() {
  let input =
    "import gleam/option.{type Option}
import glimr/session/session.{type Session}

pub type Context {
  Context(
    session: Session,
  )
}"

  let result = make_auth_service.inject_into_context(input, "user", "main")

  should.be_true(has_line(result, "user: Option(user.User),"))
}

pub fn context_adds_repository_import_test() {
  let input =
    "import gleam/option.{type Option}
import glimr/session/session.{type Session}

pub type Context {
  Context(
    session: Session,
  )
}"

  let result = make_auth_service.inject_into_context(input, "user", "main")

  should.be_true(has_line(result, "import data/main/models/user/gen/user"))
}

pub fn context_field_injection_with_custom_model_test() {
  let input =
    "import gleam/option.{type Option}
import glimr/session/session.{type Session}

pub type Context {
  Context(
    session: Session,
  )
}"

  let result =
    make_auth_service.inject_into_context(input, "customer", "postgres")

  should.be_true(has_line(result, "customer: Option(customer.Customer),"))
  should.be_true(has_line(
    result,
    "import data/postgres/models/customer/gen/customer",
  ))
}

pub fn context_field_added_after_last_existing_field_test() {
  let input =
    "import gleam/option.{type Option}
import glimr/session/session.{type Session}

pub type Context {
  Context(
    cache: CacheContext,
    db: DbContext,
    session: Session,
  )
}"

  let result = make_auth_service.inject_into_context(input, "user", "main")
  let lines = string.split(result, "\n")

  let session_idx = find_line_index(lines, "session: Session,", 0)
  let user_idx = find_line_index(lines, "user: Option(user.User),", 0)

  // user field should come right after session field
  should.be_true(user_idx == session_idx + 1)
}

pub fn context_adds_option_import_when_missing_test() {
  let input =
    "import glimr/session/session.{type Session}

pub type Context {
  Context(
    session: Session,
  )
}"

  let result = make_auth_service.inject_into_context(input, "user", "main")

  should.be_true(has_line(result, "import gleam/option.{type Option}"))
  should.be_true(has_line(result, "user: Option(user.User),"))
}

pub fn context_preserves_existing_option_import_test() {
  let input =
    "import gleam/option.{type Option}
import glimr/session/session.{type Session}

pub type Context {
  Context(
    session: Session,
  )
}"

  let result = make_auth_service.inject_into_context(input, "user", "main")

  // Should have exactly one Option import
  let count = count_occurrences(result, "import gleam/option")
  should.equal(count, 1)
}

// ------------------------------------------------------------- Context: Single-Line Constructor

pub fn context_injection_works_on_single_line_constructor_test() {
  let input =
    "import gleam/option.{type Option}
import glimr/session/session.{type Session}

pub type Context {
  Context(cache: CacheContext, db: DbContext, session: Session)
}"

  let result = make_auth_service.inject_into_context(input, "user", "main")

  should.be_true(has_line(result, "user: Option(user.User),"))
}

// ------------------------------------------------------------- Ctx Provider: Field Injection

pub fn adds_none_field_to_ctx_provider_test() {
  let input =
    "import app/http/context/ctx.{type Context}
import app/http/context/ctx_db

pub fn register() -> Context {
  let db = ctx_db.load()

  ctx.Context(
    db: db,
    // ...
  )
}"

  let result = make_auth_service.inject_into_ctx_provider(input, "user")

  should.be_true(has_line(result, "user: option.None,"))
}

pub fn ctx_provider_adds_option_import_when_missing_test() {
  let input =
    "import app/http/context/ctx.{type Context}

pub fn register() -> Context {
  ctx.Context(
    db: db,
    // ...
  )
}"

  let result = make_auth_service.inject_into_ctx_provider(input, "user")

  should.be_true(has_line(result, "import gleam/option"))
}

pub fn ctx_provider_preserves_existing_option_import_test() {
  let input =
    "import app/http/context/ctx.{type Context}
import gleam/option

pub fn register() -> Context {
  ctx.Context(
    db: db,
    // ...
  )
}"

  let result = make_auth_service.inject_into_ctx_provider(input, "user")

  let count = count_occurrences(result, "import gleam/option")
  should.equal(count, 1)
}

pub fn ctx_provider_field_after_last_field_test() {
  let input =
    "import app/http/context/ctx.{type Context}

pub fn register() -> Context {
  ctx.Context(
    cache: cache,
    db: db,
    session: session,
    // ...
  )
}"

  let result = make_auth_service.inject_into_ctx_provider(input, "user")
  let lines = string.split(result, "\n")

  let session_idx = find_line_index(lines, "session: session,", 0)
  let user_idx = find_line_index(lines, "user: option.None,", 0)

  should.be_true(user_idx == session_idx + 1)
}

pub fn ctx_provider_works_on_single_line_constructor_test() {
  let input =
    "import app/http/context/ctx.{type Context}

pub fn register() -> Context {
  let db = ctx_db.load()
  ctx.Context(cache: cache, db: db, session: session)
}"

  let result = make_auth_service.inject_into_ctx_provider(input, "user")

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
