import gleam/list
import gleam/string
import gleeunit/should
import glimr/internal/actions/compile_routes
import simplifile

const test_dir = "test/tmp/discover"

fn setup() {
  let _ = simplifile.delete(test_dir)
  let _ = simplifile.create_directory_all(test_dir)
  Nil
}

fn teardown() {
  let _ = simplifile.delete(test_dir)
  Nil
}

fn create_file(path: String) {
  let full = test_dir <> "/" <> path
  let parts = string.split(full, "/")
  let dir =
    parts
    |> list.take(list.length(parts) - 1)
    |> string.join("/")
  let _ = simplifile.create_directory_all(dir)
  let assert Ok(_) = simplifile.write(full, "// test controller")
  Nil
}

// ------------------------------------------------------------- Convention-based Discovery

pub fn discovers_controller_suffix_files_test() {
  setup()
  create_file("app/billing/invoice_controller.gleam")

  let files = compile_routes.discover_controller_files(test_dir)

  files
  |> list.any(fn(f) { string.contains(f, "invoice_controller.gleam") })
  |> should.be_true

  teardown()
}

pub fn discovers_controller_gleam_files_test() {
  setup()
  create_file("app/billing/controller.gleam")

  let files = compile_routes.discover_controller_files(test_dir)

  files
  |> list.any(fn(f) { string.contains(f, "billing/controller.gleam") })
  |> should.be_true

  teardown()
}

pub fn discovers_nested_ddd_controllers_test() {
  setup()
  create_file("app/billing/payments/payment_controller.gleam")
  create_file("app/users/user_controller.gleam")

  let files = compile_routes.discover_controller_files(test_dir)

  files |> list.length |> should.equal(2)

  teardown()
}

pub fn ignores_non_controller_gleam_files_test() {
  setup()
  create_file("app/billing/invoice.gleam")
  create_file("app/billing/helpers.gleam")

  let files = compile_routes.discover_controller_files(test_dir)

  files |> list.length |> should.equal(0)

  teardown()
}

// ------------------------------------------------------------- Legacy Discovery

pub fn discovers_legacy_controller_dir_files_test() {
  setup()
  create_file("app/http/controllers/user.gleam")
  create_file("app/http/controllers/post.gleam")

  let files = compile_routes.discover_controller_files(test_dir)

  files |> list.length |> should.equal(2)

  teardown()
}

pub fn legacy_files_without_controller_suffix_are_found_test() {
  setup()
  create_file("app/http/controllers/home.gleam")

  let files = compile_routes.discover_controller_files(test_dir)

  files
  |> list.any(fn(f) { string.contains(f, "home.gleam") })
  |> should.be_true

  teardown()
}

// ------------------------------------------------------------- Deduplication

pub fn deduplicates_files_matching_both_strategies_test() {
  setup()
  create_file("app/http/controllers/user_controller.gleam")

  let files = compile_routes.discover_controller_files(test_dir)

  // Should appear once, not twice
  files |> list.length |> should.equal(1)

  teardown()
}

// ------------------------------------------------------------- Mixed Discovery

pub fn discovers_both_legacy_and_convention_controllers_test() {
  setup()
  // Legacy: no _controller suffix, in controllers/ dir
  create_file("app/http/controllers/home.gleam")
  // Convention: _controller suffix, outside controllers/ dir
  create_file("app/billing/invoice_controller.gleam")
  // Convention: controller.gleam, outside controllers/ dir
  create_file("app/payments/controller.gleam")

  let files = compile_routes.discover_controller_files(test_dir)

  files |> list.length |> should.equal(3)

  teardown()
}

// ------------------------------------------------------------- Empty / Missing

pub fn returns_empty_for_missing_directory_test() {
  let files = compile_routes.discover_controller_files("nonexistent/path")

  files |> should.equal([])
}

pub fn returns_empty_for_empty_directory_test() {
  setup()

  let files = compile_routes.discover_controller_files(test_dir)

  files |> should.equal([])

  teardown()
}
