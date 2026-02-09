import gleeunit/should
import glimr/loom/registry
import simplifile

// Test registry path - we'll clean this up after tests
const test_registry_path = "priv/storage/framework/loom/modules.json"

// ------------------------------------------------------------- Setup/Teardown

fn cleanup() {
  let _ = simplifile.delete(test_registry_path)
  Nil
}

// ------------------------------------------------------------- register_module Tests

pub fn register_module_creates_registry_test() {
  cleanup()

  registry.register_module(
    "compiled/loom/counter",
    "src/views/counter.loom.html",
  )
  |> should.be_ok()

  // Verify file was created
  simplifile.is_file(test_registry_path)
  |> should.equal(Ok(True))

  cleanup()
}

pub fn register_module_adds_to_registry_test() {
  cleanup()

  registry.register_module(
    "compiled/loom/counter",
    "src/views/counter.loom.html",
  )
  |> should.be_ok()

  registry.is_valid_module("compiled/loom/counter")
  |> should.equal(True)

  cleanup()
}

pub fn register_multiple_modules_test() {
  cleanup()

  registry.register_module(
    "compiled/loom/counter",
    "src/views/counter.loom.html",
  )
  |> should.be_ok()

  registry.register_module("compiled/loom/todo", "src/views/todo.loom.html")
  |> should.be_ok()

  registry.is_valid_module("compiled/loom/counter")
  |> should.equal(True)

  registry.is_valid_module("compiled/loom/todo")
  |> should.equal(True)

  cleanup()
}

pub fn register_module_updates_existing_test() {
  cleanup()

  registry.register_module("compiled/loom/counter", "src/views/old.loom.html")
  |> should.be_ok()

  registry.register_module("compiled/loom/counter", "src/views/new.loom.html")
  |> should.be_ok()

  // Should still be valid
  registry.is_valid_module("compiled/loom/counter")
  |> should.equal(True)

  // Check we only have one entry
  registry.list_modules()
  |> should.equal([
    registry.ModuleEntry("compiled/loom/counter", "src/views/new.loom.html"),
  ])

  cleanup()
}

// ------------------------------------------------------------- is_valid_module Tests

pub fn is_valid_module_returns_false_for_unregistered_test() {
  cleanup()

  registry.is_valid_module("compiled/loom/nonexistent")
  |> should.equal(False)

  cleanup()
}

pub fn is_valid_module_returns_false_for_empty_registry_test() {
  cleanup()

  registry.is_valid_module("compiled/loom/counter")
  |> should.equal(False)

  cleanup()
}

pub fn is_valid_module_returns_true_for_registered_test() {
  cleanup()

  registry.register_module(
    "compiled/loom/counter",
    "src/views/counter.loom.html",
  )
  |> should.be_ok()

  registry.is_valid_module("compiled/loom/counter")
  |> should.equal(True)

  cleanup()
}

pub fn is_valid_module_rejects_similar_names_test() {
  cleanup()

  registry.register_module(
    "compiled/loom/counter",
    "src/views/counter.loom.html",
  )
  |> should.be_ok()

  // Similar but not exact - should be rejected
  registry.is_valid_module("compiled/loom/counter2")
  |> should.equal(False)

  registry.is_valid_module("compiled/loom/count")
  |> should.equal(False)

  registry.is_valid_module("compiled/loom/counters")
  |> should.equal(False)

  cleanup()
}

// ------------------------------------------------------------- unregister_module Tests

pub fn unregister_module_removes_from_registry_test() {
  cleanup()

  registry.register_module(
    "compiled/loom/counter",
    "src/views/counter.loom.html",
  )
  |> should.be_ok()

  registry.is_valid_module("compiled/loom/counter")
  |> should.equal(True)

  registry.unregister_module("compiled/loom/counter")
  |> should.be_ok()

  registry.is_valid_module("compiled/loom/counter")
  |> should.equal(False)

  cleanup()
}

pub fn unregister_module_leaves_others_intact_test() {
  cleanup()

  registry.register_module(
    "compiled/loom/counter",
    "src/views/counter.loom.html",
  )
  |> should.be_ok()

  registry.register_module("compiled/loom/todo", "src/views/todo.loom.html")
  |> should.be_ok()

  registry.unregister_module("compiled/loom/counter")
  |> should.be_ok()

  registry.is_valid_module("compiled/loom/counter")
  |> should.equal(False)

  registry.is_valid_module("compiled/loom/todo")
  |> should.equal(True)

  cleanup()
}

pub fn unregister_nonexistent_module_succeeds_test() {
  cleanup()

  // Should not error when unregistering something that doesn't exist
  registry.unregister_module("compiled/loom/nonexistent")
  |> should.be_ok()

  cleanup()
}

// ------------------------------------------------------------- clear_registry Tests

pub fn clear_registry_removes_all_modules_test() {
  cleanup()

  registry.register_module(
    "compiled/loom/counter",
    "src/views/counter.loom.html",
  )
  |> should.be_ok()

  registry.register_module("compiled/loom/todo", "src/views/todo.loom.html")
  |> should.be_ok()

  registry.clear_registry()
  |> should.be_ok()

  registry.is_valid_module("compiled/loom/counter")
  |> should.equal(False)

  registry.is_valid_module("compiled/loom/todo")
  |> should.equal(False)

  registry.list_modules()
  |> should.equal([])

  cleanup()
}

// ------------------------------------------------------------- list_modules Tests

pub fn list_modules_returns_empty_for_no_registry_test() {
  cleanup()

  registry.list_modules()
  |> should.equal([])

  cleanup()
}

pub fn list_modules_returns_all_registered_test() {
  cleanup()

  registry.register_module(
    "compiled/loom/counter",
    "src/views/counter.loom.html",
  )
  |> should.be_ok()

  registry.register_module("compiled/loom/todo", "src/views/todo.loom.html")
  |> should.be_ok()

  let modules = registry.list_modules()

  // Should have 2 modules
  case modules {
    [_, _] -> should.be_ok(Ok(Nil))
    _ -> should.fail()
  }

  cleanup()
}

// ------------------------------------------------------------- Security Tests

pub fn rejects_arbitrary_module_names_test() {
  cleanup()

  // Only register one specific module
  registry.register_module(
    "compiled/loom/counter",
    "src/views/counter.loom.html",
  )
  |> should.be_ok()

  // Attacker tries various module names - all should be rejected
  registry.is_valid_module("gleam/io")
  |> should.equal(False)

  registry.is_valid_module("glimr/internal/secrets")
  |> should.equal(False)

  registry.is_valid_module("../../../etc/passwd")
  |> should.equal(False)

  registry.is_valid_module("")
  |> should.equal(False)

  cleanup()
}
