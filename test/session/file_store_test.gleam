import gleam/dict
import gleam/int
import gleeunit/should
import glimr/cache/file
import glimr/session/file_store
import glimr/session/store
import glimr/utils/unix_timestamp
import simplifile

const test_cache_path = "priv/test/session_cache"

const config_dir = "config"

const cache_config_file = "config/cache.toml"

const session_config_file = "config/session.toml"

fn setup_config() -> Nil {
  let _ = simplifile.create_directory_all(config_dir)
  let _ = simplifile.write(cache_config_file, "[stores.session_test]
  driver = \"file\"
  path = \"" <> test_cache_path <> "\"
")
  let _ =
    simplifile.write(
      session_config_file,
      "[session]
  table = \"sessions\"
  cookie = \"test_session\"
  lifetime = 120
  expire_on_close = false
",
    )
  clear_cache_config()
  clear_session_config()
  Nil
}

fn cleanup_config() -> Nil {
  let _ = simplifile.delete(cache_config_file)
  let _ = simplifile.delete(session_config_file)
  let _ = simplifile.delete(test_cache_path)
  clear_cache_config()
  clear_session_config()
  clear_session_store()
  Nil
}

fn with_clean_session(f: fn() -> a) -> a {
  let _ = simplifile.delete(test_cache_path)
  let _ = simplifile.create_directory_all(test_cache_path)

  setup_config()

  let pool = file.start("session_test")

  // Create and cache the session store
  let session = file_store.create(pool)
  store.cache_store(session)

  let result = f()

  cleanup_config()
  result
}

// ------------------------------------------------------------- Load Tests

pub fn load_nonexistent_session_returns_empty_test() {
  with_clean_session(fn() {
    let #(data, flash) = store.load("nonexistent-id")

    data |> should.equal(dict.new())
    flash |> should.equal(dict.new())
  })
}

// ------------------------------------------------------------- Save and Load Tests

pub fn save_and_load_data_test() {
  with_clean_session(fn() {
    let data =
      dict.new()
      |> dict.insert("user_id", "42")
      |> dict.insert("role", "admin")

    store.save("file-sess-1", data, dict.new())

    let #(loaded_data, loaded_flash) = store.load("file-sess-1")

    dict.get(loaded_data, "user_id") |> should.equal(Ok("42"))
    dict.get(loaded_data, "role") |> should.equal(Ok("admin"))
    loaded_flash |> should.equal(dict.new())
  })
}

pub fn save_and_load_flash_test() {
  with_clean_session(fn() {
    let flash =
      dict.new()
      |> dict.insert("success", "Saved!")
      |> dict.insert("info", "Note this")

    store.save("file-sess-2", dict.new(), flash)

    let #(loaded_data, loaded_flash) = store.load("file-sess-2")

    loaded_data |> should.equal(dict.new())
    dict.get(loaded_flash, "success") |> should.equal(Ok("Saved!"))
    dict.get(loaded_flash, "info") |> should.equal(Ok("Note this"))
  })
}

pub fn save_and_load_data_and_flash_test() {
  with_clean_session(fn() {
    let data =
      dict.new()
      |> dict.insert("user_id", "99")

    let flash =
      dict.new()
      |> dict.insert("warning", "Check your email")

    store.save("file-sess-3", data, flash)

    let #(loaded_data, loaded_flash) = store.load("file-sess-3")

    dict.get(loaded_data, "user_id") |> should.equal(Ok("99"))
    dict.get(loaded_flash, "warning") |> should.equal(Ok("Check your email"))
  })
}

pub fn save_overwrites_existing_session_test() {
  with_clean_session(fn() {
    let data1 =
      dict.new()
      |> dict.insert("key", "first")

    store.save("file-sess-4", data1, dict.new())

    let data2 =
      dict.new()
      |> dict.insert("key", "second")

    store.save("file-sess-4", data2, dict.new())

    let #(loaded_data, _) = store.load("file-sess-4")
    dict.get(loaded_data, "key") |> should.equal(Ok("second"))
  })
}

// ------------------------------------------------------------- Destroy Tests

pub fn destroy_removes_session_test() {
  with_clean_session(fn() {
    let data =
      dict.new()
      |> dict.insert("key", "value")

    store.save("file-sess-5", data, dict.new())

    // Verify it exists
    let #(loaded, _) = store.load("file-sess-5")
    dict.get(loaded, "key") |> should.equal(Ok("value"))

    // Destroy it
    store.destroy("file-sess-5")

    // Should be gone
    let #(loaded_after, _) = store.load("file-sess-5")
    loaded_after |> should.equal(dict.new())
  })
}

pub fn destroy_nonexistent_does_not_crash_test() {
  with_clean_session(fn() { store.destroy("nonexistent") })
}

// ------------------------------------------------------------- Expiration Tests

pub fn load_expired_session_returns_empty_test() {
  with_clean_session(fn() {
    let session_dir = test_cache_path <> "/sessions"
    let _ = simplifile.create_directory_all(session_dir)

    // Write a session file with an expired timestamp (far in the past)
    let expired_payload = "{\"_data\":{\"key\":\"expired\"},\"_flash\":{}}"
    let expired_content = "1000\n" <> expired_payload
    let _ = simplifile.write(session_dir <> "/expired-sess", expired_content)

    // Loading an expired session should return empty dicts,
    // even without GC having run
    let #(data, flash) = store.load("expired-sess")
    data |> should.equal(dict.new())
    flash |> should.equal(dict.new())
  })
}

pub fn load_valid_session_returns_data_test() {
  with_clean_session(fn() {
    let session_dir = test_cache_path <> "/sessions"
    let _ = simplifile.create_directory_all(session_dir)

    // Write a session file with a future expiration
    let future = unix_timestamp.now() + 7200
    let valid_payload = "{\"_data\":{\"key\":\"valid\"},\"_flash\":{}}"
    let valid_content = int.to_string(future) <> "\n" <> valid_payload
    let _ = simplifile.write(session_dir <> "/valid-sess", valid_content)

    // Valid session should load fine
    let #(data, _) = store.load("valid-sess")
    dict.get(data, "key") |> should.equal(Ok("valid"))
  })
}

// ------------------------------------------------------------- GC Tests

pub fn gc_does_not_crash_test() {
  with_clean_session(fn() { store.gc() })
}

pub fn gc_removes_expired_session_test() {
  with_clean_session(fn() {
    let session_dir = test_cache_path <> "/sessions"
    let _ = simplifile.create_directory_all(session_dir)

    // Write a session file with an expired timestamp (in the past)
    let expired_payload = "{\"_data\":{\"key\":\"expired\"},\"_flash\":{}}"
    let expired_content = "1000\n" <> expired_payload
    let _ = simplifile.write(session_dir <> "/expired-sess", expired_content)

    // Run gc — should remove the expired session file
    store.gc()

    // File should be deleted from disk
    let #(data_after, _) = store.load("expired-sess")
    data_after |> should.equal(dict.new())
  })
}

pub fn gc_preserves_valid_session_test() {
  with_clean_session(fn() {
    let session_dir = test_cache_path <> "/sessions"
    let _ = simplifile.create_directory_all(session_dir)

    // Write a session file with a future expiration
    let future = unix_timestamp.now() + 7200
    let valid_payload = "{\"_data\":{\"key\":\"valid\"},\"_flash\":{}}"
    let valid_content = int.to_string(future) <> "\n" <> valid_payload
    let _ = simplifile.write(session_dir <> "/valid-sess", valid_content)

    // Run gc — should NOT remove the valid session
    store.gc()

    // Should still be loadable
    let #(data, _) = store.load("valid-sess")
    dict.get(data, "key") |> should.equal(Ok("valid"))
  })
}

// ------------------------------------------------------------- Multiple Sessions

pub fn multiple_sessions_independent_test() {
  with_clean_session(fn() {
    let data_a =
      dict.new()
      |> dict.insert("user", "alice")

    let data_b =
      dict.new()
      |> dict.insert("user", "bob")

    store.save("file-sess-a", data_a, dict.new())
    store.save("file-sess-b", data_b, dict.new())

    let #(loaded_a, _) = store.load("file-sess-a")
    let #(loaded_b, _) = store.load("file-sess-b")

    dict.get(loaded_a, "user") |> should.equal(Ok("alice"))
    dict.get(loaded_b, "user") |> should.equal(Ok("bob"))
  })
}

// ------------------------------------------------------------- FFI Helpers

@external(erlang, "glimr_session_test_ffi", "clear_cache_config")
fn clear_cache_config() -> Nil

@external(erlang, "glimr_session_test_ffi", "clear_session_config")
fn clear_session_config() -> Nil

@external(erlang, "glimr_session_test_ffi", "clear_session_store")
fn clear_session_store() -> Nil
