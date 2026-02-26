import gleam/dynamic/decode
import gleam/json
import gleam/string
import gleeunit/should
import glimr/cache/cache.{
  type CachePool, NotFound, SerializationError,
}
import glimr/cache/file
import glimr/cache/file/cache as file_cache
import glimr/cache/file/pool
import simplifile

const test_cache_path = "priv/test/cache"

const config_dir = "config"

const config_file = "config/cache.toml"

fn setup_config() -> Nil {
  let _ = simplifile.create_directory_all(config_dir)
  let _ = simplifile.write(config_file, "[stores.test]
  driver = \"file\"
  path = \"" <> test_cache_path <> "\"
")
  Nil
}

fn cleanup_config() -> Nil {
  let _ = simplifile.delete(config_file)
  Nil
}

fn setup_test_pool() -> CachePool {
  // Clean up any existing test cache
  let _ = simplifile.delete(test_cache_path)
  let _ = simplifile.create_directory_all(test_cache_path)

  setup_config()
  file.start("test")
}

fn setup_internal_pool() -> pool.Pool {
  let _ = simplifile.delete(test_cache_path)
  let _ = simplifile.create_directory_all(test_cache_path)

  setup_config()
  file.start_pool("test")
}

fn cleanup_test_pool() {
  let _ = simplifile.delete(test_cache_path)
  cleanup_config()
}

// ------------------------------------------------------------- get/put

pub fn put_and_get_test() {
  let pool = setup_test_pool()

  cache.put(pool, "test_key", "test_value", 3600)
  |> should.be_ok()

  cache.get(pool, "test_key")
  |> should.be_ok()
  |> should.equal("test_value")

  cleanup_test_pool()
}

pub fn get_not_found_test() {
  let pool = setup_test_pool()

  cache.get(pool, "nonexistent_key")
  |> should.be_error()
  |> should.equal(NotFound)

  cleanup_test_pool()
}

pub fn put_overwrites_existing_test() {
  let pool = setup_test_pool()

  cache.put(pool, "key", "first_value", 3600)
  |> should.be_ok()

  cache.put(pool, "key", "second_value", 3600)
  |> should.be_ok()

  cache.get(pool, "key")
  |> should.be_ok()
  |> should.equal("second_value")

  cleanup_test_pool()
}

// ------------------------------------------------------------- put_forever

pub fn put_forever_test() {
  let pool = setup_test_pool()

  cache.put_forever(pool, "permanent_key", "permanent_value")
  |> should.be_ok()

  cache.get(pool, "permanent_key")
  |> should.be_ok()
  |> should.equal("permanent_value")

  cleanup_test_pool()
}

// ------------------------------------------------------------- forget

pub fn forget_existing_key_test() {
  let pool = setup_test_pool()

  cache.put(pool, "to_delete", "value", 3600)
  |> should.be_ok()

  cache.forget(pool, "to_delete")
  |> should.be_ok()

  cache.get(pool, "to_delete")
  |> should.be_error()
  |> should.equal(NotFound)

  cleanup_test_pool()
}

pub fn forget_nonexistent_key_test() {
  let pool = setup_test_pool()

  // Should not error when deleting non-existent key
  cache.forget(pool, "never_existed")
  |> should.be_ok()

  cleanup_test_pool()
}

// ------------------------------------------------------------- has

pub fn has_existing_key_test() {
  let pool = setup_test_pool()

  cache.put(pool, "exists", "value", 3600)
  |> should.be_ok()

  cache.has(pool, "exists")
  |> should.equal(True)

  cleanup_test_pool()
}

pub fn has_nonexistent_key_test() {
  let pool = setup_test_pool()

  cache.has(pool, "does_not_exist")
  |> should.equal(False)

  cleanup_test_pool()
}

// ------------------------------------------------------------- flush

pub fn flush_test() {
  let pool = setup_test_pool()

  cache.put(pool, "key1", "value1", 3600) |> should.be_ok()
  cache.put(pool, "key2", "value2", 3600) |> should.be_ok()
  cache.put(pool, "key3", "value3", 3600) |> should.be_ok()

  cache.flush(pool)
  |> should.be_ok()

  cache.has(pool, "key1") |> should.equal(False)
  cache.has(pool, "key2") |> should.equal(False)
  cache.has(pool, "key3") |> should.equal(False)

  cleanup_test_pool()
}

// ------------------------------------------------------------- pull

pub fn pull_existing_key_test() {
  let pool = setup_test_pool()

  cache.put(pool, "pull_key", "pull_value", 3600)
  |> should.be_ok()

  cache.pull(pool, "pull_key")
  |> should.be_ok()
  |> should.equal("pull_value")

  // Key should be gone after pull
  cache.has(pool, "pull_key")
  |> should.equal(False)

  cleanup_test_pool()
}

pub fn pull_nonexistent_key_test() {
  let pool = setup_test_pool()

  cache.pull(pool, "nonexistent")
  |> should.be_error()
  |> should.equal(NotFound)

  cleanup_test_pool()
}

// ------------------------------------------------------------- increment/decrement

pub fn increment_new_key_test() {
  let pool = setup_test_pool()

  cache.increment(pool, "counter", 1)
  |> should.be_ok()
  |> should.equal(1)

  cleanup_test_pool()
}

pub fn increment_existing_key_test() {
  let pool = setup_test_pool()

  cache.increment(pool, "counter", 1) |> should.be_ok()
  cache.increment(pool, "counter", 1) |> should.be_ok()

  cache.increment(pool, "counter", 1)
  |> should.be_ok()
  |> should.equal(3)

  cleanup_test_pool()
}

pub fn increment_by_amount_test() {
  let pool = setup_test_pool()

  cache.increment(pool, "counter", 5)
  |> should.be_ok()
  |> should.equal(5)

  cache.increment(pool, "counter", 10)
  |> should.be_ok()
  |> should.equal(15)

  cleanup_test_pool()
}

pub fn decrement_test() {
  let pool = setup_test_pool()

  cache.increment(pool, "counter", 10) |> should.be_ok()

  cache.decrement(pool, "counter", 3)
  |> should.be_ok()
  |> should.equal(7)

  cleanup_test_pool()
}

pub fn decrement_below_zero_test() {
  let pool = setup_test_pool()

  cache.decrement(pool, "counter", 5)
  |> should.be_ok()
  |> should.equal(-5)

  cleanup_test_pool()
}

// ------------------------------------------------------------- JSON operations

type User {
  User(name: String, age: Int)
}

fn user_encoder(user: User) -> json.Json {
  json.object([
    #("name", json.string(user.name)),
    #("age", json.int(user.age)),
  ])
}

fn user_decoder() -> decode.Decoder(User) {
  use name <- decode.field("name", decode.string)
  use age <- decode.field("age", decode.int)
  decode.success(User(name:, age:))
}

pub fn put_json_and_get_json_test() {
  let pool = setup_test_pool()
  let user = User(name: "Alice", age: 30)

  cache.put_json(pool, "user", user, user_encoder, 3600)
  |> should.be_ok()

  cache.get_json(pool, "user", user_decoder())
  |> should.be_ok()
  |> should.equal(user)

  cleanup_test_pool()
}

pub fn put_json_forever_test() {
  let pool = setup_test_pool()
  let user = User(name: "Bob", age: 25)

  cache.put_json_forever(pool, "permanent_user", user, user_encoder)
  |> should.be_ok()

  cache.get_json(pool, "permanent_user", user_decoder())
  |> should.be_ok()
  |> should.equal(user)

  cleanup_test_pool()
}

pub fn get_json_invalid_format_test() {
  let pool = setup_test_pool()

  // Store invalid JSON for User type
  cache.put(pool, "invalid", "not valid json", 3600)
  |> should.be_ok()

  cache.get_json(pool, "invalid", user_decoder())
  |> should.be_error()
  |> should.equal(SerializationError("Failed to decode JSON"))

  cleanup_test_pool()
}

// ------------------------------------------------------------- remember

pub fn remember_returns_cached_value_test() {
  let pool = setup_test_pool()

  cache.put(pool, "cached", "existing_value", 3600)
  |> should.be_ok()

  // Compute function should not be called
  cache.remember(pool, "cached", 3600, fn() { "computed_value" })
  |> should.equal("existing_value")

  cleanup_test_pool()
}

pub fn remember_computes_when_missing_test() {
  let pool = setup_test_pool()

  cache.remember(pool, "missing", 3600, fn() { "computed_value" })
  |> should.equal("computed_value")

  // Value should now be cached
  cache.get(pool, "missing")
  |> should.be_ok()
  |> should.equal("computed_value")

  cleanup_test_pool()
}

pub fn remember_forever_test() {
  let pool = setup_test_pool()

  cache.remember_forever(pool, "permanent", fn() { "computed" })
  |> should.equal("computed")

  cache.get(pool, "permanent")
  |> should.be_ok()
  |> should.equal("computed")

  cleanup_test_pool()
}

// ------------------------------------------------------------- remember_json

pub fn remember_json_returns_cached_test() {
  let pool = setup_test_pool()
  let user = User(name: "Cached", age: 40)

  cache.put_json(pool, "user_cached", user, user_encoder, 3600)
  |> should.be_ok()

  cache.remember_json(
    pool,
    "user_cached",
    3600,
    user_decoder(),
    user_encoder,
    fn() { User(name: "Computed", age: 99) },
  )
  |> should.equal(user)

  cleanup_test_pool()
}

pub fn remember_json_computes_when_missing_test() {
  let pool = setup_test_pool()
  let user = User(name: "New", age: 20)

  cache.remember_json(
    pool,
    "new_user",
    3600,
    user_decoder(),
    user_encoder,
    fn() { user },
  )
  |> should.equal(user)

  // Should now be cached
  cache.get_json(pool, "new_user", user_decoder())
  |> should.be_ok()
  |> should.equal(user)

  cleanup_test_pool()
}

// ------------------------------------------------------------- key_to_path

pub fn key_to_path_creates_nested_structure_test() {
  let pool = setup_internal_pool()

  let path = file_cache.key_to_path(pool, "test_key")

  // Path should contain the base path and 2-level directory structure
  path
  |> should.not_equal(test_cache_path <> "/test_key")

  // Path should be longer due to hash directories
  { string.length(path) > string.length(test_cache_path) + 10 }
  |> should.equal(True)

  cleanup_test_pool()
}

pub fn key_to_path_is_deterministic_test() {
  let pool = setup_internal_pool()

  let path1 = file_cache.key_to_path(pool, "same_key")
  let path2 = file_cache.key_to_path(pool, "same_key")

  path1
  |> should.equal(path2)

  cleanup_test_pool()
}

pub fn key_to_path_different_keys_different_paths_test() {
  let pool = setup_internal_pool()

  let path1 = file_cache.key_to_path(pool, "key1")
  let path2 = file_cache.key_to_path(pool, "key2")

  path1
  |> should.not_equal(path2)

  cleanup_test_pool()
}
