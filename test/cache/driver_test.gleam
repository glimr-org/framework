import gleeunit/should
import glimr/cache/driver.{File, FileStore, Redis, RedisStore}

// ------------------------------------------------------------- store_type

pub fn store_type_file_test() {
  let store = FileStore(name: "main", path: "priv/cache")

  driver.store_type(store)
  |> should.equal(File)
}

pub fn store_type_redis_test() {
  let store =
    RedisStore(
      name: "redis",
      url: Ok("redis://localhost:6379"),
      pool_size: Ok(10),
    )

  driver.store_type(store)
  |> should.equal(Redis)
}

// ------------------------------------------------------------- store_name

pub fn store_name_file_test() {
  let store = FileStore(name: "cache", path: "priv/cache")

  driver.store_name(store)
  |> should.equal("cache")
}

pub fn store_name_redis_test() {
  let store =
    RedisStore(
      name: "session",
      url: Ok("redis://localhost:6379"),
      pool_size: Ok(5),
    )

  driver.store_name(store)
  |> should.equal("session")
}

// ------------------------------------------------------------- store_path

pub fn store_path_file_test() {
  let store = FileStore(name: "main", path: "priv/storage/framework/cache/data")

  driver.store_path(store)
  |> should.equal("priv/storage/framework/cache/data")
}

// ------------------------------------------------------------- find_by_name

pub fn find_by_name_file_store_test() {
  let stores = [
    FileStore(name: "main", path: "priv/cache"),
    RedisStore(
      name: "redis",
      url: Ok("redis://localhost:6379"),
      pool_size: Ok(10),
    ),
  ]

  let found = driver.find_by_name("main", stores)

  driver.store_name(found)
  |> should.equal("main")

  driver.store_type(found)
  |> should.equal(File)
}

pub fn find_by_name_redis_store_test() {
  let stores = [
    FileStore(name: "main", path: "priv/cache"),
    RedisStore(
      name: "session",
      url: Ok("redis://localhost:6379"),
      pool_size: Ok(10),
    ),
  ]

  let found = driver.find_by_name("session", stores)

  driver.store_name(found)
  |> should.equal("session")

  driver.store_type(found)
  |> should.equal(Redis)
}

pub fn find_by_name_first_in_list_test() {
  let stores = [
    FileStore(name: "primary", path: "priv/cache/primary"),
    FileStore(name: "secondary", path: "priv/cache/secondary"),
  ]

  let found = driver.find_by_name("primary", stores)

  driver.store_name(found)
  |> should.equal("primary")

  driver.store_path(found)
  |> should.equal("priv/cache/primary")
}

pub fn find_by_name_last_in_list_test() {
  let stores = [
    FileStore(name: "first", path: "priv/cache/first"),
    FileStore(name: "second", path: "priv/cache/second"),
    FileStore(name: "third", path: "priv/cache/third"),
  ]

  let found = driver.find_by_name("third", stores)

  driver.store_name(found)
  |> should.equal("third")
}
