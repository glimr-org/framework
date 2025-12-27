import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleeunit/should
import glimr/db/connection
import glimr/db/db
import glimr/db/pool
import glimr/db/query

// ------------------------------------------------------------- Transaction Commits on Success

pub fn transaction_commits_on_success_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 1)
  let assert Ok(p) = pool.start(config)

  // Create a table
  pool.get_connection(p, fn(conn) {
    let assert Ok(_) =
      query.execute(
        conn,
        "CREATE TABLE test_tx (id INTEGER PRIMARY KEY, name TEXT)",
        [],
      )
    Nil
  })

  // Insert in a transaction
  let result =
    db.transaction(p, 0, fn(conn) {
      query.execute(
        conn,
        "INSERT INTO test_tx (id, name) VALUES (1, 'Alice')",
        [],
      )
    })

  result
  |> should.be_ok()

  // Verify the data was committed
  let name =
    pool.get_connection(p, fn(conn) {
      let decoder = decode.at([0], decode.string)
      let assert Ok(query.QueryResult(count: 1, rows: [name])) =
        query.select(conn, "SELECT name FROM test_tx WHERE id = 1", [], decoder)
      name
    })
  name
  |> should.equal("Alice")

  pool.stop(p)
}

// ------------------------------------------------------------- Transaction Rolls Back on Error

pub fn transaction_rolls_back_on_error_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 1)
  let assert Ok(p) = pool.start(config)

  // Create a table with initial data
  pool.get_connection(p, fn(conn) {
    let assert Ok(_) =
      query.execute(
        conn,
        "CREATE TABLE test_tx (id INTEGER PRIMARY KEY, name TEXT)",
        [],
      )
    let assert Ok(_) =
      query.execute(
        conn,
        "INSERT INTO test_tx (id, name) VALUES (1, 'Alice')",
        [],
      )
    Nil
  })

  // Try to update in a transaction that fails
  let result =
    db.transaction(p, 0, fn(conn) {
      // Update the name
      let assert Ok(_) =
        query.execute(conn, "UPDATE test_tx SET name = 'Bob' WHERE id = 1", [])

      // Now return an error to trigger rollback
      Error(connection.QueryError("Intentional error"))
    })

  result
  |> should.be_error()

  // Verify the data was NOT changed (rolled back)
  let name =
    pool.get_connection(p, fn(conn) {
      let decoder = decode.at([0], decode.string)
      let assert Ok(query.QueryResult(count: 1, rows: [name])) =
        query.select(conn, "SELECT name FROM test_tx WHERE id = 1", [], decoder)
      name
    })
  name
  |> should.equal("Alice")

  pool.stop(p)
}

// ------------------------------------------------------------- Transaction Sees Own Changes

pub fn transaction_sees_own_uncommitted_changes_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 1)
  let assert Ok(p) = pool.start(config)

  // Create a table
  pool.get_connection(p, fn(conn) {
    let assert Ok(_) =
      query.execute(
        conn,
        "CREATE TABLE test_tx (id INTEGER PRIMARY KEY, name TEXT)",
        [],
      )
    Nil
  })

  // Insert and read within the same transaction
  let result =
    db.transaction(p, 0, fn(conn) {
      let assert Ok(_) =
        query.execute(
          conn,
          "INSERT INTO test_tx (id, name) VALUES (1, 'Alice')",
          [],
        )

      // Read within the same transaction - should see the insert
      let decoder = decode.at([0], decode.string)
      let assert Ok(query.QueryResult(count: 1, rows: [name])) =
        query.select(conn, "SELECT name FROM test_tx WHERE id = 1", [], decoder)

      Ok(name)
    })

  let assert Ok(name) = result
  name
  |> should.equal("Alice")

  pool.stop(p)
}

// ------------------------------------------------------------- Multiple Operations in Transaction

pub fn multiple_operations_in_transaction_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 1)
  let assert Ok(p) = pool.start(config)

  // Create a table
  pool.get_connection(p, fn(conn) {
    let assert Ok(_) =
      query.execute(
        conn,
        "CREATE TABLE test_tx (id INTEGER PRIMARY KEY, value INTEGER)",
        [],
      )
    Nil
  })

  // Do multiple operations in one transaction
  let result =
    db.transaction(p, 0, fn(conn) {
      let assert Ok(_) =
        query.execute(
          conn,
          "INSERT INTO test_tx (id, value) VALUES (1, 10)",
          [],
        )
      let assert Ok(_) =
        query.execute(
          conn,
          "INSERT INTO test_tx (id, value) VALUES (2, 20)",
          [],
        )
      let assert Ok(_) =
        query.execute(
          conn,
          "UPDATE test_tx SET value = value + 5 WHERE id = 1",
          [],
        )

      // Return sum of values
      let decoder = decode.at([0], decode.int)
      let assert Ok(query.QueryResult(count: 1, rows: [sum])) =
        query.select(conn, "SELECT SUM(value) FROM test_tx", [], decoder)

      Ok(sum)
    })

  let assert Ok(sum) = result
  // 15 (10+5) + 20 = 35
  sum
  |> should.equal(35)

  // Verify outside transaction
  let sum_outside =
    pool.get_connection(p, fn(conn) {
      let decoder = decode.at([0], decode.int)
      let assert Ok(query.QueryResult(count: 1, rows: [sum])) =
        query.select(conn, "SELECT SUM(value) FROM test_tx", [], decoder)
      sum
    })
  sum_outside
  |> should.equal(35)

  pool.stop(p)
}

// ------------------------------------------------------------- Rollback Undoes All Changes

pub fn rollback_undoes_all_changes_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 1)
  let assert Ok(p) = pool.start(config)

  // Create table with initial data
  pool.get_connection(p, fn(conn) {
    let assert Ok(_) =
      query.execute(
        conn,
        "CREATE TABLE test_tx (id INTEGER PRIMARY KEY, value INTEGER)",
        [],
      )
    let assert Ok(_) =
      query.execute(conn, "INSERT INTO test_tx (id, value) VALUES (1, 100)", [])
    Nil
  })

  // Do multiple operations then fail
  let result =
    db.transaction(p, 0, fn(conn) {
      let assert Ok(_) =
        query.execute(conn, "UPDATE test_tx SET value = 200 WHERE id = 1", [])
      let assert Ok(_) =
        query.execute(
          conn,
          "INSERT INTO test_tx (id, value) VALUES (2, 300)",
          [],
        )

      // Fail the transaction
      Error(connection.QueryError("Intentional failure"))
    })

  result
  |> should.be_error()

  // Verify ALL changes were rolled back
  pool.get_connection(p, fn(conn) {
    // Original value should be intact
    let decoder = decode.at([0], decode.int)
    let assert Ok(query.QueryResult(count: 1, rows: [value])) =
      query.select(conn, "SELECT value FROM test_tx WHERE id = 1", [], decoder)
    value
    |> should.equal(100)

    // Second row should not exist
    let assert Ok(query.QueryResult(count: 0, rows: [])) =
      query.select(conn, "SELECT value FROM test_tx WHERE id = 2", [], decoder)

    Nil
  })

  pool.stop(p)
}

// ------------------------------------------------------------- Negative Retries Error

pub fn negative_retries_returns_error_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 1)
  let assert Ok(p) = pool.start(config)

  let result = db.transaction(p, -1, fn(_conn) { Ok("should not run") })

  case result {
    Error(connection.ConnectionError(msg)) ->
      msg
      |> should.equal("Transaction retries cannot be negative")
    _ -> should.fail()
  }

  pool.stop(p)
}

// ------------------------------------------------------------- Transaction Returns Value

pub fn transaction_returns_callback_value_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 1)
  let assert Ok(p) = pool.start(config)

  let result = db.transaction(p, 0, fn(_conn) { Ok(#("hello", 42, True)) })

  let assert Ok(value) = result
  value
  |> should.equal(#("hello", 42, True))

  pool.stop(p)
}

// ------------------------------------------------------------- Retry on Deadlock Error

pub fn transaction_retries_on_deadlock_error_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 1)
  let assert Ok(p) = pool.start(config)

  // Create a table
  pool.get_connection(p, fn(conn) {
    let assert Ok(_) =
      query.execute(
        conn,
        "CREATE TABLE test_tx (id INTEGER PRIMARY KEY, value INTEGER)",
        [],
      )
    Nil
  })

  // Use atomics counter to track call count
  let counter = new_counter()

  // Transaction that fails with deadlock first, then succeeds
  let result =
    db.transaction(p, 3, fn(conn) {
      let calls = increment_counter(counter)

      case calls {
        1 -> {
          // First call - simulate deadlock
          Error(connection.QueryError("database is locked"))
        }
        _ -> {
          // Second call - succeed
          let assert Ok(_) =
            query.execute(
              conn,
              "INSERT INTO test_tx (id, value) VALUES (1, 42)",
              [],
            )
          Ok("success")
        }
      }
    })

  let assert Ok(value) = result
  value
  |> should.equal("success")

  // Verify the insert happened
  let db_value =
    pool.get_connection(p, fn(conn) {
      let decoder = decode.at([0], decode.int)
      let assert Ok(query.QueryResult(count: 1, rows: [value])) =
        query.select(
          conn,
          "SELECT value FROM test_tx WHERE id = 1",
          [],
          decoder,
        )
      value
    })
  db_value
  |> should.equal(42)

  pool.stop(p)
}

pub fn transaction_retries_on_busy_error_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 1)
  let assert Ok(p) = pool.start(config)

  let counter = new_counter()

  let result =
    db.transaction(p, 2, fn(_conn) {
      let calls = increment_counter(counter)

      case calls {
        1 -> Error(connection.QueryError("SQLITE_BUSY"))
        _ -> Ok("recovered")
      }
    })

  let assert Ok(value) = result
  value
  |> should.equal("recovered")

  pool.stop(p)
}

pub fn transaction_exhausts_retries_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 1)
  let assert Ok(p) = pool.start(config)

  let counter = new_counter()

  // Always fail with deadlock - should exhaust retries
  let result =
    db.transaction(p, 2, fn(_conn) {
      let _ = increment_counter(counter)
      Error(connection.QueryError("database is locked"))
    })

  // Should fail after exhausting retries
  result
  |> should.be_error()

  // Should have been called 3 times (initial + 2 retries)
  let calls = get_counter(counter)
  calls
  |> should.equal(3)

  pool.stop(p)
}

pub fn transaction_no_retry_on_non_deadlock_error_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 1)
  let assert Ok(p) = pool.start(config)

  let counter = new_counter()

  // Fail with a non-deadlock error
  let result =
    db.transaction(p, 3, fn(_conn) {
      let _ = increment_counter(counter)
      Error(connection.QueryError("some other error"))
    })

  result
  |> should.be_error()

  // Should only have been called once (no retries for non-deadlock)
  let calls = get_counter(counter)
  calls
  |> should.equal(1)

  pool.stop(p)
}

// ------------------------------------------------------------- Helper Functions

/// Create an atomic counter starting at 0
@external(erlang, "atomics", "new")
fn atomics_new(size: Int, opts: List(Dynamic)) -> Dynamic

fn new_counter() -> Dynamic {
  atomics_new(1, [])
}

/// Increment counter and return new value
@external(erlang, "atomics", "add_get")
fn atomics_add_get(ref: Dynamic, index: Int, value: Int) -> Int

fn increment_counter(counter: Dynamic) -> Int {
  atomics_add_get(counter, 1, 1)
}

/// Get current counter value
@external(erlang, "atomics", "get")
fn atomics_get(ref: Dynamic, index: Int) -> Int

fn get_counter(counter: Dynamic) -> Int {
  atomics_get(counter, 1)
}
