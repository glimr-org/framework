import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleeunit/should
import glimr/db/connection
import glimr/db/pool
import sqlight

// ------------------------------------------------------------- Basic Checkout/Checkin

pub fn basic_checkout_checkin_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 2)
  let assert Ok(p) = pool.start(config)

  // Checkout a connection
  let assert Ok(conn) = pool.checkout(p)

  // Verify it's a valid SQLite connection by running a simple query
  let raw_conn = connection.to_sqlight(conn)
  let assert Ok(_) = sqlight.exec("SELECT 1", raw_conn)

  // Release the connection
  pool.release(p, conn)

  // Should be able to checkout again
  let assert Ok(conn2) = pool.checkout(p)
  pool.release(p, conn2)

  pool.stop(p)
}

// ------------------------------------------------------------- Connection Returned on Normal Release

pub fn get_connection_returns_connection_to_pool_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 1)
  let assert Ok(p) = pool.start(config)

  // Use get_connection which automatically returns connection
  let result =
    pool.get_connection(p, fn(conn) {
      let raw_conn = connection.to_sqlight(conn)
      let assert Ok(_) = sqlight.exec("SELECT 1", raw_conn)
      "success"
    })

  result
  |> should.equal("success")

  // Connection should be back in pool - can checkout again
  let assert Ok(conn) = pool.checkout(p)
  pool.release(p, conn)

  pool.stop(p)
}

pub fn get_connection_or_returns_connection_to_pool_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 1)
  let assert Ok(p) = pool.start(config)

  // Use get_connection_or which automatically returns connection
  let result =
    pool.get_connection_or(p, fn(conn) {
      let raw_conn = connection.to_sqlight(conn)
      case sqlight.exec("SELECT 1", raw_conn) {
        Ok(_) -> Ok("success")
        Error(_) -> Error(connection.QueryError("failed"))
      }
    })

  result
  |> should.equal(Ok("success"))

  // Connection should be back in pool - can checkout again
  let assert Ok(conn) = pool.checkout(p)
  pool.release(p, conn)

  pool.stop(p)
}

// ------------------------------------------------------------- Connection Reclaimed on Process Crash

pub fn connection_reclaimed_when_process_crashes_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 1)
  let assert Ok(p) = pool.start(config)

  // Spawn a process that checks out the only connection and then crashes
  let parent: Subject(String) = process.new_subject()

  let child =
    spawn_process(fn() {
      let assert Ok(_conn) = pool.checkout(p)
      // Signal parent we have the connection
      process.send(parent, "checked_out")
      // Wait a bit then crash without checking in
      process.sleep(50)
      panic as "simulated crash"
    })

  // Wait for child to checkout
  let assert Ok("checked_out") = process.receive(parent, 1000)

  // Kill the child process
  let _ = exit_process(child, kill_atom())

  // Give the pool time to receive ETS-TRANSFER and reclaim
  // The pool needs time to process the ETS-TRANSFER message
  process.sleep(50)

  // Now we should be able to checkout the connection again
  // (it was reclaimed by the pool)
  let assert Ok(conn) = pool.checkout(p)

  // Verify the connection still works
  let raw_conn = connection.to_sqlight(conn)
  let assert Ok(_) = sqlight.exec("SELECT 1", raw_conn)

  pool.release(p, conn)
  pool.stop(p)
}

pub fn connection_reclaimed_when_process_exits_normally_without_checkin_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 1)
  let assert Ok(p) = pool.start(config)

  // Spawn a process that checks out and exits without checking in
  let parent: Subject(String) = process.new_subject()

  let _child =
    spawn_process(fn() {
      let assert Ok(_conn) = pool.checkout(p)
      process.send(parent, "checked_out")
      // Exit normally without checkin
      Nil
    })

  // Wait for child to checkout and exit
  let assert Ok("checked_out") = process.receive(parent, 1000)

  // Give the pool time to receive ETS-TRANSFER
  // The pool needs time to process the ETS-TRANSFER message
  process.sleep(50)

  // Connection should be reclaimed
  let assert Ok(conn) = pool.checkout(p)
  pool.release(p, conn)

  pool.stop(p)
}

// ------------------------------------------------------------- Pool Exhaustion

pub fn pool_exhaustion_returns_error_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 1)
  let assert Ok(p) = pool.start(config)

  // Checkout the only connection
  let assert Ok(conn1) = pool.checkout(p)

  // Try to checkout another - should fail (pool exhausted)
  // Note: The pool uses a timeout, so this should return an error
  let result = pool.checkout(p)

  case result {
    Error(_) -> should.be_true(True)
    Ok(_) -> should.fail()
  }

  // Release the first connection
  pool.release(p, conn1)

  // Now checkout should work again
  let assert Ok(conn2) = pool.checkout(p)
  pool.release(p, conn2)

  pool.stop(p)
}

// ------------------------------------------------------------- Multiple Concurrent Checkouts

pub fn multiple_concurrent_checkouts_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 3)
  let assert Ok(p) = pool.start(config)

  // Checkout all 3 connections
  let assert Ok(conn1) = pool.checkout(p)
  let assert Ok(conn2) = pool.checkout(p)
  let assert Ok(conn3) = pool.checkout(p)

  // All connections should work
  let assert Ok(_) = sqlight.exec("SELECT 1", connection.to_sqlight(conn1))
  let assert Ok(_) = sqlight.exec("SELECT 2", connection.to_sqlight(conn2))
  let assert Ok(_) = sqlight.exec("SELECT 3", connection.to_sqlight(conn3))

  // Release all
  pool.release(p, conn1)
  pool.release(p, conn2)
  pool.release(p, conn3)

  pool.stop(p)
}

pub fn concurrent_processes_can_use_pool_test() {
  let config = connection.sqlite_config(":memory:", pool_size: 3)
  let assert Ok(p) = pool.start(config)

  let parent: Subject(Int) = process.new_subject()

  // Spawn 3 processes that each checkout, use, and release a connection
  list.range(1, 3)
  |> list.each(fn(i) {
    spawn_process(fn() {
      pool.get_connection(p, fn(conn) {
        let raw_conn = connection.to_sqlight(conn)
        let assert Ok(_) = sqlight.exec("SELECT 1", raw_conn)
        process.send(parent, i)
      })
    })
    Nil
  })

  // Wait for all 3 to complete
  let assert Ok(_) = process.receive(parent, 1000)
  let assert Ok(_) = process.receive(parent, 1000)
  let assert Ok(_) = process.receive(parent, 1000)

  // All connections should be back in pool
  let assert Ok(conn1) = pool.checkout(p)
  let assert Ok(conn2) = pool.checkout(p)
  let assert Ok(conn3) = pool.checkout(p)

  pool.release(p, conn1)
  pool.release(p, conn2)
  pool.release(p, conn3)

  pool.stop(p)
}

// ------------------------------------------------------------- FFI Helpers

@external(erlang, "erlang", "spawn")
fn spawn_process(func: fn() -> a) -> Pid

@external(erlang, "erlang", "exit")
fn exit_process(pid: Pid, reason: Dynamic) -> Dynamic

@external(erlang, "pool_test_ffi", "kill_atom")
fn kill_atom() -> Dynamic
