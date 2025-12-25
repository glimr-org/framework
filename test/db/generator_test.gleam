import gleam/option.{None}
import gleam/string
import gleeunit/should
import glimr/db/gen/generator
import glimr/db/gen/parser.{ParsedQuery}
import glimr/db/gen/parser/columns.{SelectedColumn}
import glimr/db/gen/schema_parser.{Column, Id, Int, String, Table}

// ------------------------------------------------------------- Query Function Generation

pub fn generate_produces_pool_and_wc_variants_test() {
  let table =
    Table("users", [
      Column("id", Id, False, None, None),
      Column("name", String, False, None, None),
    ])

  let parsed =
    ParsedQuery(
      tables: ["users"],
      columns: [
        SelectedColumn(table: None, name: "id", alias: None),
        SelectedColumn(table: None, name: "name", alias: None),
      ],
      params: [1],
      param_columns: [#(1, "id")],
    )

  let queries = [#("find", "SELECT id, name FROM users WHERE id = $1", parsed)]

  let result = generator.generate("user", table, queries)

  // Should contain the main function with Pool
  result
  |> string.contains("pub fn find(pool pool: Pool")
  |> should.be_true()

  // Should contain the _wc function with Connection
  result
  |> string.contains("pub fn find_wc(conn conn: Connection")
  |> should.be_true()
}

pub fn generate_list_query_produces_pool_and_wc_variants_test() {
  let table =
    Table("users", [
      Column("id", Id, False, None, None),
      Column("name", String, False, None, None),
    ])

  let parsed =
    ParsedQuery(
      tables: ["users"],
      columns: [
        SelectedColumn(table: None, name: "id", alias: None),
        SelectedColumn(table: None, name: "name", alias: None),
      ],
      params: [],
      param_columns: [],
    )

  let queries = [#("list_all", "SELECT id, name FROM users", parsed)]

  let result = generator.generate("user", table, queries)

  // Should contain the main function with Pool returning List
  result
  |> string.contains("pub fn list_all(pool pool: Pool")
  |> should.be_true()

  result
  |> string.contains(") -> Result(List(")
  |> should.be_true()

  // Should contain the _wc function with Connection
  result
  |> string.contains("pub fn list_all_wc(conn conn: Connection")
  |> should.be_true()
}

pub fn generate_execute_query_produces_pool_and_wc_variants_test() {
  let table =
    Table("users", [
      Column("id", Id, False, None, None),
      Column("name", String, False, None, None),
    ])

  let parsed =
    ParsedQuery(tables: ["users"], columns: [], params: [1], param_columns: [
      #(1, "id"),
    ])

  let queries = [#("delete", "DELETE FROM users WHERE id = $1", parsed)]

  let result = generator.generate("user", table, queries)

  // Should contain the main function with Pool
  result
  |> string.contains("pub fn delete(pool pool: Pool")
  |> should.be_true()

  // Should contain the _wc function with Connection
  result
  |> string.contains("pub fn delete_wc(conn conn: Connection")
  |> should.be_true()

  // Both should return Result(Int, DbError)
  result
  |> string.contains(") -> Result(Int, DbError)")
  |> should.be_true()
}

pub fn generate_main_function_uses_checkout_and_release_test() {
  let table =
    Table("users", [
      Column("id", Id, False, None, None),
      Column("name", String, False, None, None),
    ])

  let parsed =
    ParsedQuery(
      tables: ["users"],
      columns: [
        SelectedColumn(table: None, name: "id", alias: None),
        SelectedColumn(table: None, name: "name", alias: None),
      ],
      params: [1],
      param_columns: [#(1, "id")],
    )

  let queries = [#("find", "SELECT id, name FROM users WHERE id = $1", parsed)]

  let result = generator.generate("user", table, queries)

  // Main function should use pool.checkout
  result
  |> string.contains("pool.checkout(pool)")
  |> should.be_true()

  // Main function should use pool.release
  result
  |> string.contains("pool.release(pool, conn)")
  |> should.be_true()

  // Main function should call the _wc variant
  result
  |> string.contains("find_wc(conn: conn")
  |> should.be_true()
}

pub fn generate_imports_pool_test() {
  let table =
    Table("users", [
      Column("id", Id, False, None, None),
      Column("name", String, False, None, None),
    ])

  let parsed =
    ParsedQuery(
      tables: ["users"],
      columns: [SelectedColumn(table: None, name: "*", alias: None)],
      params: [],
      param_columns: [],
    )

  let queries = [#("list_all", "SELECT * FROM users", parsed)]

  let result = generator.generate("user", table, queries)

  // Should import Pool
  result
  |> string.contains("import glimr/db/pool.{type Pool}")
  |> should.be_true()
}

pub fn generate_wc_function_uses_connection_directly_test() {
  let table =
    Table("users", [
      Column("id", Id, False, None, None),
      Column("name", String, False, None, None),
    ])

  let parsed =
    ParsedQuery(
      tables: ["users"],
      columns: [
        SelectedColumn(table: None, name: "id", alias: None),
        SelectedColumn(table: None, name: "name", alias: None),
      ],
      params: [1],
      param_columns: [#(1, "id")],
    )

  let queries = [#("find", "SELECT id, name FROM users WHERE id = $1", parsed)]

  let result = generator.generate("user", table, queries)

  // _wc function should use query.select_all directly with conn
  result
  |> string.contains("query.select_all(conn,")
  |> should.be_true()
}

pub fn generate_multiple_queries_all_have_both_variants_test() {
  let table =
    Table("users", [
      Column("id", Id, False, None, None),
      Column("name", String, False, None, None),
      Column("email", String, False, None, None),
    ])

  let find_parsed =
    ParsedQuery(
      tables: ["users"],
      columns: [
        SelectedColumn(table: None, name: "id", alias: None),
        SelectedColumn(table: None, name: "name", alias: None),
      ],
      params: [1],
      param_columns: [#(1, "id")],
    )

  let list_parsed =
    ParsedQuery(
      tables: ["users"],
      columns: [SelectedColumn(table: None, name: "*", alias: None)],
      params: [],
      param_columns: [],
    )

  let delete_parsed =
    ParsedQuery(tables: ["users"], columns: [], params: [1], param_columns: [
      #(1, "id"),
    ])

  let queries = [
    #("find", "SELECT id, name FROM users WHERE id = $1", find_parsed),
    #("list_all", "SELECT * FROM users", list_parsed),
    #("delete", "DELETE FROM users WHERE id = $1", delete_parsed),
  ]

  let result = generator.generate("user", table, queries)

  // find
  result
  |> string.contains("pub fn find(pool pool: Pool")
  |> should.be_true()
  result
  |> string.contains("pub fn find_wc(conn conn: Connection")
  |> should.be_true()

  // list_all
  result
  |> string.contains("pub fn list_all(pool pool: Pool")
  |> should.be_true()
  result
  |> string.contains("pub fn list_all_wc(conn conn: Connection")
  |> should.be_true()

  // delete
  result
  |> string.contains("pub fn delete(pool pool: Pool")
  |> should.be_true()
  result
  |> string.contains("pub fn delete_wc(conn conn: Connection")
  |> should.be_true()
}

pub fn generate_query_with_multiple_params_test() {
  let table =
    Table("users", [
      Column("id", Id, False, None, None),
      Column("name", String, False, None, None),
      Column("status", Int, False, None, None),
    ])

  let parsed =
    ParsedQuery(
      tables: ["users"],
      columns: [SelectedColumn(table: None, name: "*", alias: None)],
      params: [1, 2],
      param_columns: [#(1, "name"), #(2, "status")],
    )

  let queries = [
    #(
      "find_by_name_and_status",
      "SELECT * FROM users WHERE name = $1 AND status = $2",
      parsed,
    ),
  ]

  let result = generator.generate("user", table, queries)

  // Main function should have both params
  result
  |> string.contains(
    "pub fn find_by_name_and_status(pool pool: Pool, name name: String, status status: Int)",
  )
  |> should.be_true()

  // _wc function should also have both params
  result
  |> string.contains(
    "pub fn find_by_name_and_status_wc(conn conn: Connection, name name: String, status status: Int)",
  )
  |> should.be_true()

  // Main function should pass params to _wc
  result
  |> string.contains(
    "find_by_name_and_status_wc(conn: conn, name: name, status: status)",
  )
  |> should.be_true()
}
