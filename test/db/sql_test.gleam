import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import glimr/db/gen/migrate/snapshot.{
  ColumnSnapshot, IndexSnapshot, Snapshot, TableSnapshot,
}
import glimr/db/gen/migrate/sql.{Postgres, Sqlite}
import glimr/db/gen/schema_parser.{Column, Index, Table}

// ------------------------------------------------------------- Create Index SQL

pub fn create_index_postgres_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("email", schema_parser.String, False, None, None),
      ],
      indexes: [Index(columns: ["email"], unique: False, name: None)],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("email", "String", False, False),
            ],
            indexes: [IndexSnapshot(["email"], False, None)],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  result
  |> should.equal(
    "CREATE TABLE users (\n  id SERIAL PRIMARY KEY NOT NULL,\n  email VARCHAR(255) NOT NULL\n);\n\nCREATE INDEX idx_users_email ON users (email);",
  )
}

pub fn create_unique_index_postgres_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("email", schema_parser.String, False, None, None),
      ],
      indexes: [Index(columns: ["email"], unique: True, name: None)],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("email", "String", False, False),
            ],
            indexes: [IndexSnapshot(["email"], True, None)],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("CREATE UNIQUE INDEX idx_users_email ON users (email);")
  |> should.be_true()
}

pub fn create_composite_index_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("first_name", schema_parser.String, False, None, None),
        Column("last_name", schema_parser.String, False, None, None),
      ],
      indexes: [
        Index(columns: ["first_name", "last_name"], unique: False, name: None),
      ],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("first_name", "String", False, False),
              ColumnSnapshot("last_name", "String", False, False),
            ],
            indexes: [IndexSnapshot(["first_name", "last_name"], False, None)],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains(
    "CREATE INDEX idx_users_first_name_last_name ON users (first_name, last_name);",
  )
  |> should.be_true()
}

pub fn create_named_index_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("email", schema_parser.String, False, None, None),
      ],
      indexes: [
        Index(columns: ["email"], unique: False, name: Some("idx_custom")),
      ],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("email", "String", False, False),
            ],
            indexes: [IndexSnapshot(["email"], False, Some("idx_custom"))],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("CREATE INDEX idx_custom ON users (email);")
  |> should.be_true()
}

pub fn create_index_sqlite_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("email", schema_parser.String, False, None, None),
      ],
      indexes: [Index(columns: ["email"], unique: True, name: None)],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("email", "String", False, False),
            ],
            indexes: [IndexSnapshot(["email"], True, None)],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> string.contains("CREATE UNIQUE INDEX idx_users_email ON users (email);")
  |> should.be_true()
}

// ------------------------------------------------------------- Drop Index

pub fn drop_index_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("email", schema_parser.String, False, None, None),
      ],
      indexes: [],
    )

  let old =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("email", "String", False, False),
            ],
            indexes: [IndexSnapshot(["email"], True, None)],
          ),
        ),
      ]),
    )
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("email", "String", False, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  result
  |> should.equal("DROP INDEX idx_users_email;")
}

pub fn drop_named_index_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("email", schema_parser.String, False, None, None),
      ],
      indexes: [],
    )

  let old =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("email", "String", False, False),
            ],
            indexes: [IndexSnapshot(["email"], False, Some("idx_custom"))],
          ),
        ),
      ]),
    )
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("email", "String", False, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  result
  |> should.equal("DROP INDEX idx_custom;")
}

// ------------------------------------------------------------- No Change

pub fn no_index_changes_when_same_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("email", schema_parser.String, False, None, None),
      ],
      indexes: [Index(columns: ["email"], unique: True, name: None)],
    )

  let snap =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("email", "String", False, False),
            ],
            indexes: [IndexSnapshot(["email"], True, None)],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(snap, snap, [table], False)
  diff.changes |> should.equal([])
}

// ------------------------------------------------------------- Add Index to Existing Table

pub fn add_index_to_existing_table_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("email", schema_parser.String, False, None, None),
      ],
      indexes: [Index(columns: ["email"], unique: False, name: None)],
    )

  let old =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("email", "String", False, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("email", "String", False, False),
            ],
            indexes: [IndexSnapshot(["email"], False, None)],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  result
  |> should.equal("CREATE INDEX idx_users_email ON users (email);")
}

// ------------------------------------------------------------- Describe Change

pub fn describe_create_index_test() {
  let change =
    sql.CreateIndex(
      "users",
      Index(columns: ["email"], unique: True, name: None),
    )

  sql.describe_change(change)
  |> should.equal("Create index: idx_users_email")
}

pub fn describe_drop_index_test() {
  let change = sql.DropIndex("users", "idx_users_email")

  sql.describe_change(change)
  |> should.equal("Drop index: idx_users_email")
}
