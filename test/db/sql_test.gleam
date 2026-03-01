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

// ------------------------------------------------------------- Array Column SQL (Postgres)

pub fn create_table_with_string_array_postgres_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "tags",
          schema_parser.Array(schema_parser.String),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "posts",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("tags", "Array(String)", False, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  // Postgres should use VARCHAR(255)[]
  result
  |> string.contains("tags VARCHAR(255)[] NOT NULL")
  |> should.be_true()
}

pub fn create_table_with_int_array_postgres_test() {
  let table =
    Table(
      name: "data",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "scores",
          schema_parser.Array(schema_parser.Int),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "data",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("scores", "Array(Int)", False, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("scores INTEGER[] NOT NULL")
  |> should.be_true()
}

pub fn create_table_with_nested_array_postgres_test() {
  let table =
    Table(
      name: "data",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "matrix",
          schema_parser.Array(schema_parser.Array(schema_parser.Int)),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "data",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("matrix", "Array(Array(Int))", False, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  // Nested array: INTEGER[][]
  result
  |> string.contains("matrix INTEGER[][] NOT NULL")
  |> should.be_true()
}

pub fn create_table_with_boolean_array_postgres_test() {
  let table =
    Table(
      name: "flags",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "active_flags",
          schema_parser.Array(schema_parser.Boolean),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "flags",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("active_flags", "Array(Boolean)", False, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("active_flags BOOLEAN[] NOT NULL")
  |> should.be_true()
}

// ------------------------------------------------------------- Array Column SQL (SQLite)

pub fn create_table_with_array_sqlite_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "tags",
          schema_parser.Array(schema_parser.String),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "posts",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("tags", "Array(String)", False, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Sqlite)

  // SQLite always stores arrays as TEXT (JSON)
  result
  |> string.contains("tags TEXT NOT NULL")
  |> should.be_true()
}

pub fn create_table_with_nested_array_sqlite_test() {
  let table =
    Table(
      name: "data",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "matrix",
          schema_parser.Array(schema_parser.Array(schema_parser.Int)),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "data",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("matrix", "Array(Array(Int))", False, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Sqlite)

  // Even nested arrays are just TEXT in SQLite
  result
  |> string.contains("matrix TEXT NOT NULL")
  |> should.be_true()
}

// ------------------------------------------------------------- DefaultEmptyArray SQL

pub fn default_empty_array_postgres_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "tags",
          schema_parser.Array(schema_parser.String),
          False,
          Some(schema_parser.DefaultEmptyArray),
          None,
        ),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "posts",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("tags", "Array(String)", False, True),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  // Postgres empty array literal
  result
  |> string.contains("DEFAULT '{}'")
  |> should.be_true()
}

pub fn default_empty_array_sqlite_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "tags",
          schema_parser.Array(schema_parser.String),
          False,
          Some(schema_parser.DefaultEmptyArray),
          None,
        ),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "posts",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("tags", "Array(String)", False, True),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Sqlite)

  // SQLite empty JSON array
  result
  |> string.contains("DEFAULT '[]'")
  |> should.be_true()
}

// ------------------------------------------------------------- Add Array Column to Existing Table

pub fn add_array_column_postgres_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("title", schema_parser.String, False, None, None),
        Column(
          "tags",
          schema_parser.Array(schema_parser.String),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let old =
    Snapshot(
      tables: dict.from_list([
        #(
          "posts",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("title", "String", False, False),
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
          "posts",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("title", "String", False, False),
              ColumnSnapshot("tags", "Array(String)", False, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  result
  |> should.equal("ALTER TABLE posts ADD COLUMN tags VARCHAR(255)[] NOT NULL;")
}

pub fn add_array_column_sqlite_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("title", schema_parser.String, False, None, None),
        Column(
          "tags",
          schema_parser.Array(schema_parser.String),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let old =
    Snapshot(
      tables: dict.from_list([
        #(
          "posts",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("title", "String", False, False),
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
          "posts",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("title", "String", False, False),
              ColumnSnapshot("tags", "Array(String)", False, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> should.equal("ALTER TABLE posts ADD COLUMN tags TEXT NOT NULL;")
}

// ------------------------------------------------------------- Nullable Array Column

pub fn nullable_array_column_postgres_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "tags",
          schema_parser.Array(schema_parser.String),
          True,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "posts",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("tags", "Array(String)", True, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  // Nullable array should NOT have NOT NULL
  result
  |> string.contains("tags VARCHAR(255)[]")
  |> should.be_true()
  // Make sure there's no "NOT NULL" after the array definition
  // The line should end with just the type, not "NOT NULL"
  result
  |> string.contains("tags VARCHAR(255)[] NOT NULL")
  |> should.be_false()
}

// ------------------------------------------------------------- Enum SQL

pub fn create_table_with_enum_postgres_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "status",
          schema_parser.Enum("status", ["active", "inactive", "banned"]),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
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
              ColumnSnapshot(
                "status",
                "Enum(status:active,inactive,banned)",
                False,
                False,
              ),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  // Should have CREATE TYPE before CREATE TABLE
  result
  |> string.contains(
    "CREATE TYPE status AS ENUM ('active', 'inactive', 'banned');",
  )
  |> should.be_true()

  // Column should use the enum type name
  result
  |> string.contains("status status NOT NULL")
  |> should.be_true()
}

pub fn create_table_with_enum_sqlite_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "status",
          schema_parser.Enum("status", ["active", "inactive"]),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
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
              ColumnSnapshot(
                "status",
                "Enum(status:active,inactive)",
                False,
                False,
              ),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Sqlite)

  // SQLite should use TEXT with CHECK constraint
  result
  |> string.contains(
    "status TEXT NOT NULL CHECK (status IN ('active', 'inactive'))",
  )
  |> should.be_true()
}

// ------------------------------------------------------------- Decimal SQL

pub fn create_table_with_decimal_postgres_test() {
  let table =
    Table(
      name: "products",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("price", schema_parser.Decimal(10, 2), False, None, None),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "products",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("price", "Decimal(10,2)", False, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("price NUMERIC(10, 2) NOT NULL")
  |> should.be_true()
}

pub fn create_table_with_decimal_sqlite_test() {
  let table =
    Table(
      name: "products",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("price", schema_parser.Decimal(10, 2), False, None, None),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "products",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("price", "Decimal(10,2)", False, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> string.contains("price TEXT NOT NULL")
  |> should.be_true()
}

// ------------------------------------------------------------- Blob SQL

pub fn create_table_with_blob_postgres_test() {
  let table =
    Table(
      name: "files",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("data", schema_parser.Blob, False, None, None),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "files",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("data", "Blob", False, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("data BYTEA NOT NULL")
  |> should.be_true()
}

pub fn create_table_with_blob_sqlite_test() {
  let table =
    Table(
      name: "files",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("data", schema_parser.Blob, False, None, None),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "files",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("data", "Blob", False, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> string.contains("data BLOB NOT NULL")
  |> should.be_true()
}

// ------------------------------------------------------------- Time SQL

pub fn create_table_with_time_postgres_test() {
  let table =
    Table(
      name: "events",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("starts_at", schema_parser.Time, False, None, None),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "events",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("starts_at", "Time", False, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("starts_at TIME NOT NULL")
  |> should.be_true()
}

pub fn create_table_with_time_sqlite_test() {
  let table =
    Table(
      name: "events",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("starts_at", schema_parser.Time, False, None, None),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "events",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot("starts_at", "Time", False, False),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> string.contains("starts_at TEXT NOT NULL")
  |> should.be_true()
}

// ------------------------------------------------------------- Foreign Key Actions SQL

pub fn foreign_on_delete_cascade_postgres_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "user_id",
          schema_parser.Foreign("users", Some(schema_parser.Cascade), None),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "posts",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot(
                "user_id",
                "Foreign(users,onDelete:Cascade)",
                False,
                False,
              ),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("REFERENCES users(id) ON DELETE CASCADE")
  |> should.be_true()
}

pub fn foreign_on_update_restrict_postgres_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "user_id",
          schema_parser.Foreign("users", None, Some(schema_parser.Restrict)),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "posts",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot(
                "user_id",
                "Foreign(users,onUpdate:Restrict)",
                False,
                False,
              ),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("ON UPDATE RESTRICT")
  |> should.be_true()
}

pub fn foreign_both_actions_postgres_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "user_id",
          schema_parser.Foreign(
            "users",
            Some(schema_parser.Cascade),
            Some(schema_parser.SetNull),
          ),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "posts",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot(
                "user_id",
                "Foreign(users,onDelete:Cascade,onUpdate:SetNull)",
                False,
                False,
              ),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("ON DELETE CASCADE")
  |> should.be_true()

  result
  |> string.contains("ON UPDATE SET NULL")
  |> should.be_true()
}

pub fn foreign_on_delete_cascade_sqlite_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "user_id",
          schema_parser.Foreign("users", Some(schema_parser.Cascade), None),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let old = Snapshot(tables: dict.new())
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "posts",
          TableSnapshot(
            columns: [
              ColumnSnapshot("id", "Id", False, False),
              ColumnSnapshot(
                "user_id",
                "Foreign(users,onDelete:Cascade)",
                False,
                False,
              ),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let diff = sql.compute_diff(old, new, [table], False)
  let result = sql.generate_sql(diff, Sqlite)

  // SQLite also supports FK actions
  result
  |> string.contains("ON DELETE CASCADE")
  |> should.be_true()
}

// ------------------------------------------------------------- Describe Change

pub fn describe_create_enum_type_test() {
  let change = sql.CreateEnumType("status", ["active", "inactive"])

  sql.describe_change(change)
  |> should.equal("Create enum type: status")
}

// ------------------------------------------------------------- SmallInt SQL

pub fn create_table_smallint_postgres_test() {
  let tables = [
    Table(
      name: "counters",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("priority", schema_parser.SmallInt, False, None, None),
      ],
      indexes: [],
    ),
  ]

  let old = Snapshot(tables: dict.new())
  let new = snapshot.build(tables)
  let diff = sql.compute_diff(old, new, tables, False)
  let result = sql.generate_sql(diff, Postgres)

  result |> string.contains("priority SMALLINT NOT NULL") |> should.be_true()
}

pub fn create_table_smallint_sqlite_test() {
  let tables = [
    Table(
      name: "counters",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("priority", schema_parser.SmallInt, False, None, None),
      ],
      indexes: [],
    ),
  ]

  let old = Snapshot(tables: dict.new())
  let new = snapshot.build(tables)
  let diff = sql.compute_diff(old, new, tables, False)
  let result = sql.generate_sql(diff, Sqlite)

  result |> string.contains("priority INTEGER NOT NULL") |> should.be_true()
}
