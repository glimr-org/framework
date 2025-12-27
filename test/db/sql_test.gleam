import gleam/dict
import gleam/dynamic/decode
import gleam/list
import gleam/option.{None, Some}
import gleam/regexp
import gleam/string
import gleeunit/should
import glimr/db/gen/migrate/snapshot.{ColumnSnapshot, Snapshot, TableSnapshot}
import glimr/db/gen/migrate/sql.{Postgres, Sqlite}
import glimr/db/gen/schema_parser.{
  BigInt, Boolean, Column, Date, DefaultAutoUuid, DefaultBool, DefaultFloat,
  DefaultInt, DefaultNow, DefaultNull, DefaultString, DefaultUnixNow, Float,
  Foreign, Id, Int, Json, String, Table, Text, Timestamp, UnixTimestamp, Uuid,
}
import simplifile
import sqlight

// ------------------------------------------------------------- Column Types - Postgres

pub fn id_column_postgres_test() {
  let table = Table("test", [Column("id", Id, False, None, None)])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> should.equal(
    "CREATE TABLE test (
  id SERIAL PRIMARY KEY NOT NULL
);",
  )
}

pub fn string_column_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("name", String, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("name VARCHAR(255) NOT NULL")
  |> should.be_true()
}

pub fn text_column_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("bio", Text, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("bio TEXT NOT NULL")
  |> should.be_true()
}

pub fn int_column_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("age", Int, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("age INTEGER NOT NULL")
  |> should.be_true()
}

pub fn bigint_column_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("count", BigInt, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("count BIGINT NOT NULL")
  |> should.be_true()
}

pub fn float_column_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("price", Float, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("price DOUBLE PRECISION NOT NULL")
  |> should.be_true()
}

pub fn boolean_column_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("active", Boolean, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("active BOOLEAN NOT NULL")
  |> should.be_true()
}

pub fn timestamp_column_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("created_at", Timestamp, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("created_at TIMESTAMP NOT NULL")
  |> should.be_true()
}

pub fn unix_timestamp_column_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("created_at", UnixTimestamp, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("created_at BIGINT NOT NULL")
  |> should.be_true()
}

pub fn date_column_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("birth_date", Date, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("birth_date DATE NOT NULL")
  |> should.be_true()
}

pub fn json_column_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("metadata", Json, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("metadata JSONB NOT NULL")
  |> should.be_true()
}

pub fn uuid_column_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("external_id", Uuid, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("external_id UUID NOT NULL")
  |> should.be_true()
}

pub fn foreign_column_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("user_id", Foreign("users"), False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("user_id INTEGER REFERENCES users(id) NOT NULL")
  |> should.be_true()
}

// ------------------------------------------------------------- Column Types - SQLite

pub fn id_column_sqlite_test() {
  let table = Table("test", [Column("id", Id, False, None, None)])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> should.equal(
    "CREATE TABLE test (
  id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL
);",
  )
}

pub fn string_column_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("name", String, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> string.contains("name TEXT NOT NULL")
  |> should.be_true()
}

pub fn text_column_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("bio", Text, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> string.contains("bio TEXT NOT NULL")
  |> should.be_true()
}

pub fn int_column_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("age", Int, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> string.contains("age INTEGER NOT NULL")
  |> should.be_true()
}

pub fn bigint_column_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("count", BigInt, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  // SQLite uses INTEGER for bigint
  result
  |> string.contains("count INTEGER NOT NULL")
  |> should.be_true()
}

pub fn float_column_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("price", Float, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> string.contains("price REAL NOT NULL")
  |> should.be_true()
}

pub fn boolean_column_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("active", Boolean, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  // SQLite uses INTEGER for boolean
  result
  |> string.contains("active INTEGER NOT NULL")
  |> should.be_true()
}

pub fn timestamp_column_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("created_at", Timestamp, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  // SQLite uses TEXT for timestamp
  result
  |> string.contains("created_at TEXT NOT NULL")
  |> should.be_true()
}

pub fn unix_timestamp_column_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("created_at", UnixTimestamp, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> string.contains("created_at INTEGER NOT NULL")
  |> should.be_true()
}

pub fn date_column_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("birth_date", Date, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  // SQLite uses TEXT for date
  result
  |> string.contains("birth_date TEXT NOT NULL")
  |> should.be_true()
}

pub fn json_column_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("metadata", Json, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  // SQLite uses TEXT for JSON
  result
  |> string.contains("metadata TEXT NOT NULL")
  |> should.be_true()
}

pub fn uuid_column_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("external_id", Uuid, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  // SQLite uses TEXT for UUID
  result
  |> string.contains("external_id TEXT NOT NULL")
  |> should.be_true()
}

pub fn foreign_column_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("user_id", Foreign("users"), False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  // SQLite foreign keys are just INTEGER (no REFERENCES in column def)
  result
  |> string.contains("user_id INTEGER NOT NULL")
  |> should.be_true()
}

// ------------------------------------------------------------- Default Values - Postgres

pub fn default_bool_true_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("active", Boolean, False, Some(DefaultBool(True)), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("active BOOLEAN NOT NULL DEFAULT true")
  |> should.be_true()
}

pub fn default_bool_false_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("archived", Boolean, False, Some(DefaultBool(False)), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("archived BOOLEAN NOT NULL DEFAULT false")
  |> should.be_true()
}

pub fn default_string_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("role", String, False, Some(DefaultString("user")), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("role VARCHAR(255) NOT NULL DEFAULT 'user'")
  |> should.be_true()
}

pub fn default_string_with_quote_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("message", String, False, Some(DefaultString("it's ok")), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  // Single quotes should be escaped
  result
  |> string.contains("DEFAULT 'it''s ok'")
  |> should.be_true()
}

pub fn default_int_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("count", Int, False, Some(DefaultInt(0)), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("count INTEGER NOT NULL DEFAULT 0")
  |> should.be_true()
}

pub fn default_float_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("rate", Float, False, Some(DefaultFloat(0.5)), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("rate DOUBLE PRECISION NOT NULL DEFAULT 0.5")
  |> should.be_true()
}

pub fn default_now_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("created_at", Timestamp, False, Some(DefaultNow), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP")
  |> should.be_true()
}

pub fn default_unix_now_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("created_at", UnixTimestamp, False, Some(DefaultUnixNow), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("EXTRACT(EPOCH FROM CURRENT_TIMESTAMP)")
  |> should.be_true()
}

pub fn default_null_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("deleted_at", Timestamp, True, Some(DefaultNull), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("deleted_at TIMESTAMP DEFAULT NULL")
  |> should.be_true()
}

pub fn auto_uuid_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("external_id", Uuid, False, Some(DefaultAutoUuid), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("gen_random_uuid()")
  |> should.be_true()
}

// ------------------------------------------------------------- Default Values - SQLite

pub fn default_bool_true_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("active", Boolean, False, Some(DefaultBool(True)), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  // SQLite uses 1 for true
  result
  |> string.contains("active INTEGER NOT NULL DEFAULT 1")
  |> should.be_true()
}

pub fn default_bool_false_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("archived", Boolean, False, Some(DefaultBool(False)), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  // SQLite uses 0 for false
  result
  |> string.contains("archived INTEGER NOT NULL DEFAULT 0")
  |> should.be_true()
}

pub fn default_string_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("role", String, False, Some(DefaultString("user")), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> string.contains("role TEXT NOT NULL DEFAULT 'user'")
  |> should.be_true()
}

pub fn default_int_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("count", Int, False, Some(DefaultInt(42)), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> string.contains("count INTEGER NOT NULL DEFAULT 42")
  |> should.be_true()
}

pub fn default_float_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("rate", Float, False, Some(DefaultFloat(3.14)), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> string.contains("rate REAL NOT NULL DEFAULT 3.14")
  |> should.be_true()
}

pub fn default_now_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("created_at", Timestamp, False, Some(DefaultNow), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> string.contains("created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP")
  |> should.be_true()
}

pub fn default_unix_now_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("created_at", UnixTimestamp, False, Some(DefaultUnixNow), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> string.contains("strftime('%s', 'now')")
  |> should.be_true()
}

pub fn default_null_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("deleted_at", Timestamp, True, Some(DefaultNull), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> string.contains("deleted_at TEXT DEFAULT NULL")
  |> should.be_true()
}

pub fn auto_uuid_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("external_id", Uuid, False, Some(DefaultAutoUuid), None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> string.contains("randomblob")
  |> should.be_true()
}

// ------------------------------------------------------------- Nullable Columns

pub fn nullable_column_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("bio", Text, True, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  // Nullable columns should NOT have NOT NULL
  result
  |> should.equal(
    "CREATE TABLE test (
  id SERIAL PRIMARY KEY NOT NULL,
  bio TEXT
);",
  )
}

pub fn nullable_column_sqlite_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("bio", Text, True, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> should.equal(
    "CREATE TABLE test (
  id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  bio TEXT
);",
  )
}

pub fn non_nullable_column_postgres_test() {
  let table =
    Table("test", [
      Column("id", Id, False, None, None),
      Column("name", String, False, None, None),
    ])
  let diff = sql.SchemaDiff([sql.CreateTable(table)])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> string.contains("name VARCHAR(255) NOT NULL")
  |> should.be_true()
}

// ------------------------------------------------------------- Schema Diffing

pub fn diff_new_table_test() {
  let old = Snapshot(tables: dict.new())
  let new_table = Table("users", [Column("id", Id, False, None, None)])

  let diff =
    sql.compute_diff(old, Snapshot(tables: dict.new()), [new_table], False)

  case diff.changes {
    [sql.CreateTable(table)] -> table.name |> should.equal("users")
    _ -> should.fail()
  }
}

pub fn diff_dropped_table_test() {
  let old =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(columns: [ColumnSnapshot("id", "Id", False, False)]),
        ),
      ]),
    )
  let new = Snapshot(tables: dict.new())

  let diff = sql.compute_diff(old, new, [], False)

  case diff.changes {
    [sql.DropTable(name)] -> name |> should.equal("users")
    _ -> should.fail()
  }
}

pub fn diff_dropped_table_skipped_when_filtered_test() {
  let old =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(columns: [ColumnSnapshot("id", "Id", False, False)]),
        ),
      ]),
    )
  let new = Snapshot(tables: dict.new())

  // When is_filtered is true, drops should be skipped
  let diff = sql.compute_diff(old, new, [], True)

  diff.changes
  |> should.equal([])
}

pub fn diff_added_column_test() {
  let old =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(columns: [ColumnSnapshot("id", "Id", False, False)]),
        ),
      ]),
    )
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(columns: [
            ColumnSnapshot("id", "Id", False, False),
            ColumnSnapshot("name", "String", False, False),
          ]),
        ),
      ]),
    )
  let new_table =
    Table("users", [
      Column("id", Id, False, None, None),
      Column("name", String, False, None, None),
    ])

  let diff = sql.compute_diff(old, new, [new_table], False)

  case diff.changes {
    [sql.AddColumn(table, column)] -> {
      table |> should.equal("users")
      column.name |> should.equal("name")
    }
    _ -> should.fail()
  }
}

pub fn diff_dropped_column_test() {
  let old =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(columns: [
            ColumnSnapshot("id", "Id", False, False),
            ColumnSnapshot("old_field", "String", False, False),
          ]),
        ),
      ]),
    )
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(columns: [ColumnSnapshot("id", "Id", False, False)]),
        ),
      ]),
    )
  let new_table = Table("users", [Column("id", Id, False, None, None)])

  let diff = sql.compute_diff(old, new, [new_table], False)

  case diff.changes {
    [sql.DropColumn(table, column)] -> {
      table |> should.equal("users")
      column |> should.equal("old_field")
    }
    _ -> should.fail()
  }
}

pub fn diff_renamed_column_test() {
  let old =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(columns: [
            ColumnSnapshot("id", "Id", False, False),
            ColumnSnapshot("name", "String", False, False),
          ]),
        ),
      ]),
    )
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(columns: [
            ColumnSnapshot("id", "Id", False, False),
            ColumnSnapshot("full_name", "String", False, False),
          ]),
        ),
      ]),
    )
  let new_table =
    Table("users", [
      Column("id", Id, False, None, None),
      Column("full_name", String, False, None, Some("name")),
    ])

  let diff = sql.compute_diff(old, new, [new_table], False)

  case diff.changes {
    [sql.RenameColumn(table, old_name, new_name)] -> {
      table |> should.equal("users")
      old_name |> should.equal("name")
      new_name |> should.equal("full_name")
    }
    _ -> should.fail()
  }
}

pub fn diff_altered_column_type_test() {
  let old =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(columns: [
            ColumnSnapshot("id", "Id", False, False),
            ColumnSnapshot("count", "Int", False, False),
          ]),
        ),
      ]),
    )
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(columns: [
            ColumnSnapshot("id", "Id", False, False),
            ColumnSnapshot("count", "BigInt", False, False),
          ]),
        ),
      ]),
    )
  let new_table =
    Table("users", [
      Column("id", Id, False, None, None),
      Column("count", BigInt, False, None, None),
    ])

  let diff = sql.compute_diff(old, new, [new_table], False)

  case diff.changes {
    [sql.AlterColumn(table, column, _old)] -> {
      table |> should.equal("users")
      column.name |> should.equal("count")
    }
    _ -> should.fail()
  }
}

pub fn diff_altered_column_nullability_test() {
  let old =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(columns: [
            ColumnSnapshot("id", "Id", False, False),
            ColumnSnapshot("bio", "Text", False, False),
          ]),
        ),
      ]),
    )
  let new =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(columns: [
            ColumnSnapshot("id", "Id", False, False),
            ColumnSnapshot("bio", "Text", True, False),
          ]),
        ),
      ]),
    )
  let new_table =
    Table("users", [
      Column("id", Id, False, None, None),
      Column("bio", Text, True, None, None),
    ])

  let diff = sql.compute_diff(old, new, [new_table], False)

  case diff.changes {
    [sql.AlterColumn(table, column, _old)] -> {
      table |> should.equal("users")
      column.name |> should.equal("bio")
      column.nullable |> should.be_true()
    }
    _ -> should.fail()
  }
}

// ------------------------------------------------------------- ALTER Statements

pub fn drop_table_sql_test() {
  let diff = sql.SchemaDiff([sql.DropTable("users")])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> should.equal("DROP TABLE users;")
}

pub fn add_column_sql_postgres_test() {
  let diff =
    sql.SchemaDiff([
      sql.AddColumn("users", Column("email", String, False, None, None)),
    ])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> should.equal("ALTER TABLE users ADD COLUMN email VARCHAR(255) NOT NULL;")
}

pub fn add_column_sql_sqlite_test() {
  let diff =
    sql.SchemaDiff([
      sql.AddColumn("users", Column("email", String, False, None, None)),
    ])
  let result = sql.generate_sql(diff, Sqlite)

  result
  |> should.equal("ALTER TABLE users ADD COLUMN email TEXT NOT NULL;")
}

pub fn add_nullable_column_sql_test() {
  let diff =
    sql.SchemaDiff([
      sql.AddColumn("users", Column("bio", Text, True, None, None)),
    ])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> should.equal("ALTER TABLE users ADD COLUMN bio TEXT;")
}

pub fn add_column_with_default_sql_test() {
  let diff =
    sql.SchemaDiff([
      sql.AddColumn(
        "users",
        Column("active", Boolean, False, Some(DefaultBool(True)), None),
      ),
    ])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> should.equal(
    "ALTER TABLE users ADD COLUMN active BOOLEAN NOT NULL DEFAULT true;",
  )
}

pub fn drop_column_sql_test() {
  let diff = sql.SchemaDiff([sql.DropColumn("users", "old_field")])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> should.equal("ALTER TABLE users DROP COLUMN old_field;")
}

pub fn rename_column_sql_test() {
  let diff = sql.SchemaDiff([sql.RenameColumn("users", "name", "full_name")])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> should.equal("ALTER TABLE users RENAME COLUMN name TO full_name;")
}

pub fn alter_column_type_postgres_test() {
  let diff =
    sql.SchemaDiff([
      sql.AlterColumn(
        "users",
        Column("count", BigInt, False, None, None),
        ColumnSnapshot("count", "Int", False, False),
      ),
    ])
  let result = sql.generate_sql(diff, Postgres)

  result
  |> should.equal("ALTER TABLE users ALTER COLUMN count TYPE BIGINT;")
}

pub fn alter_column_sqlite_not_supported_test() {
  let diff =
    sql.SchemaDiff([
      sql.AlterColumn(
        "users",
        Column("count", BigInt, False, None, None),
        ColumnSnapshot("count", "Int", False, False),
      ),
    ])
  let result = sql.generate_sql(diff, Sqlite)

  // SQLite doesn't support ALTER COLUMN
  result
  |> string.contains("SQLite: ALTER COLUMN not supported")
  |> should.be_true()
}

// ------------------------------------------------------------- Describe Change

pub fn describe_create_table_test() {
  let change = sql.CreateTable(Table("users", []))
  sql.describe_change(change)
  |> should.equal("Create table: users")
}

pub fn describe_drop_table_test() {
  let change = sql.DropTable("users")
  sql.describe_change(change)
  |> should.equal("Drop table: users")
}

pub fn describe_add_column_test() {
  let change =
    sql.AddColumn("users", Column("email", String, False, None, None))
  sql.describe_change(change)
  |> should.equal("Add column: users.email")
}

pub fn describe_drop_column_test() {
  let change = sql.DropColumn("users", "old_field")
  sql.describe_change(change)
  |> should.equal("Drop column: users.old_field")
}

pub fn describe_alter_column_test() {
  let change =
    sql.AlterColumn(
      "users",
      Column("count", BigInt, False, None, None),
      ColumnSnapshot("count", "Int", False, False),
    )
  sql.describe_change(change)
  |> should.equal("Alter column: users.count")
}

pub fn describe_rename_column_test() {
  let change = sql.RenameColumn("users", "name", "full_name")
  sql.describe_change(change)
  |> should.equal("Rename column: users.name -> full_name")
}

// ------------------------------------------------------------- SQLite UUID Validation

pub fn sqlite_auto_uuid_generates_valid_uuid_test() {
  let assert Ok(conn) = sqlight.open(":memory:")

  let create_sql =
    "CREATE TABLE test (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    external_id TEXT NOT NULL DEFAULT (lower(hex(randomblob(4))) || '-' || lower(hex(randomblob(2))) || '-4' || substr(lower(hex(randomblob(2))),2) || '-' || substr('89ab',abs(random()) % 4 + 1, 1) || substr(lower(hex(randomblob(2))),2) || '-' || lower(hex(randomblob(6))))
  )"

  let assert Ok(_) = sqlight.exec(create_sql, conn)
  let assert Ok(_) = sqlight.exec("INSERT INTO test (id) VALUES (1)", conn)

  let decoder = decode.at([0], decode.string)
  let assert Ok(rows) =
    sqlight.query(
      "SELECT external_id FROM test WHERE id = 1",
      conn,
      [],
      decoder,
    )

  let assert [uuid] = rows

  is_valid_uuid_v4(uuid)
  |> should.be_true()
}

pub fn sqlite_auto_uuid_generates_unique_uuids_test() {
  let assert Ok(conn) = sqlight.open(":memory:")

  let create_sql =
    "CREATE TABLE test (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    external_id TEXT NOT NULL DEFAULT (lower(hex(randomblob(4))) || '-' || lower(hex(randomblob(2))) || '-4' || substr(lower(hex(randomblob(2))),2) || '-' || substr('89ab',abs(random()) % 4 + 1, 1) || substr(lower(hex(randomblob(2))),2) || '-' || lower(hex(randomblob(6))))
  )"

  let assert Ok(_) = sqlight.exec(create_sql, conn)
  let assert Ok(_) = sqlight.exec("INSERT INTO test (id) VALUES (1)", conn)
  let assert Ok(_) = sqlight.exec("INSERT INTO test (id) VALUES (2)", conn)
  let assert Ok(_) = sqlight.exec("INSERT INTO test (id) VALUES (3)", conn)

  let decoder = decode.at([0], decode.string)
  let assert Ok(rows) =
    sqlight.query("SELECT external_id FROM test", conn, [], decoder)

  let assert [uuid1, uuid2, uuid3] = rows

  { uuid1 != uuid2 && uuid2 != uuid3 && uuid1 != uuid3 }
  |> should.be_true()
}

// ------------------------------------------------------------- Snapshot Save/Load

pub fn snapshot_save_load_roundtrip_test() {
  // Create a snapshot with multiple tables
  let original_snapshot =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(columns: [
            ColumnSnapshot("id", "Id", False, False),
            ColumnSnapshot("name", "String", False, False),
            ColumnSnapshot("email", "String", False, False),
          ]),
        ),
        #(
          "posts",
          TableSnapshot(columns: [
            ColumnSnapshot("id", "Id", False, False),
            ColumnSnapshot("title", "String", False, False),
            ColumnSnapshot("user_id", "Foreign(users)", False, False),
          ]),
        ),
      ]),
    )

  // Save to a temp file
  let test_path = "/tmp/glimr_test_snapshot.json"
  let assert Ok(_) = snapshot.save(test_path, original_snapshot)

  // Read the raw file content to verify JSON format
  let assert Ok(raw_content) = simplifile.read(test_path)

  // Verify the JSON contains lowercase true/false
  raw_content
  |> string.contains("\"nullable\": false")
  |> should.be_true()

  raw_content
  |> string.contains("\"has_default\": false")
  |> should.be_true()

  // Verify it does NOT contain uppercase True/False
  raw_content
  |> string.contains("True")
  |> should.be_false()

  raw_content
  |> string.contains("False")
  |> should.be_false()

  // Load it back
  let loaded_snapshot = snapshot.load(test_path)

  // Verify tables are preserved
  dict.size(loaded_snapshot.tables)
  |> should.equal(2)

  // Verify users table
  let assert Ok(users_table) = dict.get(loaded_snapshot.tables, "users")
  list.length(users_table.columns)
  |> should.equal(3)

  // Verify posts table
  let assert Ok(posts_table) = dict.get(loaded_snapshot.tables, "posts")
  list.length(posts_table.columns)
  |> should.equal(3)

  // Clean up
  let _ = simplifile.delete(test_path)
  Nil
}

pub fn snapshot_load_nonexistent_returns_empty_test() {
  let loaded = snapshot.load("/tmp/nonexistent_snapshot_file_12345.json")

  dict.size(loaded.tables)
  |> should.equal(0)
}

// ------------------------------------------------------------- Migration Workflow - Add New Model

pub fn migration_workflow_add_new_model_should_not_recreate_existing_test() {
  // Bug scenario: User has existing tables, adds a new model,
  // and existing tables are incorrectly marked for recreation

  let test_path = "/tmp/glimr_test_add_model.json"

  // Clean up any existing file
  let _ = simplifile.delete(test_path)

  // Step 1: Initial state - two tables exist
  let table_users =
    Table("users", [
      Column("id", Id, False, None, None),
      Column("name", String, False, None, None),
    ])
  let table_posts =
    Table("posts", [
      Column("id", Id, False, None, None),
      Column("title", String, False, None, None),
    ])

  let initial_tables = [table_users, table_posts]
  let initial_snapshot = snapshot.build(initial_tables)

  // Save the initial snapshot (simulating first migration run)
  let assert Ok(_) = snapshot.save(test_path, initial_snapshot)

  // Verify the file was written and can be read back as valid JSON
  let assert Ok(raw_json) = simplifile.read(test_path)

  // The JSON should contain our table names
  raw_json
  |> string.contains("\"users\"")
  |> should.be_true()

  raw_json
  |> string.contains("\"posts\"")
  |> should.be_true()

  // Step 2: User adds a NEW model (comments)
  let table_comments =
    Table("comments", [
      Column("id", Id, False, None, None),
      Column("body", Text, False, None, None),
    ])

  // Now we have 3 tables
  let updated_tables = [table_users, table_posts, table_comments]

  // Step 3: Load old snapshot and compute diff (simulating migration regeneration)
  let old_snapshot = snapshot.load(test_path)

  // Verify old snapshot loaded correctly with 2 tables
  dict.size(old_snapshot.tables)
  |> should.equal(2)

  // Verify the table names are in the loaded snapshot
  let old_table_names = dict.keys(old_snapshot.tables)
  old_table_names
  |> list.contains("users")
  |> should.be_true()

  old_table_names
  |> list.contains("posts")
  |> should.be_true()

  let new_snapshot = snapshot.build(updated_tables)

  let diff = sql.compute_diff(old_snapshot, new_snapshot, updated_tables, False)

  // Step 4: Verify ONLY the new table is created, NOT the existing ones
  let create_table_names =
    list.filter_map(diff.changes, fn(change) {
      case change {
        sql.CreateTable(table) -> Ok(table.name)
        _ -> Error(Nil)
      }
    })

  // Should only create 'comments', not 'users' or 'posts'
  create_table_names
  |> should.equal(["comments"])

  // Verify no DROP TABLE changes
  let drop_count =
    list.count(diff.changes, fn(change) {
      case change {
        sql.DropTable(_) -> True
        _ -> False
      }
    })

  drop_count
  |> should.equal(0)

  // Clean up
  let _ = simplifile.delete(test_path)
  Nil
}

// ------------------------------------------------------------- Migration Workflow - Delete Model

pub fn migration_workflow_delete_model_test() {
  // Simulate: User created tables A (users) and B (posts)
  // Then deleted model B, expecting DROP TABLE posts

  let table_a = Table("users", [Column("id", Id, False, None, None)])
  let table_b = Table("posts", [Column("id", Id, False, None, None)])

  // Step 1: Build initial snapshot with both tables
  let initial_snapshot = snapshot.build([table_a, table_b])

  // Verify both tables are in snapshot
  dict.size(initial_snapshot.tables)
  |> should.equal(2)

  // Step 2: Simulate user deleting model B - now only table A exists
  let remaining_tables = [table_a]
  let new_snapshot = snapshot.build(remaining_tables)

  // Step 3: Compute diff
  let diff =
    sql.compute_diff(initial_snapshot, new_snapshot, remaining_tables, False)

  // Step 4: Verify DROP TABLE is generated for posts
  let has_drop_posts =
    list.any(diff.changes, fn(change) {
      case change {
        sql.DropTable(name) -> name == "posts"
        _ -> False
      }
    })

  has_drop_posts
  |> should.be_true()

  // Verify no CREATE TABLE for users (it already exists)
  let has_create_users =
    list.any(diff.changes, fn(change) {
      case change {
        sql.CreateTable(table) -> table.name == "users"
        _ -> False
      }
    })

  has_create_users
  |> should.be_false()
}

pub fn migration_workflow_delete_model_with_file_test() {
  // Full workflow test with actual file save/load

  let table_a = Table("users", [Column("id", Id, False, None, None)])
  let table_b = Table("posts", [Column("id", Id, False, None, None)])

  // Step 1: Build and save initial snapshot with both tables
  let initial_snapshot = snapshot.build([table_a, table_b])
  let test_path = "/tmp/glimr_test_migration_workflow.json"
  let assert Ok(_) = snapshot.save(test_path, initial_snapshot)

  // Step 2: Load the snapshot (simulating new migration run)
  let loaded_snapshot = snapshot.load(test_path)

  // Verify loaded snapshot has both tables
  dict.size(loaded_snapshot.tables)
  |> should.equal(2)

  // Step 3: Simulate user deleting model B
  let remaining_tables = [table_a]
  let new_snapshot = snapshot.build(remaining_tables)

  // Step 4: Compute diff using loaded (old) snapshot
  let diff =
    sql.compute_diff(loaded_snapshot, new_snapshot, remaining_tables, False)

  // Step 5: Verify DROP TABLE is generated
  let has_drop_posts =
    list.any(diff.changes, fn(change) {
      case change {
        sql.DropTable(name) -> name == "posts"
        _ -> False
      }
    })

  has_drop_posts
  |> should.be_true()

  // Verify no CREATE TABLE for remaining tables
  let has_any_create =
    list.any(diff.changes, fn(change) {
      case change {
        sql.CreateTable(_) -> True
        _ -> False
      }
    })

  has_any_create
  |> should.be_false()

  // Verify the SQL generated is DROP TABLE
  let result = sql.generate_sql(diff, Postgres)
  result
  |> should.equal("DROP TABLE posts;")

  // Clean up
  let _ = simplifile.delete(test_path)
  Nil
}

pub fn migration_workflow_add_then_delete_model_test() {
  // Complete workflow: Empty -> Add two tables -> Delete one

  let test_path = "/tmp/glimr_test_full_workflow.json"

  // Clean up any existing file
  let _ = simplifile.delete(test_path)

  // Step 1: First run - no existing snapshot
  let old_snapshot_1 = snapshot.load(test_path)
  dict.size(old_snapshot_1.tables)
  |> should.equal(0)

  // Create two tables
  let table_a =
    Table("users", [
      Column("id", Id, False, None, None),
      Column("name", String, False, None, None),
    ])
  let table_b =
    Table("posts", [
      Column("id", Id, False, None, None),
      Column("title", String, False, None, None),
    ])

  let tables_1 = [table_a, table_b]
  let new_snapshot_1 = snapshot.build(tables_1)

  let diff_1 = sql.compute_diff(old_snapshot_1, new_snapshot_1, tables_1, False)

  // Should create both tables
  list.length(diff_1.changes)
  |> should.equal(2)

  let create_count =
    list.count(diff_1.changes, fn(change) {
      case change {
        sql.CreateTable(_) -> True
        _ -> False
      }
    })

  create_count
  |> should.equal(2)

  // Save snapshot after first migration
  let assert Ok(_) = snapshot.save(test_path, new_snapshot_1)

  // Step 2: Second run - delete model B
  let old_snapshot_2 = snapshot.load(test_path)

  // Verify snapshot loaded correctly
  dict.size(old_snapshot_2.tables)
  |> should.equal(2)

  // Only table A remains
  let tables_2 = [table_a]
  let new_snapshot_2 = snapshot.build(tables_2)

  let diff_2 = sql.compute_diff(old_snapshot_2, new_snapshot_2, tables_2, False)

  // Should drop posts table only
  case diff_2.changes {
    [sql.DropTable(name)] -> name |> should.equal("posts")
    _ -> should.fail()
  }

  // Clean up
  let _ = simplifile.delete(test_path)
  Nil
}

// ------------------------------------------------------------- Foreign Key Dependency Ordering

pub fn create_tables_ordered_by_foreign_key_dependencies_test() {
  // Tables with dependencies: users (no deps), posts (depends on users),
  // comments (depends on users and posts)
  // Input them in WRONG alphabetical order to verify sorting works

  let table_comments =
    Table("comments", [
      Column("id", Id, False, None, None),
      Column("user_id", Foreign("users"), False, None, None),
      Column("post_id", Foreign("posts"), False, None, None),
      Column("body", Text, False, None, None),
    ])

  let table_posts =
    Table("posts", [
      Column("id", Id, False, None, None),
      Column("user_id", Foreign("users"), False, None, None),
      Column("title", String, False, None, None),
    ])

  let table_users =
    Table("users", [
      Column("id", Id, False, None, None),
      Column("name", String, False, None, None),
    ])

  // Create diff with tables in alphabetical order (wrong dependency order)
  let diff =
    sql.SchemaDiff([
      sql.CreateTable(table_comments),
      sql.CreateTable(table_posts),
      sql.CreateTable(table_users),
    ])

  let result = sql.generate_sql(diff, Postgres)

  // Find the position of each CREATE TABLE statement
  let users_idx = find_substring_index(result, "CREATE TABLE users")
  let posts_idx = find_substring_index(result, "CREATE TABLE posts")
  let comments_idx = find_substring_index(result, "CREATE TABLE comments")

  // All should be found
  { users_idx >= 0 }
  |> should.be_true()

  { posts_idx >= 0 }
  |> should.be_true()

  { comments_idx >= 0 }
  |> should.be_true()

  // users before posts (posts depends on users)
  { users_idx < posts_idx }
  |> should.be_true()

  // users before comments (comments depends on users)
  { users_idx < comments_idx }
  |> should.be_true()

  // posts before comments (comments depends on posts)
  { posts_idx < comments_idx }
  |> should.be_true()
}

pub fn create_tables_no_dependencies_preserves_order_test() {
  // Tables with no foreign key dependencies should still work
  let table_alpha =
    Table("alpha", [
      Column("id", Id, False, None, None),
    ])

  let table_beta =
    Table("beta", [
      Column("id", Id, False, None, None),
    ])

  let table_gamma =
    Table("gamma", [
      Column("id", Id, False, None, None),
    ])

  let diff =
    sql.SchemaDiff([
      sql.CreateTable(table_gamma),
      sql.CreateTable(table_alpha),
      sql.CreateTable(table_beta),
    ])

  let result = sql.generate_sql(diff, Postgres)

  // All tables should be created (order doesn't matter for independent tables)
  result
  |> string.contains("CREATE TABLE alpha")
  |> should.be_true()

  result
  |> string.contains("CREATE TABLE beta")
  |> should.be_true()

  result
  |> string.contains("CREATE TABLE gamma")
  |> should.be_true()
}

pub fn create_tables_mixed_with_other_changes_test() {
  // CreateTable changes should be sorted, but other changes come after
  let table_posts =
    Table("posts", [
      Column("id", Id, False, None, None),
      Column("user_id", Foreign("users"), False, None, None),
    ])

  let table_users =
    Table("users", [
      Column("id", Id, False, None, None),
    ])

  let diff =
    sql.SchemaDiff([
      sql.CreateTable(table_posts),
      sql.AddColumn(
        "existing_table",
        Column("new_col", String, False, None, None),
      ),
      sql.CreateTable(table_users),
    ])

  let result = sql.generate_sql(diff, Postgres)

  // Find positions
  let users_idx = find_substring_index(result, "CREATE TABLE users")
  let posts_idx = find_substring_index(result, "CREATE TABLE posts")
  let alter_idx = find_substring_index(result, "ALTER TABLE")

  // All should be found
  { users_idx >= 0 }
  |> should.be_true()

  { posts_idx >= 0 }
  |> should.be_true()

  { alter_idx >= 0 }
  |> should.be_true()

  // users before posts
  { users_idx < posts_idx }
  |> should.be_true()

  // ALTER TABLE comes after all CREATE TABLEs
  { posts_idx < alter_idx }
  |> should.be_true()
}

// ------------------------------------------------------------- Helper Functions

fn is_valid_uuid_v4(uuid: String) -> Bool {
  let assert Ok(uuid_regexp) =
    regexp.from_string(
      "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$",
    )
  regexp.check(uuid_regexp, uuid)
}

fn find_substring_index(haystack: String, needle: String) -> Int {
  do_find_substring_index(haystack, needle, 0)
}

fn do_find_substring_index(haystack: String, needle: String, index: Int) -> Int {
  case string.starts_with(haystack, needle) {
    True -> index
    False -> {
      case string.pop_grapheme(haystack) {
        Ok(#(_, rest)) -> do_find_substring_index(rest, needle, index + 1)
        Error(_) -> -1
      }
    }
  }
}
