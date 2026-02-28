import gleam/dict
import gleam/option.{None, Some}
import gleeunit/should
import glimr/db/gen/migrate/snapshot.{
  ColumnSnapshot, IndexSnapshot, Snapshot, TableSnapshot,
}
import glimr/db/gen/schema_parser.{Column, Index, Table}

// ------------------------------------------------------------- Build

pub fn build_snapshot_with_indexes_test() {
  let tables = [
    Table(
      name: "users",
      columns: [
        Column(
          name: "id",
          column_type: schema_parser.Id,
          nullable: False,
          default: None,
          renamed_from: None,
        ),
        Column(
          name: "email",
          column_type: schema_parser.String,
          nullable: False,
          default: None,
          renamed_from: None,
        ),
      ],
      indexes: [
        Index(columns: ["email"], unique: True, name: None),
        Index(
          columns: ["email"],
          unique: False,
          name: Some("idx_users_email_lookup"),
        ),
      ],
    ),
  ]

  let snap = snapshot.build(tables)

  let assert Ok(table_snap) = dict.get(snap.tables, "users")

  table_snap.indexes
  |> should.equal([
    IndexSnapshot(columns: ["email"], unique: True, name: None),
    IndexSnapshot(
      columns: ["email"],
      unique: False,
      name: Some("idx_users_email_lookup"),
    ),
  ])
}

pub fn build_snapshot_without_indexes_test() {
  let tables = [
    Table(
      name: "posts",
      columns: [
        Column(
          name: "id",
          column_type: schema_parser.Id,
          nullable: False,
          default: None,
          renamed_from: None,
        ),
      ],
      indexes: [],
    ),
  ]

  let snap = snapshot.build(tables)

  let assert Ok(table_snap) = dict.get(snap.tables, "posts")

  table_snap.indexes
  |> should.equal([])
}

// ------------------------------------------------------------- Save / Load Round-trip

pub fn snapshot_save_load_roundtrip_with_indexes_test() {
  let snap =
    Snapshot(
      tables: dict.from_list([
        #(
          "users",
          TableSnapshot(
            columns: [
              ColumnSnapshot(
                name: "id",
                column_type: "Id",
                nullable: False,
                has_default: False,
              ),
              ColumnSnapshot(
                name: "email",
                column_type: "String",
                nullable: False,
                has_default: False,
              ),
            ],
            indexes: [
              IndexSnapshot(columns: ["email"], unique: True, name: None),
              IndexSnapshot(
                columns: ["email", "password"],
                unique: False,
                name: Some("idx_users_login"),
              ),
            ],
          ),
        ),
      ]),
    )

  let path = "/tmp/glimr_test_snapshot_indexes.json"

  let assert Ok(_) = snapshot.save(path, snap)
  let loaded = snapshot.load(path)

  let assert Ok(table_snap) = dict.get(loaded.tables, "users")

  table_snap.columns
  |> should.equal([
    ColumnSnapshot(
      name: "id",
      column_type: "Id",
      nullable: False,
      has_default: False,
    ),
    ColumnSnapshot(
      name: "email",
      column_type: "String",
      nullable: False,
      has_default: False,
    ),
  ])

  table_snap.indexes
  |> should.equal([
    IndexSnapshot(columns: ["email"], unique: True, name: None),
    IndexSnapshot(
      columns: ["email", "password"],
      unique: False,
      name: Some("idx_users_login"),
    ),
  ])
}

pub fn snapshot_backwards_compat_no_indexes_test() {
  // Simulate an old snapshot JSON without the indexes field
  let path = "/tmp/glimr_test_snapshot_no_indexes.json"
  let content =
    "{
  \"tables\": {
    \"users\": {
      \"columns\": [
        {\"name\": \"id\", \"type\": \"Id\", \"nullable\": false, \"has_default\": false}
      ]
    }
  }
}
"

  let assert Ok(_) = simplifile.write(path, content)
  let loaded = snapshot.load(path)

  let assert Ok(table_snap) = dict.get(loaded.tables, "users")

  // Should default to empty indexes list
  table_snap.indexes
  |> should.equal([])
}

import simplifile

// ------------------------------------------------------------- Array Column Type Strings

pub fn column_type_to_string_array_of_string_test() {
  snapshot.column_type_to_string(schema_parser.Array(schema_parser.String))
  |> should.equal("Array(String)")
}

pub fn column_type_to_string_array_of_int_test() {
  snapshot.column_type_to_string(schema_parser.Array(schema_parser.Int))
  |> should.equal("Array(Int)")
}

pub fn column_type_to_string_nested_array_test() {
  snapshot.column_type_to_string(
    schema_parser.Array(schema_parser.Array(schema_parser.Int)),
  )
  |> should.equal("Array(Array(Int))")
}

pub fn column_type_to_string_triple_nested_array_test() {
  snapshot.column_type_to_string(
    schema_parser.Array(
      schema_parser.Array(schema_parser.Array(schema_parser.Float)),
    ),
  )
  |> should.equal("Array(Array(Array(Float)))")
}

pub fn column_type_to_string_array_of_boolean_test() {
  snapshot.column_type_to_string(schema_parser.Array(schema_parser.Boolean))
  |> should.equal("Array(Boolean)")
}

pub fn column_type_to_string_array_of_uuid_test() {
  snapshot.column_type_to_string(schema_parser.Array(schema_parser.Uuid))
  |> should.equal("Array(Uuid)")
}

// ------------------------------------------------------------- Snapshot Build with Array

pub fn build_snapshot_with_array_column_test() {
  let tables = [
    Table(
      name: "posts",
      columns: [
        Column(
          name: "id",
          column_type: schema_parser.Id,
          nullable: False,
          default: None,
          renamed_from: None,
        ),
        Column(
          name: "tags",
          column_type: schema_parser.Array(schema_parser.String),
          nullable: False,
          default: None,
          renamed_from: None,
        ),
      ],
      indexes: [],
    ),
  ]

  let snap = snapshot.build(tables)

  let assert Ok(table_snap) = dict.get(snap.tables, "posts")

  table_snap.columns
  |> should.equal([
    ColumnSnapshot(
      name: "id",
      column_type: "Id",
      nullable: False,
      has_default: False,
    ),
    ColumnSnapshot(
      name: "tags",
      column_type: "Array(String)",
      nullable: False,
      has_default: False,
    ),
  ])
}

pub fn build_snapshot_with_nested_array_test() {
  let tables = [
    Table(
      name: "data",
      columns: [
        Column(
          name: "matrix",
          column_type: schema_parser.Array(schema_parser.Array(
            schema_parser.Int,
          )),
          nullable: False,
          default: None,
          renamed_from: None,
        ),
      ],
      indexes: [],
    ),
  ]

  let snap = snapshot.build(tables)

  let assert Ok(table_snap) = dict.get(snap.tables, "data")
  let assert [col] = table_snap.columns

  col.column_type
  |> should.equal("Array(Array(Int))")
}

// ------------------------------------------------------------- Snapshot Round-trip with Array

pub fn snapshot_roundtrip_with_array_column_test() {
  let snap =
    Snapshot(
      tables: dict.from_list([
        #(
          "posts",
          TableSnapshot(
            columns: [
              ColumnSnapshot(
                name: "id",
                column_type: "Id",
                nullable: False,
                has_default: False,
              ),
              ColumnSnapshot(
                name: "tags",
                column_type: "Array(String)",
                nullable: False,
                has_default: False,
              ),
              ColumnSnapshot(
                name: "matrix",
                column_type: "Array(Array(Int))",
                nullable: False,
                has_default: False,
              ),
            ],
            indexes: [],
          ),
        ),
      ]),
    )

  let path = "/tmp/glimr_test_snapshot_array.json"

  let assert Ok(_) = snapshot.save(path, snap)
  let loaded = snapshot.load(path)

  let assert Ok(table_snap) = dict.get(loaded.tables, "posts")

  table_snap.columns
  |> should.equal([
    ColumnSnapshot(
      name: "id",
      column_type: "Id",
      nullable: False,
      has_default: False,
    ),
    ColumnSnapshot(
      name: "tags",
      column_type: "Array(String)",
      nullable: False,
      has_default: False,
    ),
    ColumnSnapshot(
      name: "matrix",
      column_type: "Array(Array(Int))",
      nullable: False,
      has_default: False,
    ),
  ])
}
