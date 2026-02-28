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
