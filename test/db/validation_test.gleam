import gleam/option.{None, Some}
import gleeunit/should
import glimr/db/gen/migrate/validation
import glimr/db/gen/schema_parser.{Column, Index, Table}

// ------------------------------------------------------------- Valid Indexes

pub fn validate_valid_indexes_test() {
  let tables = [
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("email", schema_parser.String, False, None, None),
        Column("first_name", schema_parser.String, False, None, None),
      ],
      indexes: [
        Index(columns: ["email"], unique: True, name: None),
        Index(
          columns: ["first_name", "email"],
          unique: False,
          name: Some("idx_users_name_email"),
        ),
      ],
    ),
  ]

  // Should not panic
  validation.validate_indexes(tables)
}

pub fn validate_empty_indexes_test() {
  let tables = [
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
      ],
      indexes: [],
    ),
  ]

  // Should not panic
  validation.validate_indexes(tables)
}

// ------------------------------------------------------------- Invalid: Non-existent Column

pub fn validate_nonexistent_column_panics_test() {
  let tables = [
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
      ],
      indexes: [
        Index(columns: ["nonexistent"], unique: False, name: None),
      ],
    ),
  ]

  let result = panic_to_result(fn() { validation.validate_indexes(tables) })

  result |> should.be_error()
}

// ------------------------------------------------------------- Invalid: Duplicate Indexes

pub fn validate_duplicate_indexes_panics_test() {
  let tables = [
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("email", schema_parser.String, False, None, None),
      ],
      indexes: [
        Index(columns: ["email"], unique: True, name: None),
        Index(columns: ["email"], unique: True, name: Some("dupe")),
      ],
    ),
  ]

  let result = panic_to_result(fn() { validation.validate_indexes(tables) })

  result |> should.be_error()
}

pub fn validate_same_columns_different_unique_not_duplicate_test() {
  let tables = [
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("email", schema_parser.String, False, None, None),
      ],
      indexes: [
        Index(columns: ["email"], unique: True, name: None),
        Index(columns: ["email"], unique: False, name: None),
      ],
    ),
  ]

  // Same columns but different unique flag — should not panic
  validation.validate_indexes(tables)
}

// ------------------------------------------------------------- Valid Array Types

pub fn validate_valid_array_types_test() {
  let tables = [
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
        Column(
          "scores",
          schema_parser.Array(schema_parser.Int),
          False,
          None,
          None,
        ),
        Column(
          "matrix",
          schema_parser.Array(schema_parser.Array(schema_parser.Float)),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    ),
  ]

  // Should not panic
  validation.validate_array_types(tables)
}

pub fn validate_array_of_boolean_test() {
  let tables = [
    Table(
      name: "test",
      columns: [
        Column(
          "flags",
          schema_parser.Array(schema_parser.Boolean),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    ),
  ]

  // Should not panic
  validation.validate_array_types(tables)
}

pub fn validate_array_of_uuid_test() {
  let tables = [
    Table(
      name: "test",
      columns: [
        Column(
          "refs",
          schema_parser.Array(schema_parser.Uuid),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    ),
  ]

  // Should not panic
  validation.validate_array_types(tables)
}

pub fn validate_array_of_timestamp_test() {
  let tables = [
    Table(
      name: "test",
      columns: [
        Column(
          "dates",
          schema_parser.Array(schema_parser.Timestamp),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    ),
  ]

  // Should not panic
  validation.validate_array_types(tables)
}

pub fn validate_no_arrays_test() {
  let tables = [
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("name", schema_parser.String, False, None, None),
      ],
      indexes: [],
    ),
  ]

  // Should not panic
  validation.validate_array_types(tables)
}

// ------------------------------------------------------------- Invalid: Array(Id)

pub fn validate_array_of_id_panics_test() {
  let tables = [
    Table(
      name: "bad",
      columns: [
        Column("ids", schema_parser.Array(schema_parser.Id), False, None, None),
      ],
      indexes: [],
    ),
  ]

  let result = panic_to_result(fn() { validation.validate_array_types(tables) })

  result |> should.be_error()
}

// ------------------------------------------------------------- Invalid: Array(Foreign)

pub fn validate_array_of_foreign_panics_test() {
  let tables = [
    Table(
      name: "bad",
      columns: [
        Column(
          "user_ids",
          schema_parser.Array(schema_parser.Foreign("users")),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    ),
  ]

  let result = panic_to_result(fn() { validation.validate_array_types(tables) })

  result |> should.be_error()
}

// ------------------------------------------------------------- Invalid: Nested Array(Id)

pub fn validate_nested_array_of_id_panics_test() {
  let tables = [
    Table(
      name: "bad",
      columns: [
        Column(
          "ids",
          schema_parser.Array(schema_parser.Array(schema_parser.Id)),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    ),
  ]

  let result = panic_to_result(fn() { validation.validate_array_types(tables) })

  result |> should.be_error()
}

pub fn validate_nested_array_of_foreign_panics_test() {
  let tables = [
    Table(
      name: "bad",
      columns: [
        Column(
          "refs",
          schema_parser.Array(
            schema_parser.Array(schema_parser.Foreign("users")),
          ),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    ),
  ]

  let result = panic_to_result(fn() { validation.validate_array_types(tables) })

  result |> should.be_error()
}

// ------------------------------------------------------------- Helper

@external(erlang, "glimr_test_helpers", "panic_to_result")
fn panic_to_result(f: fn() -> Nil) -> Result(Nil, String)
