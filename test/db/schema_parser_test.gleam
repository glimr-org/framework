import gleam/option.{None, Some}
import gleeunit/should
import glimr/db/gen/schema_parser.{
  Array, BigInt, Blob, Boolean, Cascade, Column, Date, Decimal, DefaultAutoUuid,
  DefaultBool, DefaultEmptyArray, DefaultFloat, DefaultInt, DefaultNow,
  DefaultNull, DefaultString, DefaultUnixNow, Enum, Float, Foreign, Id, Index,
  Int, Json, NoAction, Restrict, SetDefault, SetNull, SmallInt, String, Text,
  Time, Timestamp, UnixTimestamp, Uuid,
}

// ------------------------------------------------------------- Basic Parsing

pub fn parse_simple_table_test() {
  let content =
    "
    pub const table_name = \"users\"

    pub fn define() {
      table(table_name, [
        id(),
        string(\"name\"),
      ])
    }
  "

  let assert Ok(table) = schema_parser.parse(content)

  table.name
  |> should.equal("users")

  table.columns
  |> should.equal([
    Column(
      name: "id",
      column_type: Id,
      nullable: False,
      default: None,
      renamed_from: None,
    ),
    Column(
      name: "name",
      column_type: String,
      nullable: False,
      default: None,
      renamed_from: None,
    ),
  ])
}

pub fn parse_missing_name_fails_test() {
  let content =
    "
    pub fn define() {
      table(table_name, [id()])
    }
  "

  case schema_parser.parse(content) {
    Error(msg) ->
      msg
      |> should.equal(
        "Could not find table name (pub const table_name = \"...\")",
      )
    Ok(_) -> should.fail()
  }
}

pub fn parse_missing_columns_fails_test() {
  let content =
    "
    pub const table_name = \"users\"

    pub fn other() {
      something_else()
    }
  "

  case schema_parser.parse(content) {
    Error(msg) -> msg |> should.equal("Could not find table column list")
    Ok(_) -> should.fail()
  }
}

// ------------------------------------------------------------- Column Types

pub fn parse_id_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [id()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("id")
  col.column_type |> should.equal(Id)
}

pub fn parse_string_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [string(\"email\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("email")
  col.column_type |> should.equal(String)
}

pub fn parse_text_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [text(\"bio\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("bio")
  col.column_type |> should.equal(Text)
}

pub fn parse_int_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [int(\"age\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("age")
  col.column_type |> should.equal(Int)
}

pub fn parse_bigint_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [bigint(\"count\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("count")
  col.column_type |> should.equal(BigInt)
}

pub fn parse_float_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [float(\"price\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("price")
  col.column_type |> should.equal(Float)
}

pub fn parse_boolean_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [boolean(\"active\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("active")
  col.column_type |> should.equal(Boolean)
}

pub fn parse_timestamp_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [timestamp(\"expires_at\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("expires_at")
  col.column_type |> should.equal(Timestamp)
}

pub fn parse_unix_timestamp_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [unix_timestamp(\"created_unix\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("created_unix")
  col.column_type |> should.equal(UnixTimestamp)
}

pub fn parse_date_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [date(\"birth_date\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("birth_date")
  col.column_type |> should.equal(Date)
}

pub fn parse_json_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [json(\"metadata\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("metadata")
  col.column_type |> should.equal(Json)
}

pub fn parse_uuid_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [uuid(\"external_id\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("external_id")
  col.column_type |> should.equal(Uuid)
}

pub fn parse_foreign_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [foreign(\"user_id\", \"users\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("user_id")
  col.column_type |> should.equal(Foreign("users", None, None))
}

// ------------------------------------------------------------- Nullable

pub fn parse_nullable_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [string(\"bio\") |> nullable()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.nullable |> should.be_true()
}

pub fn parse_non_nullable_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [string(\"name\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.nullable |> should.be_false()
}

// ------------------------------------------------------------- Default Values

pub fn parse_default_bool_true_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [boolean(\"active\") |> default_bool(True)]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.default |> should.equal(Some(DefaultBool(True)))
}

pub fn parse_default_bool_false_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [boolean(\"archived\") |> default_bool(False)]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.default |> should.equal(Some(DefaultBool(False)))
}

pub fn parse_default_string_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [string(\"role\") |> default_string(\"user\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.default |> should.equal(Some(DefaultString("user")))
}

pub fn parse_default_int_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [int(\"count\") |> default_int(0)]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.default |> should.equal(Some(DefaultInt(0)))
}

pub fn parse_default_float_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [float(\"rate\") |> default_float(0.0)]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.default |> should.equal(Some(DefaultFloat(0.0)))
}

pub fn parse_default_now_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [timestamp(\"created_at\") |> default_now()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.default |> should.equal(Some(DefaultNow))
}

pub fn parse_default_unix_now_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [unix_timestamp(\"created_at\") |> default_unix_now()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.column_type |> should.equal(UnixTimestamp)
  col.default |> should.equal(Some(DefaultUnixNow))
}

pub fn parse_auto_uuid_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [uuid(\"external_id\") |> auto_uuid()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.column_type |> should.equal(Uuid)
  col.default |> should.equal(Some(DefaultAutoUuid))
}

pub fn parse_default_null_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [string(\"nickname\") |> nullable() |> default_null()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.nullable |> should.be_true()
  col.default |> should.equal(Some(DefaultNull))
}

// ------------------------------------------------------------- Timestamps Helper

pub fn parse_timestamps_helper_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [id(), timestamps()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)

  // timestamps() expands to created_at and updated_at (without defaults)
  table.columns
  |> should.equal([
    Column(
      name: "id",
      column_type: Id,
      nullable: False,
      default: None,
      renamed_from: None,
    ),
    Column(
      name: "created_at",
      column_type: Timestamp,
      nullable: False,
      default: None,
      renamed_from: None,
    ),
    Column(
      name: "updated_at",
      column_type: Timestamp,
      nullable: False,
      default: None,
      renamed_from: None,
    ),
  ])
}

pub fn parse_unix_timestamps_helper_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [id(), unix_timestamps()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)

  table.columns
  |> should.equal([
    Column(
      name: "id",
      column_type: Id,
      nullable: False,
      default: None,
      renamed_from: None,
    ),
    Column(
      name: "created_at",
      column_type: UnixTimestamp,
      nullable: False,
      default: None,
      renamed_from: None,
    ),
    Column(
      name: "updated_at",
      column_type: UnixTimestamp,
      nullable: False,
      default: None,
      renamed_from: None,
    ),
  ])
}

// ------------------------------------------------------------- Renamed From

pub fn parse_rename_from_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [string(\"full_name\") |> rename_from(\"name\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("full_name")
  col.renamed_from |> should.equal(Some("name"))
}

// ------------------------------------------------------------- Combined Modifiers

pub fn parse_nullable_with_default_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [string(\"bio\") |> nullable() |> default_null()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.nullable |> should.be_true()
  col.default |> should.equal(Some(DefaultNull))
}

// ------------------------------------------------------------- Complete Table

pub fn parse_complete_table_test() {
  let content =
    "
    pub const table_name = \"users\"

    pub fn define() {
      table(table_name, [
        id(),
        string(\"email\"),
        string(\"name\"),
        string(\"bio\") |> nullable(),
        boolean(\"is_admin\") |> default_bool(False),
        foreign(\"organization_id\", \"organizations\") |> nullable(),
        timestamps(),
      ])
    }
  "

  let assert Ok(table) = schema_parser.parse(content)

  table.name
  |> should.equal("users")

  table.columns
  |> should.equal([
    Column(
      name: "id",
      column_type: Id,
      nullable: False,
      default: None,
      renamed_from: None,
    ),
    Column(
      name: "email",
      column_type: String,
      nullable: False,
      default: None,
      renamed_from: None,
    ),
    Column(
      name: "name",
      column_type: String,
      nullable: False,
      default: None,
      renamed_from: None,
    ),
    Column(
      name: "bio",
      column_type: String,
      nullable: True,
      default: None,
      renamed_from: None,
    ),
    Column(
      name: "is_admin",
      column_type: Boolean,
      nullable: False,
      default: Some(DefaultBool(False)),
      renamed_from: None,
    ),
    Column(
      name: "organization_id",
      column_type: Foreign("organizations", None, None),
      nullable: True,
      default: None,
      renamed_from: None,
    ),
    Column(
      name: "created_at",
      column_type: Timestamp,
      nullable: False,
      default: None,
      renamed_from: None,
    ),
    Column(
      name: "updated_at",
      column_type: Timestamp,
      nullable: False,
      default: None,
      renamed_from: None,
    ),
  ])
}

// ------------------------------------------------------------- Index Parsing

pub fn parse_no_indexes_test() {
  let content =
    "
    pub const table_name = \"users\"
    pub fn define() { table(table_name, [id()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)

  table.indexes
  |> should.equal([])
}

pub fn parse_single_index_test() {
  let content =
    "
    pub const table_name = \"users\"
    pub fn definition() {
      table(table_name, [
        id(),
        string(\"email\"),
      ])
      |> indexes([
        index([\"email\"]),
      ])
    }
  "

  let assert Ok(table) = schema_parser.parse(content)

  table.indexes
  |> should.equal([
    Index(columns: ["email"], unique: False, name: None),
  ])
}

pub fn parse_unique_index_test() {
  let content =
    "
    pub const table_name = \"users\"
    pub fn definition() {
      table(table_name, [id(), string(\"email\")])
      |> indexes([unique([\"email\"])])
    }
  "

  let assert Ok(table) = schema_parser.parse(content)

  table.indexes
  |> should.equal([
    Index(columns: ["email"], unique: True, name: None),
  ])
}

pub fn parse_composite_index_test() {
  let content =
    "
    pub const table_name = \"users\"
    pub fn definition() {
      table(table_name, [id(), string(\"first_name\"), string(\"last_name\")])
      |> indexes([index([\"first_name\", \"last_name\"])])
    }
  "

  let assert Ok(table) = schema_parser.parse(content)

  table.indexes
  |> should.equal([
    Index(columns: ["first_name", "last_name"], unique: False, name: None),
  ])
}

pub fn parse_named_index_test() {
  let content =
    "
    pub const table_name = \"users\"
    pub fn definition() {
      table(table_name, [id(), string(\"email\")])
      |> indexes([index([\"email\"]) |> named(\"idx_custom\")])
    }
  "

  let assert Ok(table) = schema_parser.parse(content)

  table.indexes
  |> should.equal([
    Index(columns: ["email"], unique: False, name: Some("idx_custom")),
  ])
}

pub fn parse_multiple_indexes_test() {
  let content =
    "
    pub const table_name = \"users\"
    pub fn definition() {
      table(table_name, [
        id(),
        string(\"email\"),
        string(\"first_name\"),
        string(\"last_name\"),
      ])
      |> indexes([
        unique([\"email\"]),
        index([\"first_name\", \"last_name\"]) |> named(\"idx_users_full_name\"),
      ])
    }
  "

  let assert Ok(table) = schema_parser.parse(content)

  table.indexes
  |> should.equal([
    Index(columns: ["email"], unique: True, name: None),
    Index(
      columns: ["first_name", "last_name"],
      unique: False,
      name: Some("idx_users_full_name"),
    ),
  ])
}

// ------------------------------------------------------------- Array Modifier

pub fn parse_string_array_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [string(\"tags\") |> array()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("tags")
  col.column_type |> should.equal(Array(String))
  col.nullable |> should.be_false()
}

pub fn parse_int_array_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [int(\"scores\") |> array()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("scores")
  col.column_type |> should.equal(Array(Int))
}

pub fn parse_float_array_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [float(\"weights\") |> array()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.column_type |> should.equal(Array(Float))
}

pub fn parse_boolean_array_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [boolean(\"flags\") |> array()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.column_type |> should.equal(Array(Boolean))
}

pub fn parse_nested_array_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [int(\"matrix\") |> array() |> array()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("matrix")
  col.column_type |> should.equal(Array(Array(Int)))
}

pub fn parse_triple_nested_array_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [int(\"cube\") |> array() |> array() |> array()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.column_type |> should.equal(Array(Array(Array(Int))))
}

pub fn parse_nullable_array_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [string(\"tags\") |> array() |> nullable()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.column_type |> should.equal(Array(String))
  col.nullable |> should.be_true()
}

pub fn parse_array_with_default_empty_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [string(\"tags\") |> array() |> default_empty_array()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.column_type |> should.equal(Array(String))
  col.default |> should.equal(Some(DefaultEmptyArray))
}

pub fn parse_nullable_array_with_default_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [string(\"tags\") |> array() |> nullable() |> default_empty_array()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.column_type |> should.equal(Array(String))
  col.nullable |> should.be_true()
  col.default |> should.equal(Some(DefaultEmptyArray))
}

pub fn parse_array_among_other_columns_test() {
  let content =
    "
    pub const table_name = \"posts\"
    pub fn define() {
      table(table_name, [
        id(),
        string(\"title\"),
        string(\"tags\") |> array(),
        int(\"scores\") |> array() |> array(),
        boolean(\"is_published\") |> default_bool(False),
        timestamps(),
      ])
    }
  "

  let assert Ok(table) = schema_parser.parse(content)

  table.columns
  |> should.equal([
    Column(
      name: "id",
      column_type: Id,
      nullable: False,
      default: None,
      renamed_from: None,
    ),
    Column(
      name: "title",
      column_type: String,
      nullable: False,
      default: None,
      renamed_from: None,
    ),
    Column(
      name: "tags",
      column_type: Array(String),
      nullable: False,
      default: None,
      renamed_from: None,
    ),
    Column(
      name: "scores",
      column_type: Array(Array(Int)),
      nullable: False,
      default: None,
      renamed_from: None,
    ),
    Column(
      name: "is_published",
      column_type: Boolean,
      nullable: False,
      default: Some(DefaultBool(False)),
      renamed_from: None,
    ),
    Column(
      name: "created_at",
      column_type: Timestamp,
      nullable: False,
      default: None,
      renamed_from: None,
    ),
    Column(
      name: "updated_at",
      column_type: Timestamp,
      nullable: False,
      default: None,
      renamed_from: None,
    ),
  ])
}

pub fn parse_uuid_array_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [uuid(\"ref_ids\") |> array()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.column_type |> should.equal(Array(Uuid))
}

pub fn parse_text_array_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [text(\"paragraphs\") |> array()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.column_type |> should.equal(Array(Text))
}

// ------------------------------------------------------------- Enum Parsing

pub fn parse_enum_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [schema.enum(\"status\", [\"active\", \"inactive\", \"banned\"])]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("status")
  col.column_type
  |> should.equal(Enum("status", ["active", "inactive", "banned"]))
}

pub fn parse_enum_with_name_override_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [schema.enum(\"status\", [\"active\", \"inactive\"]) |> enum_name(\"user_status\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.column_type
  |> should.equal(Enum("user_status", ["active", "inactive"]))
}

pub fn parse_enum_nullable_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [schema.enum(\"status\", [\"active\", \"inactive\"]) |> nullable()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.column_type |> should.equal(Enum("status", ["active", "inactive"]))
  col.nullable |> should.be_true()
}

pub fn parse_enum_with_default_string_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [schema.enum(\"status\", [\"active\", \"inactive\"]) |> default_string(\"active\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.column_type |> should.equal(Enum("status", ["active", "inactive"]))
  col.default |> should.equal(Some(DefaultString("active")))
}

// ------------------------------------------------------------- Decimal Parsing

pub fn parse_decimal_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [decimal(\"price\", 10, 2)]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("price")
  col.column_type |> should.equal(Decimal(10, 2))
}

// ------------------------------------------------------------- Blob Parsing

pub fn parse_blob_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [blob(\"data\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("data")
  col.column_type |> should.equal(Blob)
}

// ------------------------------------------------------------- Time Parsing

pub fn parse_time_column_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [time(\"starts_at\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("starts_at")
  col.column_type |> should.equal(Time)
}

// ------------------------------------------------------------- Foreign Key Actions

pub fn parse_foreign_on_delete_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [foreign(\"user_id\", \"users\") |> on_delete(schema.Cascade)]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("user_id")
  col.column_type |> should.equal(Foreign("users", Some(Cascade), None))
}

pub fn parse_foreign_on_update_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [foreign(\"user_id\", \"users\") |> on_update(schema.Restrict)]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.column_type |> should.equal(Foreign("users", None, Some(Restrict)))
}

pub fn parse_foreign_both_actions_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [foreign(\"user_id\", \"users\") |> on_delete(schema.Cascade) |> on_update(schema.SetNull)]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.column_type
  |> should.equal(Foreign("users", Some(Cascade), Some(SetNull)))
}

pub fn parse_foreign_set_default_action_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [foreign(\"user_id\", \"users\") |> on_delete(schema.SetDefault)]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.column_type |> should.equal(Foreign("users", Some(SetDefault), None))
}

pub fn parse_foreign_no_action_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [foreign(\"user_id\", \"users\") |> on_delete(schema.NoAction)]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.column_type |> should.equal(Foreign("users", Some(NoAction), None))
}

// ------------------------------------------------------------- SmallInt

pub fn parse_smallint_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [smallint(\"priority\")]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("priority")
  col.column_type |> should.equal(SmallInt)
}

pub fn parse_smallint_nullable_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [smallint(\"priority\") |> nullable()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [col] = table.columns

  col.name |> should.equal("priority")
  col.column_type |> should.equal(SmallInt)
  col.nullable |> should.be_true()
}

// ------------------------------------------------------------- Soft Deletes

pub fn parse_soft_deletes_test() {
  let content =
    "
    pub const table_name = \"test\"
    pub fn define() { table(table_name, [id(), soft_deletes()]) }
  "

  let assert Ok(table) = schema_parser.parse(content)
  let assert [_id, deleted_at] = table.columns

  deleted_at.name |> should.equal("deleted_at")
  deleted_at.column_type |> should.equal(Timestamp)
  deleted_at.nullable |> should.be_true()
}

pub fn parse_soft_deletes_with_other_columns_test() {
  let content =
    "
    pub const table_name = \"users\"
    pub fn define() {
      table(table_name, [
        id(),
        string(\"name\"),
        soft_deletes(),
        timestamps(),
      ])
    }
  "

  let assert Ok(table) = schema_parser.parse(content)

  let assert [_id, _name, deleted_at, _created_at, _updated_at] = table.columns
  deleted_at.name |> should.equal("deleted_at")
  deleted_at.nullable |> should.be_true()
}
