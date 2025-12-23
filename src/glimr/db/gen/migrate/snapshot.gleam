//// ------------------------------------------------------------
//// Migration Snapshot
//// ------------------------------------------------------------
////
//// Handles schema snapshots for migration diffing. Snapshots
//// capture the current state of table schemas and are stored
//// as JSON between migration runs to detect changes.
////

import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option
import gleam/string
import glimr/db/gen/schema_parser.{type ColumnType, type Table}
import simplifile

// ------------------------------------------------------------- Public Types

/// ------------------------------------------------------------
/// Snapshot Type
/// ------------------------------------------------------------
///
/// Snapshot of all table schemas, stored as JSON between runs.
/// Used to detect what has changed since the last migration.
///
pub type Snapshot {
  Snapshot(tables: Dict(String, TableSnapshot))
}

/// ------------------------------------------------------------
/// Table Snapshot Type
/// ------------------------------------------------------------
///
/// Snapshot of a single table's column definitions.
///
pub type TableSnapshot {
  TableSnapshot(columns: List(ColumnSnapshot))
}

/// ------------------------------------------------------------
/// Column Snapshot Type
/// ------------------------------------------------------------
///
/// Snapshot of a single column's properties including name,
/// type, nullability, and whether it has a default value.
///
pub type ColumnSnapshot {
  ColumnSnapshot(
    name: String,
    column_type: String,
    nullable: Bool,
    has_default: Bool,
  )
}

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Load Snapshot
/// ------------------------------------------------------------
///
/// Load the schema snapshot from a JSON file. Returns an empty
/// snapshot if the file doesn't exist or can't be parsed.
///
pub fn load(path: String) -> Snapshot {
  case simplifile.read(path) {
    Ok(content) -> parse(content)
    Error(_) -> Snapshot(tables: dict.new())
  }
}

/// ------------------------------------------------------------
/// Save Snapshot
/// ------------------------------------------------------------
///
/// Save a snapshot to a JSON file.
///
pub fn save(path: String, snapshot: Snapshot) -> Result(Nil, Nil) {
  let content = to_json(snapshot)
  case simplifile.write(path, content) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error(Nil)
  }
}

/// ------------------------------------------------------------
/// Build Snapshot
/// ------------------------------------------------------------
///
/// Build a new snapshot from a list of parsed Table schemas.
///
pub fn build(tables: List(Table)) -> Snapshot {
  let table_dict =
    tables
    |> list.map(fn(table) {
      let columns =
        table.columns
        |> list.map(fn(col) {
          ColumnSnapshot(
            name: col.name,
            column_type: column_type_to_string(col.column_type),
            nullable: col.nullable,
            has_default: option.is_some(col.default),
          )
        })
      #(table.name, TableSnapshot(columns: columns))
    })
    |> dict.from_list()

  Snapshot(tables: table_dict)
}

/// ------------------------------------------------------------
/// Merge Snapshots
/// ------------------------------------------------------------
///
/// Merge new snapshot into old snapshot (used when filtering by
/// model). Tables in the new snapshot will overwrite those in
/// the old snapshot, but tables only in the old snapshot are
/// preserved.
///
pub fn merge(old: Snapshot, new: Snapshot) -> Snapshot {
  let merged_tables = dict.merge(old.tables, new.tables)
  Snapshot(tables: merged_tables)
}

/// ------------------------------------------------------------
/// Column Type To String
/// ------------------------------------------------------------
///
/// Convert a ColumnType to its string representation for
/// snapshots.
///
pub fn column_type_to_string(col_type: ColumnType) -> String {
  case col_type {
    schema_parser.Id -> "Id"
    schema_parser.String -> "String"
    schema_parser.Text -> "Text"
    schema_parser.Int -> "Int"
    schema_parser.BigInt -> "BigInt"
    schema_parser.Float -> "Float"
    schema_parser.Boolean -> "Boolean"
    schema_parser.Timestamp -> "Timestamp"
    schema_parser.UnixTimestamp -> "UnixTimestamp"
    schema_parser.Date -> "Date"
    schema_parser.Json -> "Json"
    schema_parser.Uuid -> "Uuid"
    schema_parser.Foreign(ref) -> "Foreign(" <> ref <> ")"
  }
}

// ------------------------------------------------------------- Private Functions

/// ------------------------------------------------------------
/// Parse Snapshot
/// ------------------------------------------------------------
///
/// Parse JSON content into a Snapshot. Returns empty on parse
/// failure.
///
fn parse(content: String) -> Snapshot {
  case json.parse(content, using: decoder()) {
    Ok(snapshot) -> snapshot
    Error(_) -> Snapshot(tables: dict.new())
  }
}

/// ------------------------------------------------------------
/// Snapshot Decoder
/// ------------------------------------------------------------
///
/// JSON decoder for the root Snapshot type.
///
fn decoder() -> decode.Decoder(Snapshot) {
  use tables <- decode.field(
    "tables",
    decode.dict(decode.string, table_decoder()),
  )
  decode.success(Snapshot(tables: tables))
}

/// ------------------------------------------------------------
/// Table Snapshot Decoder
/// ------------------------------------------------------------
///
/// JSON decoder for a TableSnapshot.
///
fn table_decoder() -> decode.Decoder(TableSnapshot) {
  use columns <- decode.field("columns", decode.list(column_decoder()))
  decode.success(TableSnapshot(columns: columns))
}

/// ------------------------------------------------------------
/// Column Snapshot Decoder
/// ------------------------------------------------------------
///
/// JSON decoder for a ColumnSnapshot.
///
fn column_decoder() -> decode.Decoder(ColumnSnapshot) {
  use name <- decode.field("name", decode.string)
  use column_type <- decode.field("type", decode.string)
  use nullable <- decode.field("nullable", decode.bool)
  use has_default <- decode.field("has_default", decode.bool)
  decode.success(ColumnSnapshot(
    name: name,
    column_type: column_type,
    nullable: nullable,
    has_default: has_default,
  ))
}

/// ------------------------------------------------------------
/// Snapshot To JSON
/// ------------------------------------------------------------
///
/// Serialize a Snapshot to formatted JSON.
///
fn to_json(snapshot: Snapshot) -> String {
  let tables_json =
    snapshot.tables
    |> dict.to_list()
    |> list.map(fn(pair) {
      let #(name, table) = pair
      "    \""
      <> name
      <> "\": {\n      \"columns\": [\n"
      <> table_to_json(table)
      <> "\n      ]\n    }"
    })
    |> string.join(",\n")

  "{\n  \"tables\": {\n" <> tables_json <> "\n  }\n}\n"
}

/// ------------------------------------------------------------
/// Table Snapshot To JSON
/// ------------------------------------------------------------
///
/// Serialize a TableSnapshot's columns to JSON.
///
fn table_to_json(table: TableSnapshot) -> String {
  table.columns
  |> list.map(fn(col) {
    "        {"
    <> "\"name\": \""
    <> col.name
    <> "\", "
    <> "\"type\": \""
    <> col.column_type
    <> "\", "
    <> "\"nullable\": "
    <> bool_to_json(col.nullable)
    <> ", "
    <> "\"has_default\": "
    <> bool_to_json(col.has_default)
    <> "}"
  })
  |> string.join(",\n")
}

/// ------------------------------------------------------------
/// Bool To JSON
/// ------------------------------------------------------------
///
/// Convert a Bool to its JSON representation (lowercase true/false).
///
fn bool_to_json(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}
