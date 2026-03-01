//// Migration Snapshot
////
//// The migration system needs to know what the schema looked
//// like last time to figure out what changed. This module
//// saves the current schema as a JSON snapshot after each
//// migration run, then loads it back on the next run for
//// diffing. Without it, every run would think the entire
//// schema is new and try to CREATE TABLE on tables that
//// already exist.

import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import glimr/db/gen/schema_parser.{type ColumnType, type Table}
import simplifile

// ------------------------------------------------------------- Public Types

/// The top-level container saved to
/// `.glimr_schema_snapshot.json`. It's a dictionary keyed by
/// table name so the diff algorithm can look up any table's
/// previous state in O(1) instead of scanning a list.
///
pub type Snapshot {
  Snapshot(tables: Dict(String, TableSnapshot))
}

/// Columns are kept in definition order (not sorted) so the
/// diff can detect position changes if needed in the future.
/// Indexes live alongside columns because they're part of the
/// same table definition and need to be diffed together.
///
pub type TableSnapshot {
  TableSnapshot(columns: List(ColumnSnapshot), indexes: List(IndexSnapshot))
}

/// Captures an index's state at snapshot time so the next
/// migration run can detect whether indexes were added,
/// removed, or changed. The optional name preserves custom
/// names through the snapshot so they survive across migration
/// generations.
///
pub type IndexSnapshot {
  IndexSnapshot(columns: List(String), unique: Bool, name: Option(String))
}

/// Stores just enough about a column to detect changes worth
/// migrating: type, nullability, and whether a default exists.
/// The actual default value isn't stored because changing a
/// default doesn't require a migration — it only affects new
/// rows, not the table structure.
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

/// Reads the snapshot file from disk. Returns an empty snapshot
/// if the file doesn't exist (first run) or can't be parsed
/// (corrupted file). Falling back to empty means the differ
/// will treat every table as new and generate full CREATE TABLE
/// statements — which is the right thing on a fresh project.
///
pub fn load(path: String) -> Snapshot {
  case simplifile.read(path) {
    Ok(content) -> parse(content)
    Error(_) -> Snapshot(tables: dict.new())
  }
}

/// Writes the snapshot to disk after migration generation. The
/// JSON is human-readable (indented, one column per line) so
/// developers can inspect it when debugging why a migration was
/// or wasn't generated. Returns Error only if the file write
/// fails.
///
pub fn save(path: String, snapshot: Snapshot) -> Result(Nil, Nil) {
  let content = to_json(snapshot)
  case simplifile.write(path, content) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error(Nil)
  }
}

/// Converts live Table definitions into a snapshot that can be
/// saved to JSON. Strips out information the diff doesn't need
/// (like the actual default value — only whether one exists
/// matters) to keep the snapshot file small and the diff logic
/// simple.
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
      let indexes =
        table.indexes
        |> list.map(fn(idx) {
          IndexSnapshot(
            columns: idx.columns,
            unique: idx.unique,
            name: idx.name,
          )
        })
      #(table.name, TableSnapshot(columns: columns, indexes: indexes))
    })
    |> dict.from_list()

  Snapshot(tables: table_dict)
}

/// When generating a migration for just one model, we only have
/// a snapshot of that model's tables. Without merging, saving
/// would wipe out all other tables from the snapshot file, and
/// the next full run would think those tables are new. This
/// overlays the filtered snapshot onto the existing one so
/// unrelated tables are preserved.
///
pub fn merge(old: Snapshot, new: Snapshot) -> Snapshot {
  let merged_tables = dict.merge(old.tables, new.tables)
  Snapshot(tables: merged_tables)
}

/// Column types are stored as strings in the JSON snapshot
/// because Gleam's custom types can't be directly serialized.
/// The string format is stable across versions — changing it
/// would make every existing snapshot look like a type change,
/// generating spurious ALTER COLUMN migrations.
///
pub fn column_type_to_string(col_type: ColumnType) -> String {
  case col_type {
    schema_parser.Id -> "Id"
    schema_parser.String -> "String"
    schema_parser.Text -> "Text"
    schema_parser.Int -> "Int"
    schema_parser.SmallInt -> "SmallInt"
    schema_parser.BigInt -> "BigInt"
    schema_parser.Float -> "Float"
    schema_parser.Boolean -> "Boolean"
    schema_parser.Timestamp -> "Timestamp"
    schema_parser.UnixTimestamp -> "UnixTimestamp"
    schema_parser.Date -> "Date"
    schema_parser.Json -> "Json"
    schema_parser.Uuid -> "Uuid"
    schema_parser.Foreign(ref, on_delete, on_update) -> {
      let base = "Foreign(" <> ref
      let del = case on_delete {
        Some(action) -> ",onDelete:" <> foreign_action_to_string(action)
        None -> ""
      }
      let upd = case on_update {
        Some(action) -> ",onUpdate:" <> foreign_action_to_string(action)
        None -> ""
      }
      base <> del <> upd <> ")"
    }
    schema_parser.Array(inner) ->
      "Array(" <> column_type_to_string(inner) <> ")"
    schema_parser.Enum(name, variants) ->
      "Enum(" <> name <> ":" <> string.join(variants, ",") <> ")"
    schema_parser.Decimal(p, s) ->
      "Decimal(" <> int.to_string(p) <> "," <> int.to_string(s) <> ")"
    schema_parser.Blob -> "Blob"
    schema_parser.Time -> "Time"
  }
}

/// The snapshot stores FK actions as readable strings like
/// "Cascade" and "SetNull" rather than the SQL keywords
/// "CASCADE" and "SET NULL". This keeps the snapshot format
/// database-agnostic — the SQL module handles the dialect-
/// specific conversion when generating migrations.
///
fn foreign_action_to_string(action: schema_parser.ForeignAction) -> String {
  case action {
    schema_parser.Cascade -> "Cascade"
    schema_parser.Restrict -> "Restrict"
    schema_parser.SetNull -> "SetNull"
    schema_parser.SetDefault -> "SetDefault"
    schema_parser.NoAction -> "NoAction"
  }
}

// ------------------------------------------------------------- Private Functions

/// Deserializes the JSON file content into a Snapshot. Falls
/// back to an empty snapshot on any parse error rather than
/// crashing — a corrupted snapshot file shouldn't block the
/// developer; they'll just get a migration that recreates
/// everything.
///
fn parse(content: String) -> Snapshot {
  case json.parse(content, using: decoder()) {
    Ok(snapshot) -> snapshot
    Error(_) -> Snapshot(tables: dict.new())
  }
}

/// The snapshot format is intentionally simple — just
/// `{"tables": {...}}` at the top level. If we ever need to
/// store metadata (schema version, generation timestamp),
/// there's room to add sibling keys without breaking existing
/// snapshots.
///
fn decoder() -> decode.Decoder(Snapshot) {
  use tables <- decode.field(
    "tables",
    decode.dict(decode.string, table_decoder()),
  )
  decode.success(Snapshot(tables: tables))
}

/// Decodes a single table's snapshot. The `indexes` field is
/// optional so that snapshot files from before index support
/// was added still load correctly — they'll just have an empty
/// index list, and the diff will see all current indexes as new
/// additions.
///
fn table_decoder() -> decode.Decoder(TableSnapshot) {
  use columns <- decode.field("columns", decode.list(column_decoder()))
  use indexes <- decode.optional_field(
    "indexes",
    [],
    decode.list(index_decoder()),
  )
  decode.success(TableSnapshot(columns: columns, indexes: indexes))
}

/// Decodes an index from the snapshot JSON file. Mirrors the
/// structure written by indexes_to_json so a round-trip through
/// save/load produces identical data for the diff algorithm to
/// compare against.
///
fn index_decoder() -> decode.Decoder(IndexSnapshot) {
  use columns <- decode.field("columns", decode.list(decode.string))
  use is_unique <- decode.field("unique", decode.bool)
  use name <- decode.field("name", decode.optional(decode.string))
  decode.success(IndexSnapshot(columns: columns, unique: is_unique, name: name))
}

/// Decodes one column entry from the snapshot. The field names
/// here (`name`, `type`, `nullable`, `has_default`) must stay
/// in sync with `columns_to_json` — if either side changes, the
/// round-trip breaks and every column looks "altered" on the
/// next migration run.
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

/// Hand-rolled JSON serialization instead of using a library
/// because we want exact control over formatting. The snapshot
/// file gets committed to version control, so consistent
/// indentation and key ordering prevents noisy diffs when
/// nothing actually changed.
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
      <> columns_to_json(table)
      <> "\n      ],\n      \"indexes\": [\n"
      <> indexes_to_json(table)
      <> "\n      ]\n    }"
    })
    |> string.join(",\n")

  "{\n  \"tables\": {\n" <> tables_json <> "\n  }\n}\n"
}

/// The write side of the snapshot round-trip. If a field name
/// here drifts from what `column_decoder` expects, every column
/// silently looks "changed" on the next run and you get a
/// migration full of no-op ALTER statements. Keep these two in
/// lockstep.
///
fn columns_to_json(table: TableSnapshot) -> String {
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

/// Serializes a table's indexes to JSON for the snapshot file.
/// Each index becomes a JSON object with columns, unique, and
/// name fields — matching the structure that index_decoder
/// expects on the read side.
///
fn indexes_to_json(table: TableSnapshot) -> String {
  table.indexes
  |> list.map(fn(idx) {
    let name_json = case idx.name {
      Some(n) -> "\"" <> n <> "\""
      None -> "null"
    }
    let cols_json =
      idx.columns
      |> list.map(fn(c) { "\"" <> c <> "\"" })
      |> string.join(", ")
    "        {"
    <> "\"columns\": ["
    <> cols_json
    <> "], "
    <> "\"unique\": "
    <> bool_to_json(idx.unique)
    <> ", "
    <> "\"name\": "
    <> name_json
    <> "}"
  })
  |> string.join(",\n")
}

/// JSON requires lowercase `true`/`false` — Gleam's
/// `True`/`False` would be invalid JSON and silently break the
/// snapshot parser on the next load.
///
fn bool_to_json(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}
