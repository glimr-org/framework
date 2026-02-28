//// Schema Codegen Type Mapping
////
//// The schema generator reads your migration files and writes
//// Gleam model code — type definitions, decoders, and JSON
//// encoders. But it needs to know that a Timestamp column
//// becomes a String in Gleam, uses decode.string for query
//// results, and json.string for API output. These three
//// mapping functions are the single source of truth for those
//// decisions, so every generated model stays consistent.

import glimr/db/gen/schema_parser.{
  type ColumnType, Array, BigInt, Boolean, Date, Float, Foreign, Id, Int, Json,
  String, Text, Timestamp, UnixTimestamp, Uuid,
}

// ------------------------------------------------------------- Public Functions

/// Database columns like Timestamp and Date store temporal data
/// but come across the wire as strings — representing them as
/// String in Gleam avoids pulling in a datetime library as a
/// hard dependency. Foreign keys are always integer IDs
/// regardless of what they point to, and Boolean maps to Bool
/// rather than Gleam's bitwise types.
///
pub fn gleam_type(col_type: ColumnType) -> String {
  case col_type {
    Id -> "Int"
    String -> "String"
    Text -> "String"
    Int -> "Int"
    BigInt -> "Int"
    Float -> "Float"
    Boolean -> "Bool"
    Timestamp -> "String"
    UnixTimestamp -> "Int"
    Date -> "String"
    Json -> "String"
    Uuid -> "String"
    Foreign(_) -> "Int"
    Array(inner) -> "List(" <> gleam_type(inner) <> ")"
  }
}

/// Generated models need decoders to parse database query
/// results into typed records. Most columns use the obvious
/// decoder, but Boolean is the exception — SQLite returns
/// booleans as 0/1 integers, so we use a custom
/// glimr_decode.bool() that handles both true/false and 0/1
/// instead of the standard decode.bool which would fail on
/// SQLite results.
///
pub fn decoder_fn(col_type: ColumnType) -> String {
  case col_type {
    Id -> "decode.int"
    String -> "decode.string"
    Text -> "decode.string"
    Int -> "decode.int"
    BigInt -> "decode.int"
    Float -> "decode.float"
    Boolean -> "glimr_decode.bool()"
    Timestamp -> "decode.string"
    UnixTimestamp -> "decode.int"
    Date -> "decode.string"
    Json -> "decode.string"
    Uuid -> "decode.string"
    Foreign(_) -> "decode.int"
    Array(inner) -> "glimr_decode.list_of(" <> decoder_fn(inner) <> ")"
  }
}

/// The schema generator writes JSON encoder functions for each
/// model so developers don't have to maintain them by hand.
/// Picking the right json.* call per column type here means a
/// Timestamp field gets json.string (ISO format) and a Foreign
/// key gets json.int — getting this wrong would produce JSON
/// that silently breaks API consumers.
///
pub fn json_encoder_fn(col_type: ColumnType) -> String {
  case col_type {
    Id -> "json.int"
    String -> "json.string"
    Text -> "json.string"
    Int -> "json.int"
    BigInt -> "json.int"
    Float -> "json.float"
    Boolean -> "json.bool"
    Timestamp -> "json.string"
    UnixTimestamp -> "json.int"
    Date -> "json.string"
    Json -> "json.string"
    Uuid -> "json.string"
    Foreign(_) -> "json.int"
    Array(_) -> panic as "Use generate_encoder_expr for Array types"
  }
}

/// Scalars and arrays encode completely differently —
/// `json.int(model.age)` vs `json.array(model.tags,
/// json.string)`. The argument order flips and arrays need a
/// mapping function instead of a direct call. This builds the
/// right expression for either case, and for nested arrays it
/// recurses to produce the right nesting of `json.array` calls.
///
pub fn json_encoder_expr(col_type: ColumnType, accessor: String) -> String {
  case col_type {
    Array(inner) -> {
      let inner_fn = json_array_item_encoder(inner)
      "json.array(" <> accessor <> ", " <> inner_fn <> ")"
    }
    _ -> json_encoder_fn(col_type) <> "(" <> accessor <> ")"
  }
}

/// `json.array` takes a function that encodes each element. For
/// `List(String)` that's just `json.string`, but for
/// `List(List(String))` we need `fn(v) { json.array(v,
/// json.string) }` — a closure that itself calls `json.array`.
/// This recursion handles arbitrary nesting depth.
///
fn json_array_item_encoder(col_type: ColumnType) -> String {
  case col_type {
    Array(inner) -> {
      let inner_fn = json_array_item_encoder(inner)
      "fn(v) { json.array(v, " <> inner_fn <> ") }"
    }
    _ -> json_encoder_fn(col_type)
  }
}

/// Array columns need different codegen paths — different
/// import (`glimr_decode`), different encoder expression
/// (`json.array` instead of `json.int`), different decoder
/// (`glimr_decode.list_of` instead of `decode.int`). The
/// generator checks this to decide which path to take.
///
pub fn is_array(col_type: ColumnType) -> Bool {
  case col_type {
    Array(_) -> True
    _ -> False
  }
}
