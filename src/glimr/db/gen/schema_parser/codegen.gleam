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
  type ColumnType, BigInt, Boolean, Date, Float, Foreign, Id, Int, Json, String,
  Text, Timestamp, UnixTimestamp, Uuid,
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
  }
}
