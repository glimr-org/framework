//// ------------------------------------------------------------
//// Schema Parser Code Generation Helpers
//// ------------------------------------------------------------
////
//// Helper functions for code generation that map column types
//// to their Gleam type names and decoder functions.
////

import glimr/db/gen/schema_parser.{
  type ColumnType, BigInt, Boolean, Date, Float, Foreign, Id, Int, Json, String,
  Text, Timestamp, UnixTimestamp, Uuid,
}

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Gleam Type
/// ------------------------------------------------------------
///
/// Get the Gleam type name for a column type. Used for code
/// generation to determine the appropriate Gleam type.
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

/// ------------------------------------------------------------
/// Decoder Function
/// ------------------------------------------------------------
///
/// Get the decoder function name for a column type. Used for
/// code generation to determine the appropriate decoder.
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
