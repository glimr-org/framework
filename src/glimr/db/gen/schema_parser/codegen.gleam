//// Schema Codegen Type Mapping
////
//// The schema generator reads your migration files and writes
//// Gleam model code — type definitions, decoders, and JSON
//// encoders. But it needs to know that a Timestamp column
//// becomes a String in Gleam, uses decode.string for query
//// results, and json.string for API output. These three
//// mapping functions are the single source of truth for those
//// decisions, so every generated model stays consistent.

import gleam/string
import glimr/db/gen/schema_parser.{
  type ColumnType, Array, BigInt, Blob, Boolean, Date, Decimal, Enum, Float,
  Foreign, Id, Int, Json, SmallInt, String, Text, Time, Timestamp, UnixTimestamp,
  Uuid,
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
    SmallInt -> "Int"
    BigInt -> "Int"
    Float -> "Float"
    Boolean -> "Bool"
    Timestamp -> "String"
    UnixTimestamp -> "Int"
    Date -> "String"
    Json -> "String"
    Uuid -> "String"
    Foreign(_, _, _) -> "Int"
    Array(inner) -> "List(" <> gleam_type(inner) <> ")"
    Enum(name, _) -> enum_type_name(name)
    Decimal(_, _) -> "String"
    Blob -> "BitArray"
    Time -> "String"
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
    SmallInt -> "decode.int"
    BigInt -> "decode.int"
    Float -> "decode.float"
    Boolean -> "glimr_decode.bool()"
    Timestamp -> "decode.string"
    UnixTimestamp -> "decode.int"
    Date -> "decode.string"
    Json -> "decode.string"
    Uuid -> "decode.string"
    Foreign(_, _, _) -> "decode.int"
    Array(inner) -> "glimr_decode.list_of(" <> decoder_fn(inner) <> ")"
    Enum(_, _) -> "decode.string"
    Decimal(_, _) -> "decode.string"
    Blob -> "decode.bit_array"
    Time -> "decode.string"
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
    SmallInt -> "json.int"
    BigInt -> "json.int"
    Float -> "json.float"
    Boolean -> "json.bool"
    Timestamp -> "json.string"
    UnixTimestamp -> "json.int"
    Date -> "json.string"
    Json -> "json.string"
    Uuid -> "json.string"
    Foreign(_, _, _) -> "json.int"
    Array(_) -> panic as "Use json_encoder_expr for Array types"
    Enum(_, _) -> panic as "Use json_encoder_expr for Enum types"
    Blob -> panic as "Use json_encoder_expr for Blob types"
    Decimal(_, _) -> "json.string"
    Time -> "json.string"
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
    Enum(name, _) -> {
      let fn_name = snake_case(name) <> "_to_string"
      "json.string(" <> fn_name <> "(" <> accessor <> "))"
    }
    Blob -> "json.string(bit_array.base64_encode(" <> accessor <> ", True))"
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

/// Blobs go through base64 encoding for JSON output and need a
/// `gleam/bit_array` import that other column types don't. The
/// generator uses this check to add that import only when the
/// table actually has a blob column.
///
pub fn is_blob(col_type: ColumnType) -> Bool {
  case col_type {
    Blob -> True
    _ -> False
  }
}

/// Enum columns get their own Gleam custom type (e.g. `type
/// Status { Active | Inactive }`) plus converter functions. The
/// generator needs to know which columns are enums so it can
/// emit these type definitions alongside the model code.
///
pub fn is_enum(col_type: ColumnType) -> Bool {
  case col_type {
    Enum(_, _) -> True
    _ -> False
  }
}

/// Gleam requires PascalCase for custom type names, but schema
/// column names are snake_case. An enum column named
/// `payment_status` with variants becomes a Gleam type
/// `PaymentStatus` — this does that conversion.
///
pub fn enum_type_name(name: String) -> String {
  pascal_case(name)
}

// ------------------------------------------------------------- Private Functions

/// Splits on underscores, capitalizes each word, and joins
/// them. `payment_status` becomes `PaymentStatus`. This is the
/// inverse of `snake_case` and both are needed because Gleam
/// types use PascalCase but database columns use snake_case.
///
fn pascal_case(s: String) -> String {
  s
  |> string.split("_")
  |> list.map(capitalize)
  |> string.join("")
}

import gleam/list

/// Building block for `pascal_case` — uppercases just the first
/// grapheme while leaving the rest untouched. Using grapheme
/// operations instead of byte slicing means this handles
/// Unicode correctly.
///
fn capitalize(s: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(first, rest)) -> string.uppercase(first) <> rest
    Error(_) -> s
  }
}

/// Enum type names arrive in PascalCase from `enum_type_name`,
/// but some codegen paths need the snake_case version for
/// function names (e.g. `status_to_string`). This inserts
/// underscores before uppercase letters that follow lowercase
/// ones.
///
fn snake_case(s: String) -> String {
  do_snake_case(string.to_graphemes(s), "", False)
}

/// Tracks whether the previous character was lowercase so we
/// know when a capital letter marks a word boundary.
/// `PaymentStatus` → `P` (no underscore, start of string),
/// `ayment` (lowercase), `S` (uppercase after lowercase →
/// insert underscore) → `payment_status`.
///
fn do_snake_case(chars: List(String), acc: String, prev_lower: Bool) -> String {
  case chars {
    [] -> string.lowercase(acc)
    [c, ..rest] -> {
      let is_upper = c == string.uppercase(c) && c != string.lowercase(c)
      case is_upper && prev_lower {
        True -> do_snake_case(rest, acc <> "_" <> c, False)
        False -> do_snake_case(rest, acc <> c, !is_upper)
      }
    }
  }
}
