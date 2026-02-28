//// Database Decoder Utilities
////
//// Custom decoders that handle differences between PostgreSQL
//// and SQLite. SQLite lacks native boolean support and stores
//// booleans as integers (0/1), while PostgreSQL has true
//// booleans. Array columns come back as native Erlang lists in
//// PostgreSQL but as JSON text in SQLite.
////
//// These decoders abstract over driver differences so
//// application code can work with Gleam types regardless of
//// the backend.

import gleam/dynamic/decode
import gleam/json

// ------------------------------------------------------------- Public Functions

/// Decodes a boolean value from either a native boolean
/// (PostgreSQL) or an integer 0/1 (SQLite). Returns `True` for
/// any non-zero integer, `False` for zero.
///
/// *Example:*
///
/// ```gleam
/// let decoder = {
///   use active <- decode.field("active", db_decode.bool())
///   decode.success(active)
/// }
/// ```
///
pub fn bool() -> decode.Decoder(Bool) {
  decode.one_of(decode.bool, [int_as_bool()])
}

/// PostgreSQL returns array columns as native Erlang lists, but
/// SQLite doesn't have arrays — it stores them as JSON text
/// like `["a","b"]`. Without this decoder, generated models
/// would need separate code paths per database. Trying the
/// native list first means Postgres takes the fast path, and
/// the JSON fallback only kicks in for SQLite.
///
pub fn list_of(inner: decode.Decoder(a)) -> decode.Decoder(List(a)) {
  decode.one_of(decode.list(inner), [json_string_to_list(inner)])
}

// ------------------------------------------------------------- Private Functions

/// Decodes an integer as a boolean. Returns `True` for any
/// non-zero value, `False` for zero. Used as a fallback decoder
/// for SQLite which stores booleans as integers.
///
fn int_as_bool() -> decode.Decoder(Bool) {
  decode.int
  |> decode.map(fn(i) { i != 0 })
}

/// SQLite fallback for array columns. The value arrives as a
/// plain string like `["a","b"]`, so we decode it as a string
/// first, then parse that string as JSON. An empty list on
/// parse failure is intentional — a corrupted JSON array in a
/// column shouldn't crash the whole query, and an empty list is
/// a safe degradation.
///
fn json_string_to_list(inner: decode.Decoder(a)) -> decode.Decoder(List(a)) {
  decode.string
  |> decode.then(fn(json_str) {
    case json.parse(json_str, decode.list(inner)) {
      Ok(items) -> decode.success(items)
      Error(_) -> decode.success([])
    }
  })
}
