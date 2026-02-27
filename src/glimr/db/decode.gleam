//// Database Decoder Utilities
////
//// Custom decoders that handle differences between PostgreSQL
//// and SQLite. SQLite lacks native boolean support and stores
//// booleans as integers (0/1), while PostgreSQL has true
//// booleans.
////
//// These decoders abstract over driver differences so
//// application code can work with Gleam types regardless of
//// the backend.

import gleam/dynamic/decode

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

// ------------------------------------------------------------- Private Functions

/// Decodes an integer as a boolean. Returns `True` for any
/// non-zero value, `False` for zero. Used as a fallback decoder
/// for SQLite which stores booleans as integers.
///
fn int_as_bool() -> decode.Decoder(Bool) {
  decode.int
  |> decode.map(fn(i) { i != 0 })
}
