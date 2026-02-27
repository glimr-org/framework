//// Unix Timestamp Utils
////
//// Convenience functions for working with Unix timestamps.
//// Returns the current time as integer seconds or nanoseconds
//// since the Unix epoch (January 1, 1970). Useful for storing
//// timestamps in database columns with unix_timestamp().

import gleam/time/timestamp

// ------------------------------------------------------------- Public Functions

/// Returns the current Unix timestamp in seconds since epoch.
/// Use this for storing timestamps in database columns with the
/// unix_timestamp() schema type.
///
/// *Example:*
///
/// ```gleam
/// let created_at = unix_timestamp.now()
/// // => 1734700800
/// ```
///
pub fn now() -> Int {
  let #(seconds, _) = {
    timestamp.system_time()
    |> timestamp.to_unix_seconds_and_nanoseconds()
  }

  seconds
}

/// Returns the nanosecond component of the current timestamp.
/// Useful when you need sub-second precision in addition to the
/// seconds from now().
///
/// *Example:*
///
/// ```gleam
/// let nano = unix_timestamp.now_nano()
/// // => 123456789
/// ```
///
pub fn now_nano() -> Int {
  let #(_, nano) = {
    timestamp.system_time()
    |> timestamp.to_unix_seconds_and_nanoseconds()
  }

  nano
}
