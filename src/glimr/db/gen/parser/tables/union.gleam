//// UNION Handling
////
//// Functions for splitting SQL queries on UNION and UNION ALL
//// keywords while preserving original case.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// ------------------------------------------------------------- Public Functions

/// Split SQL query on UNION and UNION ALL keywords, returning a
/// list of individual query parts. Preserves original case and
/// handles both UNION and UNION ALL.
///
pub fn split(sql: String) -> List(String) {
  do_split(string.uppercase(sql), sql, [])
  |> list.reverse
}

// ------------------------------------------------------------- Private Functions

/// Recursive helper to split on UNION keywords while preserving
/// original case. Tries UNION ALL first (longer match) before
/// plain UNION.
///
fn do_split(upper: String, original: String, acc: List(String)) -> List(String) {
  // Try UNION ALL first (longer match), then plain UNION
  case try_split_on_separator(upper, original, " UNION ALL ", 11) {
    Some(#(before, after_upper, after_original)) ->
      do_split(after_upper, after_original, [before, ..acc])
    None ->
      case try_split_on_separator(upper, original, " UNION ", 7) {
        Some(#(before, after_upper, after_original)) ->
          do_split(after_upper, after_original, [before, ..acc])
        None -> [original, ..acc]
      }
  }
}

/// Attempt to split on a separator, returning the before part
/// (from original), and the after parts (both upper and
/// original) for continued processing.
///
fn try_split_on_separator(
  upper: String,
  original: String,
  separator: String,
  separator_len: Int,
) -> Option(#(String, String, String)) {
  case string.split_once(upper, separator) {
    Ok(#(before, after)) -> {
      let before_len = string.length(before)
      let before_original = string.slice(original, 0, before_len)
      let after_original =
        string.drop_start(original, before_len + separator_len)
      Some(#(before_original, after, after_original))
    }
    Error(_) -> None
  }
}
