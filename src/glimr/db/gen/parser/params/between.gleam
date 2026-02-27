//// BETWEEN Parameter Extraction
////
//// Functions for extracting parameter-to-column mappings from
//// BETWEEN ... AND ... patterns. Names parameters as
//// start_<col> and end_<col> for clarity.

import gleam/list
import gleam/option
import gleam/result
import gleam/string
import glimr/db/gen/parser/util

// ------------------------------------------------------------- Public Functions

/// Extract params from BETWEEN ... AND ... patterns. Names the
/// parameters as start_<column> and end_<column> for range
/// queries on date/time and numeric columns.
///
pub fn extract(clause: String) -> List(#(Int, String)) {
  do_extract(string.uppercase(clause), clause, [])
}

// ------------------------------------------------------------- Private Functions

/// Recursive helper that finds BETWEEN patterns and extracts
/// the associated parameters. Uses start_/end_ naming for the
/// lower and upper bounds.
///
fn do_extract(
  upper: String,
  original: String,
  acc: List(#(Int, String)),
) -> List(#(Int, String)) {
  case string.split_once(upper, " BETWEEN ") {
    Error(_) -> acc
    Ok(#(before_between, after_between)) -> {
      let next_original =
        string.drop_start(original, string.length(before_between) + 9)

      let col =
        util.extract_last_identifier(string.slice(
          original,
          0,
          string.length(before_between),
        ))

      let new_params = case col {
        "" -> []
        c ->
          case util.is_sql_keyword(c) {
            True -> []
            False -> extract_pair_params(after_between, c)
          }
      }

      do_extract(after_between, next_original, list.append(acc, new_params))
    }
  }
}

/// Extract the two params from "BETWEEN $N AND $M" pattern.
/// Returns mappings for both start_<col> and end_<col> if both
/// parameters are found.
///
fn extract_pair_params(
  after_between: String,
  col: String,
) -> List(#(Int, String)) {
  case extract_first_param(after_between) {
    Error(_) -> []
    Ok(#(param1, rest1)) -> {
      let start_param = #(param1, "start_" <> col)
      case extract_second_param(rest1) {
        Error(_) -> [start_param]
        Ok(param2) -> [start_param, #(param2, "end_" <> col)]
      }
    }
  }
}

/// Extract the first parameter ($N) after BETWEEN keyword.
/// Returns the parameter number and remaining string for
/// continued parsing of the AND portion.
///
fn extract_first_param(s: String) -> Result(#(Int, String), Nil) {
  use #(_, after_dollar) <- result.try(string.split_once(s, "$"))
  let #(num_str, rest) = util.consume_digits(after_dollar, "")
  use param <- result.try(util.parse_int(num_str) |> option.to_result(Nil))
  Ok(#(param, rest))
}

/// Extract the second parameter ($M) after AND keyword. Returns
/// just the parameter number since this completes the BETWEEN
/// pattern extraction.
///
fn extract_second_param(rest: String) -> Result(Int, Nil) {
  use #(_, after_and) <- result.try(string.split_once(
    string.uppercase(rest),
    " AND ",
  ))
  use #(_, after_dollar) <- result.try(string.split_once(after_and, "$"))
  let #(num_str, _) = util.consume_digits(after_dollar, "")
  util.parse_int(num_str) |> option.to_result(Nil)
}
