//// ------------------------------------------------------------
//// Parameter Extraction
//// ------------------------------------------------------------
////
//// Functions for extracting SQL parameters ($1, $2, etc.) and
//// mapping them to column names. This module provides the main
//// entry points for parameter extraction.
////
//// The parsing logic is split across sub-modules:
////
//// - params/insert: INSERT statement handling
//// - params/update: UPDATE SET clause handling
//// - params/where: WHERE clause parsing
//// - params/between: BETWEEN pattern handling
////

import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/string
import glimr/db/gen/parser/params/insert
import glimr/db/gen/parser/params/update
import glimr/db/gen/parser/params/where
import glimr/db/gen/parser/util

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Extract Params
/// ------------------------------------------------------------
///
/// Extract all parameter numbers ($1, $2, etc.) from SQL query.
/// Returns a sorted, deduplicated list of parameter numbers.
///
pub fn extract(sql: String) -> List(Int) {
  do_extract_params(sql, [])
  |> list.unique
  |> list.sort(by: int.compare)
}

/// ------------------------------------------------------------
/// Extract Param Columns
/// ------------------------------------------------------------
///
/// Extract parameter-to-column mappings from SQL. Handles
/// INSERT, UPDATE SET, and WHERE clauses.
///
pub fn extract_columns(sql: String) -> List(#(Int, String)) {
  let insert_params = insert.extract(sql)
  let update_params = update.extract(sql)
  let where_params = where.extract(sql)

  // Merge all, preferring earlier sources (INSERT/UPDATE over WHERE for same param)
  let all_params = list.flatten([insert_params, update_params, where_params])

  // Deduplicate by param number, keeping first occurrence
  dedupe_by_param_num(all_params)
}

// ------------------------------------------------------------- Private Functions

/// ------------------------------------------------------------
/// Do Extract Params
/// ------------------------------------------------------------
///
/// Recursive helper that scans for $ followed by digits and
/// accumulates found parameter numbers.
///
fn do_extract_params(s: String, acc: List(Int)) -> List(Int) {
  case string.split_once(s, "$") {
    Ok(#(_, after_dollar)) -> {
      let #(num_str, rest) = util.consume_digits(after_dollar, "")
      case util.parse_int(num_str) {
        Some(n) -> do_extract_params(rest, [n, ..acc])
        _ -> do_extract_params(after_dollar, acc)
      }
    }
    Error(_) -> acc
  }
}

/// ------------------------------------------------------------
/// Dedupe By Param Num
/// ------------------------------------------------------------
///
/// Remove duplicate parameter mappings, keeping only the first
/// occurrence of each parameter number.
///
fn dedupe_by_param_num(params: List(#(Int, String))) -> List(#(Int, String)) {
  list.fold(params, #([], []), fn(state, param) {
    let #(seen, acc) = state
    let #(num, _) = param
    case list.contains(seen, num) {
      True -> state
      False -> #([num, ..seen], [param, ..acc])
    }
  })
  |> fn(state) { list.reverse(state.1) }
}
