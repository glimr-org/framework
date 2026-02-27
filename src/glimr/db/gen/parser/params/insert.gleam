//// INSERT Parameter Extraction
////
//// Functions for extracting parameter-to-column mappings from
//// INSERT statements. Handles the VALUES clause to match
//// column positions with parameter positions.

import gleam/list
import gleam/option
import gleam/result
import gleam/string
import glimr/db/gen/parser/util

// ------------------------------------------------------------- Public Functions

/// Extract parameter mappings from INSERT statement. Matches
/// column positions with parameter positions in VALUES clause.
/// Pattern: INSERT INTO table (col1, col2) VALUES ($1, $2)
///
pub fn extract(sql: String) -> List(#(Int, String)) {
  do_extract(sql)
  |> result.unwrap([])
}

// ------------------------------------------------------------- Private Functions

/// Internal implementation that returns Result for cleaner
/// error handling with result.try. Parses column names and
/// delegates to values extraction.
///
fn do_extract(sql: String) -> Result(List(#(Int, String)), Nil) {
  let upper = string.uppercase(sql)

  use #(_, after_insert) <- result.try(string.split_once(upper, "INSERT INTO "))
  use #(_, after_paren) <- result.try(string.split_once(after_insert, "("))
  use #(_, _) <- result.try(string.split_once(after_paren, ")"))

  use #(before_open, _) <- result.try(string.split_once(upper, "("))
  use #(before_close, _) <- result.try(string.split_once(upper, ")"))

  let insert_offset = string.length(before_open) + 1
  let close_offset = string.length(before_close)
  let cols_str = string.slice(sql, insert_offset, close_offset - insert_offset)
  let columns =
    cols_str
    |> string.split(",")
    |> list.map(string.trim)

  Ok(extract_values_params(sql, columns))
}

/// Extract parameter-to-column mappings from a VALUES clause.
/// Matches column names from the INSERT column list with
/// parameter numbers from the VALUES list.
///
fn extract_values_params(
  sql: String,
  columns: List(String),
) -> List(#(Int, String)) {
  do_extract_values_params(sql, columns)
  |> result.unwrap([])
}

/// Internal implementation that returns Result for cleaner
/// error handling. Parses VALUES clause and zips with column
/// names to create mappings.
///
fn do_extract_values_params(
  sql: String,
  columns: List(String),
) -> Result(List(#(Int, String)), Nil) {
  let upper = string.uppercase(sql)

  use #(before_values, _) <- result.try(string.split_once(upper, "VALUES"))
  let values_offset = string.length(before_values) + 6
  let after_values = string.drop_start(sql, values_offset)

  use #(_, after_paren) <- result.try(string.split_once(after_values, "("))
  use #(values_str, _) <- result.try(string.split_once(after_paren, ")"))

  let values =
    values_str
    |> string.split(",")
    |> list.map(string.trim)

  Ok(
    list.zip(columns, values)
    |> list.filter_map(fn(pair) {
      let #(col, val) = pair
      case string.starts_with(val, "$") {
        False -> Error(Nil)
        True -> {
          let num_str = string.drop_start(val, 1)
          util.parse_int(num_str)
          |> option.map(fn(n) { #(n, col) })
          |> option.to_result(Nil)
        }
      }
    }),
  )
}
