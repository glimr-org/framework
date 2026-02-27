//// UPDATE Parameter Extraction
////
//// Functions for extracting parameter-to-column mappings from
//// UPDATE SET clauses. Parses assignments like col = $1.

import gleam/list
import gleam/option.{Some}
import gleam/result
import gleam/string
import glimr/db/gen/parser/util

// ------------------------------------------------------------- Public Functions

/// Extract parameter mappings from UPDATE SET clause. Parses
/// column = $n assignments to map parameters to columns.
/// Pattern: UPDATE table SET col1 = $1, col2 = $2
///
pub fn extract(sql: String) -> List(#(Int, String)) {
  do_extract(sql)
  |> result.unwrap([])
}

// ------------------------------------------------------------- Private Functions

/// Internal implementation that returns Result for cleaner
/// error handling with result.try. Extracts SET clause and
/// delegates to assignment parsing.
///
fn do_extract(sql: String) -> Result(List(#(Int, String)), Nil) {
  let upper = string.uppercase(sql)

  use #(before_set, _) <- result.try(string.split_once(upper, " SET "))
  let set_offset = string.length(before_set) + 5
  let after_set = string.drop_start(sql, set_offset)

  // Find where SET clause ends (at WHERE or end of string)
  let set_clause = case
    string.split_once(string.uppercase(after_set), " WHERE")
  {
    Ok(#(before_where, _)) ->
      string.slice(after_set, 0, string.length(before_where))
    Error(_) -> after_set
  }

  Ok(parse_set_assignments(set_clause))
}

/// Parse SET clause assignments like "col = $1, col2 = $2" and
/// extract parameter-to-column mappings. Only includes
/// assignments where the value is a parameter placeholder.
///
fn parse_set_assignments(clause: String) -> List(#(Int, String)) {
  let parts = string.split(clause, ",")
  list.filter_map(parts, fn(part) {
    let trimmed = string.trim(part)
    case string.split_once(trimmed, "=") {
      Error(_) -> Error(Nil)
      Ok(#(col_part, val_part)) -> {
        let col = string.trim(col_part)
        let val = string.trim(val_part)
        case string.starts_with(val, "$") {
          False -> Error(Nil)
          True -> {
            let num_str = string.drop_start(val, 1)
            let clean_num = extract_digits(num_str)
            case util.parse_int(clean_num) {
              Some(n) -> Ok(#(n, col))
              _ -> Error(Nil)
            }
          }
        }
      }
    }
  })
}

/// Extract only the leading digits from a string. Used to parse
/// parameter numbers that may have trailing content like commas
/// or spaces.
///
fn extract_digits(s: String) -> String {
  let #(digits, _) = util.consume_digits(s, "")
  digits
}
