//// WHERE Parameter Extraction
////
//// Functions for extracting parameter-to-column mappings from
//// WHERE clauses. Handles comparison operators and delegates
//// BETWEEN patterns to the between module.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import glimr/db/gen/parser/params/between
import glimr/db/gen/parser/util

// ------------------------------------------------------------- Private Constants

/// SQL keyword operators to strip when extracting column names.
/// Each entry is (suffix, length) for efficient removal.
///
const keyword_operators = [
  #(" LIKE", 5),
  #(" ILIKE", 6),
  #(" IN", 3),
  #(" IS", 3),
  #(" BETWEEN", 8),
]

/// Symbol operators to strip when extracting column names.
///
const symbol_operators = ["=", "!", ">", "<"]

// ------------------------------------------------------------- Public Functions

/// Extract parameter-to-column mappings from WHERE clause.
/// Handles BETWEEN patterns specially for range queries and
/// other comparison operators for standard conditions.
///
pub fn extract(sql: String) -> List(#(Int, String)) {
  let upper = string.uppercase(sql)

  case string.split_once(upper, "WHERE ") {
    Ok(#(before, _)) -> {
      let offset = string.length(before) + 6
      let where_clause = string.drop_start(sql, offset)
      // First extract BETWEEN patterns (they need special handling)
      let between_params = between.extract(where_clause)
      // Then extract other patterns
      let other_params = parse_conditions(where_clause, [])
      // Merge, preferring BETWEEN params (they're more accurate)
      merge_param_columns(between_params, other_params)
    }
    Error(_) -> []
  }
}

// ------------------------------------------------------------- Private Functions

/// Merge two param column lists, preferring the first list
/// (primary) when the same parameter appears in both. Used to
/// prioritize BETWEEN params over generic condition params.
///
fn merge_param_columns(
  primary: List(#(Int, String)),
  secondary: List(#(Int, String)),
) -> List(#(Int, String)) {
  let primary_nums = list.map(primary, fn(p) { p.0 })
  let filtered_secondary =
    list.filter(secondary, fn(p) { !list.contains(primary_nums, p.0) })
  list.append(primary, filtered_secondary)
}

/// Recursively parse WHERE clause conditions to extract
/// parameter-to-column mappings from comparisons like column =
/// $1 or $1 = column.
///
fn parse_conditions(
  clause: String,
  acc: List(#(Int, String)),
) -> List(#(Int, String)) {
  let result = find_param_column_pair(clause)
  case result {
    Some(#(param_num, col_name, rest)) -> {
      parse_conditions(rest, [#(param_num, col_name), ..acc])
    }
    None -> list.reverse(acc)
  }
}

/// Find the next parameter and its associated column in the
/// WHERE clause. Returns the param number, column name, and
/// remaining string for continued parsing.
///
fn find_param_column_pair(s: String) -> Option(#(Int, String, String)) {
  case string.split_once(s, "$") {
    Error(_) -> None
    Ok(#(before, after)) -> {
      let #(num_str, rest) = util.consume_digits(after, "")
      case util.parse_int(num_str) {
        None -> find_param_column_pair(after)
        Some(param_num) -> {
          case find_column_for_param(before, rest) {
            Some(#(col, remaining)) -> Some(#(param_num, col, remaining))
            None -> find_param_column_pair(rest)
          }
        }
      }
    }
  }
}

/// Try to find the column associated with a parameter by
/// checking both before (column = $1) and after ($1 = column)
/// positions in the comparison.
///
fn find_column_for_param(
  before: String,
  rest: String,
) -> Option(#(String, String)) {
  case find_column_before_param(before) {
    Some(col) -> Some(#(col, rest))
    None -> find_column_after_param(rest)
  }
}

/// Look for a column name before the parameter in patterns like
/// "column = $1". Strips trailing operators and filters out SQL
/// keywords to find the actual column name.
///
fn find_column_before_param(s: String) -> Option(String) {
  let trimmed = string.trim_end(s)
  let without_op =
    trimmed
    |> string.trim_end()
    |> remove_trailing_operator()

  let col = util.extract_last_identifier(without_op)
  case col {
    "" -> None
    c -> {
      case util.is_sql_keyword(c) {
        True -> None
        False -> Some(c)
      }
    }
  }
}

/// Recursively remove trailing comparison operators from a
/// string. Handles both keyword operators (LIKE, IN) and symbol
/// operators (=, !, >, <).
///
fn remove_trailing_operator(s: String) -> String {
  let trimmed = string.trim_end(s)
  let upper = string.uppercase(trimmed)

  case try_remove_keyword_operator(upper, trimmed, keyword_operators) {
    Some(result) -> remove_trailing_operator(result)
    None -> {
      case try_remove_symbol_operator(trimmed, symbol_operators) {
        Some(result) -> remove_trailing_operator(result)
        None -> trimmed
      }
    }
  }
}

/// Try to remove a keyword operator (LIKE, IN, etc.) from the
/// end of a string. Returns the shortened string if a matching
/// operator suffix is found.
///
fn try_remove_keyword_operator(
  upper: String,
  original: String,
  operators: List(#(String, Int)),
) -> Option(String) {
  list.find_map(operators, fn(op) {
    let #(suffix, len) = op
    case string.ends_with(upper, suffix) {
      True -> Ok(string.drop_end(original, len))
      False -> Error(Nil)
    }
  })
  |> option.from_result()
}

/// Try to remove a symbol operator (=, !, >, <) from the end of
/// a string. Returns the shortened string if a matching
/// operator character is found.
///
fn try_remove_symbol_operator(
  s: String,
  operators: List(String),
) -> Option(String) {
  list.find_map(operators, fn(op) {
    case string.ends_with(s, op) {
      True -> Ok(string.drop_end(s, 1))
      False -> Error(Nil)
    }
  })
  |> option.from_result()
}

/// Look for a column name after the parameter in patterns like
/// "$1 = column". Returns the column name and remaining string
/// for continued parsing.
///
fn find_column_after_param(s: String) -> Option(#(String, String)) {
  let trimmed = string.trim_start(s)
  case string.starts_with(trimmed, "=") {
    True -> {
      let after_eq = string.drop_start(trimmed, 1) |> string.trim_start()
      let col = util.extract_identifier(after_eq)
      case col {
        "" -> None
        c -> Some(#(c, string.drop_start(after_eq, string.length(c))))
      }
    }
    False -> None
  }
}
