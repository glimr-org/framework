//// Column Extraction
////
//// Functions for extracting selected columns from SQL queries.
//// Handles SELECT and RETURNING clauses, including CTEs.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

// ------------------------------------------------------------- Public Types

/// A column selected in a SELECT or RETURNING clause. Tracks
/// the optional table alias, column name or expression, and any
/// AS alias.
///
pub type SelectedColumn {
  SelectedColumn(table: Option(String), name: String, alias: Option(String))
}

// ------------------------------------------------------------- Public Functions

/// Extract selected columns from SELECT or RETURNING clauses.
/// Handles CTEs by finding the main SELECT at parenthesis depth
/// zero.
///
pub fn extract(sql: String) -> List(SelectedColumn) {
  let upper = string.uppercase(sql)

  // Handle CTEs: find the main SELECT (outside parentheses)
  case find_keyword_at_depth_zero(upper, "S", "SELECT ") {
    None -> extract_returning_columns(sql, upper)
    Some(offset) -> {
      let after_select = string.drop_start(sql, offset + 7)
      let after_select_upper = string.drop_start(upper, offset + 7)

      // Find FROM that's at the same parenthesis level (depth 0)
      case find_keyword_at_depth_zero(after_select_upper, " ", " FROM") {
        None -> extract_returning_columns(sql, upper)
        Some(from_offset) -> {
          let columns_str = string.slice(after_select, 0, from_offset)
          parse_column_list(columns_str)
        }
      }
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// Find a SQL keyword that appears at parenthesis depth zero.
/// Used to find the main SELECT/FROM in queries with CTEs or
/// subqueries. Returns the position of the keyword if found.
///
fn find_keyword_at_depth_zero(
  s: String,
  trigger_char: String,
  keyword: String,
) -> Option(Int) {
  do_find_keyword_at_depth_zero(s, trigger_char, keyword, 0, 0)
}

/// Recursively Find a SQL keyword that appears at parenthesis
/// depth zero. Used to find the main SELECT/FROM in queries
/// with CTEs or subqueries.
///
fn do_find_keyword_at_depth_zero(
  s: String,
  trigger_char: String,
  keyword: String,
  pos: Int,
  depth: Int,
) -> Option(Int) {
  case string.pop_grapheme(s) {
    Error(_) -> None
    Ok(#(c, rest)) -> {
      case c {
        "(" ->
          do_find_keyword_at_depth_zero(
            rest,
            trigger_char,
            keyword,
            pos + 1,
            depth + 1,
          )
        ")" ->
          do_find_keyword_at_depth_zero(
            rest,
            trigger_char,
            keyword,
            pos + 1,
            depth - 1,
          )
        _ if c == trigger_char && depth == 0 -> {
          case string.starts_with(s, keyword) {
            True -> Some(pos)
            False ->
              do_find_keyword_at_depth_zero(
                rest,
                trigger_char,
                keyword,
                pos + 1,
                depth,
              )
          }
        }
        _ ->
          do_find_keyword_at_depth_zero(
            rest,
            trigger_char,
            keyword,
            pos + 1,
            depth,
          )
      }
    }
  }
}

/// Extract columns from a RETURNING clause used with INSERT,
/// UPDATE, and DELETE statements. Falls back to empty list if
/// no RETURNING clause is found.
///
fn extract_returning_columns(sql: String, upper: String) -> List(SelectedColumn) {
  case string.split_once(upper, "RETURNING ") {
    Ok(#(before, _)) -> {
      let offset = string.length(before) + 10
      let columns_str = string.drop_start(sql, offset) |> string.trim()
      parse_column_list(columns_str)
    }
    Error(_) -> []
  }
}

/// Parse a comma-separated list of column expressions into
/// SelectedColumn structs. Handles special case of star (*) for
/// all columns.
///
fn parse_column_list(columns_str: String) -> List(SelectedColumn) {
  let parts = split_respecting_parens(columns_str)
  list.filter_map(parts, fn(part) {
    let trimmed = string.trim(part)
    case trimmed {
      "" -> Error(Nil)
      "*" -> Ok(SelectedColumn(table: None, name: "*", alias: None))
      _ -> Ok(parse_column_expr(trimmed))
    }
  })
}

/// Split a string on commas, but don't split inside parentheses
/// to correctly parse column lists with function calls like
/// COUNT(*) or COALESCE(a, b).
///
fn split_respecting_parens(s: String) -> List(String) {
  do_split_respecting_parens(s, 0, "", [])
}

/// Recursive helper that tracks parenthesis depth and only
/// splits on commas when at depth zero. Accumulates parts in
/// reverse order for efficiency.
///
fn do_split_respecting_parens(
  s: String,
  depth: Int,
  current: String,
  acc: List(String),
) -> List(String) {
  case string.pop_grapheme(s) {
    Error(_) -> {
      case current {
        "" -> list.reverse(acc)
        _ -> list.reverse([current, ..acc])
      }
    }
    Ok(#(c, rest)) -> {
      case c {
        "(" -> do_split_respecting_parens(rest, depth + 1, current <> c, acc)
        ")" -> do_split_respecting_parens(rest, depth - 1, current <> c, acc)
        "," if depth == 0 -> {
          do_split_respecting_parens(rest, depth, "", [current, ..acc])
        }
        _ -> do_split_respecting_parens(rest, depth, current <> c, acc)
      }
    }
  }
}

/// Parse a single column expression like "u.name AS user_name"
/// into a SelectedColumn struct. Extracts optional table alias,
/// column name, and AS alias.
///
fn parse_column_expr(expr: String) -> SelectedColumn {
  let upper = string.uppercase(expr)

  // Check for AS alias
  case string.split_once(upper, " AS ") {
    Ok(#(before_as, _)) -> {
      let col_part =
        string.trim(string.slice(expr, 0, string.length(before_as)))
      let alias_part =
        string.trim(string.drop_start(expr, string.length(before_as) + 4))
      let #(table, name) = parse_table_column(col_part)
      SelectedColumn(table: table, name: name, alias: Some(alias_part))
    }
    Error(_) -> {
      let #(table, name) = parse_table_column(expr)
      SelectedColumn(table: table, name: name, alias: None)
    }
  }
}

/// Parse a column reference like "u.name" into optional table
/// alias and column name. Function calls containing parentheses
/// are returned as-is without splitting.
///
fn parse_table_column(expr: String) -> #(Option(String), String) {
  let trimmed = string.trim(expr)

  // If expression contains parentheses, it's a function call - don't split
  case string.contains(trimmed, "(") {
    True -> #(None, trimmed)
    False -> {
      case string.split_once(trimmed, ".") {
        Ok(#(table, column)) -> #(Some(string.trim(table)), string.trim(column))
        Error(_) -> #(None, trimmed)
      }
    }
  }
}
