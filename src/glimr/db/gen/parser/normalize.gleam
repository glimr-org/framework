//// ------------------------------------------------------------
//// SQL Normalization
//// ------------------------------------------------------------
////
//// Functions for normalizing SQL queries by stripping comments
//// and collapsing whitespace.
////

import gleam/list
import gleam/string

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Normalize SQL
/// ------------------------------------------------------------
///
/// Normalize a SQL query by stripping comments, converting
/// whitespace to single spaces, and trimming.
///
pub fn normalize(sql: String) -> String {
  sql
  |> strip_comments()
  |> string.replace("\n", " ")
  |> string.replace("\t", " ")
  |> collapse_spaces()
  |> string.trim()
}

// ------------------------------------------------------------- Private Functions

/// ------------------------------------------------------------
/// Strip Comments
/// ------------------------------------------------------------
///
/// Strip SQL comments from the query. Handles both -- single-
/// line comments and /* */ multi-line block comments.
///
fn strip_comments(sql: String) -> String {
  sql
  |> strip_block_comments()
  |> strip_line_comments()
}

/// ------------------------------------------------------------
/// Strip Block Comments
/// ------------------------------------------------------------
///
/// Recursively strip /* */ block comments from SQL.
///
fn strip_block_comments(sql: String) -> String {
  case string.split_once(sql, "/*") {
    Error(_) -> sql
    Ok(#(before, after)) -> {
      case string.split_once(after, "*/") {
        Error(_) -> before
        Ok(#(_, rest)) -> strip_block_comments(before <> " " <> rest)
      }
    }
  }
}

/// ------------------------------------------------------------
/// Strip Line Comments
/// ------------------------------------------------------------
///
/// Strip -- line comments (everything from -- to end of line).
///
fn strip_line_comments(sql: String) -> String {
  let lines = string.split(sql, "\n")
  lines
  |> list.map(fn(line) {
    case string.split_once(line, "--") {
      Error(_) -> line
      Ok(#(before, _)) -> before
    }
  })
  |> string.join("\n")
}

/// ------------------------------------------------------------
/// Collapse Spaces
/// ------------------------------------------------------------
///
/// Recursively collapse multiple consecutive spaces into single
/// spaces.
///
fn collapse_spaces(sql: String) -> String {
  case string.contains(sql, "  ") {
    False -> sql
    True -> collapse_spaces(string.replace(sql, "  ", " "))
  }
}
