//// SQL Normalization
////
//// Functions for normalizing SQL queries by stripping comments
//// and collapsing whitespace ensuring valid SQL is parsed.

import gleam/list
import gleam/string

// ------------------------------------------------------------- Public Functions

/// Normalize a SQL query by stripping comments, converting
/// whitespace to single spaces, and trimming unneeded
/// whitespace from the SQL code.
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

/// Strip SQL comments from the query. This handles both single
/// line comments "-- like this" and multi-line block comments
/// /* like this */ from being parsed as valid SQL.
///
fn strip_comments(sql: String) -> String {
  sql
  |> strip_block_comments()
  |> strip_line_comments()
}

/// Recursively strip multi-line comments from SQL which would
/// be /* */ block comments specifically. This ensures block
/// comments are not parsed as valid SQL.
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

/// Strip single line comments "-- like these" from SQL. Works
/// by stripping everything from -- to the end of each line,
/// preserving the code before the comment.
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

/// Recursively collapse multiple consecutive spaces into single
/// spaces. Continues until no double-spaces remain, producing
/// clean normalized SQL.
///
fn collapse_spaces(sql: String) -> String {
  case string.contains(sql, "  ") {
    False -> sql
    True -> collapse_spaces(string.replace(sql, "  ", " "))
  }
}
