//// ------------------------------------------------------------
//// Subquery Helpers
//// ------------------------------------------------------------
////
//// Utilities for extracting content from subqueries, primarily
//// handling balanced parentheses extraction.
////

import gleam/option.{type Option, None, Some}
import gleam/string

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Extract Parenthesized Content
/// ------------------------------------------------------------
///
/// Extract content from balanced parentheses, handling nested
/// parentheses correctly. Returns the content inside the parens
/// without the surrounding parentheses.
///
/// Expects the input to start after the opening parenthesis,
/// and returns the content up to the matching closing paren.
///
pub fn extract_parenthesized_content(sql: String) -> Option(String) {
  do_extract_parenthesized_content(sql, 1, "")
}

// ------------------------------------------------------------- Private Functions

/// ------------------------------------------------------------
/// Do Extract Parenthesized Content
/// ------------------------------------------------------------
///
/// Recursive helper that tracks parenthesis depth. Starts at
/// depth 1 (after the opening paren) and returns when depth
/// reaches 0.
///
fn do_extract_parenthesized_content(
  sql: String,
  depth: Int,
  acc: String,
) -> Option(String) {
  case string.pop_grapheme(sql) {
    Error(_) -> None
    Ok(#("(", rest)) ->
      do_extract_parenthesized_content(rest, depth + 1, acc <> "(")
    Ok(#(")", rest)) -> {
      case depth {
        1 -> Some(acc)
        _ -> do_extract_parenthesized_content(rest, depth - 1, acc <> ")")
      }
    }
    Ok(#(c, rest)) -> do_extract_parenthesized_content(rest, depth, acc <> c)
  }
}
