//// ------------------------------------------------------------
//// SQL Parser Utilities
//// ------------------------------------------------------------
////
//// Shared utility functions for SQL parsing including character
//// classification, identifier extraction, and keyword detection.
////

import gleam/int
import gleam/list
import gleam/option.{type Option, None}
import gleam/string

// ------------------------------------------------------------- Constants

/// ------------------------------------------------------------
/// Digit Chars
/// ------------------------------------------------------------
///
/// Valid digit characters for parsing parameter numbers.
///
pub const digit_chars = "0123456789"

/// ------------------------------------------------------------
/// Identifier Chars
/// ------------------------------------------------------------
///
/// Valid identifier characters for SQL column and table names
/// (letters, digits, and underscore).
///
pub const identifier_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Strip String Literals
/// ------------------------------------------------------------
///
/// Remove content from single-quoted string literals to prevent
/// false positives when parsing SQL keywords. Replaces 'content'
/// with '' to preserve SQL structure.
///
pub fn strip_string_literals(sql: String) -> String {
  do_strip_string_literals(sql, "", False)
}

/// ------------------------------------------------------------
/// Consume Digits
/// ------------------------------------------------------------
///
/// Consume consecutive digit characters from the start of a
/// string. Returns the consumed digits and remaining string.
///
pub fn consume_digits(s: String, acc: String) -> #(String, String) {
  case string.pop_grapheme(s) {
    Ok(#(c, rest)) -> {
      case string.contains(digit_chars, c) {
        True -> consume_digits(rest, acc <> c)
        False -> #(acc, s)
      }
    }
    Error(_) -> #(acc, s)
  }
}

/// ------------------------------------------------------------
/// Parse Int
/// ------------------------------------------------------------
///
/// Parse an integer from a string, returning None for empty
/// strings or invalid integers.
///
pub fn parse_int(s: String) -> Option(Int) {
  case s {
    "" -> None
    _ -> option.from_result(int.parse(s))
  }
}

/// ------------------------------------------------------------
/// Is Identifier Char
/// ------------------------------------------------------------
///
/// Check if a character is valid in a SQL identifier (letters,
/// digits, underscore).
///
pub fn is_identifier_char(c: String) -> Bool {
  string.contains(identifier_chars, c)
}

/// ------------------------------------------------------------
/// Extract Identifier
/// ------------------------------------------------------------
///
/// Extract a SQL identifier (table or column name) from the
/// start of a string. Handles both regular identifiers and
/// double-quoted identifiers like "table-name" or 
/// "schema"."table".
///
pub fn extract_identifier(s: String) -> String {
  let trimmed = string.trim(s)
  case string.pop_grapheme(trimmed) {
    // Quoted identifier
    Ok(#("\"", rest)) -> extract_quoted_identifier(rest, "")
    // Regular identifier
    Ok(_) -> do_extract_identifier(trimmed, "")
    Error(_) -> ""
  }
}

/// ------------------------------------------------------------
/// Extract Last Identifier
/// ------------------------------------------------------------
///
/// Extract the last SQL identifier from a string by working
/// backwards from the end.
///
pub fn extract_last_identifier(s: String) -> String {
  let trimmed = string.trim_end(s)
  let chars = string.to_graphemes(trimmed)
  let reversed = list.reverse(chars)
  extract_identifier_chars(reversed, [])
}

/// ------------------------------------------------------------
/// Extract Identifier Chars
/// ------------------------------------------------------------
///
/// Recursive helper that extracts valid identifier characters
/// from a reversed character list. Used by 
/// extract_last_identifier to work backwards from the end of 
/// a string.
///
fn extract_identifier_chars(chars: List(String), acc: List(String)) -> String {
  case chars {
    [] -> string.join(acc, "")
    [c, ..rest] -> {
      case is_identifier_char(c) {
        True -> extract_identifier_chars(rest, [c, ..acc])
        False -> string.join(acc, "")
      }
    }
  }
}

/// ------------------------------------------------------------
/// Is SQL Keyword
/// ------------------------------------------------------------
///
/// Check if a string is a SQL keyword (not a valid column name).
/// Used to filter out false positives in column detection.
///
pub fn is_sql_keyword(s: String) -> Bool {
  let upper = string.uppercase(s)
  case upper {
    "LIKE"
    | "ILIKE"
    | "IN"
    | "NOT"
    | "AND"
    | "OR"
    | "IS"
    | "NULL"
    | "TRUE"
    | "FALSE"
    | "BETWEEN"
    | "EXISTS"
    | "ANY"
    | "ALL"
    | "SOME"
    | "CASE"
    | "WHEN"
    | "THEN"
    | "ELSE"
    | "END"
    | "AS"
    | "ON"
    | "USING"
    | "HAVING"
    | "LIMIT"
    | "OFFSET"
    | "ORDER"
    | "BY"
    | "ASC"
    | "DESC"
    | "NULLS"
    | "FIRST"
    | "LAST"
    | "DISTINCT"
    | "FROM"
    | "WHERE"
    | "GROUP"
    | "JOIN"
    | "LEFT"
    | "RIGHT"
    | "INNER"
    | "OUTER"
    | "CROSS"
    | "FULL"
    | "NATURAL"
    | "SELECT"
    | "INSERT"
    | "UPDATE"
    | "DELETE"
    | "INTO"
    | "VALUES"
    | "SET"
    | "RETURNING"
    | "WITH"
    | "RECURSIVE"
    | "UNION"
    | "INTERSECT"
    | "EXCEPT"
    | "CAST"
    | "COALESCE"
    | "NULLIF"
    | "GREATEST"
    | "LEAST"
    | "SIMILAR"
    | "TO"
    | "ESCAPE" -> True
    _ -> False
  }
}

// ------------------------------------------------------------- Private Functions

/// ------------------------------------------------------------
/// Do Strip String Literals
/// ------------------------------------------------------------
///
/// Recursive helper that processes SQL character by character,
/// tracking whether we're inside a string literal. Characters
/// inside strings are skipped, quotes are preserved.
///
fn do_strip_string_literals(sql: String, acc: String, in_string: Bool) -> String {
  case string.pop_grapheme(sql) {
    Error(_) -> acc
    Ok(#("'", rest)) -> {
      case in_string {
        // Exiting string - add the closing quote
        True -> do_strip_string_literals(rest, acc <> "'", False)
        // Entering string - add the opening quote
        False -> do_strip_string_literals(rest, acc <> "'", True)
      }
    }
    Ok(#(c, rest)) -> {
      case in_string {
        // Inside string - skip the character
        True -> do_strip_string_literals(rest, acc, True)
        // Outside string - keep the character
        False -> do_strip_string_literals(rest, acc <> c, False)
      }
    }
  }
}

/// ------------------------------------------------------------
/// Do Extract Identifier
/// ------------------------------------------------------------
///
/// Recursive helper that extracts characters until a delimiter
/// is found. Stops at whitespace, commas, or parentheses.
///
fn do_extract_identifier(s: String, acc: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(c, rest)) -> {
      case c {
        " " | "\n" | "\t" | "," | "(" | ")" -> acc
        _ -> do_extract_identifier(rest, acc <> c)
      }
    }
    Error(_) -> acc
  }
}

/// ------------------------------------------------------------
/// Extract Quoted Identifier
/// ------------------------------------------------------------
///
/// Extract content from a double-quoted identifier. Processes
/// characters until the closing quote is found, then delegates
/// to after_closing_quote for schema.table pattern handling.
///
fn extract_quoted_identifier(s: String, acc: String) -> String {
  case string.pop_grapheme(s) {
    Error(_) -> acc
    Ok(#("\"", rest)) -> after_closing_quote(rest, acc)
    Ok(#(c, rest)) -> extract_quoted_identifier(rest, acc <> c)
  }
}

/// ------------------------------------------------------------
/// After Closing Quote
/// ------------------------------------------------------------
///
/// Handle what comes after a closing quote. Checks for a dot
/// separator indicating schema.table patterns like 
/// "schema"."table" or "schema".table.
///
fn after_closing_quote(s: String, acc: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(".", rest)) -> after_dot_separator(rest, acc)
    _ -> acc
  }
}

/// ------------------------------------------------------------
/// After Dot Separator
/// ------------------------------------------------------------
///
/// Extract identifier after a dot separator in schema.table
/// pattern. Handles both quoted ("schema"."table") and unquoted
/// ("schema".table) table names after the dot.
///
fn after_dot_separator(s: String, acc: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#("\"", rest)) -> extract_quoted_identifier(rest, acc <> ".")
    _ -> acc <> "." <> do_extract_identifier(s, "")
  }
}
