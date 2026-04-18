//// SQL Parser
////
//// Parses SQL query files to extract table names, columns, and
//// parameter references. Handles normalization, table extraction
//// (FROM, JOIN, INSERT INTO, UPDATE, DELETE FROM, UNION, subqueries),
//// column extraction (SELECT, RETURNING), and parameter extraction
//// with column mapping (WHERE, INSERT VALUES, UPDATE SET, BETWEEN).

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ------------------------------------------------------------- Public Types

/// The result of parsing a SQL query, containing all extracted
/// metadata needed for code generation including tables,
/// columns, parameters, and parameter-column mappings.
///
pub type ParsedQuery {
  ParsedQuery(
    tables: List(String),
    columns: List(SelectedColumn),
    params: List(Int),
    param_columns: List(#(Int, String)),
  )
}

/// A column selected in a SELECT or RETURNING clause. Tracks
/// the optional table alias, column name or expression, and any
/// AS alias.
///
pub type SelectedColumn {
  SelectedColumn(table: Option(String), name: String, alias: Option(String))
}

// ------------------------------------------------------------- Public Functions

/// Parse a SQL query to extract tables, columns, parameters,
/// and parameter-to-column mappings. This is the main entry
/// point for the SQL parser.
///
pub fn parse_sql(sql: String) -> Result(ParsedQuery, String) {
  let normalized = normalize(sql)

  // Extract tables from FROM and JOIN clauses
  let extracted_tables = extract_tables(normalized)

  // Extract selected columns
  let extracted_columns = extract_selected_columns(normalized)

  // Extract parameter positions ($1, $2, etc.)
  let extracted_params = extract_params(normalized)

  // Extract parameter-to-column mappings from WHERE clause
  let extracted_param_columns = extract_param_columns(normalized)

  Ok(ParsedQuery(
    tables: extracted_tables,
    columns: extracted_columns,
    params: extracted_params,
    param_columns: extracted_param_columns,
  ))
}

// ------------------------------------------------------------- Private Functions

// ---- Normalization ----

/// Normalize a SQL query by stripping comments, converting
/// whitespace to single spaces, and trimming unneeded
/// whitespace from the SQL code.
///
fn normalize(sql: String) -> String {
  sql
  |> strip_comments()
  |> string.replace("\n", " ")
  |> string.replace("\t", " ")
  |> collapse_spaces()
  |> string.trim()
}

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

// ---- Utility ----

/// Valid digit characters for parsing parameter numbers.
const digit_chars = "0123456789"

/// Valid identifier characters for SQL column and table names
const identifier_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"

/// Remove content from single-quoted string literals to prevent
/// false positives when parsing SQL keywords. Replaces
/// 'content' with '' to preserve SQL structure.
///
fn strip_string_literals(sql: String) -> String {
  do_strip_string_literals(sql, "", False)
}

/// Consume consecutive digit characters from the start of a
/// string. Returns a tuple of consumed digits and the remaining
/// string for further processing.
///
fn consume_digits(s: String, acc: String) -> #(String, String) {
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

/// Parse an integer from a string, returning None for empty
/// strings or invalid integers. Used for parsing parameter
/// numbers in SQL queries.
///
fn parse_int(s: String) -> Option(Int) {
  case s {
    "" -> None
    _ -> option.from_result(int.parse(s))
  }
}

/// Check if a character is valid in a SQL identifier. Valid
/// characters include letters, digits, and underscores for
/// table and column names.
///
fn is_identifier_char(c: String) -> Bool {
  string.contains(identifier_chars, c)
}

/// Extract a SQL identifier (table or column name) from the
/// start of a string. Handles both regular identifiers and
/// double-quoted identifiers like "table-name" or
/// "schema"."table".
///
fn extract_identifier(s: String) -> String {
  let trimmed = string.trim(s)
  case string.pop_grapheme(trimmed) {
    // Quoted identifier
    Ok(#("\"", rest)) -> extract_quoted_identifier(rest, "")
    // Regular identifier
    Ok(_) -> do_extract_identifier(trimmed, "")
    Error(_) -> ""
  }
}

/// Extract the last SQL identifier from a string by working
/// backwards from the end. Used for extracting column names
/// that appear after operators.
///
fn extract_last_identifier(s: String) -> String {
  let trimmed = string.trim_end(s)
  let chars = string.to_graphemes(trimmed)
  let reversed = list.reverse(chars)
  extract_identifier_chars(reversed, [])
}

/// Recursive helper that extracts valid identifier characters
/// from a reversed character list. Used by
/// extract_last_identifier to work backwards from the end of a
/// string.
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

/// Check if a string is a SQL keyword (not a valid column
/// name). Used to filter out false positives in column
/// detection during SQL parsing.
///
fn is_sql_keyword(s: String) -> Bool {
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

/// Recursive helper that extracts characters until a delimiter
/// is found. Stops at whitespace, commas, or parentheses and
/// returns the accumulated identifier.
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

// ---- Column Extraction ----

/// Extract selected columns from SELECT or RETURNING clauses.
/// Handles CTEs by finding the main SELECT at parenthesis depth
/// zero.
///
fn extract_selected_columns(sql: String) -> List(SelectedColumn) {
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

// ---- Table Extraction ----

/// Extract all table names from the SQL query. Handles FROM,
/// JOIN, INSERT INTO, UPDATE, DELETE FROM, UNION queries, and
/// subqueries.
///
fn extract_tables(sql: String) -> List(String) {
  // Strip string literals to avoid false positives from SQL keywords in strings
  let cleaned_sql = strip_string_literals(sql)

  // Split on UNION/UNION ALL and extract tables from each part
  let parts = split_union(cleaned_sql)

  let tables =
    parts
    |> list.flat_map(extract_from_single_query)

  // Also extract tables from subqueries
  let subquery_tables = extract_from_subqueries(cleaned_sql)

  list.append(tables, subquery_tables)
  |> list.unique
  |> list.sort(string.compare)
}

/// Extract tables from a single SQL query statement. This
/// handles FROM, JOIN, INSERT INTO, UPDATE, and DELETE FROM but
/// does not split UNION queries.
///
fn extract_from_single_query(sql: String) -> List(String) {
  let upper = string.uppercase(sql)

  // Extract tables from simple keyword patterns
  let tables =
    ["INSERT INTO ", "UPDATE ", "DELETE FROM "]
    |> list.filter_map(fn(keyword) {
      extract_table_after_keyword(upper, sql, keyword)
    })

  // Handle FROM clause separately (need to exclude DELETE FROM)
  let tables = case string.split_once(upper, "FROM ") {
    Ok(#(before, _)) -> {
      case string.contains(before, "DELETE") {
        True -> tables
        False -> {
          case extract_table_name_from_clause(sql) {
            Some(table) -> [table, ..tables]
            None -> tables
          }
        }
      }
    }
    Error(_) -> tables
  }

  // Find JOIN clauses
  find_join_tables(upper, sql, tables)
}

/// Extract a table name that appears after a SQL keyword like
/// INSERT INTO, UPDATE, or DELETE FROM. Preserves original case
/// from the SQL query.
///
fn extract_table_after_keyword(
  upper: String,
  original: String,
  keyword: String,
) -> Result(String, Nil) {
  case string.split_once(upper, keyword) {
    Ok(#(_, after)) -> {
      let offset = string.length(upper) - string.length(after)
      let table = extract_identifier(string.drop_start(original, offset))
      case table {
        "" -> Error(Nil)
        t -> Ok(t)
      }
    }
    Error(_) -> Error(Nil)
  }
}

/// Extract the table name from a FROM clause, preserving the
/// original case from the SQL. Returns None if no valid table
/// identifier is found.
///
fn extract_table_name_from_clause(original: String) -> Option(String) {
  let upper_full = string.uppercase(original)
  let offset = case string.split_once(upper_full, "FROM ") {
    Ok(#(before, _)) -> string.length(before) + 5
    Error(_) -> 0
  }

  let rest = string.drop_start(original, offset)
  let table = extract_identifier(rest)
  case table {
    "" -> None
    t -> Some(t)
  }
}

/// Recursively find all table names from JOIN clauses including
/// LEFT JOIN, RIGHT JOIN, INNER JOIN, etc. Accumulates found
/// tables in the acc list.
///
fn find_join_tables(
  upper: String,
  original: String,
  acc: List(String),
) -> List(String) {
  case string.split_once(upper, " JOIN ") {
    Ok(#(before, after)) -> {
      let offset = string.length(before) + 6
      let rest = string.drop_start(original, offset)
      let table = extract_identifier(rest)
      case table {
        "" -> acc
        t ->
          find_join_tables(after, string.drop_start(original, offset), [
            t,
            ..acc
          ])
      }
    }
    Error(_) -> acc
  }
}

/// Find and extract tables from subqueries (SELECT statements
/// inside parentheses). Entry point for recursive subquery
/// extraction.
///
fn extract_from_subqueries(sql: String) -> List(String) {
  do_extract_from_subqueries(string.uppercase(sql), sql, [])
}

/// Recursively find and extract tables from subqueries (SELECT
/// statements inside parentheses). Handles nested subqueries by
/// calling extract_tables recursively.
///
fn do_extract_from_subqueries(
  upper: String,
  original: String,
  acc: List(String),
) -> List(String) {
  // Look for "(SELECT" pattern indicating a subquery
  case string.split_once(upper, "(SELECT ") {
    Error(_) -> acc
    Ok(#(before, after)) -> {
      let offset = string.length(before) + 1
      let rest_original = string.drop_start(original, offset)

      // Extract the subquery content (find matching close paren)
      case extract_parenthesized_content(rest_original) {
        None -> do_extract_from_subqueries(after, rest_original, acc)
        Some(subquery_content) -> {
          // Recursively extract tables from this subquery
          let subquery_tables = extract_tables(subquery_content)
          let new_acc = list.append(acc, subquery_tables)
          // Continue searching for more subqueries
          do_extract_from_subqueries(after, rest_original, new_acc)
        }
      }
    }
  }
}

// ---- Union Handling ----

/// Split SQL query on UNION and UNION ALL keywords, returning a
/// list of individual query parts. Preserves original case and
/// handles both UNION and UNION ALL.
///
fn split_union(sql: String) -> List(String) {
  do_split_union(string.uppercase(sql), sql, [])
  |> list.reverse
}

/// Recursive helper to split on UNION keywords while preserving
/// original case. Tries UNION ALL first (longer match) before
/// plain UNION.
///
fn do_split_union(
  upper: String,
  original: String,
  acc: List(String),
) -> List(String) {
  // Try UNION ALL first (longer match), then plain UNION
  case try_split_on_separator(upper, original, " UNION ALL ", 11) {
    Some(#(before, after_upper, after_original)) ->
      do_split_union(after_upper, after_original, [before, ..acc])
    None ->
      case try_split_on_separator(upper, original, " UNION ", 7) {
        Some(#(before, after_upper, after_original)) ->
          do_split_union(after_upper, after_original, [before, ..acc])
        None -> [original, ..acc]
      }
  }
}

/// Attempt to split on a separator, returning the before part
/// (from original), and the after parts (both upper and
/// original) for continued processing.
///
fn try_split_on_separator(
  upper: String,
  original: String,
  separator: String,
  separator_len: Int,
) -> Option(#(String, String, String)) {
  case string.split_once(upper, separator) {
    Ok(#(before, after)) -> {
      let before_len = string.length(before)
      let before_original = string.slice(original, 0, before_len)
      let after_original =
        string.drop_start(original, before_len + separator_len)
      Some(#(before_original, after, after_original))
    }
    Error(_) -> None
  }
}

// ---- Subquery Helpers ----

/// Extract content from balanced parentheses, handling nested
/// parentheses correctly. Returns the content inside the parens
/// without the surrounding parentheses.
///
/// Expects the input to start after the opening parenthesis,
/// and returns the content up to the matching closing paren.
///
fn extract_parenthesized_content(sql: String) -> Option(String) {
  do_extract_parenthesized_content(sql, 1, "")
}

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

// ---- Parameter Extraction ----

/// Extract all parameter numbers ($1, $2, etc.) from SQL query.
/// Returns a sorted, deduplicated list of parameter numbers
/// found anywhere in the query.
///
fn extract_params(sql: String) -> List(Int) {
  do_extract_params(sql, [])
  |> list.unique
  |> list.sort(by: int.compare)
}

/// Extract parameter-to-column mappings from SQL. Combines
/// mappings from INSERT, UPDATE SET, and WHERE clauses with
/// INSERT/UPDATE taking precedence for duplicate parameters.
///
fn extract_param_columns(sql: String) -> List(#(Int, String)) {
  let insert_params = extract_insert_params(sql)
  let update_params = extract_update_params(sql)
  let where_params = extract_where_params(sql)

  // Merge all, preferring earlier sources (INSERT/UPDATE over WHERE for same param)
  let all_params = list.flatten([insert_params, update_params, where_params])

  // Deduplicate by param number, keeping first occurrence
  dedupe_by_param_num(all_params)
}

/// Recursive helper that scans for $ followed by digits and
/// accumulates found parameter numbers. Continues scanning
/// after each found parameter.
///
fn do_extract_params(s: String, acc: List(Int)) -> List(Int) {
  case string.split_once(s, "$") {
    Ok(#(_, after_dollar)) -> {
      let #(num_str, rest) = consume_digits(after_dollar, "")
      case parse_int(num_str) {
        Some(n) -> do_extract_params(rest, [n, ..acc])
        _ -> do_extract_params(after_dollar, acc)
      }
    }
    Error(_) -> acc
  }
}

/// Remove duplicate parameter mappings, keeping only the first
/// occurrence of each parameter number. Preserves the order
/// from the original list.
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

// ---- INSERT Parameter Extraction ----

/// Extract parameter mappings from INSERT statement. Matches
/// column positions with parameter positions in VALUES clause.
/// Pattern: INSERT INTO table (col1, col2) VALUES ($1, $2)
///
fn extract_insert_params(sql: String) -> List(#(Int, String)) {
  do_extract_insert_params(sql)
  |> result.unwrap([])
}

/// Internal implementation that returns Result for cleaner
/// error handling with result.try. Parses column names and
/// delegates to values extraction.
///
fn do_extract_insert_params(sql: String) -> Result(List(#(Int, String)), Nil) {
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
          parse_int(num_str)
          |> option.map(fn(n) { #(n, col) })
          |> option.to_result(Nil)
        }
      }
    }),
  )
}

// ---- UPDATE Parameter Extraction ----

/// Extract parameter mappings from UPDATE SET clause. Parses
/// column = $n assignments to map parameters to columns.
/// Pattern: UPDATE table SET col1 = $1, col2 = $2
///
fn extract_update_params(sql: String) -> List(#(Int, String)) {
  do_extract_update_params(sql)
  |> result.unwrap([])
}

/// Internal implementation that returns Result for cleaner
/// error handling with result.try. Extracts SET clause and
/// delegates to assignment parsing.
///
fn do_extract_update_params(sql: String) -> Result(List(#(Int, String)), Nil) {
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
            case parse_int(clean_num) {
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
  let #(digits, _) = consume_digits(s, "")
  digits
}

// ---- WHERE Parameter Extraction ----

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

/// Extract parameter-to-column mappings from WHERE clause.
/// Handles BETWEEN patterns specially for range queries and
/// other comparison operators for standard conditions.
///
fn extract_where_params(sql: String) -> List(#(Int, String)) {
  let upper = string.uppercase(sql)

  case string.split_once(upper, "WHERE ") {
    Ok(#(before, _)) -> {
      let offset = string.length(before) + 6
      let where_clause = string.drop_start(sql, offset)
      // First extract BETWEEN patterns (they need special handling)
      let between_params = extract_between_params(where_clause)
      // Then extract other patterns
      let other_params = parse_conditions(where_clause, [])
      // Merge, preferring BETWEEN params (they're more accurate)
      merge_param_columns(between_params, other_params)
    }
    Error(_) -> []
  }
}

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
  let found = find_param_column_pair(clause)
  case found {
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
      let #(num_str, rest) = consume_digits(after, "")
      case parse_int(num_str) {
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

  let col = extract_last_identifier(without_op)
  case col {
    "" -> None
    c -> {
      case is_sql_keyword(c) {
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
      let col = extract_identifier(after_eq)
      case col {
        "" -> None
        c -> Some(#(c, string.drop_start(after_eq, string.length(c))))
      }
    }
    False -> None
  }
}

// ---- BETWEEN Parameter Extraction ----

/// Extract params from BETWEEN ... AND ... patterns. Names the
/// parameters as start_<column> and end_<column> for range
/// queries on date/time and numeric columns.
///
fn extract_between_params(clause: String) -> List(#(Int, String)) {
  do_extract_between_params(string.uppercase(clause), clause, [])
}

/// Recursive helper that finds BETWEEN patterns and extracts
/// the associated parameters. Uses start_/end_ naming for the
/// lower and upper bounds.
///
fn do_extract_between_params(
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
        extract_last_identifier(string.slice(
          original,
          0,
          string.length(before_between),
        ))

      let new_params = case col {
        "" -> []
        c ->
          case is_sql_keyword(c) {
            True -> []
            False -> extract_pair_params(after_between, c)
          }
      }

      do_extract_between_params(
        after_between,
        next_original,
        list.append(acc, new_params),
      )
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
  let #(num_str, rest) = consume_digits(after_dollar, "")
  use param <- result.try(parse_int(num_str) |> option.to_result(Nil))
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
  let #(num_str, _) = consume_digits(after_dollar, "")
  parse_int(num_str) |> option.to_result(Nil)
}
