//// Table Extraction
////
//// Functions for extracting table names from SQL queries.
//// Handles FROM, JOIN, INSERT INTO, UPDATE, DELETE FROM,
//// UNION/UNION ALL queries, and subqueries.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import glimr/db/gen/parser/tables/subquery
import glimr/db/gen/parser/tables/union
import glimr/db/gen/parser/util

// ------------------------------------------------------------- Public Functions

/// Extract all table names from the SQL query. Handles FROM,
/// JOIN, INSERT INTO, UPDATE, DELETE FROM, UNION queries, and
/// subqueries.
///
pub fn extract(sql: String) -> List(String) {
  // Strip string literals to avoid false positives from SQL keywords in strings
  let cleaned_sql = util.strip_string_literals(sql)

  // Split on UNION/UNION ALL and extract tables from each part
  let parts = union.split(cleaned_sql)

  let tables =
    parts
    |> list.flat_map(extract_from_single_query)

  // Also extract tables from subqueries
  let subquery_tables = extract_from_subqueries(cleaned_sql)

  list.append(tables, subquery_tables)
  |> list.unique
  |> list.sort(string.compare)
}

// ------------------------------------------------------------- Private Functions

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
      let table = util.extract_identifier(string.drop_start(original, offset))
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
  let table = util.extract_identifier(rest)
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
      let table = util.extract_identifier(rest)
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
/// calling extract recursively.
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
      case subquery.extract_parenthesized_content(rest_original) {
        None -> do_extract_from_subqueries(after, rest_original, acc)
        Some(subquery_content) -> {
          // Recursively extract tables from this subquery
          let subquery_tables = extract(subquery_content)
          let new_acc = list.append(acc, subquery_tables)
          // Continue searching for more subqueries
          do_extract_from_subqueries(after, rest_original, new_acc)
        }
      }
    }
  }
}
