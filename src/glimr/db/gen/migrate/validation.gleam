//// Migration Validation
////
//// Validation helpers for schema definitions. Ensures schemas
//// are valid before generating migrations.

import gleam/list
import gleam/string
import glimr/db/gen/schema_parser.{type Table}

// ------------------------------------------------------------- Public Functions

/// Validate that no table has duplicate column names. Panics
/// with an error message if duplicates are found, listing which
/// columns appear more than once.
///
pub fn validate_no_duplicate_columns(tables: List(Table)) -> Nil {
  list.each(tables, fn(table) {
    let col_names = list.map(table.columns, fn(c) { c.name })
    let duplicates = find_duplicates(col_names)
    case duplicates {
      [] -> Nil
      dupes -> {
        let red = "\u{001b}[31m"
        let reset = "\u{001b}[0m"
        let error_msg =
          red
          <> "Error: Duplicate column names in table '"
          <> table.name
          <> "': "
          <> string.join(dupes, ", ")
          <> reset
        panic as error_msg
      }
    }
  })
}

// ------------------------------------------------------------- Private Functions

/// Find duplicate strings in a list. Returns a list of strings
/// that appear more than once, with each duplicate appearing
/// only once in the result.
///
fn find_duplicates(items: List(String)) -> List(String) {
  find_duplicates_helper(items, [], [])
}

/// Recursive helper for find_duplicates. Tracks seen items and
/// accumulates duplicates, ensuring each duplicate is only
/// added once to the result list.
///
fn find_duplicates_helper(
  items: List(String),
  seen: List(String),
  duplicates: List(String),
) -> List(String) {
  case items {
    [] -> duplicates
    [item, ..rest] -> {
      case list.contains(seen, item) {
        True -> {
          case list.contains(duplicates, item) {
            True -> find_duplicates_helper(rest, seen, duplicates)
            False -> find_duplicates_helper(rest, seen, [item, ..duplicates])
          }
        }
        False -> find_duplicates_helper(rest, [item, ..seen], duplicates)
      }
    }
  }
}
