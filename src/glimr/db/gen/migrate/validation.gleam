//// Migration Validation
////
//// Catches schema mistakes at generation time instead of
//// letting them become cryptic database errors at migration
//// time. A typo in an index column or a copy-pasted duplicate
//// column name would otherwise surface as a raw SQL error that
//// doesn't point back at your schema file — these checks give
//// you a clear message with the exact table and column
//// involved.

import gleam/list
import gleam/string
import glimr/db/gen/schema_parser.{type Table}

// ------------------------------------------------------------- Public Functions

/// A duplicate column name would generate invalid SQL (CREATE
/// TABLE with two columns of the same name), but the database
/// error message won't tell you which schema file caused it.
/// This catches the mistake early and names the table and the
/// duplicated columns so you can go fix it immediately.
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

/// Catches index mistakes before they become cryptic SQL
/// errors. Indexing a column that doesn't exist would fail at
/// migration time with a database error that doesn't point at
/// your schema file. Duplicate indexes waste disk space and
/// slow down writes for no benefit. Both are caught here with
/// clear error messages pointing at the exact table and columns
/// involved.
///
pub fn validate_indexes(tables: List(Table)) -> Nil {
  list.each(tables, fn(table) {
    let col_names = list.map(table.columns, fn(c) { c.name })

    // Check that all indexed columns exist
    list.each(table.indexes, fn(idx) {
      list.each(idx.columns, fn(col) {
        case list.contains(col_names, col) {
          True -> Nil
          False -> {
            let red = "\u{001b}[31m"
            let reset = "\u{001b}[0m"
            let error_msg =
              red
              <> "Error: Index on table '"
              <> table.name
              <> "' references non-existent column '"
              <> col
              <> "'"
              <> reset
            panic as error_msg
          }
        }
      })
    })

    // Check for duplicate indexes (same columns + unique flag)
    check_duplicate_indexes(table.name, table.indexes, [])
  })
}

// ------------------------------------------------------------- Private Functions

/// Walks the index list tracking what we've seen so far. Two
/// indexes are duplicates if they cover the same columns with
/// the same uniqueness — even if one has a custom name and the
/// other doesn't. Using a recursive accumulator pattern keeps
/// this simple without needing a mutable set.
///
fn check_duplicate_indexes(
  table_name: String,
  indexes: List(schema_parser.Index),
  seen: List(#(List(String), Bool)),
) -> Nil {
  case indexes {
    [] -> Nil
    [idx, ..rest] -> {
      let key = #(idx.columns, idx.unique)
      case list.contains(seen, key) {
        True -> {
          let red = "\u{001b}[31m"
          let reset = "\u{001b}[0m"
          let error_msg =
            red
            <> "Error: Duplicate index on table '"
            <> table_name
            <> "' for columns: "
            <> string.join(idx.columns, ", ")
            <> reset
          panic as error_msg
        }
        False -> check_duplicate_indexes(table_name, rest, [key, ..seen])
      }
    }
  }
}

/// Scans a list and returns any string that appears more than
/// once. Each duplicate is reported only once even if it
/// appears three or more times — the error message just needs
/// to know which names are duplicated, not how many times.
///
fn find_duplicates(items: List(String)) -> List(String) {
  find_duplicates_helper(items, [], [])
}

/// Without the second accumulator, a column name appearing
/// three times would show up twice in the error message, which
/// is confusing — the developer just needs to know "this name
/// is duplicated", not "this name is duplicated and also
/// duplicated again."
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
