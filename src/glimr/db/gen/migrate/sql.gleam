//// Migration SQL Generation
////
//// Handles schema diffing and SQL generation for migrations.
//// Compares old and new snapshots to detect changes, then
//// generates driver-specific SQL statements.

import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import glimr/db/gen/migrate/snapshot.{
  type ColumnSnapshot, type Snapshot, column_type_to_string,
}
import glimr/db/gen/schema_parser.{type Column, type ColumnType, type Table}

// ------------------------------------------------------------- Public Types

/// Database driver for SQL generation. Determines syntax
/// differences between PostgreSQL and SQLite for things like
/// types, auto-increment, and functions.
///
pub type Driver {
  Postgres
  Sqlite
}

/// The computed difference between old and new schema
/// snapshots. Contains a list of changes that need to be
/// migrated, including table and column additions, drops, and
/// alterations.
///
pub type SchemaDiff {
  SchemaDiff(changes: List(Change))
}

/// A single schema change that needs to be migrated. Variants
/// cover table creation/deletion and column operations
/// including add, drop, alter, and rename.
///
pub type Change {
  CreateTable(table: Table)
  DropTable(name: String)
  AddColumn(table: String, column: Column)
  DropColumn(table: String, column: String)
  AlterColumn(table: String, column: Column, old: ColumnSnapshot)
  RenameColumn(table: String, old_name: String, new_name: String)
}

// ------------------------------------------------------------- Public Functions

/// Compute the diff between old and new snapshots. Detects new
/// tables, dropped tables, and column changes. When is_filtered
/// is true, skips drop detection to avoid false positives.
///
pub fn compute_diff(
  old: Snapshot,
  new: Snapshot,
  tables: List(Table),
  is_filtered: Bool,
) -> SchemaDiff {
  let old_names = dict.keys(old.tables)
  let new_names = dict.keys(new.tables)

  // Find new tables
  let new_tables =
    list.filter(tables, fn(t) { !list.contains(old_names, t.name) })
    |> list.map(CreateTable)

  // Find dropped tables (skip when filtering by model to avoid false positives)
  let dropped_tables = case is_filtered {
    True -> []
    False ->
      list.filter(old_names, fn(name) { !list.contains(new_names, name) })
      |> list.map(DropTable)
  }

  // Find column changes in existing tables
  let column_changes =
    tables
    |> list.filter(fn(t) { list.contains(old_names, t.name) })
    |> list.flat_map(fn(table) { compute_table_diff(old, table) })

  SchemaDiff(
    changes: list.flatten([new_tables, dropped_tables, column_changes]),
  )
}

/// Topologically sort CreateTable changes so tables are created
/// after their foreign key dependencies. Other changes are
/// preserved in their original order at the end.
///
fn sort_changes_by_dependency(changes: List(Change)) -> List(Change) {
  // Separate CreateTable from other changes
  let #(create_tables, other_changes) =
    list.partition(changes, fn(c) {
      case c {
        CreateTable(_) -> True
        _ -> False
      }
    })

  // Extract tables from CreateTable changes
  let tables =
    list.filter_map(create_tables, fn(c) {
      case c {
        CreateTable(t) -> Ok(t)
        _ -> Error(Nil)
      }
    })

  // Get all table names being created
  let table_names = list.map(tables, fn(t) { t.name })

  // Sort tables by dependencies (topological sort)
  let sorted_tables = topological_sort(tables, table_names)

  // Convert back to CreateTable changes
  let sorted_creates = list.map(sorted_tables, CreateTable)

  // CreateTables first (in dependency order), then other changes
  list.append(sorted_creates, other_changes)
}

/// Sort tables so that tables with foreign key dependencies
/// come after the tables they reference. Uses Kahn's algorithm
/// for dependency resolution.
///
fn topological_sort(tables: List(Table), all_names: List(String)) -> List(Table) {
  // Get dependencies for each table (only count deps on tables being created)
  let get_deps = fn(table: Table) -> List(String) {
    table.columns
    |> list.filter_map(fn(col) {
      case col.column_type {
        schema_parser.Foreign(ref) -> {
          // Extract table name from "table(id)" format
          let ref_table =
            string.split(ref, "(")
            |> list.first
            |> option.from_result
            |> option.unwrap("")
          case list.contains(all_names, ref_table) {
            True -> Ok(ref_table)
            False -> Error(Nil)
          }
        }
        _ -> Error(Nil)
      }
    })
  }

  // Kahn's algorithm
  do_topological_sort(tables, get_deps, [])
}

/// Recursive helper for topological sort using Kahn's
/// algorithm. Each iteration finds tables whose dependencies
/// are already in the sorted list, adds them, and recurses with
/// the remainder. If no tables are ready (circular dependency),
/// returns remaining tables in original order to avoid infinite
/// recursion.
///
fn do_topological_sort(
  remaining: List(Table),
  get_deps: fn(Table) -> List(String),
  sorted: List(Table),
) -> List(Table) {
  case remaining {
    [] -> list.reverse(sorted)
    _ -> {
      // Find tables whose dependencies are all already sorted
      let sorted_names = list.map(sorted, fn(t) { t.name })

      let #(ready, not_ready) =
        list.partition(remaining, fn(table) {
          let deps = get_deps(table)
          list.all(deps, fn(dep) { list.contains(sorted_names, dep) })
        })

      case ready {
        [] -> {
          // Circular dependency or bug - just return remaining in original order
          list.append(list.reverse(sorted), remaining)
        }
        _ -> {
          do_topological_sort(not_ready, get_deps, list.append(ready, sorted))
        }
      }
    }
  }
}

/// Generate SQL for all changes in a diff. CreateTable changes
/// are sorted by dependency order so tables referencing other
/// tables are created after their dependencies.
///
pub fn generate_sql(diff: SchemaDiff, driver: Driver) -> String {
  diff.changes
  |> sort_changes_by_dependency
  |> list.map(fn(change) { change_to_sql(change, driver) })
  |> string.join("\n\n")
}

/// Human-readable description of a Change for CLI output.
/// Formats the change type and affected table/column names into
/// a user-friendly string.
///
pub fn describe_change(change: Change) -> String {
  case change {
    CreateTable(table) -> "Create table: " <> table.name
    DropTable(name) -> "Drop table: " <> name
    AddColumn(table, col) -> "Add column: " <> table <> "." <> col.name
    DropColumn(table, col) -> "Drop column: " <> table <> "." <> col
    AlterColumn(table, col, _) -> "Alter column: " <> table <> "." <> col.name
    RenameColumn(table, old_name, new_name) ->
      "Rename column: " <> table <> "." <> old_name <> " -> " <> new_name
  }
}

// ------------------------------------------------------------- Private Functions

/// Compute column-level changes for a single table. Detects
/// renames (via rename_from), additions, drops, and alterations
/// by comparing against the old snapshot.
///
fn compute_table_diff(old: Snapshot, table: Table) -> List(Change) {
  case dict.get(old.tables, table.name) {
    Ok(old_table) -> {
      let old_col_names = list.map(old_table.columns, fn(c) { c.name })
      let new_col_names = list.map(table.columns, fn(c) { c.name })

      // First, find columns that are renames (have renamed_from set)
      let renames =
        table.columns
        |> list.filter_map(fn(col) {
          case col.renamed_from {
            option.Some(old_name) -> {
              // Validate that the old column exists in the snapshot
              case list.contains(old_col_names, old_name) {
                True -> {
                  // Validate that the old column doesn't also exist in current schema
                  case list.contains(new_col_names, old_name) {
                    True -> {
                      let red = "\u{001b}[31m"
                      let reset = "\u{001b}[0m"
                      let error_msg =
                        red
                        <> "Error: rename_from('"
                        <> old_name
                        <> "') specified for column '"
                        <> col.name
                        <> "' in table '"
                        <> table.name
                        <> "', but column '"
                        <> old_name
                        <> "' still exists in the schema. Remove the old column or the rename_from modifier."
                        <> reset
                      panic as error_msg
                    }
                    False -> {
                      // Validate that the types are compatible
                      let old_col_snapshot =
                        list.find(old_table.columns, fn(c) {
                          c.name == old_name
                        })
                      case old_col_snapshot {
                        Ok(old_col) -> {
                          let new_type = column_type_to_string(col.column_type)
                          case
                            types_compatible_for_rename(
                              old_col.column_type,
                              new_type,
                            )
                          {
                            True ->
                              Ok(RenameColumn(table.name, old_name, col.name))
                            False -> {
                              let red = "\u{001b}[31m"
                              let reset = "\u{001b}[0m"
                              let error_msg =
                                red
                                <> "Error: rename_from('"
                                <> old_name
                                <> "') specified for column '"
                                <> col.name
                                <> "' in table '"
                                <> table.name
                                <> "', but types are incompatible: '"
                                <> old_col.column_type
                                <> "' cannot be renamed to '"
                                <> new_type
                                <> "'. Change the type in a separate migration."
                                <> reset
                              panic as error_msg
                            }
                          }
                        }
                        Error(_) ->
                          Ok(RenameColumn(table.name, old_name, col.name))
                      }
                    }
                  }
                }
                False -> {
                  let red = "\u{001b}[31m"
                  let reset = "\u{001b}[0m"
                  let error_msg =
                    red
                    <> "Error: rename_from('"
                    <> old_name
                    <> "') specified for column '"
                    <> col.name
                    <> "' in table '"
                    <> table.name
                    <> "', but column '"
                    <> old_name
                    <> "' does not exist in the schema snapshot."
                    <> reset
                  panic as error_msg
                }
              }
            }
            option.None -> Error(Nil)
          }
        })

      // Get list of old names that are being renamed (to exclude from dropped)
      let renamed_old_names =
        list.filter_map(table.columns, fn(col) {
          case col.renamed_from {
            option.Some(old_name) -> Ok(old_name)
            option.None -> Error(Nil)
          }
        })

      // Get list of new names that are renames (to exclude from added)
      let renamed_new_names =
        list.filter_map(table.columns, fn(col) {
          case col.renamed_from {
            option.Some(_) -> Ok(col.name)
            option.None -> Error(Nil)
          }
        })

      // New columns (excluding renames)
      let added =
        table.columns
        |> list.filter(fn(c) {
          !list.contains(old_col_names, c.name)
          && !list.contains(renamed_new_names, c.name)
        })
        |> list.map(fn(c) { AddColumn(table.name, c) })

      // Dropped columns (excluding columns that are being renamed)
      let dropped =
        old_table.columns
        |> list.filter(fn(c) {
          !list.contains(new_col_names, c.name)
          && !list.contains(renamed_old_names, c.name)
        })
        |> list.map(fn(c) { DropColumn(table.name, c.name) })

      // Altered columns
      let altered =
        table.columns
        |> list.filter_map(fn(col) {
          case list.find(old_table.columns, fn(c) { c.name == col.name }) {
            Ok(old_col) -> {
              let new_type = column_type_to_string(col.column_type)
              case
                old_col.column_type != new_type
                || old_col.nullable != col.nullable
              {
                True -> Ok(AlterColumn(table.name, col, old_col))
                False -> Error(Nil)
              }
            }
            Error(_) -> Error(Nil)
          }
        })

      list.flatten([renames, added, dropped, altered])
    }
    Error(_) -> []
  }
}

/// Convert a single Change to its SQL representation.
/// Dispatches to specific SQL generators based on the change
/// type (create, drop, alter, rename).
///
fn change_to_sql(change: Change, driver: Driver) -> String {
  case change {
    CreateTable(table) -> create_table_sql(table, driver)
    DropTable(name) -> "DROP TABLE " <> name <> ";"
    AddColumn(table, column) ->
      "ALTER TABLE "
      <> table
      <> " ADD COLUMN "
      <> column_definition(column, driver)
      <> ";"
    DropColumn(table, column) ->
      "ALTER TABLE " <> table <> " DROP COLUMN " <> column <> ";"
    AlterColumn(table, column, _old) -> alter_column_sql(table, column, driver)
    RenameColumn(table, old_name, new_name) ->
      "ALTER TABLE "
      <> table
      <> " RENAME COLUMN "
      <> old_name
      <> " TO "
      <> new_name
      <> ";"
  }
}

/// Generate CREATE TABLE SQL with all column definitions.
/// Includes primary keys, constraints, and default values for
/// each column in the table.
///
fn create_table_sql(table: Table, driver: Driver) -> String {
  let columns_sql =
    table.columns
    |> list.map(fn(col) { "  " <> column_definition(col, driver) })
    |> string.join(",\n")

  "CREATE TABLE " <> table.name <> " (\n" <> columns_sql <> "\n);"
}

/// Generate a column definition including type, constraints,
/// and defaults. Combines name, SQL type, nullability, and
/// default value into a complete column spec.
///
fn column_definition(column: Column, driver: Driver) -> String {
  let type_sql = column_type_sql(column.column_type, driver)
  let nullable_sql = case column.nullable {
    True -> ""
    False -> " NOT NULL"
  }
  let default_sql = case column.default {
    option.Some(default_value) ->
      " DEFAULT " <> default_to_sql(default_value, driver)
    option.None -> ""
  }
  let pk_sql = case column.column_type {
    schema_parser.Id -> primary_key_sql(driver)
    _ -> ""
  }

  column.name <> " " <> type_sql <> pk_sql <> nullable_sql <> default_sql
}

/// Map a ColumnType to driver-specific SQL type. Handles
/// differences between Postgres types (VARCHAR, BOOLEAN) and
/// SQLite types (TEXT, INTEGER).
///
fn column_type_sql(col_type: ColumnType, driver: Driver) -> String {
  case driver {
    Postgres ->
      case col_type {
        schema_parser.Id -> "SERIAL"
        schema_parser.String -> "VARCHAR(255)"
        schema_parser.Text -> "TEXT"
        schema_parser.Int -> "INTEGER"
        schema_parser.BigInt -> "BIGINT"
        schema_parser.Float -> "DOUBLE PRECISION"
        schema_parser.Boolean -> "BOOLEAN"
        schema_parser.Timestamp -> "TIMESTAMP"
        schema_parser.UnixTimestamp -> "BIGINT"
        schema_parser.Date -> "DATE"
        schema_parser.Json -> "JSONB"
        schema_parser.Uuid -> "UUID"
        schema_parser.Foreign(ref) -> "INTEGER REFERENCES " <> ref <> "(id)"
      }
    Sqlite ->
      case col_type {
        schema_parser.Id -> "INTEGER"
        schema_parser.String -> "TEXT"
        schema_parser.Text -> "TEXT"
        schema_parser.Int -> "INTEGER"
        schema_parser.BigInt -> "INTEGER"
        schema_parser.Float -> "REAL"
        schema_parser.Boolean -> "INTEGER"
        schema_parser.Timestamp -> "TEXT"
        schema_parser.UnixTimestamp -> "INTEGER"
        schema_parser.Date -> "TEXT"
        schema_parser.Json -> "TEXT"
        schema_parser.Uuid -> "TEXT"
        schema_parser.Foreign(_) -> "INTEGER"
      }
  }
}

/// Generate PRIMARY KEY clause for Id columns. Postgres uses
/// SERIAL with PRIMARY KEY, SQLite uses INTEGER with PRIMARY
/// KEY AUTOINCREMENT.
///
fn primary_key_sql(driver: Driver) -> String {
  case driver {
    Postgres -> " PRIMARY KEY"
    Sqlite -> " PRIMARY KEY AUTOINCREMENT"
  }
}

/// Convert a DefaultValue to its SQL representation. Handles
/// booleans, strings, numbers, timestamps, and auto-generated
/// UUIDs with driver-specific syntax.
///
fn default_to_sql(
  default_value: schema_parser.DefaultValue,
  driver: Driver,
) -> String {
  case default_value {
    schema_parser.DefaultBool(True) ->
      case driver {
        Postgres -> "true"
        Sqlite -> "1"
      }
    schema_parser.DefaultBool(False) ->
      case driver {
        Postgres -> "false"
        Sqlite -> "0"
      }
    schema_parser.DefaultString(s) -> "'" <> escape_sql_string(s) <> "'"
    schema_parser.DefaultInt(n) -> int.to_string(n)
    schema_parser.DefaultFloat(f) -> float.to_string(f)
    schema_parser.DefaultNow -> "CURRENT_TIMESTAMP"
    schema_parser.DefaultUnixNow ->
      case driver {
        Postgres -> "(EXTRACT(EPOCH FROM CURRENT_TIMESTAMP)::BIGINT)"
        Sqlite -> "(strftime('%s', 'now'))"
      }
    schema_parser.DefaultAutoUuid ->
      case driver {
        Postgres -> "gen_random_uuid()"
        Sqlite ->
          "(lower(hex(randomblob(4))) || '-' || lower(hex(randomblob(2))) || '-4' || substr(lower(hex(randomblob(2))),2) || '-' || substr('89ab',abs(random()) % 4 + 1, 1) || substr(lower(hex(randomblob(2))),2) || '-' || lower(hex(randomblob(6))))"
      }
    schema_parser.DefaultNull -> "NULL"
  }
}

/// Escape single quotes in SQL string literals. Doubles any
/// single quote characters to prevent SQL injection and syntax
/// errors in string defaults.
///
fn escape_sql_string(s: String) -> String {
  string.replace(s, "'", "''")
}

/// Generate ALTER COLUMN SQL. Note: SQLite doesn't support
/// ALTER COLUMN, so a comment is generated instead to indicate
/// manual table recreation is needed.
///
fn alter_column_sql(table: String, column: Column, driver: Driver) -> String {
  case driver {
    Postgres -> {
      let type_sql = column_type_sql(column.column_type, Postgres)
      "ALTER TABLE "
      <> table
      <> " ALTER COLUMN "
      <> column.name
      <> " TYPE "
      <> type_sql
      <> ";"
    }
    Sqlite -> {
      // SQLite doesn't support ALTER COLUMN, need to recreate table
      "-- SQLite: ALTER COLUMN not supported. Recreate table manually."
    }
  }
}

/// Check if two column types are compatible for a rename
/// operation. Allows exact matches and semantically equivalent
/// types (e.g., String/Text, Int/BigInt).
///
fn types_compatible_for_rename(old_type: String, new_type: String) -> Bool {
  case old_type == new_type {
    True -> True
    False -> {
      // Check for compatible type pairs
      case old_type, new_type {
        // String <-> Text (both are text types)
        "String", "Text" -> True
        "Text", "String" -> True
        // Int <-> BigInt (same integer family)
        "Int", "BigInt" -> True
        "BigInt", "Int" -> True
        // Everything else is incompatible
        _, _ -> False
      }
    }
  }
}
