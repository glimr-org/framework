//// Migration SQL Generation
////
//// The bridge between "what changed in the schema" and "what
//// SQL to run." This module diffs old and new snapshots to
//// discover table/column/index changes, then emits
//// driver-specific DDL — Postgres and SQLite have enough
//// syntax differences (SERIAL vs INTEGER PRIMARY KEY
//// AUTOINCREMENT, BOOLEAN vs INTEGER, JSONB vs TEXT) that a
//// single SQL template can't cover both.

import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import glimr/db/gen/migrate/snapshot.{
  type ColumnSnapshot, type IndexSnapshot, type Snapshot, column_type_to_string,
}
import glimr/db/gen/schema_parser.{type Column, type ColumnType, type Table}

// ------------------------------------------------------------- Public Types

/// Every SQL generator function branches on this to pick the
/// right dialect. Postgres has real BOOLEAN, JSONB, UUID, and
/// SERIAL types; SQLite folds most of those into TEXT or
/// INTEGER. Keeping the driver as a simple enum means we can
/// pattern-match cleanly instead of threading config through
/// every function.
///
pub type Driver {
  Postgres
  Sqlite
}

/// The output of comparing two snapshots. This flat list of
/// changes becomes the input to `generate_sql`, which turns
/// each one into a DDL statement. Keeping it as a plain list
/// rather than a nested tree makes it easy to filter, reorder
/// (topological sort for FK deps), and map over when generating
/// both the SQL and the CLI summary output.
///
pub type SchemaDiff {
  SchemaDiff(changes: List(Change))
}

/// Each variant maps 1:1 to a SQL DDL statement. Having
/// explicit variants instead of a generic "run this SQL" lets
/// us topologically sort CreateTable by FK deps, order drops
/// before creates for indexes, and produce human-readable
/// descriptions for the CLI — none of which would be possible
/// with raw SQL strings.
///
pub type Change {
  CreateTable(table: Table)
  DropTable(name: String)
  AddColumn(table: String, column: Column)
  DropColumn(table: String, column: String)
  AlterColumn(table: String, column: Column, old: ColumnSnapshot)
  RenameColumn(table: String, old_name: String, new_name: String)
  CreateIndex(table: String, index: schema_parser.Index)
  DropIndex(table: String, index_name: String)
}

// ------------------------------------------------------------- Public Functions

/// The heart of the migration system. Compares the saved
/// snapshot against the current schema to figure out what SQL
/// to generate. The `is_filtered` flag matters when you're
/// generating migrations for a single model — in that case we
/// skip drop detection, because a table missing from the filter
/// doesn't mean it was deleted, it just wasn't included this
/// run.
///
pub fn compute_diff(
  old: Snapshot,
  new: Snapshot,
  tables: List(Table),
  is_filtered: Bool,
) -> SchemaDiff {
  let old_names = dict.keys(old.tables)
  let new_names = dict.keys(new.tables)

  // Find new tables (and their indexes)
  let new_table_list =
    list.filter(tables, fn(t) { !list.contains(old_names, t.name) })
  let new_tables = list.map(new_table_list, CreateTable)
  let new_table_indexes =
    list.flat_map(new_table_list, fn(t) {
      list.map(t.indexes, fn(idx) { CreateIndex(t.name, idx) })
    })

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
    changes: list.flatten([
      new_tables,
      new_table_indexes,
      dropped_tables,
      column_changes,
    ]),
  )
}

/// If you create `posts` before `users` and `posts` has a FK to
/// `users`, the migration blows up. This pulls CreateTable
/// changes out, topologically sorts them by FK deps, then puts
/// them back at the front — followed by CreateIndex (which
/// needs the tables to exist), then everything else (adds,
/// drops, alters) in original order.
///
fn sort_changes_by_dependency(changes: List(Change)) -> List(Change) {
  // Separate CreateTable and CreateIndex from other changes
  let #(create_tables, non_create_tables) =
    list.partition(changes, fn(c) {
      case c {
        CreateTable(_) -> True
        _ -> False
      }
    })
  let #(create_indexes, other_changes) =
    list.partition(non_create_tables, fn(c) {
      case c {
        CreateIndex(_, _) -> True
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

  // CreateTables first (in dependency order), then CreateIndexes, then other changes
  list.flatten([sorted_creates, create_indexes, other_changes])
}

/// Kahn's algorithm for dependency ordering. Only counts FK
/// references to tables being created in this batch —
/// references to existing tables don't count as deps because
/// those tables are already in the database.
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

/// Each iteration peels off tables whose deps are already
/// sorted, then recurses on the rest. If nothing is ready
/// (circular FK references), it dumps the remaining tables in
/// their original order rather than looping forever — the
/// migration will still fail at the DB level, but at least we
/// don't hang.
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

/// Turns the diff into runnable SQL. Before emitting anything,
/// it topologically sorts CreateTable changes so a `posts`
/// table that references `users` gets created after `users` —
/// otherwise the FK constraint would reference a table that
/// doesn't exist yet. Indexes come after their tables for the
/// same reason.
///
pub fn generate_sql(diff: SchemaDiff, driver: Driver) -> String {
  diff.changes
  |> sort_changes_by_dependency
  |> list.map(fn(change) { change_to_sql(change, driver) })
  |> string.join("\n\n")
}

/// Produces the summary lines you see in the terminal when a
/// migration is generated ("Create table: users", "Add column:
/// posts.slug"). Keeping this separate from the SQL generation
/// means the CLI can show what will happen before the SQL is
/// actually written to disk.
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
    CreateIndex(table, idx) -> "Create index: " <> index_name(table, idx)
    DropIndex(_, name) -> "Drop index: " <> name
  }
}

// ------------------------------------------------------------- Private Functions

/// The per-table workhorse. Compares the current table
/// definition against its snapshot to find what actually
/// changed — renames first (so a renamed column doesn't show up
/// as a drop+add), then new columns, dropped columns,
/// type/nullable alterations, and finally index changes.
/// Renames are checked first because they affect which columns
/// count as "new" vs "dropped."
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

      // Index changes
      let index_changes =
        compute_index_diff(table.name, old_table.indexes, table.indexes)

      list.flatten([renames, added, dropped, altered, index_changes])
    }
    Error(_) -> []
  }
}

/// This is where the Driver enum earns its keep — a CreateIndex
/// is the same SQL on both databases, but a CreateTable needs
/// completely different type mappings, and an AlterColumn is
/// impossible on SQLite. Each variant decides how much
/// driver-awareness it needs.
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
    CreateIndex(table, idx) -> {
      let idx_name = index_name(table, idx)
      let unique_str = case idx.unique {
        True -> "UNIQUE "
        False -> ""
      }
      let cols = string.join(idx.columns, ", ")
      "CREATE "
      <> unique_str
      <> "INDEX "
      <> idx_name
      <> " ON "
      <> table
      <> " ("
      <> cols
      <> ");"
    }
    DropIndex(_, name) -> "DROP INDEX " <> name <> ";"
  }
}

/// Builds a complete CREATE TABLE statement with every column's
/// type, constraints, and defaults. Each column is indented on
/// its own line for readability in the generated migration file
/// — developers often review these before running them.
///
fn create_table_sql(table: Table, driver: Driver) -> String {
  let columns_sql =
    table.columns
    |> list.map(fn(col) { "  " <> column_definition(col, driver) })
    |> string.join(",\n")

  "CREATE TABLE " <> table.name <> " (\n" <> columns_sql <> "\n);"
}

/// Assembles a single column's SQL fragment: name, type,
/// PRIMARY KEY (for Id columns), NOT NULL, and DEFAULT. The
/// ordering matters — SQL requires PRIMARY KEY before NOT NULL
/// and DEFAULT, and getting it wrong produces syntax errors
/// that are annoying to debug.
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

/// The big type mapping table. Postgres has rich types
/// (VARCHAR, BOOLEAN, JSONB, UUID) while SQLite squashes most
/// things into TEXT or INTEGER. This is where that impedance
/// mismatch lives — one ColumnType variant in, the right SQL
/// type string out.
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

/// SQLite requires AUTOINCREMENT alongside PRIMARY KEY to get
/// auto-incrementing IDs, while Postgres uses SERIAL (which
/// already implies a sequence). Without this split, SQLite
/// would create a plain integer PK that doesn't auto-increment,
/// leading to "NOT NULL constraint failed" on inserts that omit
/// the id.
///
fn primary_key_sql(driver: Driver) -> String {
  case driver {
    Postgres -> " PRIMARY KEY"
    Sqlite -> " PRIMARY KEY AUTOINCREMENT"
  }
}

/// Default values are where Postgres and SQLite diverge the
/// most. Booleans are `true`/`false` in Postgres but `1`/`0` in
/// SQLite. Unix timestamps use `EXTRACT` in Postgres vs
/// `strftime` in SQLite. Auto-UUIDs use `gen_random_uuid()` in
/// Postgres but need a gnarly `randomblob` expression in
/// SQLite. Getting any of these wrong means silent data
/// corruption.
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

/// String defaults go into the migration as SQL literals, so a
/// value like `it's` would break the SQL syntax if we didn't
/// double the quote. This is the only escaping needed since
/// defaults come from the developer's schema code, not user
/// input.
///
fn escape_sql_string(s: String) -> String {
  string.replace(s, "'", "''")
}

/// Postgres supports ALTER COLUMN TYPE directly, but SQLite
/// famously doesn't — you have to recreate the entire table to
/// change a column type. Rather than attempting that
/// automatically (which risks data loss), we emit a SQL comment
/// telling the developer to handle it manually.
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

/// Renaming a column should preserve data, so changing the type
/// at the same time is suspicious — it probably means the
/// developer intended a drop+add, not a rename. But some type
/// changes are safe: String and Text are both text storage, Int
/// and BigInt are both integers. We allow those and reject
/// everything else to prevent accidental data reinterpretation.
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

/// Index names need to be deterministic so the migration system
/// can generate matching DROP INDEX statements later. Custom
/// names take priority — otherwise we build
/// `idx_{table}_{col1}_{col2}` which is predictable enough that
/// compute_index_diff can reconstruct it from the snapshot when
/// generating drops.
///
fn index_name(table: String, idx: schema_parser.Index) -> String {
  case idx.name {
    option.Some(name) -> name
    option.None -> "idx_" <> table <> "_" <> string.join(idx.columns, "_")
  }
}

/// Compares old snapshot indexes against new schema indexes to
/// find what was added or removed. Matching is by column list +
/// unique flag — if you change which columns an index covers,
/// it shows up as a drop of the old one and a create of the new
/// one. Drops come before creates in the output so the
/// migration doesn't fail on duplicate index names.
///
fn compute_index_diff(
  table_name: String,
  old_indexes: List(IndexSnapshot),
  new_indexes: List(schema_parser.Index),
) -> List(Change) {
  // Convert new indexes to a comparable form
  let index_matches = fn(old: IndexSnapshot, new: schema_parser.Index) -> Bool {
    old.columns == new.columns && old.unique == new.unique
  }

  // Added indexes: in new but not in old
  let added =
    new_indexes
    |> list.filter(fn(new_idx) {
      !list.any(old_indexes, fn(old_idx) { index_matches(old_idx, new_idx) })
    })
    |> list.map(fn(idx) { CreateIndex(table_name, idx) })

  // Dropped indexes: in old but not in new
  let dropped =
    old_indexes
    |> list.filter(fn(old_idx) {
      !list.any(new_indexes, fn(new_idx) { index_matches(old_idx, new_idx) })
    })
    |> list.map(fn(old_idx) {
      let name = case old_idx.name {
        option.Some(n) -> n
        option.None ->
          "idx_" <> table_name <> "_" <> string.join(old_idx.columns, "_")
      }
      DropIndex(table_name, name)
    })

  list.append(dropped, added)
}
