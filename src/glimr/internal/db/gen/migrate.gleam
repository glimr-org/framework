//// Migration Generator
////
//// Writing migration SQL by hand after every schema change is
//// tedious and error-prone — developers forget columns, get
//// types wrong, or miss renames. This module compares the
//// current schema files against a stored snapshot to detect
//// changes automatically and emit driver-specific SQL. The
//// snapshot is updated after each run so only new changes
//// appear in subsequent migrations.

import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import glimr/console/console
import glimr/db/driver
import glimr/internal/db/gen/schema_parser.{
  type Column, type ColumnType, type Table,
}
import shellout
import simplifile

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
  CreateEnumType(name: String, variants: List(String))
}

/// The top-level container saved to
/// `.glimr_schema_snapshot.json`. It's a dictionary keyed by
/// table name so the diff algorithm can look up any table's
/// previous state in O(1) instead of scanning a list.
///
pub type Snapshot {
  Snapshot(tables: Dict(String, TableSnapshot))
}

/// Columns are kept in definition order (not sorted) so the
/// diff can detect position changes if needed in the future.
/// Indexes live alongside columns because they're part of the
/// same table definition and need to be diffed together.
///
pub type TableSnapshot {
  TableSnapshot(columns: List(ColumnSnapshot), indexes: List(IndexSnapshot))
}

/// Captures an index's state at snapshot time so the next
/// migration run can detect whether indexes were added,
/// removed, or changed. The optional name preserves custom
/// names through the snapshot so they survive across migration
/// generations.
///
pub type IndexSnapshot {
  IndexSnapshot(columns: List(String), unique: Bool, name: Option(String))
}

/// Stores just enough about a column to detect changes worth
/// migrating: type, nullability, and whether a default exists.
/// The actual default value isn't stored because changing a
/// default doesn't require a migration — it only affects new
/// rows, not the table structure.
///
pub type ColumnSnapshot {
  ColumnSnapshot(
    name: String,
    column_type: String,
    nullable: Bool,
    has_default: Bool,
  )
}

// ------------------------------------------------------------- Public Functions

/// Pass a connection name and the migration system figures out
/// the rest — reads the driver type from database.toml, finds
/// the schema files, diffs against the snapshot, and writes the
/// migration SQL. The optional model filter lets you generate
/// migrations for just the tables you changed.
///
pub fn run(name: String, model_filter: Option(List(String)), verbose: Bool) {
  case verbose {
    True -> {
      io.println(console.warning("Glimr Migration Generator"))
      io.println("  Connection: " <> name)
    }
    False -> Nil
  }

  // Resolve driver type from database.toml config
  let connections = driver.load_connections()
  let connection = driver.find_by_name(name, connections)
  let driver_type = driver.connection_type(connection)

  let driver_type_str = case driver_type {
    driver.Postgres -> "postgres"
    driver.Sqlite -> "sqlite"
  }

  case verbose {
    True -> io.println("  Driver: " <> driver_type_str)
    False -> Nil
  }

  let is_filtered = option.is_some(model_filter)
  case verbose, model_filter {
    True, Some(models) -> io.println("  Models: " <> string.join(models, ", "))
    _, _ -> Nil
  }

  // Folder structure: src/database/{name}/...
  let base_path = "src/database/" <> name
  let models_path = base_path <> "/models"
  let snapshot_path = base_path <> "/._schema_snapshot.json"
  let migrations_path = base_path <> "/_migrations"

  // Convert driver type to Driver
  let sql_driver = case driver_type {
    driver.Postgres -> Postgres
    driver.Sqlite -> Sqlite
  }

  do_run(
    models_path,
    snapshot_path,
    migrations_path,
    sql_driver,
    driver_type_str,
    model_filter,
    is_filtered,
    verbose,
  )

  case verbose {
    True -> Nil
    False -> console.line_success("Generated migrations (" <> name <> ")")
  }
}

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
    CreateEnumType(name, _) -> "Create enum type: " <> name
  }
}

// ------------------------------------------------------------- Snapshot Public Functions

/// Reads the snapshot file from disk. Returns an empty snapshot
/// if the file doesn't exist (first run) or can't be parsed
/// (corrupted file). Falling back to empty means the differ
/// will treat every table as new and generate full CREATE TABLE
/// statements — which is the right thing on a fresh project.
///
pub fn load_snapshot(path: String) -> Snapshot {
  case simplifile.read(path) {
    Ok(content) -> parse_snapshot(content)
    Error(_) -> Snapshot(tables: dict.new())
  }
}

/// Writes the snapshot to disk after migration generation. The
/// JSON is human-readable (indented, one column per line) so
/// developers can inspect it when debugging why a migration was
/// or wasn't generated. Returns Error only if the file write
/// fails.
///
pub fn save_snapshot(path: String, snapshot: Snapshot) -> Result(Nil, Nil) {
  let content = snapshot_to_json(snapshot)
  case simplifile.write(path, content) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error(Nil)
  }
}

/// Converts live Table definitions into a snapshot that can be
/// saved to JSON. Strips out information the diff doesn't need
/// (like the actual default value — only whether one exists
/// matters) to keep the snapshot file small and the diff logic
/// simple.
///
pub fn build_snapshot(tables: List(Table)) -> Snapshot {
  let table_dict =
    tables
    |> list.map(fn(table) {
      let columns =
        table.columns
        |> list.map(fn(col) {
          ColumnSnapshot(
            name: col.name,
            column_type: column_type_to_string(col.column_type),
            nullable: col.nullable,
            has_default: option.is_some(col.default),
          )
        })
      let indexes =
        table.indexes
        |> list.map(fn(idx) {
          IndexSnapshot(
            columns: idx.columns,
            unique: idx.unique,
            name: idx.name,
          )
        })
      #(table.name, TableSnapshot(columns: columns, indexes: indexes))
    })
    |> dict.from_list()

  Snapshot(tables: table_dict)
}

/// When generating a migration for just one model, we only have
/// a snapshot of that model's tables. Without merging, saving
/// would wipe out all other tables from the snapshot file, and
/// the next full run would think those tables are new. This
/// overlays the filtered snapshot onto the existing one so
/// unrelated tables are preserved.
///
pub fn merge_snapshots(old: Snapshot, new: Snapshot) -> Snapshot {
  let merged_tables = dict.merge(old.tables, new.tables)
  Snapshot(tables: merged_tables)
}

/// Column types are stored as strings in the JSON snapshot
/// because Gleam's custom types can't be directly serialized.
/// The string format is stable across versions — changing it
/// would make every existing snapshot look like a type change,
/// generating spurious ALTER COLUMN migrations.
///
pub fn column_type_to_string(col_type: ColumnType) -> String {
  case col_type {
    schema_parser.Id -> "Id"
    schema_parser.String -> "String"
    schema_parser.Text -> "Text"
    schema_parser.Int -> "Int"
    schema_parser.SmallInt -> "SmallInt"
    schema_parser.BigInt -> "BigInt"
    schema_parser.Float -> "Float"
    schema_parser.Boolean -> "Boolean"
    schema_parser.Timestamp -> "Timestamp"
    schema_parser.UnixTimestamp -> "UnixTimestamp"
    schema_parser.Date -> "Date"
    schema_parser.Json -> "Json"
    schema_parser.Uuid -> "Uuid"
    schema_parser.Foreign(ref, on_delete, on_update) -> {
      let base = "Foreign(" <> ref
      let del = case on_delete {
        Some(action) -> ",onDelete:" <> foreign_action_to_string(action)
        None -> ""
      }
      let upd = case on_update {
        Some(action) -> ",onUpdate:" <> foreign_action_to_string(action)
        None -> ""
      }
      base <> del <> upd <> ")"
    }
    schema_parser.Array(inner) ->
      "Array(" <> column_type_to_string(inner) <> ")"
    schema_parser.Enum(name, variants) ->
      "Enum(" <> name <> ":" <> string.join(variants, ",") <> ")"
    schema_parser.Decimal(p, s) ->
      "Decimal(" <> int.to_string(p) <> "," <> int.to_string(s) <> ")"
    schema_parser.Blob -> "Blob"
    schema_parser.Time -> "Time"
  }
}

// ------------------------------------------------------------- Validation Public Functions

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

/// An `Array(Id)` would mean an auto-incrementing primary key
/// that's also a list — which no database supports.
/// `Array(Foreign("users"))` would imply FK constraints on each
/// element, but Postgres arrays can't enforce that. Both would
/// generate invalid SQL, so we catch them here with a message
/// that explains why instead of letting the database reject the
/// migration cryptically.
///
pub fn validate_array_types(tables: List(Table)) -> Nil {
  list.each(tables, fn(table) {
    list.each(table.columns, fn(col) {
      validate_array_inner(table.name, col.name, col.column_type)
    })
  })
}

/// Enum columns need at least one variant, can't have empty
/// variant strings, and can't have duplicate variants. Without
/// these checks, the generated Gleam custom type would be
/// invalid or the database CHECK/CREATE TYPE would fail.
///
pub fn validate_enum_variants(tables: List(Table)) -> Nil {
  list.each(tables, fn(table) {
    list.each(table.columns, fn(col) {
      case col.column_type {
        schema_parser.Enum(_, variants) -> {
          let red = "\u{001b}[31m"
          let reset = "\u{001b}[0m"
          // Must have at least one variant
          case variants {
            [] -> {
              let error_msg =
                red
                <> "Error: Enum column '"
                <> col.name
                <> "' in table '"
                <> table.name
                <> "' must have at least one variant."
                <> reset
              panic as error_msg
            }
            _ -> Nil
          }
          // No empty variant strings
          list.each(variants, fn(v) {
            case v {
              "" -> {
                let error_msg =
                  red
                  <> "Error: Enum column '"
                  <> col.name
                  <> "' in table '"
                  <> table.name
                  <> "' has an empty variant string."
                  <> reset
                panic as error_msg
              }
              _ -> Nil
            }
          })
          // No duplicate variants
          let duplicates = find_duplicates(variants)
          case duplicates {
            [] -> Nil
            dupes -> {
              let error_msg =
                red
                <> "Error: Enum column '"
                <> col.name
                <> "' in table '"
                <> table.name
                <> "' has duplicate variants: "
                <> string.join(dupes, ", ")
                <> reset
              panic as error_msg
            }
          }
        }
        _ -> Nil
      }
    })
  })
}

// ------------------------------------------------------------- Cleanup Public Functions

/// Called right after migration generation. If a developer
/// forgets to remove `rename_from` from their schema, the next
/// migration run would see the same rename again and emit a
/// duplicate RENAME COLUMN that crashes. Rather than relying on
/// developers to remember, we just clean it up for them — find
/// every schema file, strip the pipes, tidy the imports, and
/// reformat.
///
pub fn clean_rename_from_modifiers(models_path: String) -> Nil {
  case simplifile.read_directory(models_path) {
    Ok(entries) -> {
      let model_dirs =
        list.filter(entries, fn(entry) {
          case simplifile.is_directory(models_path <> "/" <> entry) {
            Ok(True) -> True
            _ -> False
          }
        })

      list.each(model_dirs, fn(model_name) {
        let schema_path =
          models_path
          <> "/"
          <> model_name
          <> "/"
          <> model_name
          <> "_schema.gleam"
        case simplifile.read(schema_path) {
          Ok(content) -> {
            let cleaned = remove_rename_from_calls(content)
            case cleaned != content {
              True -> {
                case simplifile.write(schema_path, cleaned) {
                  Ok(_) -> {
                    // Format the cleaned schema file
                    let _ =
                      shellout.command(
                        "gleam",
                        ["format", schema_path],
                        ".",
                        [],
                      )
                    io.println(
                      "Cleaned rename_from from: "
                      <> model_name
                      <> "/"
                      <> model_name
                      <> "_schema.gleam",
                    )
                  }
                  Error(_) -> Nil
                }
              }
              False -> Nil
            }
          }
          Error(_) -> Nil
        }
      })
    }
    Error(_) -> Nil
  }
}

// ------------------------------------------------------------- Private Functions

/// The full migration pipeline: load the previous snapshot,
/// scan current schemas, validate them, diff to find changes,
/// generate SQL, write the migration file, and update the
/// snapshot. When filtering by model, the snapshot merge
/// preserves unrelated tables.
///
fn do_run(
  models_path: String,
  snapshot_path: String,
  migrations_path: String,
  drv: Driver,
  driver_name: String,
  model_filter: Option(List(String)),
  is_filtered: Bool,
  verbose: Bool,
) {
  // Load existing snapshot
  let old_snapshot = load_snapshot(snapshot_path)

  // Ensure migrations directory exists
  let _ = simplifile.create_directory_all(migrations_path)

  // Scan current schemas (filtered if specified)
  case scan_schemas(models_path, model_filter) {
    Ok(tables) -> {
      // Validate schemas
      validate_no_duplicate_columns(tables)
      validate_indexes(tables)
      validate_array_types(tables)
      validate_enum_variants(tables)

      case verbose {
        True ->
          io.println(
            "  Found " <> int.to_string(list.length(tables)) <> " table(s)",
          )
        False -> Nil
      }

      // Build new snapshot (only for scanned tables)
      let new_snapshot = build_snapshot(tables)

      // Compute diff (skip drop detection when filtering by model)
      let diff = compute_diff(old_snapshot, new_snapshot, tables, is_filtered)

      case diff.changes {
        [] -> {
          case verbose {
            True -> {
              io.println("")
              io.println("  No changes detected.")
            }
            False -> Nil
          }
        }
        changes -> {
          case verbose {
            True -> {
              io.println("")
              io.println(
                "  Detected "
                <> int.to_string(list.length(changes))
                <> " change(s):",
              )
              list.each(changes, fn(change) {
                io.println("    - " <> describe_change(change))
              })
            }
            False -> Nil
          }

          // Generate migration SQL for the configured driver only
          let timestamp = get_timestamp()
          let filename = timestamp <> "_migration.sql"
          let migration_sql = generate_sql(diff, drv)

          // Write migration file
          let migration_path = migrations_path <> "/" <> filename

          let content =
            "-- Generated by Glimr ✨ ("
            <> driver_name
            <> ")\n\n"
            <> migration_sql
            <> "\n"

          case simplifile.write(migration_path, content) {
            Ok(_) -> {
              case verbose {
                True -> {
                  io.println("")
                  io.println("  Generated: " <> migration_path)
                }
                False -> Nil
              }

              // Update snapshot (merge when filtered, replace when not)
              let final_snapshot = case is_filtered {
                True -> merge_snapshots(old_snapshot, new_snapshot)
                False -> new_snapshot
              }
              case save_snapshot(snapshot_path, final_snapshot) {
                Ok(_) -> {
                  case verbose {
                    True -> io.println("  Updated: " <> snapshot_path)
                    False -> Nil
                  }
                }
                Error(_) ->
                  io.println("  Warning: Could not update snapshot file")
              }

              // Clean up rename_from modifiers from schema files
              clean_rename_from_modifiers(models_path)
            }
            Error(_) -> io.println("  Error: Could not write migration file")
          }
        }
      }
    }
    Error(err) -> {
      io.println("  Error: " <> err)
    }
  }

  case verbose {
    True -> {
      io.println("")
      io.println(console.success("  Successfully generated migrations!"))
    }
    False -> Nil
  }
}

/// Reads each model's schema file and parses it into a Table.
/// The optional filter limits which models are scanned — when
/// you pass `--model user`, only the user schema gets parsed so
/// the diff doesn't accidentally generate drops for tables that
/// are just filtered out.
///
fn scan_schemas(
  models_path: String,
  model_filter: Option(List(String)),
) -> Result(List(Table), String) {
  case simplifile.read_directory(models_path) {
    Ok(entries) -> {
      let model_dirs =
        list.filter(entries, fn(entry) {
          case simplifile.is_directory(models_path <> "/" <> entry) {
            Ok(True) -> {
              // Apply filter if specified
              case model_filter {
                None -> True
                Some(allowed) -> list.contains(allowed, entry)
              }
            }
            _ -> False
          }
        })

      let tables =
        list.filter_map(model_dirs, fn(model_name) {
          let schema_path =
            models_path
            <> "/"
            <> model_name
            <> "/"
            <> model_name
            <> "_schema.gleam"
          case simplifile.read(schema_path) {
            Ok(content) -> {
              case schema_parser.parse(content) {
                Ok(table) -> Ok(table)
                Error(_) -> Error(Nil)
              }
            }
            Error(_) -> Error(Nil)
          }
        })

      Ok(tables)
    }
    Error(_) -> Error("Could not read " <> models_path)
  }
}

/// Migration files are sorted lexicographically to determine
/// execution order. A timestamp prefix guarantees chronological
/// ordering without a central counter, and matches the
/// convention used by Rails, Laravel, and other frameworks.
///
fn get_timestamp() -> String {
  case shellout.command("date", ["+%Y%m%d%H%M%S"], ".", []) {
    Ok(output) -> string.trim(output)
    Error(_) -> "00000000000000"
  }
}

// ------------------------------------------------------------- SQL Private Functions

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
      let new_columns =
        table.columns
        |> list.filter(fn(c) {
          !list.contains(old_col_names, c.name)
          && !list.contains(renamed_new_names, c.name)
        })

      // Emit CreateEnumType for new enum columns (Postgres needs
      // the type to exist before the column references it)
      let enum_types =
        new_columns
        |> list.filter_map(fn(c) {
          case c.column_type {
            schema_parser.Enum(name, variants) ->
              Ok(CreateEnumType(name, variants))
            _ -> Error(Nil)
          }
        })

      let added = list.map(new_columns, fn(c) { AddColumn(table.name, c) })

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

      list.flatten([renames, enum_types, added, dropped, altered, index_changes])
    }
    Error(_) -> []
  }
}

/// If you create `posts` before `users` and `posts` has a FK to
/// `users`, the migration blows up. This pulls CreateTable
/// changes out, topologically sorts them by FK deps, then puts
/// them back at the front — followed by CreateIndex (which
/// needs the tables to exist), then everything else (adds,
/// drops, alters) in original order.
///
fn sort_changes_by_dependency(changes: List(Change)) -> List(Change) {
  // Separate CreateEnumType first (must come before everything)
  let #(create_enums, non_enum_changes) =
    list.partition(changes, fn(c) {
      case c {
        CreateEnumType(_, _) -> True
        _ -> False
      }
    })

  // Separate CreateTable and CreateIndex from other changes
  let #(create_tables, non_create_tables) =
    list.partition(non_enum_changes, fn(c) {
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

  // CreateEnumTypes first, then CreateTables (in dependency order), then CreateIndexes, then other changes
  list.flatten([create_enums, sorted_creates, create_indexes, other_changes])
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
        schema_parser.Foreign(ref, _, _) -> {
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
    CreateEnumType(name, variants) -> {
      let variant_list =
        list.map(variants, fn(v) { "'" <> v <> "'" })
        |> string.join(", ")
      "CREATE TYPE " <> name <> " AS ENUM (" <> variant_list <> ");"
    }
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

  // For Postgres, prepend CREATE TYPE for enum columns
  let enum_types = case driver {
    Postgres ->
      table.columns
      |> list.filter_map(fn(col) {
        case col.column_type {
          schema_parser.Enum(name, variants) -> {
            let variant_list =
              list.map(variants, fn(v) { "'" <> v <> "'" })
              |> string.join(", ")
            Ok("CREATE TYPE " <> name <> " AS ENUM (" <> variant_list <> ");")
          }
          _ -> Error(Nil)
        }
      })
    Sqlite -> []
  }

  let create_table =
    "CREATE TABLE " <> table.name <> " (\n" <> columns_sql <> "\n);"

  case enum_types {
    [] -> create_table
    types -> string.join(types, "\n\n") <> "\n\n" <> create_table
  }
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

  // FK action clauses
  let fk_actions_sql = case column.column_type {
    schema_parser.Foreign(_, on_delete, on_update) ->
      foreign_action_clause(on_delete, "ON DELETE")
      <> foreign_action_clause(on_update, "ON UPDATE")
    _ -> ""
  }

  // SQLite CHECK constraint for enum columns
  let check_sql = case column.column_type, driver {
    schema_parser.Enum(_, variants), Sqlite -> {
      let variant_list =
        list.map(variants, fn(v) { "'" <> v <> "'" })
        |> string.join(", ")
      " CHECK (" <> column.name <> " IN (" <> variant_list <> "))"
    }
    _, _ -> ""
  }

  column.name
  <> " "
  <> type_sql
  <> pk_sql
  <> fk_actions_sql
  <> nullable_sql
  <> default_sql
  <> check_sql
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
        schema_parser.SmallInt -> "SMALLINT"
        schema_parser.BigInt -> "BIGINT"
        schema_parser.Float -> "DOUBLE PRECISION"
        schema_parser.Boolean -> "BOOLEAN"
        schema_parser.Timestamp -> "TIMESTAMP"
        schema_parser.UnixTimestamp -> "BIGINT"
        schema_parser.Date -> "DATE"
        schema_parser.Json -> "JSONB"
        schema_parser.Uuid -> "UUID"
        schema_parser.Foreign(ref, _, _) ->
          "INTEGER REFERENCES " <> ref <> "(id)"
        schema_parser.Array(inner) -> column_type_sql(inner, Postgres) <> "[]"
        schema_parser.Enum(name, _) -> name
        schema_parser.Decimal(p, s) ->
          "NUMERIC(" <> int.to_string(p) <> ", " <> int.to_string(s) <> ")"
        schema_parser.Blob -> "BYTEA"
        schema_parser.Time -> "TIME"
      }
    Sqlite ->
      case col_type {
        schema_parser.Id -> "INTEGER"
        schema_parser.String -> "TEXT"
        schema_parser.Text -> "TEXT"
        schema_parser.Int -> "INTEGER"
        schema_parser.SmallInt -> "INTEGER"
        schema_parser.BigInt -> "INTEGER"
        schema_parser.Float -> "REAL"
        schema_parser.Boolean -> "INTEGER"
        schema_parser.Timestamp -> "TEXT"
        schema_parser.UnixTimestamp -> "INTEGER"
        schema_parser.Date -> "TEXT"
        schema_parser.Json -> "TEXT"
        schema_parser.Uuid -> "TEXT"
        schema_parser.Foreign(_, _, _) -> "INTEGER"
        schema_parser.Array(_) -> "TEXT"
        schema_parser.Enum(_, _) -> "TEXT"
        schema_parser.Decimal(_, _) -> "TEXT"
        schema_parser.Blob -> "BLOB"
        schema_parser.Time -> "TEXT"
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
    schema_parser.DefaultEmptyArray ->
      case driver {
        Postgres -> "'{}'"
        Sqlite -> "'[]'"
      }
  }
}

/// Foreign keys optionally specify what happens on delete or
/// update — CASCADE, RESTRICT, SET NULL, etc. If no action was
/// set in the schema, we emit nothing and let the database use
/// its default (usually NO ACTION). The leading space is
/// intentional so the caller can just concatenate.
///
fn foreign_action_clause(
  action: option.Option(schema_parser.ForeignAction),
  prefix: String,
) -> String {
  case action {
    option.Some(a) -> " " <> prefix <> " " <> foreign_action_to_sql(a)
    option.None -> ""
  }
}

/// The SQL keywords for FK actions have spaces in them ("SET
/// NULL", "NO ACTION") while the Gleam constructors don't.
/// Centralizing the mapping here instead of inlining it in
/// column_definition keeps the string literals in one place so
/// a typo doesn't silently produce invalid DDL.
///
fn foreign_action_to_sql(action: schema_parser.ForeignAction) -> String {
  case action {
    schema_parser.Cascade -> "CASCADE"
    schema_parser.Restrict -> "RESTRICT"
    schema_parser.SetNull -> "SET NULL"
    schema_parser.SetDefault -> "SET DEFAULT"
    schema_parser.NoAction -> "NO ACTION"
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
        // SmallInt <-> Int <-> BigInt (same integer family)
        "SmallInt", "Int" -> True
        "SmallInt", "BigInt" -> True
        "Int", "SmallInt" -> True
        "Int", "BigInt" -> True
        "BigInt", "SmallInt" -> True
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

// ------------------------------------------------------------- Snapshot Private Functions

/// The snapshot stores FK actions as readable strings like
/// "Cascade" and "SetNull" rather than the SQL keywords
/// "CASCADE" and "SET NULL". This keeps the snapshot format
/// database-agnostic — the SQL module handles the dialect-
/// specific conversion when generating migrations.
///
fn foreign_action_to_string(action: schema_parser.ForeignAction) -> String {
  case action {
    schema_parser.Cascade -> "Cascade"
    schema_parser.Restrict -> "Restrict"
    schema_parser.SetNull -> "SetNull"
    schema_parser.SetDefault -> "SetDefault"
    schema_parser.NoAction -> "NoAction"
  }
}

/// Deserializes the JSON file content into a Snapshot. Falls
/// back to an empty snapshot on any parse error rather than
/// crashing — a corrupted snapshot file shouldn't block the
/// developer; they'll just get a migration that recreates
/// everything.
///
fn parse_snapshot(content: String) -> Snapshot {
  case json.parse(content, using: snapshot_decoder()) {
    Ok(snapshot) -> snapshot
    Error(_) -> Snapshot(tables: dict.new())
  }
}

/// The snapshot format is intentionally simple — just
/// `{"tables": {...}}` at the top level. If we ever need to
/// store metadata (schema version, generation timestamp),
/// there's room to add sibling keys without breaking existing
/// snapshots.
///
fn snapshot_decoder() -> decode.Decoder(Snapshot) {
  use tables <- decode.field(
    "tables",
    decode.dict(decode.string, table_decoder()),
  )
  decode.success(Snapshot(tables: tables))
}

/// Decodes a single table's snapshot. The `indexes` field is
/// optional so that snapshot files from before index support
/// was added still load correctly — they'll just have an empty
/// index list, and the diff will see all current indexes as new
/// additions.
///
fn table_decoder() -> decode.Decoder(TableSnapshot) {
  use columns <- decode.field("columns", decode.list(column_decoder()))
  use indexes <- decode.optional_field(
    "indexes",
    [],
    decode.list(index_decoder()),
  )
  decode.success(TableSnapshot(columns: columns, indexes: indexes))
}

/// Decodes an index from the snapshot JSON file. Mirrors the
/// structure written by indexes_to_json so a round-trip through
/// save/load produces identical data for the diff algorithm to
/// compare against.
///
fn index_decoder() -> decode.Decoder(IndexSnapshot) {
  use columns <- decode.field("columns", decode.list(decode.string))
  use is_unique <- decode.field("unique", decode.bool)
  use name <- decode.field("name", decode.optional(decode.string))
  decode.success(IndexSnapshot(columns: columns, unique: is_unique, name: name))
}

/// Decodes one column entry from the snapshot. The field names
/// here (`name`, `type`, `nullable`, `has_default`) must stay
/// in sync with `columns_to_json` — if either side changes, the
/// round-trip breaks and every column looks "altered" on the
/// next migration run.
///
fn column_decoder() -> decode.Decoder(ColumnSnapshot) {
  use name <- decode.field("name", decode.string)
  use column_type <- decode.field("type", decode.string)
  use nullable <- decode.field("nullable", decode.bool)
  use has_default <- decode.field("has_default", decode.bool)
  decode.success(ColumnSnapshot(
    name: name,
    column_type: column_type,
    nullable: nullable,
    has_default: has_default,
  ))
}

/// Hand-rolled JSON serialization instead of using a library
/// because we want exact control over formatting. The snapshot
/// file gets committed to version control, so consistent
/// indentation and key ordering prevents noisy diffs when
/// nothing actually changed.
///
fn snapshot_to_json(snapshot: Snapshot) -> String {
  let tables_json =
    snapshot.tables
    |> dict.to_list()
    |> list.map(fn(pair) {
      let #(name, table) = pair
      "    \""
      <> name
      <> "\": {\n      \"columns\": [\n"
      <> columns_to_json(table)
      <> "\n      ],\n      \"indexes\": [\n"
      <> indexes_to_json(table)
      <> "\n      ]\n    }"
    })
    |> string.join(",\n")

  "{\n  \"tables\": {\n" <> tables_json <> "\n  }\n}\n"
}

/// The write side of the snapshot round-trip. If a field name
/// here drifts from what `column_decoder` expects, every column
/// silently looks "changed" on the next run and you get a
/// migration full of no-op ALTER statements. Keep these two in
/// lockstep.
///
fn columns_to_json(table: TableSnapshot) -> String {
  table.columns
  |> list.map(fn(col) {
    "        {"
    <> "\"name\": \""
    <> col.name
    <> "\", "
    <> "\"type\": \""
    <> col.column_type
    <> "\", "
    <> "\"nullable\": "
    <> bool_to_json(col.nullable)
    <> ", "
    <> "\"has_default\": "
    <> bool_to_json(col.has_default)
    <> "}"
  })
  |> string.join(",\n")
}

/// Serializes a table's indexes to JSON for the snapshot file.
/// Each index becomes a JSON object with columns, unique, and
/// name fields — matching the structure that index_decoder
/// expects on the read side.
///
fn indexes_to_json(table: TableSnapshot) -> String {
  table.indexes
  |> list.map(fn(idx) {
    let name_json = case idx.name {
      Some(n) -> "\"" <> n <> "\""
      None -> "null"
    }
    let cols_json =
      idx.columns
      |> list.map(fn(c) { "\"" <> c <> "\"" })
      |> string.join(", ")
    "        {"
    <> "\"columns\": ["
    <> cols_json
    <> "], "
    <> "\"unique\": "
    <> bool_to_json(idx.unique)
    <> ", "
    <> "\"name\": "
    <> name_json
    <> "}"
  })
  |> string.join(",\n")
}

/// JSON requires lowercase `true`/`false` — Gleam's
/// `True`/`False` would be invalid JSON and silently break the
/// snapshot parser on the next load.
///
fn bool_to_json(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

// ------------------------------------------------------------- Validation Private Functions

/// `Array(Array(Int))` is fine — it's just nested lists. But
/// `Array(Array(Foreign("users")))` still has a FK at the leaf,
/// which is invalid no matter how deeply nested. This peels off
/// Array layers until it reaches the leaf type, then checks
/// whether that leaf is Id or Foreign.
///
fn validate_array_inner(
  table_name: String,
  col_name: String,
  col_type: ColumnType,
) -> Nil {
  case col_type {
    schema_parser.Array(schema_parser.Id) -> {
      let red = "\u{001b}[31m"
      let reset = "\u{001b}[0m"
      let error_msg =
        red
        <> "Error: Array(Id) is not allowed in table '"
        <> table_name
        <> "', column '"
        <> col_name
        <> "'. Auto-incrementing arrays are not supported."
        <> reset
      panic as error_msg
    }
    schema_parser.Array(schema_parser.Foreign(ref, _, _)) -> {
      let red = "\u{001b}[31m"
      let reset = "\u{001b}[0m"
      let error_msg =
        red
        <> "Error: Array(Foreign("
        <> ref
        <> ")) is not allowed in table '"
        <> table_name
        <> "', column '"
        <> col_name
        <> "'. Foreign key constraints cannot apply to array elements."
        <> reset
      panic as error_msg
    }
    schema_parser.Array(schema_parser.Enum(_, _)) -> {
      let red = "\u{001b}[31m"
      let reset = "\u{001b}[0m"
      let error_msg =
        red
        <> "Error: Array(Enum) is not allowed in table '"
        <> table_name
        <> "', column '"
        <> col_name
        <> "'. Enum arrays are not supported."
        <> reset
      panic as error_msg
    }
    schema_parser.Array(schema_parser.Blob) -> {
      let red = "\u{001b}[31m"
      let reset = "\u{001b}[0m"
      let error_msg =
        red
        <> "Error: Array(Blob) is not allowed in table '"
        <> table_name
        <> "', column '"
        <> col_name
        <> "'. Binary arrays are not supported."
        <> reset
      panic as error_msg
    }
    schema_parser.Array(inner) ->
      validate_array_inner(table_name, col_name, inner)
    _ -> Nil
  }
}

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

// ------------------------------------------------------------- Cleanup Private Functions

/// Removing the pipe is only half the job — if we leave
/// `rename_from` in the import list, `gleam build` will warn
/// about an unused import on every build until the developer
/// manually cleans it up. So after stripping the call sites we
/// also strip the import, keeping the schema file warning-free.
///
fn remove_rename_from_calls(content: String) -> String {
  // Split into lines and process each line
  let cleaned =
    content
    |> string.split("\n")
    |> list.map(fn(line) {
      // Check if line contains rename_from (either direct or via schema.)
      case
        string.contains(line, "|> rename_from(")
        || string.contains(line, "|> schema.rename_from(")
      {
        True -> remove_rename_from_from_line(line)
        False -> line
      }
    })
    |> string.join("\n")

  // Clean up the rename_from import if no longer used
  clean_rename_from_import(cleaned)
}

/// Schema files can use either the bare `rename_from()` or the
/// qualified `schema.rename_from()` depending on how the
/// developer imported the module. This tries the qualified form
/// first (more specific match), then falls back to the bare
/// form to catch both styles in one pass.
///
fn remove_rename_from_from_line(line: String) -> String {
  // Try both patterns: |> rename_from(...) and |> schema.rename_from(...)
  let result = case string.split_once(line, "|> schema.rename_from(") {
    Ok(#(before, after)) -> remove_rename_pattern(before, after)
    Error(_) -> {
      case string.split_once(line, "|> rename_from(") {
        Ok(#(before, after)) -> remove_rename_pattern(before, after)
        Error(_) -> line
      }
    }
  }
  result
}

/// Once we've split on the `|> rename_from(` boundary, we need
/// to find the closing paren and stitch the line back together
/// without leaving awkward double-spaces or missing separators.
/// The trailing content after the `)` might be empty, a comma,
/// or more pipe chain — each case needs slightly different
/// whitespace handling.
///
fn remove_rename_pattern(before: String, after: String) -> String {
  case find_closing_paren(after) {
    Ok(rest) -> {
      // Trim trailing whitespace from before
      let trimmed_before = string.trim_end(before)
      let trimmed_rest = string.trim_start(rest)
      // If rest starts with comma or is empty, don't add space
      case trimmed_rest {
        "" -> trimmed_before
        "," <> _ -> trimmed_before <> trimmed_rest
        _ -> trimmed_before <> " " <> trimmed_rest
      }
    }
    Error(_) -> before <> after
  }
}

/// Simple paren matching that grabs everything after the first
/// `)` in the string. This works because `rename_from` only
/// ever takes a string literal argument, so there are no nested
/// parens to worry about.
///
fn find_closing_paren(s: String) -> Result(String, Nil) {
  case string.split_once(s, ")") {
    Ok(#(_, rest)) -> Ok(rest)
    Error(_) -> Error(Nil)
  }
}

/// After removing all `rename_from` call sites, the import
/// would be left dangling and `gleam build` would warn about
/// it. This strips `rename_from` from import lists using string
/// replacement patterns that handle both single-line
/// `{rename_from, nullable}` and multi-line import formats
/// without needing a full parser.
///
fn clean_rename_from_import(content: String) -> String {
  let has_rename_from_usage =
    string.contains(content, "|> rename_from(")
    || string.contains(content, "|> schema.rename_from(")

  case has_rename_from_usage {
    True -> content
    False -> {
      // Remove rename_from from imports - handle both single-line and multi-line imports
      content
      // Multi-line: rename_from on its own line with trailing comma
      |> string.replace("  rename_from,\n", "")
      // Multi-line: rename_from on its own line (last item, no trailing comma)
      |> string.replace(",\n  rename_from\n", "\n")
      // Single-line patterns
      |> string.replace(", rename_from}", "}")
      |> string.replace("{rename_from, ", "{")
      |> string.replace(", rename_from,", ",")
      |> string.replace(", rename_from", "")
      |> string.replace("rename_from, ", "")
    }
  }
}
