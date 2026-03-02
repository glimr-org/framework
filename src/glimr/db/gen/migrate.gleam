//// Migration Generator
////
//// Writing migration SQL by hand after every schema change is
//// tedious and error-prone — developers forget columns, get
//// types wrong, or miss renames. This module compares the
//// current schema files against a stored snapshot to detect
//// changes automatically and emit driver-specific SQL. The
//// snapshot is updated after each run so only new changes
//// appear in subsequent migrations.

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import glimr/config/database
import glimr/console/console
import glimr/db/driver
import glimr/db/gen/migrate/cleanup
import glimr/db/gen/migrate/snapshot
import glimr/db/gen/migrate/sql.{Postgres, Sqlite}
import glimr/db/gen/migrate/validation
import glimr/db/gen/schema_parser.{type Table}
import shellout
import simplifile

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
  let connections = database.load()
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

  // Convert driver type to sql.Driver
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
    False -> {
      console.output()
      |> console.line_success("Generated migrations (" <> name <> ")")
      |> console.print()
    }
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
  drv: sql.Driver,
  driver_name: String,
  model_filter: Option(List(String)),
  is_filtered: Bool,
  verbose: Bool,
) {
  // Load existing snapshot
  let old_snapshot = snapshot.load(snapshot_path)

  // Ensure migrations directory exists
  let _ = simplifile.create_directory_all(migrations_path)

  // Scan current schemas (filtered if specified)
  case scan_schemas(models_path, model_filter) {
    Ok(tables) -> {
      // Validate schemas
      validation.validate_no_duplicate_columns(tables)
      validation.validate_indexes(tables)
      validation.validate_array_types(tables)
      validation.validate_enum_variants(tables)

      case verbose {
        True ->
          io.println(
            "  Found " <> int.to_string(list.length(tables)) <> " table(s)",
          )
        False -> Nil
      }

      // Build new snapshot (only for scanned tables)
      let new_snapshot = snapshot.build(tables)

      // Compute diff (skip drop detection when filtering by model)
      let diff =
        sql.compute_diff(old_snapshot, new_snapshot, tables, is_filtered)

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
                io.println("    - " <> sql.describe_change(change))
              })
            }
            False -> Nil
          }

          // Generate migration SQL for the configured driver only
          let timestamp = get_timestamp()
          let filename = timestamp <> "_migration.sql"
          let migration_sql = sql.generate_sql(diff, drv)

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
                True -> snapshot.merge(old_snapshot, new_snapshot)
                False -> new_snapshot
              }
              case snapshot.save(snapshot_path, final_snapshot) {
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
              cleanup.clean_rename_from_modifiers(models_path)
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
