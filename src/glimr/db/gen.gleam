//// Database Code Generator
////
//// Generates typed Gleam code from schema definitions and SQL
//// query files. Scans `src/data/{conn_name}/models/` for model
//// directories, reads schema definitions and SQL queries, and
//// generates repository modules with type-safe database functions.
////
//// Each model directory should contain:
//// - `{model}_schema.gleam` - Schema definition with table/columns
//// - `queries/` - Directory containing `.sql` query files
////
//// Generated code is written to `{model}/gen/{model}.gleam`
//// and automatically formatted with `gleam format`.

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import glimr/console/console
import glimr/db/gen/generator
import glimr/db/gen/parser
import glimr/db/gen/schema_parser
import shellout
import simplifile

// ------------------------------------------------------------- Public Functions

/// Runs code generation for a named database connection.
/// Scans src/data/{name}/models/ for model directories and
/// generates typed repository modules from schemas and SQL.
///
pub fn run(
  name: String,
  driver_type: String,
  model_filter: Option(List(String)),
) {
  io.println("")
  io.println(console.warning("Glimr Query Generator"))
  io.println("  Connection: " <> name)
  io.println("  Driver: " <> driver_type)

  case model_filter {
    Some(models) -> io.println("  Models: " <> string.join(models, ", "))
    None -> Nil
  }

  let models_path = "src/data/" <> name <> "/models"
  generate_models(models_path, model_filter, driver_type)
}

// ------------------------------------------------------------- Private Functions

/// Internal implementation for code generation. Reads model
/// directories, filters by the optional model list, and
/// processes each model to generate repository code.
///
fn generate_models(
  models_path: String,
  model_filter: Option(List(String)),
  driver_type: String,
) {
  case simplifile.read_directory(models_path) {
    Ok(entries) -> {
      let model_dirs =
        list.filter(entries, fn(entry) {
          case simplifile.is_directory(models_path <> "/" <> entry) {
            Ok(True) -> {
              case model_filter {
                None -> True
                Some(allowed) -> list.contains(allowed, entry)
              }
            }
            _ -> False
          }
        })

      io.println(
        "  Found " <> int.to_string(list.length(model_dirs)) <> " model(s)",
      )

      list.each(model_dirs, fn(model_name) {
        io.println("")
        io.println("  Processing: " <> model_name)
        process_model(models_path, model_name, driver_type)
      })

      io.println("")
      io.println(console.success("  Successfully generated queries!"))
    }
    Error(_) -> {
      io.println("  Error: Could not read " <> models_path)
      io.println("  Make sure you're running from your project root.")
    }
  }
}

/// Processes a single model directory. Reads the schema file,
/// parses all SQL query files, generates the repository module,
/// writes it to the gen/ directory, and formats it.
///
fn process_model(
  models_path: String,
  model_name: String,
  driver_type: String,
) -> Nil {
  let model_path = models_path <> "/" <> model_name
  let schema_path = model_path <> "/" <> model_name <> "_schema.gleam"
  let queries_path = model_path <> "/queries"
  let gen_path = model_path <> "/gen"

  case simplifile.read(schema_path) {
    Ok(schema_content) -> {
      case schema_parser.parse(schema_content) {
        Ok(table) -> {
          io.println(
            "    Schema: "
            <> table.name
            <> " ("
            <> int.to_string(list.length(table.columns))
            <> " columns)",
          )

          let queries = case simplifile.read_directory(queries_path) {
            Ok(files) -> {
              list.filter_map(files, fn(file) {
                case string.ends_with(file, ".sql") {
                  True -> {
                    let query_path = queries_path <> "/" <> file
                    case simplifile.read(query_path) {
                      Ok(sql) -> {
                        let query_name = string.replace(file, ".sql", "")
                        case parser.parse_sql(sql) {
                          Ok(parsed) -> Ok(#(query_name, sql, parsed))
                          Error(_) -> {
                            io.println("    Warning: Could not parse " <> file)
                            Error(Nil)
                          }
                        }
                      }
                      Error(_) -> Error(Nil)
                    }
                  }
                  False -> Error(Nil)
                }
              })
            }
            Error(_) -> []
          }

          io.println("    Queries: " <> int.to_string(list.length(queries)))

          let generated =
            generator.generate(model_name, table, queries, driver_type)

          let _ = simplifile.create_directory_all(gen_path)

          let output_path = gen_path <> "/" <> model_name <> ".gleam"
          case simplifile.write(output_path, generated) {
            Ok(_) -> {
              let _ =
                shellout.command("gleam", ["format", output_path], ".", [])
              io.println("    Generated: " <> output_path)
            }
            Error(_) -> io.println("    Error: Could not write " <> output_path)
          }
        }
        Error(err) -> {
          io.println("    Error parsing schema: " <> err)
        }
      }
    }
    Error(_) -> {
      io.println("    Error: Could not read " <> schema_path)
    }
  }
}
