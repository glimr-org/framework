//// Database Code Generator
////
//// Querying a database in Gleam requires writing types,
//// decoders, and wrapper functions for every model — all of
//// which must stay in sync with the schema and SQL files. This
//// module automates that by scanning model directories,
//// parsing schemas and .sql files, and emitting fully-typed
//// repository modules so schema changes only require re-
//// running the generator.

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

/// Pass a connection name and optionally a list of specific
/// models to regenerate. Without a filter, every model in the
/// connection gets processed — the filter exists so you can
/// regenerate just the model you're working on without waiting
/// for all of them.
///
pub fn run(name: String, model_filter: Option(List(String)), verbose: Bool) {
  case verbose {
    True -> {
      io.println(console.warning("Glimr Query Generator"))
      io.println("  Connection: " <> name)
    }
    False -> Nil
  }

  case verbose, model_filter {
    True, Some(models) -> io.println("  Models: " <> string.join(models, ", "))
    _, _ -> Nil
  }

  let models_path = "src/database/" <> name <> "/models"
  generate_models(models_path, model_filter, verbose)

  case verbose {
    True -> Nil
    False -> {
      console.output()
      |> console.line_success("Generated queries (" <> name <> ")")
      |> console.print()
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// Each model is processed independently so a broken schema in
/// one model doesn't prevent the others from regenerating.
/// You'll still see the error for the broken one, but the rest
/// of your queries stay up to date.
///
fn generate_models(
  models_path: String,
  model_filter: Option(List(String)),
  verbose: Bool,
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

      case verbose {
        True ->
          io.println(
            "  Found " <> int.to_string(list.length(model_dirs)) <> " model(s)",
          )
        False -> Nil
      }

      list.each(model_dirs, fn(model_name) {
        case verbose {
          True -> {
            io.println("")
            io.println("  Processing: " <> model_name)
          }
          False -> Nil
        }
        process_model(models_path, model_name, verbose)
      })

      case verbose {
        True -> {
          io.println("")
          io.println(console.success("  Successfully generated queries!"))
        }
        False -> Nil
      }
    }
    Error(_) -> {
      io.println("  Error: Could not read " <> models_path)
      io.println("  Make sure you're running from your project root.")
    }
  }
}

/// Each model is self-contained: its schema and queries live in
/// the same directory. Processing one at a time means a broken
/// schema or unparseable SQL file produces a clear error for
/// that model without affecting others. The output is
/// auto-formatted so generated code matches project style.
///
fn process_model(models_path: String, model_name: String, verbose: Bool) -> Nil {
  let model_path = models_path <> "/" <> model_name
  let schema_path = model_path <> "/" <> model_name <> "_schema.gleam"
  let queries_path = model_path <> "/queries"
  let gen_path = model_path <> "/gen"

  case simplifile.read(schema_path) {
    Ok(schema_content) -> {
      case schema_parser.parse(schema_content) {
        Ok(table) -> {
          case verbose {
            True ->
              io.println(
                "    Schema: "
                <> table.name
                <> " ("
                <> int.to_string(list.length(table.columns))
                <> " columns)",
              )
            False -> Nil
          }

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

          case verbose {
            True ->
              io.println("    Queries: " <> int.to_string(list.length(queries)))
            False -> Nil
          }

          let generated = generator.generate(model_name, table, queries)

          let _ = simplifile.create_directory_all(gen_path)

          let output_path = gen_path <> "/" <> model_name <> ".gleam"
          case simplifile.write(output_path, generated) {
            Ok(_) -> {
              let _ =
                shellout.command("gleam", ["format", output_path], ".", [])
              case verbose {
                True -> io.println("    Generated: " <> output_path)
                False -> Nil
              }
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
