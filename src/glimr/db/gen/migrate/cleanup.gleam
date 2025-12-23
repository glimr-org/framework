//// ------------------------------------------------------------
//// Migration Cleanup
//// ------------------------------------------------------------
////
//// Handles automatic cleanup of rename_from modifiers from
//// schema files after a migration is generated. This ensures
//// that rename_from directives are only used once and don't
//// accumulate in schema files.
////

import gleam/io
import gleam/list
import gleam/string
import shellout
import simplifile

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Clean Rename From Modifiers
/// ------------------------------------------------------------
///
/// Remove rename_from modifiers from schema files after
/// migration is generated.
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

/// ------------------------------------------------------------
/// Remove Rename From Calls
/// ------------------------------------------------------------
///
/// Remove |> rename_from("...") patterns from schema content.
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

/// ------------------------------------------------------------
/// Remove Rename From From Line
/// ------------------------------------------------------------
///
/// Remove rename_from call from a single line of code.
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

/// ------------------------------------------------------------
/// Remove Rename Pattern
/// ------------------------------------------------------------
///
/// Remove the rename_from pattern and handle spacing.
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

/// ------------------------------------------------------------
/// Find Closing Paren
/// ------------------------------------------------------------
///
/// Find everything after the closing paren of rename_from(...).
///
fn find_closing_paren(s: String) -> Result(String, Nil) {
  case string.split_once(s, ")") {
    Ok(#(_, rest)) -> Ok(rest)
    Error(_) -> Error(Nil)
  }
}

/// ------------------------------------------------------------
/// Clean Rename From Import
/// ------------------------------------------------------------
///
/// Remove rename_from from imports if no longer used in the
/// file.
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
