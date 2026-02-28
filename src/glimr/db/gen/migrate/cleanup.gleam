//// Migration Cleanup
////
//// After a migration is generated, `rename_from` modifiers in
//// schema files have done their job — they told the differ to
//// emit RENAME COLUMN instead of drop+add. If left in place
//// they'd fire again on the next run and generate a duplicate
//// rename that fails at migration time. This module strips
//// them out automatically so developers don't have to remember
//// to clean up after themselves.

import gleam/io
import gleam/list
import gleam/string
import shellout
import simplifile

// ------------------------------------------------------------- Public Functions

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
