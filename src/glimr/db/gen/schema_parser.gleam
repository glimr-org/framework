//// Schema Parser
////
//// Parses schema.gleam files to extract table definitions.
//// This parser handles the list-based schema definition
//// format:
////
//// table(name, [ id(), string("name"), string("bio") |>
//// nullable(), boolean("is_active") |>
//// default(DefaultBool(True)), timestamps(), ])

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

// ------------------------------------------------------------- Public Types

/// Represents a database table with a name and list of columns.
/// Parsed from schema.gleam files and used for code generation
/// and migration diffing.
///
pub type Table {
  Table(name: String, columns: List(Column), indexes: List(Index))
}

/// The parsed representation of an `index(["col"])` or
/// `unique(["col"]) |> named("custom")` call from a schema
/// file. The migration system compares these against the
/// previous snapshot to detect added or removed indexes.
///
pub type Index {
  Index(columns: List(String), unique: Bool, name: Option(String))
}

/// Represents a database column with its name, type,
/// nullability, default value, and optional rename tracking for
/// migration generation.
///
pub type Column {
  Column(
    name: String,
    column_type: ColumnType,
    nullable: Bool,
    default: Option(DefaultValue),
    renamed_from: Option(String),
  )
}

/// Represents the default value for a column. Supports boolean,
/// string, integer, float, current timestamp, current unix
/// timestamp, and null defaults.
///
pub type DefaultValue {
  DefaultBool(Bool)
  DefaultString(String)
  DefaultInt(Int)
  DefaultFloat(Float)
  DefaultNow
  DefaultUnixNow
  DefaultAutoUuid
  DefaultNull
}

/// Represents the data type of a column. Maps to appropriate
/// SQL types for each database driver (PostgreSQL and SQLite)
/// during code generation.
///
pub type ColumnType {
  Id
  String
  Text
  Int
  BigInt
  Float
  Boolean
  Timestamp
  UnixTimestamp
  Date
  Json
  Uuid
  Foreign(String)
}

// ------------------------------------------------------------- Public Functions

/// Parse a schema.gleam file content into a Table structure.
/// Extracts the table name from `pub const name = "..."` and
/// parses the column definitions from the `table(name, [...])`
/// call.
///
pub fn parse(content: String) -> Result(Table, String) {
  // Extract table name from `pub const name = "tablename"`
  case extract_table_name(content) {
    None -> Error("Could not find table name (pub const name = \"...\")")
    Some(table_name) -> {
      // Extract the list content from table(name, [...])
      case extract_column_list(content) {
        None -> Error("Could not find table column list")
        Some(list_content) -> {
          // Parse each column item in the list
          let columns = parse_column_list(list_content)
          let indexes = extract_indexes(content)
          Ok(Table(name: table_name, columns: columns, indexes: indexes))
        }
      }
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// Extract the table name from a schema file by looking for the
/// `pub const name = "tablename"` declaration at the module
/// level.
///
fn extract_table_name(content: String) -> Option(String) {
  // Look for: pub const name = "tablename"
  let lines = string.split(content, "\n")
  list.find_map(lines, fn(line) {
    let trimmed = string.trim(line)
    case string.starts_with(trimmed, "pub const name = \"") {
      True -> {
        // Extract the value between quotes
        let without_prefix = string.drop_start(trimmed, 18)
        case string.split(without_prefix, "\"") {
          [name, ..] -> Ok(name)
          _ -> Error(Nil)
        }
      }
      False -> Error(Nil)
    }
  })
  |> option.from_result()
}

/// Extract the column list content from `table(name, [...])`.
/// Returns the content inside the square brackets for further
/// parsing into individual column definitions.
///
fn extract_column_list(content: String) -> Option(String) {
  case string.split_once(content, "table(") {
    Ok(#(_, after_table)) -> {
      // Skip past the table name and comma to find the opening bracket
      case string.split_once(after_table, "[") {
        Ok(#(_, list_content)) -> {
          // Extract until the matching closing bracket
          Some(extract_until_balanced_bracket(list_content, 1, ""))
        }
        Error(_) -> None
      }
    }
    Error(_) -> None
  }
}

/// Recursively extract content until the matching closing
/// bracket is found, tracking bracket depth for nested
/// structures.
///
fn extract_until_balanced_bracket(s: String, depth: Int, acc: String) -> String {
  case depth <= 0 {
    True -> acc
    False -> {
      case string.pop_grapheme(s) {
        Ok(#("[", rest)) ->
          extract_until_balanced_bracket(rest, depth + 1, acc <> "[")
        Ok(#("]", rest)) -> extract_until_balanced_bracket(rest, depth - 1, acc)
        Ok(#(c, rest)) -> extract_until_balanced_bracket(rest, depth, acc <> c)
        Error(_) -> acc
      }
    }
  }
}

/// Parse the column list content into a list of Column structs.
/// Splits by top-level commas and parses each column definition
/// including modifiers like nullable and default values.
///
fn parse_column_list(list_content: String) -> List(Column) {
  let items = split_by_top_level_comma(list_content)
  items
  |> list.flat_map(parse_column_item)
}

/// Split a string by commas, but only at the top level (not
/// inside parentheses). Used to separate column definitions in
/// the schema list.
///
fn split_by_top_level_comma(content: String) -> List(String) {
  split_by_comma_helper(content, 0, "", [])
}

/// Recursive helper for splitting by top-level commas. Tracks
/// parenthesis depth to avoid splitting inside function calls
/// and nested expressions.
///
fn split_by_comma_helper(
  s: String,
  paren_depth: Int,
  current: String,
  acc: List(String),
) -> List(String) {
  case string.pop_grapheme(s) {
    Ok(#(",", rest)) -> {
      case paren_depth {
        0 -> {
          let trimmed = string.trim(current)
          case trimmed {
            "" -> split_by_comma_helper(rest, 0, "", acc)
            _ -> split_by_comma_helper(rest, 0, "", [trimmed, ..acc])
          }
        }
        _ -> split_by_comma_helper(rest, paren_depth, current <> ",", acc)
      }
    }
    Ok(#("(", rest)) ->
      split_by_comma_helper(rest, paren_depth + 1, current <> "(", acc)
    Ok(#(")", rest)) ->
      split_by_comma_helper(rest, paren_depth - 1, current <> ")", acc)
    Ok(#(c, rest)) ->
      split_by_comma_helper(rest, paren_depth, current <> c, acc)
    Error(_) -> {
      let trimmed = string.trim(current)
      case trimmed {
        "" -> list.reverse(acc)
        _ -> list.reverse([trimmed, ..acc])
      }
    }
  }
}

/// Parse a single column item string into Column structs.
/// Handles special cases like timestamps() which expand to
/// multiple columns, and extracts modifiers like nullable().
///
fn parse_column_item(item: String) -> List(Column) {
  let trimmed = string.trim(item)
  case string.starts_with(trimmed, "timestamps()") {
    True -> [
      Column("created_at", Timestamp, False, None, None),
      Column("updated_at", Timestamp, False, None, None),
    ]
    False ->
      case string.starts_with(trimmed, "unix_timestamps()") {
        True -> [
          Column("created_at", UnixTimestamp, False, None, None),
          Column("updated_at", UnixTimestamp, False, None, None),
        ]
        False -> {
          // Check if this item has modifiers (|> nullable(), |> default(...), |> rename_from(...))
          let #(base, is_nullable, default_value, renamed_from) =
            extract_modifiers(trimmed)

          // Parse the base column function
          case parse_column_function(base) {
            Some(col) -> [
              Column(
                ..col,
                nullable: is_nullable,
                default: default_value,
                renamed_from: renamed_from,
              ),
            ]
            None -> []
          }
        }
      }
  }
}

/// Extract modifiers from a column definition. Returns the base
/// column function, whether it's nullable, any default value,
/// and any rename_from directive.
///
fn extract_modifiers(
  item: String,
) -> #(String, Bool, Option(DefaultValue), Option(String)) {
  let parts = string.split(item, "|>")

  let base = case list.first(parts) {
    Ok(b) -> string.trim(b)
    Error(_) -> item
  }

  let is_nullable =
    list.any(parts, fn(p) { string.contains(string.trim(p), "nullable()") })

  // Extract default value
  let default_value =
    list.find_map(parts, fn(p) {
      let trimmed = string.trim(p)
      parse_default_value(trimmed)
    })
    |> option.from_result()

  // Extract rename_from value
  let renamed_from =
    list.find_map(parts, fn(p) {
      let trimmed = string.trim(p)
      parse_rename_from(trimmed)
    })
    |> option.from_result()

  #(base, is_nullable, default_value, renamed_from)
}

/// Parse a rename_from modifier and extract the old column
/// name. Handles both `rename_from("old")` and
/// `schema.rename_from("old")`.
///
fn parse_rename_from(s: String) -> Result(String, Nil) {
  case
    string.starts_with(s, "rename_from(")
    || string.starts_with(s, "schema.rename_from(")
  {
    True -> extract_quoted_string(s)
    False -> Error(Nil)
  }
}

/// Extract the first double-quoted string from the input.
/// Returns the content between the first pair of double quotes,
/// used for parsing column names and string arguments.
///
fn extract_quoted_string(s: String) -> Result(String, Nil) {
  case string.split_once(s, "\"") {
    Ok(#(_, after)) -> {
      case string.split_once(after, "\"") {
        Ok(#(value, _)) -> Ok(value)
        Error(_) -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}

/// Extract the content between the first pair of parentheses.
/// Returns the trimmed content inside the parentheses, used for
/// parsing function arguments like default values.
///
fn extract_parens_content(s: String) -> Result(String, Nil) {
  case string.split_once(s, "(") {
    Ok(#(_, after)) -> {
      case string.split_once(after, ")") {
        Ok(#(content, _)) -> Ok(string.trim(content))
        Error(_) -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}

/// Parse a default value modifier and extract the default. Uses
/// a lookup table to match prefix and delegate to the
/// appropriate extractor function.
///
fn parse_default_value(s: String) -> Result(DefaultValue, Nil) {
  let handlers = [
    #("default_bool(", extract_bool_default),
    #("default_string(", extract_string_default),
    #("default_int(", extract_int_default),
    #("default_float(", extract_float_default),
    #("default_now(", fn(_) { Ok(DefaultNow) }),
    #("default_unix_now(", fn(_) { Ok(DefaultUnixNow) }),
    #("default_null(", fn(_) { Ok(DefaultNull) }),
    #("auto_uuid(", fn(_) { Ok(DefaultAutoUuid) }),
  ]

  list.find_map(handlers, fn(handler) {
    let #(prefix, parse_fn) = handler
    case string.starts_with(s, prefix) {
      True -> Ok(parse_fn(s))
      False -> Error(Nil)
    }
  })
  |> result.flatten()
}

/// Extract a boolean default value by checking for "True" in
/// the string. Returns DefaultBool(True) if found, otherwise
/// DefaultBool(False).
///
fn extract_bool_default(s: String) -> Result(DefaultValue, Nil) {
  case string.contains(s, "True") {
    True -> Ok(DefaultBool(True))
    False -> Ok(DefaultBool(False))
  }
}

/// Extract a string default value from the quoted argument.
/// Parses the content between double quotes and wraps it in
/// DefaultString.
///
fn extract_string_default(s: String) -> Result(DefaultValue, Nil) {
  extract_quoted_string(s)
  |> result.map(DefaultString)
}

/// Extract an integer default value from the parentheses.
/// Parses the numeric string inside the parentheses and wraps
/// it in DefaultInt.
///
fn extract_int_default(s: String) -> Result(DefaultValue, Nil) {
  case extract_parens_content(s) {
    Ok(num_str) ->
      case int.parse(num_str) {
        Ok(n) -> Ok(DefaultInt(n))
        Error(_) -> Error(Nil)
      }
    Error(_) -> Error(Nil)
  }
}

/// Extract a float default value from the parentheses. Parses
/// the numeric string inside the parentheses and wraps it in
/// DefaultFloat.
///
fn extract_float_default(s: String) -> Result(DefaultValue, Nil) {
  case extract_parens_content(s) {
    Ok(num_str) ->
      case float.parse(num_str) {
        Ok(f) -> Ok(DefaultFloat(f))
        Error(_) -> Error(Nil)
      }
    Error(_) -> Error(Nil)
  }
}

/// Parse a column function call like `string("name")` into a
/// Column struct. Uses a lookup table for standard column types
/// and handles special cases for id() and foreign().
///
fn parse_column_function(func: String) -> Option(Column) {
  let trimmed = string.trim(func)
  case string.starts_with(trimmed, "id()") {
    True -> Some(Column("id", Id, False, None, None))
    False -> {
      // Handle foreign() specially since it has two arguments
      case string.starts_with(trimmed, "foreign(") {
        True -> parse_foreign_column(trimmed)
        False -> {
          // All other column types follow the same pattern: type("name")
          let column_types = [
            #("string_sized(", String),
            #("string(", String),
            #("text(", Text),
            #("int(", Int),
            #("bigint(", BigInt),
            #("float(", Float),
            #("boolean(", Boolean),
            #("timestamp(", Timestamp),
            #("unix_timestamp(", UnixTimestamp),
            #("date(", Date),
            #("json(", Json),
            #("uuid(", Uuid),
          ]

          list.find_map(column_types, fn(entry) {
            let #(prefix, col_type) = entry
            case string.starts_with(trimmed, prefix) {
              True ->
                case parse_named_column(trimmed, col_type) {
                  Some(col) -> Ok(col)
                  None -> Error(Nil)
                }
              False -> Error(Nil)
            }
          })
          |> option.from_result()
        }
      }
    }
  }
}

/// Parse a column function with a name argument like
/// `string("name")` into a Column struct with the given type
/// and default nullability/default values.
///
fn parse_named_column(func: String, col_type: ColumnType) -> Option(Column) {
  case extract_quoted_string(func) {
    Ok(name) -> Some(Column(name, col_type, False, None, None))
    Error(_) -> None
  }
}

/// Parse a foreign key column from `foreign("column_name",
/// "table")`. Extracts both the column name and the referenced
/// table.
///
fn parse_foreign_column(func: String) -> Option(Column) {
  let parts = string.split(func, "\"")
  case parts {
    [_, name, _, ref, ..] -> Some(Column(name, Foreign(ref), False, None, None))
    _ -> None
  }
}

/// Get columns in definition order from a table. Returns the
/// list of columns as defined in the schema file, preserving
/// their original order.
///
pub fn columns(table: Table) -> List(Column) {
  table.columns
}

/// Returns the parsed index definitions from a table. The
/// migration snapshot builder uses this to capture the current
/// index state for diffing against future schema changes.
///
pub fn indexes(table: Table) -> List(Index) {
  table.indexes
}

/// Scans the schema file content for an `indexes([...])` block
/// and parses each entry inside it. This runs after column
/// parsing since indexes reference column names. If there's no
/// indexes block, the table simply has no indexes — it's not an
/// error because most tables start without them.
///
fn extract_indexes(content: String) -> List(Index) {
  case string.split_once(content, "indexes([") {
    Ok(#(_, after_indexes)) -> {
      let list_content = extract_until_balanced_bracket(after_indexes, 1, "")
      let items = split_by_top_level_comma(list_content)
      list.filter_map(items, fn(item) {
        case parse_index_item(string.trim(item)) {
          Some(idx) -> Ok(idx)
          None -> Error(Nil)
        }
      })
    }
    Error(_) -> []
  }
}

/// Parses a single index entry like `unique(["email"])` or
/// `index(["a", "b"]) |> named("custom")`. The pipe chain
/// handling is important because `named()` is a modifier that
/// the developer pipes onto the base index call — just like how
/// column definitions use pipe chains for `nullable()` and
/// `default()`.
///
fn parse_index_item(item: String) -> Option(Index) {
  let parts = string.split(item, "|>")
  let base = case list.first(parts) {
    Ok(b) -> string.trim(b)
    Error(_) -> item
  }

  // Parse the base: unique([...]) or index([...])
  let base_result = case string.starts_with(base, "unique(") {
    True -> {
      case extract_index_columns(base) {
        Some(cols) -> Some(Index(columns: cols, unique: True, name: None))
        None -> None
      }
    }
    False ->
      case string.starts_with(base, "index(") {
        True -> {
          case extract_index_columns(base) {
            Some(cols) -> Some(Index(columns: cols, unique: False, name: None))
            None -> None
          }
        }
        False -> None
      }
  }

  // Apply named() modifier if present
  case base_result {
    Some(idx) -> {
      let custom_name =
        list.find_map(parts, fn(p) {
          let trimmed = string.trim(p)
          case string.starts_with(trimmed, "named(") {
            True -> extract_quoted_string(trimmed)
            False -> Error(Nil)
          }
        })
        |> option.from_result()
      Some(Index(..idx, name: custom_name))
    }
    None -> None
  }
}

/// Extracts the column name list from inside an index function
/// call like `unique(["email", "password"])`. Uses the same
/// balanced-bracket and comma-splitting helpers as the column
/// parser to handle nested brackets and multi- column indexes
/// correctly.
///
fn extract_index_columns(func: String) -> Option(List(String)) {
  case string.split_once(func, "[") {
    Ok(#(_, after_bracket)) -> {
      let content = extract_until_balanced_bracket(after_bracket, 1, "")
      let cols =
        split_by_top_level_comma(content)
        |> list.filter_map(fn(item) {
          let trimmed = string.trim(item)
          extract_quoted_string(trimmed)
        })
      case cols {
        [] -> None
        _ -> Some(cols)
      }
    }
    Error(_) -> None
  }
}
