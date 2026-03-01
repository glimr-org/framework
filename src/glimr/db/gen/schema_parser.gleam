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
  DefaultEmptyArray
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
  SmallInt
  BigInt
  Float
  Boolean
  Timestamp
  UnixTimestamp
  Date
  Json
  Uuid
  Foreign(
    table: String,
    on_delete: Option(ForeignAction),
    on_update: Option(ForeignAction),
  )
  Array(ColumnType)
  Enum(name: String, variants: List(String))
  Decimal(precision: Int, scale: Int)
  Blob
  Time
}

/// What should the database do when a referenced row is deleted
/// or updated? Without specifying this, most databases default
/// to RESTRICT (block the operation), which is safe but rigid.
/// Cascade deletes child rows automatically, SetNull clears the
/// FK, and NoAction defers the check to transaction commit
/// time.
///
pub type ForeignAction {
  Cascade
  Restrict
  SetNull
  SetDefault
  NoAction
}

// ------------------------------------------------------------- Public Functions

/// Parse a schema.gleam file content into a Table structure.
/// Extracts the table name from `pub const table_name = "..."`
/// and parses the column definitions from the
/// `table(table_name, [...])` call.
///
pub fn parse(content: String) -> Result(Table, String) {
  // Extract table name from `pub const name = "tablename"`
  case extract_table_name(content) {
    None -> Error("Could not find table name (pub const table_name = \"...\")")
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

/// Extract the table name from a schema file by looking for
/// `pub const table_name = "tablename"` (preferred) or the
/// legacy `pub const name = "tablename"` declaration.
///
fn extract_table_name(content: String) -> Option(String) {
  // Look for: pub const table_name = "tablename"
  // Also supports legacy: pub const name = "tablename"
  let lines = string.split(content, "\n")
  list.find_map(lines, fn(line) {
    let trimmed = string.trim(line)
    case string.starts_with(trimmed, "pub const table_name = \"") {
      True -> {
        let without_prefix = string.drop_start(trimmed, 24)
        case string.split(without_prefix, "\"") {
          [name, ..] -> Ok(name)
          _ -> Error(Nil)
        }
      }
      False ->
        case string.starts_with(trimmed, "pub const name = \"") {
          True -> {
            let without_prefix = string.drop_start(trimmed, 18)
            case string.split(without_prefix, "\"") {
              [name, ..] -> Ok(name)
              _ -> Error(Nil)
            }
          }
          False -> Error(Nil)
        }
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
        False ->
          case string.starts_with(trimmed, "soft_deletes()") {
            True -> [Column("deleted_at", Timestamp, True, None, None)]
            False -> {
              // Check if this item has modifiers (|> nullable(), |> default(...), |> rename_from(...), |> array())
              let mods = extract_modifiers(trimmed)

              // Parse the base column function
              case parse_column_function(mods.base) {
                Some(col) -> {
                  let col_type =
                    wrap_array_type(col.column_type, mods.array_depth)
                  // Apply FK actions if this is a Foreign column
                  let col_type = case col_type {
                    Foreign(ref, _, _) ->
                      Foreign(ref, mods.on_delete, mods.on_update)
                    _ -> col_type
                  }
                  // Apply enum name override if this is an Enum column
                  let col_type = case col_type, mods.enum_name_override {
                    Enum(_, variants), Some(override_name) ->
                      Enum(override_name, variants)
                    _, _ -> col_type
                  }
                  [
                    Column(
                      ..col,
                      column_type: col_type,
                      nullable: mods.nullable,
                      default: mods.default,
                      renamed_from: mods.renamed_from,
                    ),
                  ]
                }
                None -> []
              }
            }
          }
      }
  }
}

/// Developers can chain `|> array() |> array()` for nested
/// arrays, so the parser counts how many times `array()`
/// appears and wraps the base type that many levels deep.
/// `string("matrix") |> array() |> array()` becomes
/// `Array(Array(String))` which codegen turns into
/// `List(List(String))`.
///
fn wrap_array_type(col_type: ColumnType, depth: Int) -> ColumnType {
  case depth <= 0 {
    True -> col_type
    False -> wrap_array_type(Array(col_type), depth - 1)
  }
}

/// Column definitions in schema files are pipe chains like
/// `string("email") |> nullable() |> array()`. Rather than
/// threading all these flags through individual parse
/// functions, we extract everything into one record up front.
/// This keeps `parse_column_item` clean — it just reads the
/// modifiers and applies them to the base column.
///
pub type Modifiers {
  Modifiers(
    base: String,
    nullable: Bool,
    default: Option(DefaultValue),
    renamed_from: Option(String),
    array_depth: Int,
    on_delete: Option(ForeignAction),
    on_update: Option(ForeignAction),
    enum_name_override: Option(String),
  )
}

/// Splits a pipe chain like `string("name") |> nullable() |>
/// on_delete(Cascade)` on `|>` and scans each segment for known
/// modifiers. The base column call (first segment) gets parsed
/// separately by `parse_column_function` — this only cares
/// about the modifier segments after it.
///
fn extract_modifiers(item: String) -> Modifiers {
  let parts = string.split(item, "|>")

  let base = case list.first(parts) {
    Ok(b) -> string.trim(b)
    Error(_) -> item
  }

  let is_nullable =
    list.any(parts, fn(p) { string.contains(string.trim(p), "nullable()") })

  // Count array() modifiers
  let array_depth = list.count(parts, fn(p) { string.trim(p) == "array()" })

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

  // Extract on_delete action
  let on_delete_action =
    list.find_map(parts, fn(p) {
      let trimmed = string.trim(p)
      parse_foreign_action(trimmed, "on_delete(")
    })
    |> option.from_result()

  // Extract on_update action
  let on_update_action =
    list.find_map(parts, fn(p) {
      let trimmed = string.trim(p)
      parse_foreign_action(trimmed, "on_update(")
    })
    |> option.from_result()

  // Extract enum_name override
  let enum_name_override =
    list.find_map(parts, fn(p) {
      let trimmed = string.trim(p)
      parse_enum_name(trimmed)
    })
    |> option.from_result()

  Modifiers(
    base: base,
    nullable: is_nullable,
    default: default_value,
    renamed_from: renamed_from,
    array_depth: array_depth,
    on_delete: on_delete_action,
    on_update: on_update_action,
    enum_name_override: enum_name_override,
  )
}

/// Developers can write either `rename_from("old")` or
/// `schema.rename_from("old")` depending on their import style.
/// Both produce the same result — the old column name that the
/// migration system uses to emit RENAME COLUMN instead of
/// drop+add.
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

/// The schema DSL uses Gleam string syntax everywhere — column
/// names, table references, default values. This is the shared
/// workhorse that pulls the first double-quoted value out of
/// any expression, used by nearly every other parser function.
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

/// For modifier functions that take non-string arguments like
/// `default_int(42)` or `on_delete(Cascade)`, we need the raw
/// content inside the parens rather than a quoted string. This
/// grabs that content and trims it.
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

/// Default values have wildly different syntax —
/// `default_bool(True)` vs `default_string("hello")` vs
/// `default_now()`. A lookup table keeps this manageable
/// instead of a giant if-else chain, and adding a new default
/// type is just one more entry in the list.
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
    #("default_empty_array(", fn(_) { Ok(DefaultEmptyArray) }),
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

/// The schema DSL uses Gleam's `True`/`False` literals, so
/// `default_bool(True)` contains the string "True". We can't
/// parse it as actual Gleam — we're just doing string matching
/// on the source text — so checking for the presence of "True"
/// is the simplest reliable test.
///
fn extract_bool_default(s: String) -> Result(DefaultValue, Nil) {
  case string.contains(s, "True") {
    True -> Ok(DefaultBool(True))
    False -> Ok(DefaultBool(False))
  }
}

/// String defaults like `default_string("active")` embed the
/// value as a Gleam string literal in the schema source. We
/// just need to pull it out of the quotes — the SQL generator
/// handles escaping later when it writes the DEFAULT clause.
///
fn extract_string_default(s: String) -> Result(DefaultValue, Nil) {
  extract_quoted_string(s)
  |> result.map(DefaultString)
}

/// `default_int(0)` passes the literal `0` as Gleam source
/// text. We pull it from the parens and parse it as an actual
/// Int. If parsing fails (malformed input), returning
/// Error(Nil) means the default is silently ignored rather than
/// crashing the parser.
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

/// Same pattern as `extract_int_default` but for float literals
/// like `default_float(0.0)`. Gleam's float parser handles the
/// conversion; we just provide the raw string from inside the
/// parens.
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

/// Most column types follow the same `type("name")` pattern,
/// but a few are special: `id()` has no name argument,
/// `foreign()` takes two arguments, `enum()` takes a name and a
/// variant list, and `decimal()` takes precision and scale.
/// This dispatches to specialized parsers for those and uses a
/// lookup table for the rest to avoid repeating the same
/// extraction logic 15 times.
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
          // Handle enum() specially since it has a name and a list of variants
          case
            string.starts_with(trimmed, "enum(")
            || string.starts_with(trimmed, "schema.enum(")
          {
            True -> parse_enum_column(trimmed)
            False -> {
              // Handle decimal() specially since it has three arguments
              case
                string.starts_with(trimmed, "decimal(")
                || string.starts_with(trimmed, "schema.decimal(")
              {
                True -> parse_decimal_column(trimmed)
                False -> {
                  // All other column types follow the same pattern: type("name")
                  let column_types = [
                    #("string_sized(", String),
                    #("string(", String),
                    #("text(", Text),
                    #("int(", Int),
                    #("smallint(", SmallInt),
                    #("bigint(", BigInt),
                    #("float(", Float),
                    #("boolean(", Boolean),
                    #("timestamp(", Timestamp),
                    #("unix_timestamp(", UnixTimestamp),
                    #("date(", Date),
                    #("json(", Json),
                    #("uuid(", Uuid),
                    #("blob(", Blob),
                    #("time(", Time),
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
      }
    }
  }
}

/// The common case for 15+ column types: `string("email")`,
/// `int("age")`, `boolean("active")`, etc. They all take a
/// single quoted name argument and return a non-nullable column
/// with no default. Modifiers like nullable() and default() get
/// applied later by `parse_column_item`.
///
fn parse_named_column(func: String, col_type: ColumnType) -> Option(Column) {
  case extract_quoted_string(func) {
    Ok(name) -> Some(Column(name, col_type, False, None, None))
    Error(_) -> None
  }
}

/// Foreign keys need special parsing because they take two
/// quoted arguments — the column name and the referenced table.
/// `foreign("user_id", "users")` produces a column named
/// `user_id` with type `Foreign("users", None, None)`. The FK
/// actions (on_delete, on_update) get applied later from the
/// modifiers.
///
fn parse_foreign_column(func: String) -> Option(Column) {
  let parts = string.split(func, "\"")
  case parts {
    [_, name, _, ref, ..] ->
      Some(Column(name, Foreign(ref, None, None), False, None, None))
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

/// Enums have the most complex syntax of any column type:
/// `enum("status", ["active", "inactive"])` nests a list inside
/// the function call. The column name defaults to the enum type
/// name too (used for the Postgres CREATE TYPE), though `|>
/// enum_name("custom")` can override it later.
///
fn parse_enum_column(func: String) -> Option(Column) {
  // Extract the column name (first quoted string)
  case extract_quoted_string(func) {
    Ok(name) -> {
      // Extract the variant list from [...] inside the function call
      case string.split_once(func, "[") {
        Ok(#(_, after_bracket)) -> {
          let content = extract_until_balanced_bracket(after_bracket, 1, "")
          let variants =
            split_by_top_level_comma(content)
            |> list.filter_map(fn(item) {
              let trimmed = string.trim(item)
              extract_quoted_string(trimmed)
            })
          Some(Column(name, Enum(name, variants), False, None, None))
        }
        Error(_) -> None
      }
    }
    Error(_) -> None
  }
}

/// Decimals take three arguments: name, precision, and scale.
/// `decimal("price", 10, 2)` means 10 total digits with 2 after
/// the decimal point. We parse the integers from the trailing
/// arguments after extracting the quoted name.
///
fn parse_decimal_column(func: String) -> Option(Column) {
  case extract_quoted_string(func) {
    Ok(name) -> {
      // Get content after the closing quote of the name
      case string.split_once(func, "\"" <> name <> "\"") {
        Ok(#(_, after_name)) -> {
          // Parse the remaining args: , 10, 2)
          let args =
            after_name
            |> string.replace(")", "")
            |> string.split(",")
            |> list.filter_map(fn(s) { int.parse(string.trim(s)) })
          case args {
            [precision, scale] ->
              Some(Column(name, Decimal(precision, scale), False, None, None))
            _ -> None
          }
        }
        Error(_) -> None
      }
    }
    Error(_) -> None
  }
}

/// FK actions can be written as `on_delete(Cascade)` or
/// `on_delete(schema.Cascade)` depending on how the developer
/// imported the module. The prefix parameter lets us reuse this
/// for both `on_delete` and `on_update` without duplicating the
/// variant matching logic.
///
fn parse_foreign_action(s: String, prefix: String) -> Result(ForeignAction, Nil) {
  let has_prefix =
    string.starts_with(s, prefix) || string.starts_with(s, "schema." <> prefix)
  case has_prefix {
    True -> {
      // Extract the action name from inside the parens
      case extract_parens_content(s) {
        Ok(content) -> {
          // Handle both "Cascade" and "schema.Cascade"
          let action_str = case string.split_once(content, ".") {
            Ok(#(_, after_dot)) -> string.trim(after_dot)
            Error(_) -> string.trim(content)
          }
          case action_str {
            "Cascade" -> Ok(Cascade)
            "Restrict" -> Ok(Restrict)
            "SetNull" -> Ok(SetNull)
            "SetDefault" -> Ok(SetDefault)
            "NoAction" -> Ok(NoAction)
            _ -> Error(Nil)
          }
        }
        Error(_) -> Error(Nil)
      }
    }
    False -> Error(Nil)
  }
}

/// By default, the Postgres enum type name is derived from the
/// column name. But sometimes you want multiple columns sharing
/// one enum type, or a name that doesn't match the column. `|>
/// enum_name("payment_status")` overrides the auto-generated
/// name.
///
fn parse_enum_name(s: String) -> Result(String, Nil) {
  case
    string.starts_with(s, "enum_name(")
    || string.starts_with(s, "schema.enum_name(")
  {
    True -> extract_quoted_string(s)
    False -> Error(Nil)
  }
}
