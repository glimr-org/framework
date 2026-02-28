//// Schema DSL
////
//// A fluent builder for defining database table schemas in
//// Gleam. Schemas serve as the source of truth for code
//// generation, migration generation, and type inference.

import gleam/list
import gleam/option.{type Option, None, Some}

// ------------------------------------------------------------- Public Types

/// Represents a database table definition with its name and
/// column definitions. Used as the source of truth for code
/// generation, migrations, and type inference.
///
pub type Table {
  Table(name: String, columns: List(Column), indexes: List(IndexDef))
}

/// Database queries that filter or sort by a column benefit
/// hugely from an index — without one, the database scans every
/// row. This type captures what the developer wants indexed,
/// whether it should enforce uniqueness, and an optional custom
/// name (otherwise the migration generates
/// `idx_{table}_{col1}_{col2}` automatically).
///
pub type IndexDef {
  IndexDef(columns: List(String), unique: Bool, name: Option(String))
}

/// Represents a column definition within a table. Each column
/// has a name, type, nullability flag, and optional default
/// value.
///
pub type Column {
  Column(
    name: String,
    column_type: ColumnType,
    nullable: Bool,
    default: Option(Default),
    renamed_from: Option(String),
  )
}

/// Wrapper for column definitions that allows single columns or
/// multiple columns (like timestamps) to be used in the same
/// list.
///
pub type ColumnDef {
  Single(Column)
  Multiple(List(Column))
}

/// Defines the available column types that map to both
/// PostgreSQL and SQLite data types. The codegen tool uses
/// these to generate appropriate Gleam types and SQL DDL
/// statements.
///
pub type ColumnType {
  /// Auto-incrementing integer primary key
  Id
  /// Variable-length string with optional max length (VARCHAR)
  String(max: Option(Int))
  /// Unlimited text (TEXT)
  Text
  /// Standard integer (INT/INTEGER)
  Int
  /// Large integer (BIGINT)
  BigInt
  /// Floating point number (REAL/DOUBLE PRECISION)
  Float
  /// Boolean value (BOOLEAN/INTEGER for SQLite)
  Boolean
  /// Timestamp with timezone (TIMESTAMP/TEXT for SQLite)
  Timestamp
  /// Unix timestamp as integer seconds (INTEGER)
  UnixTimestamp
  /// Date without time (DATE/TEXT for SQLite)
  Date
  /// JSON data (JSONB for Postgres, TEXT for SQLite)
  Json
  /// UUID (UUID for Postgres, TEXT for SQLite)
  Uuid
  /// Foreign key reference to another table
  Foreign(table: String)
  /// Array of another column type (PostgreSQL native arrays, JSON
  /// in SQLite)
  Array(ColumnType)
}

/// Defines default values that can be assigned to columns. Used
/// in migration generation to produce appropriate SQL DEFAULT
/// clauses.
///
pub type Default {
  DefaultString(String)
  DefaultInt(Int)
  DefaultFloat(Float)
  DefaultBool(Bool)
  DefaultNow
  DefaultUnixNow
  DefaultAutoUuid
  DefaultNull
  DefaultEmptyArray
}

// ------------------------------------------------------------- Public Functions

/// Creates a new table definition with the given name and
/// column definitions.
///
/// *Example:*
///
/// ```gleam
/// import glimr/db/schema.{table, id, string, timestamps}
///
/// pub const name = "users"
///
/// pub fn definition() {
///   table(name, [
///     id(),
///     string("name"),
///     string("email"),
///     timestamps(),
///   ])
/// }
/// ```
///
pub fn table(name: String, column_defs: List(ColumnDef)) -> Table {
  let cols =
    column_defs
    |> list.flat_map(fn(def) {
      case def {
        Single(col) -> [col]
        Multiple(cols) -> cols
      }
    })
  Table(name: name, columns: cols, indexes: [])
}

/// Creates an auto-incrementing integer primary key column
/// named "id". This is typically the first column in a table.
///
/// Maps to:
/// - PostgreSQL: `id SERIAL PRIMARY KEY`
/// - SQLite: `id INTEGER PRIMARY KEY AUTOINCREMENT`
///
pub fn id() -> ColumnDef {
  Single(Column("id", Id, False, None, None))
}

/// Creates a VARCHAR(255) column with the given name.
///
/// Maps to:
/// - PostgreSQL: `name VARCHAR(255)`
/// - SQLite: `name TEXT`
///
pub fn string(name: String) -> ColumnDef {
  Single(Column(name, String(Some(255)), False, None, None))
}

/// Creates a VARCHAR column with a specific maximum length.
///
/// Maps to:
/// - PostgreSQL: `name VARCHAR(max)`
/// - SQLite: `name TEXT`
///
pub fn string_sized(name: String, max: Int) -> ColumnDef {
  Single(Column(name, String(Some(max)), False, None, None))
}

/// Creates an unlimited text column. Use for large text content
/// like blog posts, descriptions, etc.
///
/// Maps to:
/// - PostgreSQL: `name TEXT`
/// - SQLite: `name TEXT`
///
pub fn text(name: String) -> ColumnDef {
  Single(Column(name, Text, False, None, None))
}

/// Creates a standard integer column.
///
/// Maps to:
/// - PostgreSQL: `name INTEGER`
/// - SQLite: `name INTEGER`
///
pub fn int(name: String) -> ColumnDef {
  Single(Column(name, Int, False, None, None))
}

/// Creates a large integer column for values exceeding standard
/// integer range.
///
/// Maps to:
/// - PostgreSQL: `name BIGINT`
/// - SQLite: `name INTEGER`
///
pub fn bigint(name: String) -> ColumnDef {
  Single(Column(name, BigInt, False, None, None))
}

/// Creates a floating-point number column.
///
/// Maps to:
/// - PostgreSQL: `name DOUBLE PRECISION`
/// - SQLite: `name REAL`
///
pub fn float(name: String) -> ColumnDef {
  Single(Column(name, Float, False, None, None))
}

/// Creates a boolean column.
///
/// Maps to:
/// - PostgreSQL: `name BOOLEAN`
/// - SQLite: `name INTEGER` (0 = false, 1 = true)
///
pub fn boolean(name: String) -> ColumnDef {
  Single(Column(name, Boolean, False, None, None))
}

/// Creates a timestamp column for date/time values.
///
/// Maps to:
/// - PostgreSQL: `name TIMESTAMP WITH TIME ZONE`
/// - SQLite: `name TEXT` (ISO 8601 format)
///
pub fn timestamp(name: String) -> ColumnDef {
  Single(Column(name, Timestamp, False, None, None))
}

/// Creates an integer column for storing Unix timestamps
/// (seconds since epoch).
///
/// Maps to:
/// - PostgreSQL: `name BIGINT`
/// - SQLite: `name INTEGER`
///
pub fn unix_timestamp(name: String) -> ColumnDef {
  Single(Column(name, UnixTimestamp, False, None, None))
}

/// Creates a date column (without time component).
///
/// Maps to:
/// - PostgreSQL: `name DATE`
/// - SQLite: `name TEXT` (YYYY-MM-DD format)
///
pub fn date(name: String) -> ColumnDef {
  Single(Column(name, Date, False, None, None))
}

/// Creates a JSON column for structured data.
///
/// Maps to:
/// - PostgreSQL: `name JSONB`
/// - SQLite: `name TEXT`
///
pub fn json(name: String) -> ColumnDef {
  Single(Column(name, Json, False, None, None))
}

/// Creates a UUID column for universally unique identifiers.
///
/// Maps to:
/// - PostgreSQL: `name UUID`
/// - SQLite: `name TEXT`
///
pub fn uuid(name: String) -> ColumnDef {
  Single(Column(name, Uuid, False, None, None))
}

/// Creates an integer column that references another table's
/// id. The column name should follow the convention
/// `{table}_id`.
///
/// Maps to:
/// - PostgreSQL: `name INTEGER REFERENCES table(id)`
/// - SQLite: `name INTEGER REFERENCES table(id)`
///
/// *Example:*
///
/// ```gleam
/// table("posts", [
///   id(),
///   foreign("user_id", "users"),
///   string("title"),
/// ])
/// ```
///
pub fn foreign(name: String, references: String) -> ColumnDef {
  Single(Column(name, Foreign(references), False, None, None))
}

/// Creates both `created_at` and `updated_at` timestamp
/// columns. This is a convenience function for the common
/// pattern of tracking record creation and modification times.
///
pub fn timestamps() -> ColumnDef {
  Multiple([
    Column("created_at", Timestamp, False, None, None),
    Column("updated_at", Timestamp, False, None, None),
  ])
}

/// Creates both `created_at` and `updated_at` as unix timestamp
/// columns (integer seconds since epoch). This is a convenience
/// function for the common pattern of tracking record creation
/// and modification times using integer timestamps.
///
pub fn unix_timestamps() -> ColumnDef {
  Multiple([
    Column("created_at", UnixTimestamp, False, None, None),
    Column("updated_at", UnixTimestamp, False, None, None),
  ])
}

/// Marks the column as nullable (allows NULL). By default,
/// columns are NOT NULL.
///
/// *Example:*
///
/// ```gleam
/// table("users", [
///   id(),
///   string("name"),
///   string("bio")
///     |> nullable(),
/// ])
/// ```
///
pub fn nullable(def: ColumnDef) -> ColumnDef {
  case def {
    Single(col) -> Single(Column(..col, nullable: True))
    Multiple(cols) ->
      Multiple(list.map(cols, fn(col) { Column(..col, nullable: True) }))
  }
}

/// Sets a boolean default value for the column.
///
/// *Example:*
///
/// ```gleam
/// table("users", [
///   boolean("is_active")
///     |> default_bool(True),
/// ])
/// ```
///
pub fn default_bool(def: ColumnDef, value: Bool) -> ColumnDef {
  set_default(def, DefaultBool(value))
}

/// Sets a string default value for the column.
///
/// *Example:*
///
/// ```gleam
/// table("users", [
///   string("role")
///     |> default_string("user"),
/// ])
/// ```
///
pub fn default_string(def: ColumnDef, value: String) -> ColumnDef {
  set_default(def, DefaultString(value))
}

/// Sets an integer default value for the column.
///
/// *Example:*
///
/// ```gleam
/// table("posts", [
///   int("view_count")
///     |> default_int(0),
/// ])
/// ```
///
pub fn default_int(def: ColumnDef, value: Int) -> ColumnDef {
  set_default(def, DefaultInt(value))
}

/// Sets a float default value for the column.
///
/// *Example:*
///
/// ```gleam
/// table("products", [
///   float("price")
///     |> default_float(0.0),
/// ])
/// ```
///
pub fn default_float(def: ColumnDef, value: Float) -> ColumnDef {
  set_default(def, DefaultFloat(value))
}

/// Sets the default value to the current timestamp.
///
/// *Example:*
///
/// ```gleam
/// table("posts", [
///   timestamp("published_at")
///     |> default_now(),
/// ])
/// ```
///
pub fn default_now(def: ColumnDef) -> ColumnDef {
  set_default(def, DefaultNow)
}

/// Sets the default value to the current Unix timestamp
/// (seconds since epoch). Use with `unix_timestamp` columns.
///
/// *Example:*
///
/// ```gleam
/// table("events", [
///   unix_timestamp("created_at")
///     |> default_unix_now(),
/// ])
/// ```
///
pub fn default_unix_now(def: ColumnDef) -> ColumnDef {
  set_default(def, DefaultUnixNow)
}

/// Sets the default value to an auto-generated UUID. Use with
/// `uuid` columns for automatic unique identifier generation.
///
/// Maps to:
/// - PostgreSQL: `gen_random_uuid()`
/// - SQLite: Custom expression generating UUID v4 format
///
/// *Example:*
///
/// ```gleam
/// table("users", [
///   uuid("external_id")
///     |> auto_uuid(),
/// ])
/// ```
///
pub fn auto_uuid(def: ColumnDef) -> ColumnDef {
  set_default(def, DefaultAutoUuid)
}

/// Sets the default value to NULL. Use with nullable columns to
/// explicitly set NULL as the default rather than having no
/// default value specified.
///
pub fn default_null(def: ColumnDef) -> ColumnDef {
  set_default(def, DefaultNull)
}

/// Sometimes you need a column that holds multiple values —
/// tags, scores, email addresses. This wraps any column type in
/// an Array so `string("tags") |> array()` becomes
/// `List(String)` in Gleam, `TEXT[]` in Postgres, and JSON text
/// in SQLite. Chaining `|> array() |> array()` gives you nested
/// arrays for matrix-style data.
///
pub fn array(def: ColumnDef) -> ColumnDef {
  case def {
    Single(col) -> Single(Column(..col, column_type: Array(col.column_type)))
    Multiple(cols) ->
      Multiple(
        list.map(cols, fn(col) {
          Column(..col, column_type: Array(col.column_type))
        }),
      )
  }
}

/// Without a default, inserting a row without specifying the
/// array column would violate NOT NULL. This sets the default
/// to an empty array (`'{}'` in Postgres, `'[]'` in SQLite) so
/// new rows get a valid empty list instead of failing.
///
pub fn default_empty_array(def: ColumnDef) -> ColumnDef {
  set_default(def, DefaultEmptyArray)
}

/// Indicates that this column was renamed from a previous name.
/// The migration generator will use `RENAME COLUMN` instead of
/// drop/add, preserving data. This modifier is automatically
/// removed from the schema after the migration is generated.
///
/// *Example:*
///
/// ```gleam
/// table("users", [
///   string("email_address")
///     |> rename_from("email"),
/// ])
/// ```
///
pub fn rename_from(def: ColumnDef, old_name: String) -> ColumnDef {
  case def {
    Single(col) -> Single(Column(..col, renamed_from: Some(old_name)))
    Multiple(_) -> def
  }
}

/// Returns the table's columns in definition order. Useful for
/// iterating over columns when generating code or performing
/// schema introspection.
///
pub fn columns(t: Table) -> List(Column) {
  t.columns
}

/// Pipes onto `table()` to declare indexes alongside columns.
/// Keeping indexes in the schema definition means the migration
/// generator can detect when you add or remove an index and
/// produce the right CREATE INDEX / DROP INDEX SQL
/// automatically — no hand-written migrations needed.
///
/// *Example:*
///
/// ```gleam
/// table(name, [id(), string("email")])
/// |> indexes([unique(["email"])])
/// ```
///
pub fn indexes(table: Table, defs: List(IndexDef)) -> Table {
  Table(..table, indexes: defs)
}

/// Creates a regular (non-unique) index on the given columns.
/// Use this when you frequently query by a column but don't
/// need uniqueness enforced — for example, indexing a `status`
/// column that your list queries filter on.
///
pub fn index(columns: List(String)) -> IndexDef {
  IndexDef(columns: columns, unique: False, name: None)
}

/// Creates a unique index — the database will reject any INSERT
/// or UPDATE that would create a duplicate. This is how you
/// enforce "one account per email" or "one vote per user per
/// post" at the database level rather than relying on
/// application-level checks that can race.
///
pub fn unique(columns: List(String)) -> IndexDef {
  IndexDef(columns: columns, unique: True, name: None)
}

/// Overrides the auto-generated index name. The default
/// `idx_{table}_{col1}_{col2}` is fine most of the time, but if
/// you need a specific name for constraint error handling or
/// migration compatibility with an existing database, pipe
/// through this.
///
pub fn named(def: IndexDef, name: String) -> IndexDef {
  IndexDef(..def, name: Some(name))
}

/// Returns the table's index definitions. The migration
/// generator and snapshot system use this to detect when
/// indexes have been added, removed, or changed between schema
/// versions.
///
pub fn table_indexes(t: Table) -> List(IndexDef) {
  t.indexes
}

// ------------------------------------------------------------- Private Functions

/// Helper to set default value on a ColumnDef. Handles both
/// single columns and multiple columns (like timestamps),
/// applying the default to all columns in the definition.
///
fn set_default(def: ColumnDef, value: Default) -> ColumnDef {
  case def {
    Single(col) -> Single(Column(..col, default: Some(value)))
    Multiple(cols) ->
      Multiple(list.map(cols, fn(col) { Column(..col, default: Some(value)) }))
  }
}
