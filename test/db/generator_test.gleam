import gleam/option.{None}
import gleam/string
import gleeunit/should
import glimr/db/gen/generator
import glimr/db/gen/schema_parser.{Column, Table}

// ------------------------------------------------------------- Array Type Generation

pub fn generate_model_type_with_array_column_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "tags",
          schema_parser.Array(schema_parser.String),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("post", table, [])

  // Check model type has List(String) for tags
  result
  |> string.contains("tags: List(String)")
  |> should.be_true()
}

pub fn generate_model_type_with_nested_array_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "matrix",
          schema_parser.Array(schema_parser.Array(schema_parser.Int)),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("post", table, [])

  result
  |> string.contains("matrix: List(List(Int))")
  |> should.be_true()
}

pub fn generate_model_type_with_nullable_array_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "tags",
          schema_parser.Array(schema_parser.String),
          True,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("post", table, [])

  result
  |> string.contains("tags: Option(List(String))")
  |> should.be_true()
}

// ------------------------------------------------------------- Array Decoder Generation

pub fn generate_decoder_with_array_column_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "tags",
          schema_parser.Array(schema_parser.String),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("post", table, [])

  // Row decoder should use glimr_decode.list_of(decode.string)
  result
  |> string.contains("glimr_decode.list_of(decode.string)")
  |> should.be_true()
}

pub fn generate_decoder_with_nested_array_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "matrix",
          schema_parser.Array(schema_parser.Array(schema_parser.Int)),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("post", table, [])

  result
  |> string.contains("glimr_decode.list_of(glimr_decode.list_of(decode.int))")
  |> should.be_true()
}

pub fn generate_decoder_with_nullable_array_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "tags",
          schema_parser.Array(schema_parser.String),
          True,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("post", table, [])

  result
  |> string.contains("decode.optional(glimr_decode.list_of(decode.string))")
  |> should.be_true()
}

// ------------------------------------------------------------- Array Encoder Generation

pub fn generate_encoder_with_array_column_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "tags",
          schema_parser.Array(schema_parser.String),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("post", table, [])

  // Encoder should use json.array
  result
  |> string.contains("json.array(model.tags, json.string)")
  |> should.be_true()
}

pub fn generate_encoder_with_nested_array_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "matrix",
          schema_parser.Array(schema_parser.Array(schema_parser.Int)),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("post", table, [])

  result
  |> string.contains(
    "json.array(model.matrix, fn(v) { json.array(v, json.int) })",
  )
  |> should.be_true()
}

pub fn generate_encoder_with_nullable_array_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "tags",
          schema_parser.Array(schema_parser.String),
          True,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("post", table, [])

  // Nullable array should use json.nullable with a closure
  result
  |> string.contains(
    "json.nullable(model.tags, fn(v) { json.array(v, json.string) })",
  )
  |> should.be_true()
}

// ------------------------------------------------------------- Array Imports

pub fn generate_imports_with_array_includes_glimr_decode_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "tags",
          schema_parser.Array(schema_parser.String),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("post", table, [])

  // Should import glimr_decode for the list_of decoder
  result
  |> string.contains("import glimr/db/decode as glimr_decode")
  |> should.be_true()
}

pub fn generate_imports_without_array_no_glimr_decode_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("title", schema_parser.String, False, None, None),
      ],
      indexes: [],
    )

  let result = generator.generate("post", table, [])

  // No array or boolean columns, so no glimr_decode import
  result
  |> string.contains("glimr_decode")
  |> should.be_false()
}

// ------------------------------------------------------------- Mixed Table

pub fn generate_full_table_with_arrays_test() {
  let table =
    Table(
      name: "posts",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("title", schema_parser.String, False, None, None),
        Column(
          "tags",
          schema_parser.Array(schema_parser.String),
          False,
          None,
          None,
        ),
        Column(
          "scores",
          schema_parser.Array(schema_parser.Array(schema_parser.Int)),
          False,
          None,
          None,
        ),
        Column(
          "metadata",
          schema_parser.Array(schema_parser.Float),
          True,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("post", table, [])

  // Type definitions
  result |> string.contains("id: Int") |> should.be_true()
  result |> string.contains("title: String") |> should.be_true()
  result |> string.contains("tags: List(String)") |> should.be_true()
  result |> string.contains("scores: List(List(Int))") |> should.be_true()
  result |> string.contains("metadata: Option(List(Float))") |> should.be_true()

  // Decoders
  result
  |> string.contains("glimr_decode.list_of(decode.string)")
  |> should.be_true()
  result
  |> string.contains("glimr_decode.list_of(glimr_decode.list_of(decode.int))")
  |> should.be_true()
  result
  |> string.contains("decode.optional(glimr_decode.list_of(decode.float))")
  |> should.be_true()

  // Encoders
  result
  |> string.contains("json.array(model.tags, json.string)")
  |> should.be_true()
  result
  |> string.contains(
    "json.array(model.scores, fn(v) { json.array(v, json.int) })",
  )
  |> should.be_true()
  result
  |> string.contains(
    "json.nullable(model.metadata, fn(v) { json.array(v, json.float) })",
  )
  |> should.be_true()
}

// ------------------------------------------------------------- Enum Type Generation

pub fn generate_model_type_with_enum_column_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "status",
          schema_parser.Enum("status", ["active", "inactive", "banned"]),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("user", table, [])

  // Model type should use the custom type name
  result
  |> string.contains("status: Status")
  |> should.be_true()
}

pub fn generate_enum_type_definition_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "status",
          schema_parser.Enum("status", ["active", "inactive", "banned"]),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("user", table, [])

  // Should generate the custom type
  result
  |> string.contains("pub type Status {")
  |> should.be_true()

  result
  |> string.contains("Active")
  |> should.be_true()

  result
  |> string.contains("Inactive")
  |> should.be_true()

  result
  |> string.contains("Banned")
  |> should.be_true()
}

pub fn generate_enum_to_string_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "status",
          schema_parser.Enum("status", ["active", "inactive"]),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("user", table, [])

  result
  |> string.contains("pub fn status_to_string(value: Status) -> String {")
  |> should.be_true()

  result
  |> string.contains("Active -> \"active\"")
  |> should.be_true()
}

pub fn generate_enum_from_string_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "status",
          schema_parser.Enum("status", ["active", "inactive"]),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("user", table, [])

  result
  |> string.contains(
    "pub fn status_from_string(value: String) -> Result(Status, Nil) {",
  )
  |> should.be_true()

  result
  |> string.contains("\"active\" -> Ok(Active)")
  |> should.be_true()

  result
  |> string.contains("_ -> Error(Nil)")
  |> should.be_true()
}

pub fn generate_enum_multi_word_variants_test() {
  let table =
    Table(
      name: "tasks",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "status",
          schema_parser.Enum("status", ["in_progress", "not_started"]),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("task", table, [])

  result
  |> string.contains("InProgress")
  |> should.be_true()

  result
  |> string.contains("NotStarted")
  |> should.be_true()
}

pub fn generate_encoder_with_enum_column_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "status",
          schema_parser.Enum("status", ["active", "inactive"]),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("user", table, [])

  result
  |> string.contains("json.string(status_to_string(model.status))")
  |> should.be_true()
}

pub fn generate_decoder_with_enum_column_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "status",
          schema_parser.Enum("status", ["active", "inactive"]),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("user", table, [])

  // Should decode as string then map through from_string
  result
  |> string.contains("status_from_string(status)")
  |> should.be_true()
}

// ------------------------------------------------------------- Decimal Type Generation

pub fn generate_model_type_with_decimal_column_test() {
  let table =
    Table(
      name: "products",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("price", schema_parser.Decimal(10, 2), False, None, None),
      ],
      indexes: [],
    )

  let result = generator.generate("product", table, [])

  result
  |> string.contains("price: String")
  |> should.be_true()
}

// ------------------------------------------------------------- Blob Type Generation

pub fn generate_model_type_with_blob_column_test() {
  let table =
    Table(
      name: "files",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("data", schema_parser.Blob, False, None, None),
      ],
      indexes: [],
    )

  let result = generator.generate("file", table, [])

  result
  |> string.contains("data: BitArray")
  |> should.be_true()
}

pub fn generate_encoder_with_blob_column_test() {
  let table =
    Table(
      name: "files",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("data", schema_parser.Blob, False, None, None),
      ],
      indexes: [],
    )

  let result = generator.generate("file", table, [])

  result
  |> string.contains("bit_array.base64_encode(model.data, True)")
  |> should.be_true()
}

pub fn generate_imports_with_blob_includes_bit_array_test() {
  let table =
    Table(
      name: "files",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("data", schema_parser.Blob, False, None, None),
      ],
      indexes: [],
    )

  let result = generator.generate("file", table, [])

  result
  |> string.contains("import gleam/bit_array")
  |> should.be_true()
}

pub fn generate_imports_with_enum_includes_result_test() {
  let table =
    Table(
      name: "users",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column(
          "status",
          schema_parser.Enum("status", ["active"]),
          False,
          None,
          None,
        ),
      ],
      indexes: [],
    )

  let result = generator.generate("user", table, [])

  result
  |> string.contains("import gleam/result")
  |> should.be_true()
}

// ------------------------------------------------------------- Time Type Generation

pub fn generate_model_type_with_time_column_test() {
  let table =
    Table(
      name: "events",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("starts_at", schema_parser.Time, False, None, None),
      ],
      indexes: [],
    )

  let result = generator.generate("event", table, [])

  result
  |> string.contains("starts_at: String")
  |> should.be_true()
}
