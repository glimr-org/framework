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
