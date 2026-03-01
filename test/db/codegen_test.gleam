import gleam/option
import gleeunit/should
import glimr/db/gen/schema_parser
import glimr/db/gen/schema_parser/codegen

// ------------------------------------------------------------- gleam_type

pub fn gleam_type_array_of_string_test() {
  codegen.gleam_type(schema_parser.Array(schema_parser.String))
  |> should.equal("List(String)")
}

pub fn gleam_type_array_of_int_test() {
  codegen.gleam_type(schema_parser.Array(schema_parser.Int))
  |> should.equal("List(Int)")
}

pub fn gleam_type_array_of_float_test() {
  codegen.gleam_type(schema_parser.Array(schema_parser.Float))
  |> should.equal("List(Float)")
}

pub fn gleam_type_array_of_bool_test() {
  codegen.gleam_type(schema_parser.Array(schema_parser.Boolean))
  |> should.equal("List(Bool)")
}

pub fn gleam_type_nested_array_test() {
  codegen.gleam_type(
    schema_parser.Array(schema_parser.Array(schema_parser.Int)),
  )
  |> should.equal("List(List(Int))")
}

pub fn gleam_type_triple_nested_array_test() {
  codegen.gleam_type(
    schema_parser.Array(
      schema_parser.Array(schema_parser.Array(schema_parser.String)),
    ),
  )
  |> should.equal("List(List(List(String)))")
}

pub fn gleam_type_array_of_uuid_test() {
  codegen.gleam_type(schema_parser.Array(schema_parser.Uuid))
  |> should.equal("List(String)")
}

pub fn gleam_type_array_of_timestamp_test() {
  codegen.gleam_type(schema_parser.Array(schema_parser.Timestamp))
  |> should.equal("List(String)")
}

pub fn gleam_type_array_of_bigint_test() {
  codegen.gleam_type(schema_parser.Array(schema_parser.BigInt))
  |> should.equal("List(Int)")
}

// ------------------------------------------------------------- decoder_fn

pub fn decoder_fn_array_of_string_test() {
  codegen.decoder_fn(schema_parser.Array(schema_parser.String))
  |> should.equal("glimr_decode.list_of(decode.string)")
}

pub fn decoder_fn_array_of_int_test() {
  codegen.decoder_fn(schema_parser.Array(schema_parser.Int))
  |> should.equal("glimr_decode.list_of(decode.int)")
}

pub fn decoder_fn_array_of_float_test() {
  codegen.decoder_fn(schema_parser.Array(schema_parser.Float))
  |> should.equal("glimr_decode.list_of(decode.float)")
}

pub fn decoder_fn_array_of_bool_test() {
  codegen.decoder_fn(schema_parser.Array(schema_parser.Boolean))
  |> should.equal("glimr_decode.list_of(glimr_decode.bool())")
}

pub fn decoder_fn_nested_array_test() {
  codegen.decoder_fn(
    schema_parser.Array(schema_parser.Array(schema_parser.Int)),
  )
  |> should.equal("glimr_decode.list_of(glimr_decode.list_of(decode.int))")
}

pub fn decoder_fn_triple_nested_array_test() {
  codegen.decoder_fn(
    schema_parser.Array(
      schema_parser.Array(schema_parser.Array(schema_parser.String)),
    ),
  )
  |> should.equal(
    "glimr_decode.list_of(glimr_decode.list_of(glimr_decode.list_of(decode.string)))",
  )
}

// ------------------------------------------------------------- json_encoder_expr

pub fn json_encoder_expr_array_of_string_test() {
  codegen.json_encoder_expr(
    schema_parser.Array(schema_parser.String),
    "model.tags",
  )
  |> should.equal("json.array(model.tags, json.string)")
}

pub fn json_encoder_expr_array_of_int_test() {
  codegen.json_encoder_expr(
    schema_parser.Array(schema_parser.Int),
    "model.scores",
  )
  |> should.equal("json.array(model.scores, json.int)")
}

pub fn json_encoder_expr_array_of_float_test() {
  codegen.json_encoder_expr(
    schema_parser.Array(schema_parser.Float),
    "model.weights",
  )
  |> should.equal("json.array(model.weights, json.float)")
}

pub fn json_encoder_expr_array_of_bool_test() {
  codegen.json_encoder_expr(
    schema_parser.Array(schema_parser.Boolean),
    "model.flags",
  )
  |> should.equal("json.array(model.flags, json.bool)")
}

pub fn json_encoder_expr_nested_array_test() {
  codegen.json_encoder_expr(
    schema_parser.Array(schema_parser.Array(schema_parser.Int)),
    "model.matrix",
  )
  |> should.equal("json.array(model.matrix, fn(v) { json.array(v, json.int) })")
}

pub fn json_encoder_expr_triple_nested_array_test() {
  codegen.json_encoder_expr(
    schema_parser.Array(
      schema_parser.Array(schema_parser.Array(schema_parser.String)),
    ),
    "model.cube",
  )
  |> should.equal(
    "json.array(model.cube, fn(v) { json.array(v, fn(v) { json.array(v, json.string) }) })",
  )
}

pub fn json_encoder_expr_scalar_passthrough_test() {
  codegen.json_encoder_expr(schema_parser.String, "model.name")
  |> should.equal("json.string(model.name)")
}

pub fn json_encoder_expr_int_passthrough_test() {
  codegen.json_encoder_expr(schema_parser.Int, "model.age")
  |> should.equal("json.int(model.age)")
}

// ------------------------------------------------------------- is_array

pub fn is_array_true_test() {
  codegen.is_array(schema_parser.Array(schema_parser.String))
  |> should.be_true()
}

pub fn is_array_nested_true_test() {
  codegen.is_array(schema_parser.Array(schema_parser.Array(schema_parser.Int)))
  |> should.be_true()
}

pub fn is_array_false_string_test() {
  codegen.is_array(schema_parser.String)
  |> should.be_false()
}

pub fn is_array_false_int_test() {
  codegen.is_array(schema_parser.Int)
  |> should.be_false()
}

pub fn is_array_false_foreign_test() {
  codegen.is_array(schema_parser.Foreign("users", option.None, option.None))
  |> should.be_false()
}

// ------------------------------------------------------------- New Type gleam_type

pub fn gleam_type_enum_test() {
  codegen.gleam_type(schema_parser.Enum("status", ["active", "inactive"]))
  |> should.equal("Status")
}

pub fn gleam_type_decimal_test() {
  codegen.gleam_type(schema_parser.Decimal(10, 2))
  |> should.equal("String")
}

pub fn gleam_type_blob_test() {
  codegen.gleam_type(schema_parser.Blob)
  |> should.equal("BitArray")
}

pub fn gleam_type_time_test() {
  codegen.gleam_type(schema_parser.Time)
  |> should.equal("String")
}

// ------------------------------------------------------------- New Type decoder_fn

pub fn decoder_fn_enum_test() {
  codegen.decoder_fn(schema_parser.Enum("status", ["active"]))
  |> should.equal("decode.string")
}

pub fn decoder_fn_decimal_test() {
  codegen.decoder_fn(schema_parser.Decimal(10, 2))
  |> should.equal("decode.string")
}

pub fn decoder_fn_blob_test() {
  codegen.decoder_fn(schema_parser.Blob)
  |> should.equal("decode.bit_array")
}

pub fn decoder_fn_time_test() {
  codegen.decoder_fn(schema_parser.Time)
  |> should.equal("decode.string")
}

// ------------------------------------------------------------- New Type json_encoder

pub fn json_encoder_fn_decimal_test() {
  codegen.json_encoder_fn(schema_parser.Decimal(10, 2))
  |> should.equal("json.string")
}

pub fn json_encoder_fn_time_test() {
  codegen.json_encoder_fn(schema_parser.Time)
  |> should.equal("json.string")
}

pub fn json_encoder_expr_blob_test() {
  codegen.json_encoder_expr(schema_parser.Blob, "model.data")
  |> should.equal("json.string(bit_array.base64_encode(model.data, True))")
}

pub fn json_encoder_expr_enum_test() {
  codegen.json_encoder_expr(
    schema_parser.Enum("status", ["active", "inactive"]),
    "model.status",
  )
  |> should.equal("json.string(status_to_string(model.status))")
}

// ------------------------------------------------------------- is_blob / is_enum

pub fn is_blob_true_test() {
  codegen.is_blob(schema_parser.Blob)
  |> should.be_true()
}

pub fn is_blob_false_test() {
  codegen.is_blob(schema_parser.String)
  |> should.be_false()
}

pub fn is_enum_true_test() {
  codegen.is_enum(schema_parser.Enum("status", ["active"]))
  |> should.be_true()
}

pub fn is_enum_false_test() {
  codegen.is_enum(schema_parser.String)
  |> should.be_false()
}

// ------------------------------------------------------------- enum_type_name

pub fn enum_type_name_simple_test() {
  codegen.enum_type_name("status")
  |> should.equal("Status")
}

pub fn enum_type_name_multi_word_test() {
  codegen.enum_type_name("user_status")
  |> should.equal("UserStatus")
}

// ------------------------------------------------------------- SmallInt

pub fn gleam_type_smallint_test() {
  codegen.gleam_type(schema_parser.SmallInt)
  |> should.equal("Int")
}

pub fn decoder_fn_smallint_test() {
  codegen.decoder_fn(schema_parser.SmallInt)
  |> should.equal("decode.int")
}

pub fn json_encoder_fn_smallint_test() {
  codegen.json_encoder_fn(schema_parser.SmallInt)
  |> should.equal("json.int")
}
