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

  let result = generator.generate("post", table, [], "")

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

  let result = generator.generate("post", table, [], "")

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

  let result = generator.generate("post", table, [], "")

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

  let result = generator.generate("post", table, [], "")

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

  let result = generator.generate("post", table, [], "")

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

  let result = generator.generate("post", table, [], "")

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

  let result = generator.generate("post", table, [], "")

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

  let result = generator.generate("post", table, [], "")

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

  let result = generator.generate("post", table, [], "")

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

  let result = generator.generate("post", table, [], "")

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

  let result = generator.generate("post", table, [], "")

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

  let result = generator.generate("post", table, [], "")

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

  let result = generator.generate("user", table, [], "")

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

  let result = generator.generate("user", table, [], "")

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

  let result = generator.generate("user", table, [], "")

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

  let result = generator.generate("user", table, [], "")

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

  let result = generator.generate("task", table, [], "")

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

  let result = generator.generate("user", table, [], "")

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

  let result = generator.generate("user", table, [], "")

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

  let result = generator.generate("product", table, [], "")

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

  let result = generator.generate("file", table, [], "")

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

  let result = generator.generate("file", table, [], "")

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

  let result = generator.generate("file", table, [], "")

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

  let result = generator.generate("user", table, [], "")

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

  let result = generator.generate("event", table, [], "")

  result
  |> string.contains("starts_at: String")
  |> should.be_true()
}

// ------------------------------------------------------------- Authenticatable

fn auth_table() -> schema_parser.Table {
  Table(
    name: "users",
    columns: [
      Column("id", schema_parser.Id, False, None, None),
      Column("email", schema_parser.String, False, None, None),
      Column("password", schema_parser.String, False, None, None),
    ],
    indexes: [],
  )
}

const auth_schema = "pub const authenticatable = True"

const auth_schema_custom_throttle = "pub const authenticatable = True
pub const max_login_attempts = 10
pub const lockout_seconds = 120"

// ---- Non-authenticatable produces no auth code

pub fn generate_non_authenticatable_no_session_key_test() {
  let result = generator.generate("user", auth_table(), [], "")

  result
  |> string.contains("session_key")
  |> should.be_false()
}

pub fn generate_non_authenticatable_no_authenticate_test() {
  let result = generator.generate("user", auth_table(), [], "")

  result
  |> string.contains("pub fn authenticate")
  |> should.be_false()
}

pub fn generate_non_authenticatable_no_auth_imports_test() {
  let result = generator.generate("user", auth_table(), [], "")

  result
  |> string.contains("import glimr_auth/auth")
  |> should.be_false()

  result
  |> string.contains("import glimr_auth/hash")
  |> should.be_false()

  result
  |> string.contains("import glimr/session/session")
  |> should.be_false()
}

// ---- Authenticatable generates session_key constant

pub fn generate_authenticatable_session_key_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains("pub const session_key = \"_auth_user_id\"")
  |> should.be_true()
}

pub fn generate_authenticatable_session_key_uses_model_name_test() {
  let table =
    Table(
      name: "customers",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("email", schema_parser.String, False, None, None),
        Column("password", schema_parser.String, False, None, None),
      ],
      indexes: [],
    )

  let result = generator.generate("customer", table, [], auth_schema)

  result
  |> string.contains("pub const session_key = \"_auth_customer_id\"")
  |> should.be_true()
}

// ---- Authenticatable generates throttle constants

pub fn generate_authenticatable_default_max_login_attempts_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains("pub const max_login_attempts = 5")
  |> should.be_true()
}

pub fn generate_authenticatable_default_lockout_seconds_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains("pub const lockout_seconds = 60")
  |> should.be_true()
}

pub fn generate_authenticatable_custom_max_login_attempts_test() {
  let result =
    generator.generate("user", auth_table(), [], auth_schema_custom_throttle)

  result
  |> string.contains("pub const max_login_attempts = 10")
  |> should.be_true()
}

pub fn generate_authenticatable_custom_lockout_seconds_test() {
  let result =
    generator.generate("user", auth_table(), [], auth_schema_custom_throttle)

  result
  |> string.contains("pub const lockout_seconds = 120")
  |> should.be_true()
}

// ---- Authenticatable generates authenticate function

pub fn generate_authenticatable_function_signature_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains("pub fn authenticate(")
  |> should.be_true()

  result
  |> string.contains("session session: Session,")
  |> should.be_true()

  result
  |> string.contains("pool pool: db.DbPool,")
  |> should.be_true()

  result
  |> string.contains("email email: String,")
  |> should.be_true()

  result
  |> string.contains("password password: String,")
  |> should.be_true()
}

pub fn generate_authenticatable_return_type_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains(") -> Result(User, auth.AuthError) {")
  |> should.be_true()
}

pub fn generate_authenticatable_return_type_uses_model_name_test() {
  let table =
    Table(
      name: "customers",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("email", schema_parser.String, False, None, None),
        Column("password", schema_parser.String, False, None, None),
      ],
      indexes: [],
    )

  let result = generator.generate("customer", table, [], auth_schema)

  result
  |> string.contains(") -> Result(Customer, auth.AuthError) {")
  |> should.be_true()
}

// ---- Authenticatable checks throttle before attempting

pub fn generate_authenticatable_check_throttle_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains("auth.check_throttle(session, session_key)")
  |> should.be_true()
}

// ---- Authenticatable looks up user by email

pub fn generate_authenticatable_by_email_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains("by_email(pool: pool, email: email)")
  |> should.be_true()
}

// ---- Authenticatable does dummy verify for timing safety

pub fn generate_authenticatable_dummy_verify_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains("hash.dummy_verify(password)")
  |> should.be_true()
}

// ---- Authenticatable verifies password

pub fn generate_authenticatable_hash_verify_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains("hash.verify(password, user.password)")
  |> should.be_true()
}

pub fn generate_authenticatable_hash_verify_uses_model_name_test() {
  let table =
    Table(
      name: "customers",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("email", schema_parser.String, False, None, None),
        Column("password", schema_parser.String, False, None, None),
      ],
      indexes: [],
    )

  let result = generator.generate("customer", table, [], auth_schema)

  result
  |> string.contains("hash.verify(password, customer.password)")
  |> should.be_true()
}

// ---- Authenticatable records failure on wrong password

pub fn generate_authenticatable_record_failure_on_bad_password_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains("auth.record_failure(")
  |> should.be_true()
}

pub fn generate_authenticatable_record_failure_passes_throttle_constants_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains(
    "auth.record_failure(session, session_key, max_login_attempts, lockout_seconds)",
  )
  |> should.be_true()
}

pub fn generate_authenticatable_record_failure_custom_throttle_test() {
  let result =
    generator.generate("user", auth_table(), [], auth_schema_custom_throttle)

  // The function body references the constants by name (not inlined values)
  result
  |> string.contains(
    "auth.record_failure(session, session_key, max_login_attempts, lockout_seconds)",
  )
  |> should.be_true()

  // But the constant declarations use the custom values
  result
  |> string.contains("pub const max_login_attempts = 10")
  |> should.be_true()

  result
  |> string.contains("pub const lockout_seconds = 120")
  |> should.be_true()
}

// ---- Authenticatable returns InvalidCredentials on failure

pub fn generate_authenticatable_invalid_credentials_on_no_user_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains("Error(auth.InvalidCredentials)")
  |> should.be_true()
}

// ---- Authenticatable clears throttle and logs in on success

pub fn generate_authenticatable_clear_throttle_on_success_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains("auth.clear_throttle(session, session_key)")
  |> should.be_true()
}

pub fn generate_authenticatable_login_on_success_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains("auth.login(session, int.to_string(user.id), session_key)")
  |> should.be_true()
}

pub fn generate_authenticatable_login_uses_model_name_test() {
  let table =
    Table(
      name: "customers",
      columns: [
        Column("id", schema_parser.Id, False, None, None),
        Column("email", schema_parser.String, False, None, None),
        Column("password", schema_parser.String, False, None, None),
      ],
      indexes: [],
    )

  let result = generator.generate("customer", table, [], auth_schema)

  result
  |> string.contains(
    "auth.login(session, int.to_string(customer.id), session_key)",
  )
  |> should.be_true()
}

pub fn generate_authenticatable_returns_model_on_success_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains("Ok(user)")
  |> should.be_true()
}

// ---- Authenticatable imports

pub fn generate_authenticatable_imports_auth_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains("import glimr_auth/auth")
  |> should.be_true()
}

pub fn generate_authenticatable_imports_hash_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains("import glimr_auth/hash")
  |> should.be_true()
}

pub fn generate_authenticatable_imports_session_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains("import glimr/session/session.{type Session}")
  |> should.be_true()
}

pub fn generate_authenticatable_imports_int_test() {
  let result = generator.generate("user", auth_table(), [], auth_schema)

  result
  |> string.contains("import gleam/int")
  |> should.be_true()
}

// ---- Schema content parsing edge cases

pub fn generate_authenticatable_false_not_triggered_test() {
  let schema = "pub const authenticatable = False"
  let result = generator.generate("user", auth_table(), [], schema)

  result
  |> string.contains("pub fn authenticate")
  |> should.be_false()
}

pub fn generate_authenticatable_partial_match_not_triggered_test() {
  let schema = "pub const authenticatable_extra = True"
  let result = generator.generate("user", auth_table(), [], schema)

  result
  |> string.contains("pub fn authenticate")
  |> should.be_false()
}

pub fn generate_authenticatable_only_max_login_attempts_custom_test() {
  let schema =
    "pub const authenticatable = True
pub const max_login_attempts = 3"
  let result = generator.generate("user", auth_table(), [], schema)

  result
  |> string.contains("pub const max_login_attempts = 3")
  |> should.be_true()

  // lockout_seconds should use default
  result
  |> string.contains("pub const lockout_seconds = 60")
  |> should.be_true()
}

pub fn generate_authenticatable_only_lockout_seconds_custom_test() {
  let schema =
    "pub const authenticatable = True
pub const lockout_seconds = 300"
  let result = generator.generate("user", auth_table(), [], schema)

  // max_login_attempts should use default
  result
  |> string.contains("pub const max_login_attempts = 5")
  |> should.be_true()

  result
  |> string.contains("pub const lockout_seconds = 300")
  |> should.be_true()
}
