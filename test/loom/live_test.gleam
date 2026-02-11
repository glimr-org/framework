import gleam/crypto
import gleam/string
import gleeunit/should
import glimr/loom/live

fn sign(module: String, props: String, key: String) -> String {
  let payload = module <> ":" <> props
  crypto.sign_message(<<payload:utf8>>, <<key:utf8>>, crypto.Sha256)
}

// ------------------------------------------------------------- verify_init_token Tests

pub fn verify_init_token_valid_test() {
  let key = "test-secret"
  let token = sign("counter", "{\"count\":0}", key)

  live.verify_init_token(token, "counter", key)
  |> should.equal(Ok("{\"count\":0}"))
}

pub fn verify_init_token_empty_props_test() {
  let key = "test-secret"
  let token = sign("counter", "{}", key)

  live.verify_init_token(token, "counter", key)
  |> should.equal(Ok("{}"))
}

pub fn verify_init_token_tampered_token_test() {
  let key = "test-secret"
  let token = sign("counter", "{\"count\":0}", key)
  let tampered = string.drop_end(token, 5) <> "XXXXX"

  live.verify_init_token(tampered, "counter", key)
  |> should.be_error
}

pub fn verify_init_token_wrong_key_test() {
  let token = sign("counter", "{\"count\":0}", "correct-key")

  live.verify_init_token(token, "counter", "wrong-key")
  |> should.be_error
}

pub fn verify_init_token_module_mismatch_test() {
  let key = "test-secret"
  let token = sign("counter", "{\"count\":0}", key)

  live.verify_init_token(token, "other_module", key)
  |> should.be_error
}

pub fn verify_init_token_garbage_input_test() {
  live.verify_init_token("not-a-real-token", "counter", "key")
  |> should.be_error
}

pub fn verify_init_token_props_with_colons_test() {
  let key = "test-secret"
  let props = "{\"url\":\"https://example.com:8080\"}"
  let token = sign("my_module", props, key)

  live.verify_init_token(token, "my_module", key)
  |> should.equal(Ok(props))
}
