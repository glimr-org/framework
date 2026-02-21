import gleam/string
import gleeunit/should
import glimr/auth/hash

// ------------------------------------------------------------- Make

pub fn make_returns_encoded_hash_test() {
  let hashed = hash.make("password123")

  // Argon2id encoded hashes start with $argon2id$
  string.starts_with(hashed, "$argon2id$")
  |> should.equal(True)
}

pub fn make_produces_different_hashes_for_same_password_test() {
  let hash1 = hash.make("password123")
  let hash2 = hash.make("password123")

  // Different salts should produce different hashes
  should.not_equal(hash1, hash2)
}

// ------------------------------------------------------------- Verify

pub fn verify_correct_password_returns_true_test() {
  let hashed = hash.make("my_secret")

  hash.verify("my_secret", hashed)
  |> should.equal(True)
}

pub fn verify_wrong_password_returns_false_test() {
  let hashed = hash.make("my_secret")

  hash.verify("wrong_password", hashed)
  |> should.equal(False)
}

pub fn verify_invalid_hash_returns_false_test() {
  hash.verify("password", "not_a_valid_hash")
  |> should.equal(False)
}

// ------------------------------------------------------------- Dummy Verify

pub fn dummy_verify_returns_false_test() {
  hash.dummy_verify("any_password")
  |> should.equal(False)
}
