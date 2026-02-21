//// Password Hashing
////
//// Controllers shouldn't configure hashing parameters or
//// manage salts — those are security-critical details that
//// should be set once correctly. This module wraps the argus
//// library behind a two-function API (make/verify) so the
//// only decision callers make is what to hash or check.
//// Argon2id with OWASP-recommended defaults is used because
//// it resists both GPU and side-channel attacks better than
//// bcrypt or scrypt.
////

import argus

// ------------------------------------------------------------- Public Functions

/// Generates a fresh random salt per call so identical
/// passwords produce different hashes — without this, an
/// attacker who compromised the database could spot users
/// sharing the same password. The assert on hash success is
/// safe because argus only fails on invalid parameters, which
/// can't happen with the default hasher config.
///
pub fn make(password: String) -> String {
  let salt = argus.gen_salt()

  let assert Ok(hashes) =
    argus.hasher()
    |> argus.hash(password, salt)

  hashes.encoded_hash
}

/// Collapsing both Ok(False) and Error(_) into False simplifies
/// the call site — callers only care whether the password
/// matched, not why verification failed. Argus handles constant-
/// time comparison internally so this function doesn't leak
/// timing information about partial matches.
///
pub fn verify(password: String, encoded_hash: String) -> Bool {
  case argus.verify(encoded_hash, password) {
    Ok(True) -> True
    _ -> False
  }
}

/// Without this, login attempts for nonexistent users return
/// instantly while real users take ~100ms for the hash — an
/// attacker can enumerate valid usernames by measuring response
/// times. Hashing a dummy value burns the same CPU time as a
/// real verify, making the timing indistinguishable regardless
/// of whether the account exists.
///
pub fn dummy_verify(password: String) -> Bool {
  let dummy_hash = make("__glimr_dummy__")
  verify(password, dummy_hash)
}
