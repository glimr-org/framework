//// Auth
////
//// Controllers shouldn't use raw session keys like
//// "_auth_user_id" directly — that couples every auth check
//// to a magic string that could drift between modules. This
//// module wraps the session with semantic helpers (login,
//// logout, check, id) so auth logic reads clearly and the
//// session key is defined once in config rather than
//// scattered across the codebase.
////

import glimr/config/auth as auth_config
import glimr/session/session.{type Session}

// ------------------------------------------------------------- Public Functions

/// Regenerating the session ID after storing the user ID is
/// critical — without it, an attacker who planted a known
/// session ID before authentication could hijack the post-
/// login session. The regenerate call rotates the ID while
/// preserving all session data so the user doesn't lose
/// pre-login state like a shopping cart.
///
pub fn login(session: Session, user_id: String) -> Nil {
  let config = auth_config.load()

  session.put(session, config.session_key, user_id)
  session.regenerate(session)
}

/// Invalidate rather than just deleting the auth key — a
/// partial logout that leaves other session data intact could
/// leak state between users on shared devices. Invalidation
/// clears everything, generates a fresh ID, and tells the
/// middleware to delete the old entry from the store so it
/// can never be replayed.
///
pub fn logout(session: Session) -> Nil {
  session.invalidate(session)
}

/// A Bool check is the most common auth gate — middleware and
/// guards typically just need to know "logged in or not" to
/// decide whether to redirect. Returning Bool instead of the
/// full user ID keeps the call site clean when the ID itself
/// isn't needed for the decision.
///
pub fn check(session: Session) -> Bool {
  let config = auth_config.load()

  session.has(session, config.session_key)
}

/// Returns a Result so callers can distinguish "not logged in"
/// from a logged-in user with an empty string ID. Controllers
/// that need the actual user ID to load a profile or check
/// permissions use this, while simple auth gates use check
/// instead.
///
pub fn id(session: Session) -> Result(String, Nil) {
  let config = auth_config.load()

  session.get(session, config.session_key)
}
