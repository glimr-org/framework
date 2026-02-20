//// File Session Store
////
//// Not every deployment has Redis or a database, but every
//// server has a filesystem. This store persists sessions as
//// flat files — one per session ID — so apps can use server-
//// side sessions without any infrastructure beyond disk. The
//// expiry timestamp is stored inline as the first line of
//// each file so GC can check expiration without parsing the
//// full payload.
////

import gleam/dict
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import glimr/cache/file/pool.{type Pool}
import glimr/config/session as session_config
import glimr/session/payload
import glimr/session/store.{type SessionStore}
import glimr/utils/unix_timestamp
import simplifile

// ------------------------------------------------------------- Internal Public Functions

/// Wiring the store callbacks here keeps filesystem details
/// out of the session middleware. Reusing the file cache
/// pool's directory groups all file-based storage under one
/// configurable path. The cookie_value callback returns just
/// the session ID since the actual data lives on disk, not
/// in the cookie.
///
@internal
pub fn create(pool: Pool) -> SessionStore {
  let config = session_config.load()
  let base_path = pool.get_path(pool) <> "/sessions"

  store.new(
    load: fn(session_id) { load(base_path, session_id) },
    save: fn(session_id, data, flash) {
      save(base_path, session_id, data, flash, config.lifetime)
    },
    destroy: fn(session_id) { destroy(base_path, session_id) },
    gc: fn() { gc(base_path) },
    cookie_value: fn(id, _, _) { id },
  )
}

// ------------------------------------------------------------- Private Functions

/// The expiry timestamp is checked on read rather than relying
/// solely on GC, so an expired session is never served even if
/// GC hasn't run yet. Returning an empty tuple on any failure
/// (missing file, corrupt content, expired) means the caller
/// always gets a valid session — either hydrated or fresh.
///
fn load(
  base_path: String,
  session_id: String,
) -> #(dict.Dict(String, String), dict.Dict(String, String)) {
  let path = base_path <> "/" <> session_id
  let now = unix_timestamp.now()
  let empty = #(dict.new(), dict.new())

  let decoded = {
    use content <- result.try(
      simplifile.read(path) |> result.replace_error(Nil),
    )
    use #(expires_str, payload_json) <- result.try(string.split_once(
      content,
      "\n",
    ))
    use expires_at <- result.try(int.parse(expires_str))
    case expires_at >= now {
      True -> Ok(payload.decode(payload_json))
      False -> Error(Nil)
    }
  }
  result.unwrap(decoded, empty)
}

/// The expiry timestamp is written as the first line so GC
/// can check it without parsing the JSON payload, keeping
/// the GC scan fast even with many session files. Creating
/// the directory on every save handles the cold-start case
/// where the sessions directory doesn't exist yet.
///
fn save(
  base_path: String,
  session_id: String,
  data: dict.Dict(String, String),
  flash: dict.Dict(String, String),
  lifetime: Int,
) -> Nil {
  let path = base_path <> "/" <> session_id
  let encoded = payload.encode(data, flash)
  let expires_at = unix_timestamp.now() + lifetime * 60
  let content = int.to_string(expires_at) <> "\n" <> encoded

  let _ = simplifile.create_directory_all(base_path)
  let _ = simplifile.write(path, content)

  Nil
}

/// Session invalidation must remove the file immediately so
/// the old session ID can never be reused, even before GC
/// runs. Ignoring delete errors is safe — the file may
/// already be gone from a prior GC pass.
///
fn destroy(base_path: String, session_id: String) -> Nil {
  let path = base_path <> "/" <> session_id
  let _ = simplifile.delete(path)
  Nil
}

/// Scans all session files and deletes expired ones. Called
/// probabilistically by the session middleware so cleanup
/// cost is amortized across requests. Silently ignoring a
/// missing directory handles the case where no sessions have
/// been created yet.
///
fn gc(base_path: String) -> Nil {
  let now = unix_timestamp.now()

  case simplifile.read_directory(base_path) {
    Ok(files) -> list.each(files, gc_file(base_path, now, _))
    Error(_) -> Nil
  }
}

/// Only the first line (the expiry timestamp) needs to be
/// parsed to decide whether to delete — reading the full
/// payload would waste I/O on files that might be kept.
/// Corrupt or unreadable files are left alone rather than
/// deleted, avoiding data loss from transient I/O errors.
///
fn gc_file(base_path: String, now: Int, file: String) -> Nil {
  let path = base_path <> "/" <> file
  let expired = {
    use content <- result.try(
      simplifile.read(path) |> result.replace_error(Nil),
    )
    use #(expires_str, _) <- result.try(string.split_once(content, "\n"))
    use expires_at <- result.map(int.parse(expires_str))
    expires_at < now
  }
  case expired {
    Ok(True) -> {
      let _ = simplifile.delete(path)
      Nil
    }
    _ -> Nil
  }
}
