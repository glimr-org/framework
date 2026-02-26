//// File-Based Cache Backend
////
//// Not every project needs Redis or a database just to cache a
//// few values. This backend stores cached data as plain files
//// on disk, using a two-level directory tree derived from
//// SHA256 hashes so the filesystem doesn't choke on a single
//// directory with thousands of entries. It's the
//// zero-dependency option — no external services, no pool
//// management, just the filesystem.

import gleam/bit_array
import gleam/crypto
import gleam/int
import gleam/result
import gleam/string
import glimr/cache/cache.{
  type CacheError, ConnectionError, NotFound, SerializationError,
}
import glimr/cache/file/pool.{type Pool}
import glimr/filesystem/filesystem
import glimr/utils/unix_timestamp
import simplifile

// ------------------------------------------------------------- Public Functions

/// Expired entries aren't cleaned up by a background job —
/// instead they get deleted the next time someone asks for
/// them. This "lazy expiration" means you never waste CPU
/// scanning for stale files, and entries that nobody reads
/// again just sit harmlessly until the next flush.
///
pub fn get(pool: Pool, key: String) -> Result(String, CacheError) {
  let path = key_to_path(pool, key)

  case simplifile.read(path) {
    Ok(content) -> parse_cache_file(content, path)
    Error(_) -> Error(NotFound)
  }
}

/// The file format is dead simple: first line is the Unix
/// timestamp when the entry expires, second line onward is the
/// value. Computing the absolute expiry time here (now + TTL)
/// means reads only need a single comparison against the
/// current time, no duration arithmetic involved.
///
pub fn put(
  pool: Pool,
  key: String,
  value: String,
  ttl_seconds: Int,
) -> Result(Nil, CacheError) {
  let expires_at = unix_timestamp.now() + ttl_seconds
  let content = int.to_string(expires_at) <> "\n" <> value
  write_cache_file(pool, key, content)
}

/// A timestamp of 0 is the sentinel for "never expires." We
/// could have used a separate file format or a flag, but
/// keeping the same {timestamp}\n{value} layout means the read
/// path doesn't need to branch on format — it just checks if
/// the timestamp is 0 and skips expiry logic.
///
pub fn put_forever(
  pool: Pool,
  key: String,
  value: String,
) -> Result(Nil, CacheError) {
  let content = "0\n" <> value
  write_cache_file(pool, key, content)
}

/// Treating "file doesn't exist" as success makes this
/// idempotent — two concurrent requests can both try to delete
/// the same key without one of them getting a spurious error.
/// Only actual filesystem failures (permissions, disk full)
/// bubble up as errors.
///
pub fn forget(pool: Pool, key: String) -> Result(Nil, CacheError) {
  let path = key_to_path(pool, key)

  case simplifile.delete(path) {
    Ok(_) -> Ok(Nil)
    Error(simplifile.Enoent) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError(
        "Failed to delete cache file: " <> string.inspect(e),
      ))
  }
}

/// Delegating to get() rather than just checking if the file
/// exists means expired entries are cleaned up as a side
/// effect. If we only did a file existence check, has() would
/// return True for entries that get() would reject as expired —
/// and that inconsistency would be really confusing to debug.
///
pub fn has(pool: Pool, key: String) -> Bool {
  case get(pool, key) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Nuking the entire directory tree is much faster than walking
/// every subdirectory and deleting files one by one, especially
/// when there are thousands of cached entries. The directory
/// gets recreated automatically the next time something is
/// written, so there's no setup step needed after a flush.
///
pub fn flush(pool: Pool) -> Result(Nil, CacheError) {
  let path = pool.get_path(pool)

  case simplifile.delete(path) {
    Ok(_) -> Ok(Nil)
    Error(simplifile.Enoent) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError("Failed to flush cache: " <> string.inspect(e)))
  }
}

// ------------------------------------------------------------- Internal Public Functions

/// Storing all cache files in a flat directory would grind to a
/// halt once you have a few thousand entries — most filesystems
/// slow down dramatically with too many files in one folder.
/// Hashing the key with SHA256 and splitting the first four hex
/// characters into two directory levels (like git does with
/// objects) spreads files evenly and keeps each directory
/// small. This is @internal because the console cache:clear
/// command needs to resolve paths for display.
///
@internal
pub fn key_to_path(pool: Pool, key: String) -> String {
  let hash =
    crypto.hash(crypto.Sha256, bit_array.from_string(key))
    |> bit_array.base16_encode()
    |> string.lowercase()

  let dir1 = string.slice(hash, 0, 2)
  let dir2 = string.slice(hash, 2, 2)

  pool.get_path(pool) <> "/" <> dir1 <> "/" <> dir2 <> "/" <> hash
}

// ------------------------------------------------------------- Private Functions

/// Both put() and put_forever() need to hash the key, ensure
/// the directory tree exists, and write the file — pulling that
/// into one place means a bug in directory creation only needs
/// to be fixed once. The directory creation is particularly
/// important because the two-level hash structure means the
/// parent directories might not exist yet for a brand new key
/// prefix.
///
fn write_cache_file(
  pool: Pool,
  key: String,
  content: String,
) -> Result(Nil, CacheError) {
  let path = key_to_path(pool, key)
  let _ = filesystem.ensure_directory_exists(path)
  case simplifile.write(path, content) {
    Ok(_) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError("Failed to write cache file: " <> string.inspect(e)))
  }
}

/// The file format is intentionally simple: the first line is
/// the expiry timestamp (or 0 for "forever"), and everything
/// after the first newline is the cached value. If the
/// timestamp shows the entry has expired, we delete the file
/// right here — this is the lazy cleanup that keeps disk usage
/// in check without needing a background GC job.
///
fn parse_cache_file(content: String, path: String) -> Result(String, CacheError) {
  use #(expires_str, value) <- result.try(
    string.split_once(content, "\n")
    |> result.replace_error(SerializationError("Invalid cache file format")),
  )
  use expires_at <- result.try(
    int.parse(expires_str)
    |> result.replace_error(SerializationError("Invalid expiration timestamp")),
  )
  case expires_at == 0 || expires_at > unix_timestamp.now() {
    True -> Ok(value)
    False -> {
      let _ = simplifile.delete(path)
      Error(NotFound)
    }
  }
}
