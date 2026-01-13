//// File Cache Operations
////
//// Provides cache operations for file-based caching.
//// Files are stored in a 2-level directory structure using
//// SHA256 hashes of keys for efficient filesystem access.

import gleam/bit_array
import gleam/crypto
import gleam/dynamic/decode
import gleam/int
import gleam/json.{type Json}
import gleam/result
import gleam/string
import glimr/cache/cache.{
  type CacheError, ComputeError, ConnectionError, NotFound, SerializationError,
}
import glimr/cache/file/pool.{type Pool}
import glimr/utils/unix_timestamp
import simplifile

// ------------------------------------------------------------- Public Functions

/// Retrieves a value from the cache by key. Returns NotFound
/// if the key doesn't exist or has expired. Expired entries
/// are lazily deleted on access.
///
pub fn get(pool: Pool, key: String) -> Result(String, CacheError) {
  let path = key_to_path(pool, key)

  case simplifile.read(path) {
    Ok(content) -> parse_cache_file(content, path)
    Error(_) -> Error(NotFound)
  }
}

/// Stores a value in the cache with a TTL (time-to-live) in
/// seconds. Returns Ok(Nil) on success, or a CacheError on
/// failure. Creates directories as needed.
///
pub fn put(
  pool: Pool,
  key: String,
  value: String,
  ttl_seconds: Int,
) -> Result(Nil, CacheError) {
  let path = key_to_path(pool, key)
  let expires_at = current_timestamp() + ttl_seconds
  let content = int.to_string(expires_at) <> "\n" <> value

  ensure_directory(path)
  case simplifile.write(path, content) {
    Ok(_) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError("Failed to write cache file: " <> string.inspect(e)))
  }
}

/// Stores a value in the cache permanently (no expiration).
/// Uses a special timestamp value of 0 to indicate no TTL.
/// Creates directories as needed.
///
pub fn put_forever(
  pool: Pool,
  key: String,
  value: String,
) -> Result(Nil, CacheError) {
  let path = key_to_path(pool, key)
  let content = "0\n" <> value

  ensure_directory(path)
  case simplifile.write(path, content) {
    Ok(_) -> Ok(Nil)
    Error(e) ->
      Error(ConnectionError("Failed to write cache file: " <> string.inspect(e)))
  }
}

/// Removes a value from the cache by key. Returns Ok(Nil)
/// even if the key didn't exist, making it safe to call
/// without checking existence first.
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

/// Checks if a key exists in the cache and hasn't expired.
/// Uses get internally so expired entries are lazily deleted
/// during this check.
///
pub fn has(pool: Pool, key: String) -> Bool {
  case get(pool, key) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Removes all cached values from the cache directory.
/// Deletes the entire cache directory tree and all its
/// contents.
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

/// Retrieves a JSON value from the cache and decodes it.
/// Returns SerializationError if the cached value cannot
/// be parsed as valid JSON matching the decoder.
///
pub fn get_json(
  pool: Pool,
  key: String,
  decoder: decode.Decoder(a),
) -> Result(a, CacheError) {
  use value <- result.try(get(pool, key))

  case json.parse(value, decoder) {
    Ok(decoded) -> Ok(decoded)
    Error(_) -> Error(SerializationError("Failed to decode JSON"))
  }
}

/// Stores a value as JSON in the cache with a TTL. Encodes
/// the value using the provided encoder function before
/// storing.
///
pub fn put_json(
  pool: Pool,
  key: String,
  value: a,
  encoder: fn(a) -> Json,
  ttl_seconds: Int,
) -> Result(Nil, CacheError) {
  let json_string = json.to_string(encoder(value))
  put(pool, key, json_string, ttl_seconds)
}

/// Stores a value as JSON in the cache permanently. Encodes
/// the value using the provided encoder function before
/// storing with no expiration.
///
pub fn put_json_forever(
  pool: Pool,
  key: String,
  value: a,
  encoder: fn(a) -> Json,
) -> Result(Nil, CacheError) {
  let json_string = json.to_string(encoder(value))
  put_forever(pool, key, json_string)
}

/// Retrieves a value and removes it from the cache in one
/// operation. Useful for one-time tokens or consuming queued
/// values.
///
pub fn pull(pool: Pool, key: String) -> Result(String, CacheError) {
  case get(pool, key) {
    Ok(value) -> {
      let _ = forget(pool, key)
      Ok(value)
    }
    Error(e) -> Error(e)
  }
}

/// Increments a numeric value in the cache. If the key
/// doesn't exist, starts from 0. Stores the result with
/// no expiration.
///
pub fn increment(pool: Pool, key: String, by: Int) -> Result(Int, CacheError) {
  let current = case get(pool, key) {
    Ok(value) -> int.parse(value) |> result.unwrap(0)
    Error(_) -> 0
  }

  let new_value = current + by
  case put_forever(pool, key, int.to_string(new_value)) {
    Ok(_) -> Ok(new_value)
    Error(e) -> Error(e)
  }
}

/// Decrements a numeric value in the cache. If the key
/// doesn't exist, starts from 0. Delegates to increment
/// with a negated value.
///
pub fn decrement(pool: Pool, key: String, by: Int) -> Result(Int, CacheError) {
  increment(pool, key, -by)
}

/// Gets a value from cache, or computes and stores it if not
/// found. The compute function returns a Result to handle
/// computation errors gracefully.
///
pub fn remember(
  pool: Pool,
  key: String,
  ttl_seconds: Int,
  compute: fn() -> Result(String, e),
) -> Result(String, CacheError) {
  case get(pool, key) {
    Ok(value) -> Ok(value)
    Error(NotFound) -> {
      case compute() {
        Ok(value) -> {
          let _ = put(pool, key, value, ttl_seconds)
          Ok(value)
        }
        Error(_) -> Error(ComputeError("Compute function failed"))
      }
    }
    Error(e) -> Error(e)
  }
}

/// Gets a value from cache, or computes and stores it
/// permanently. Like remember but with no TTL for values
/// that should never expire.
///
pub fn remember_forever(
  pool: Pool,
  key: String,
  compute: fn() -> Result(String, e),
) -> Result(String, CacheError) {
  case get(pool, key) {
    Ok(value) -> Ok(value)
    Error(NotFound) -> {
      case compute() {
        Ok(value) -> {
          let _ = put_forever(pool, key, value)
          Ok(value)
        }
        Error(_) -> Error(ComputeError("Compute function failed"))
      }
    }
    Error(e) -> Error(e)
  }
}

/// Gets a JSON value from cache, or computes, encodes, and
/// stores it. Combines remember semantics with JSON encoding
/// and decoding.
///
pub fn remember_json(
  pool: Pool,
  key: String,
  ttl_seconds: Int,
  decoder: decode.Decoder(a),
  compute: fn() -> Result(a, e),
  encoder: fn(a) -> Json,
) -> Result(a, CacheError) {
  case get_json(pool, key, decoder) {
    Ok(value) -> Ok(value)
    Error(NotFound) | Error(SerializationError(_)) -> {
      case compute() {
        Ok(value) -> {
          let _ = put_json(pool, key, value, encoder, ttl_seconds)
          Ok(value)
        }
        Error(_) -> Error(ComputeError("Compute function failed"))
      }
    }
    Error(e) -> Error(e)
  }
}

// ---------------------------------------------- Internal Public Functions

/// Converts a cache key to a filesystem path using SHA256
/// hash. Creates a 2-level directory structure from first
/// 4 chars of hash for efficient filesystem access.
///
@internal
pub fn key_to_path(pool: Pool, key: String) -> String {
  let hash =
    crypto.hash(crypto.Sha256, bit_array.from_string(key))
    |> bit_array_to_hex()

  let dir1 = string.slice(hash, 0, 2)
  let dir2 = string.slice(hash, 2, 2)

  pool.get_path(pool) <> "/" <> dir1 <> "/" <> dir2 <> "/" <> hash
}

// --------------------------------------------------------- Private Functions

/// Parses cache file content and checks expiration. File
/// format is {expiration_timestamp}\n{value} where 0 means
/// no expiration.
///
fn parse_cache_file(content: String, path: String) -> Result(String, CacheError) {
  case string.split_once(content, "\n") {
    Ok(#(expires_str, value)) -> {
      case int.parse(expires_str) {
        Ok(0) ->
          // 0 means no expiration
          Ok(value)
        Ok(expires_at) -> {
          let now = current_timestamp()
          case expires_at > now {
            True -> Ok(value)
            False -> {
              // Lazy deletion - remove expired file
              let _ = simplifile.delete(path)
              Error(NotFound)
            }
          }
        }
        Error(_) -> Error(SerializationError("Invalid expiration timestamp"))
      }
    }
    Error(_) -> Error(SerializationError("Invalid cache file format"))
  }
}

/// Ensures the directory for a cache file exists. Extracts
/// the directory portion of the path and creates all parent
/// directories as needed.
///
fn ensure_directory(path: String) -> Nil {
  case string.split(path, "/") |> list_init() {
    [] -> Nil
    parts -> {
      let dir = string.join(parts, "/")
      let _ = simplifile.create_directory_all(dir)
      Nil
    }
  }
}

/// Returns all but the last element of a list. Used to
/// extract the directory path from a full file path by
/// removing the filename component.
///
fn list_init(list: List(a)) -> List(a) {
  case list {
    [] -> []
    [_] -> []
    [x, ..rest] -> [x, ..list_init(rest)]
  }
}

/// Returns the current Unix timestamp in seconds. Used for
/// calculating TTL expiration times and checking if cached
/// entries have expired.
///
fn current_timestamp() -> Int {
  unix_timestamp.now()
}

/// Converts a BitArray to a hexadecimal string. Used to
/// convert SHA256 hash output to a string suitable for
/// filesystem paths.
///
fn bit_array_to_hex(bits: BitArray) -> String {
  bit_array.base16_encode(bits)
  |> string.lowercase()
}
