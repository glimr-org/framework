//// Cache Shared Types
////
//// Provides common types used across all cache drivers
//// including file-based and Redis caching backends.

// ------------------------------------------------------------- Public Types

/// Represents errors that can occur during cache operations.
/// Includes variants for missing keys, serialization failures,
/// connection issues, expiration, and compute errors.
///
pub type CacheError {
  /// The requested key was not found in the cache
  NotFound
  /// Failed to serialize or deserialize cache data
  SerializationError(message: String)
  /// Failed to connect to or communicate with the cache store
  ConnectionError(message: String)
  /// The cached value has expired (used internally)
  Expired
  /// A computation error occurred in remember callback
  ComputeError(message: String)
}
