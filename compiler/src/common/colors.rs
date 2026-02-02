//! Terminal Colors
//!
//! ANSI escape codes for terminal output coloring.
//! Provides consistent colors across all compiler messages
//! without pulling in a full terminal styling crate.
//!

// ------------------------------------------------------------- Public Consts

/// Used for success messages like "Compiled X routes".
///
pub const GREEN: &str = "\x1b[0;32m";

/// Used for error messages and fatal conditions.
///
pub const RED: &str = "\x1b[0;31m";

/// Used for warnings and non-fatal issues.
///
pub const YELLOW: &str = "\x1b[0;33m";

/// Resets terminal to default color. Always append after
/// colored output to avoid bleeding into subsequent text.
///
pub const NC: &str = "\x1b[0m";
