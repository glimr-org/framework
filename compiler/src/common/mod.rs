//! Common Utilities
//!
//! Shared utilities used across all compiler modules.
//! Contains CLI parsing, terminal colors, and config parsing
//! so each compiler doesn't duplicate this infrastructure.
//!

pub mod args;
pub mod colors;
pub mod toml;
