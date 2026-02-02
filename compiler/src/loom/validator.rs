//! Validation
//!
//! Validates generated Gleam code by running `gleam check`
//! after compilation. On failure, reverts all modified files
//! to their previous state and reports errors to the user,
//! ensuring the codebase remains in a consistent state.
//!

use crate::common::colors::{NC, RED};
use std::fs;
use std::process::Command;

// ------------------------------------------------------------- Public Types

/// Represents a compiled file with its output path and the
/// previous content for potential rollback. Used to track
/// files modified during compilation for validation.
///
pub struct CompiledFile {
    /// Path to the generated output file.
    ///
    pub output: String,
    /// Previous file content, empty string if file was new.
    ///
    pub previous: String,
}

// ------------------------------------------------------------- Public Functions

/// Validates generated files by running `gleam check`. If
/// validation fails, all files are reverted to their previous
/// state and the process exits with code 1, preventing broken
/// code from persisting in the codebase.
///
pub fn validate_generated_files(files: &[CompiledFile]) {
    let output = match Command::new("gleam").args(["check"]).output() {
        Ok(result) => result,
        Err(e) => {
            eprintln!("{}Failed to run gleam check: {}{}", RED, e, NC);
            return;
        }
    };

    if output.status.success() {
        return;
    }

    revert_files(files);
    eprintln!("{}Loom compilation failed:{}", RED, NC);
    eprintln!("{}", String::from_utf8_lossy(&output.stderr));
    std::process::exit(1);
}

// ------------------------------------------------------------- Private Functions

/// Restores files to their previous state after a failed
/// validation. New files are deleted, while modified files
/// are restored to their original content.
///
fn revert_files(files: &[CompiledFile]) {
    for file in files {
        if file.previous.is_empty() {
            let _ = fs::remove_file(&file.output);
        } else {
            let _ = fs::write(&file.output, &file.previous);
        }
    }
}
