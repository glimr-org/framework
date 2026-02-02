//! CLI Argument Parsing
//!
//! Parses command-line arguments using pico-args for minimal
//! overhead. Validates input and exits early on unknown flags
//! so compilers receive clean, validated arguments.
//!

use super::colors::{NC, RED};
use std::process;

// ------------------------------------------------------------- Public Types

/// Holds parsed CLI arguments for the compiler binary.
/// Centralizes argument handling so main.rs stays thin and
/// subcommands receive validated input.
///
pub struct Args {
    /// Which compiler to run (e.g., "routes"). Empty string
    /// if no subcommand was provided.
    ///
    pub command: String,

    /// Shows additional output when debugging compilation
    /// issues. Enabled with `-v` or `--verbose`.
    ///
    pub verbose: bool,

    /// Used by build tools to check if compilation is needed
    /// without actually compiling. Exits 0 if stale, 1 if fresh.
    ///
    pub check_only: bool,

    /// Only compile stale files (incremental compilation).
    /// Used by build/run commands. Enabled with `--stale`.
    ///
    pub stale_only: bool,
}

// ------------------------------------------------------------- Public Functions

/// Parses arguments from the environment and validates them.
/// Exits with an error message if unknown flags are passed,
/// preventing silent misconfiguration.
///
pub fn parse() -> Args {
    let mut pargs = pico_args::Arguments::from_env();

    let args = Args {
        verbose: pargs.contains(["-v", "--verbose"]),
        check_only: pargs.contains("--check-only"),
        stale_only: pargs.contains("--stale"),
        command: pargs.free_from_str().unwrap_or_default(),
    };

    let remaining = pargs.finish();

    if !remaining.is_empty() {
        eprintln!("{}Unknown arguments: {:?}{}", RED, remaining, NC);
        process::exit(1);
    }

    args
}
