mod common;
mod loom;
mod routes;

use common::args;
use common::colors::{NC, RED};
use std::process;

/// CLI entry point that dispatches to the appropriate compiler.
/// Kept minimal so each compiler module owns its logic. Adding
/// a new compiler means adding a match arm and a module.
///
fn main() {
    let args = args::parse();

    match args.command.as_str() {
        "routes" => {
            if args.check_only {
                // Return 0 or 1 to process exit if routes should compile.
                process::exit(!routes::should_auto_compile() as i32)
            }

            routes::compile(args.verbose);
        }
        "loom" => {
            if args.check_only {
                // Return 0 or 1 to process exit if loom should compile.
                process::exit(!loom::should_auto_compile() as i32)
            }

            if args.stale_only {
                loom::compile_stale(args.verbose);
            } else {
                loom::compile(args.verbose);
            }
        }
        _ => {
            eprintln!("{}Unknown command: {}{}", RED, args.command, NC);
            process::exit(1);
        }
    }
}
