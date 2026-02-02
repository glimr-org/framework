mod config;
mod generator;
mod parser;
mod patterns;
mod validator;

use std::process;

use crate::common::colors::{GREEN, NC, RED, YELLOW};

// ------------------------------------------------------------- Public Functions

/// Re-exported from config. Returns true if routes should auto
/// compile based on the user's `glimr.toml` settings.
///
pub use config::should_auto_compile;

/// Compiles all routes from controller files.
///
/// Parses controllers, validates route annotations, and
/// generates Gleam route files grouped by path prefix.
///
pub fn compile(verbose: bool) {
    if verbose {
        eprintln!("Compiling routes...");
    }

    // Load route groups from config
    let groups = config::read_route_groups();

    // Find all controller files
    let controller_files = match parser::find_controller_files() {
        Ok(files) => files,
        Err(e) => {
            eprintln!("{}Error finding controllers: {}{}", RED, e, NC);
            process::exit(1);
        }
    };

    if controller_files.is_empty() {
        eprintln!("{}No controllers found{}", YELLOW, NC);

        return;
    }

    let mut all_routes = Vec::new();

    // Parse all controllers
    for file in &controller_files {
        match parser::parse_controller(file) {
            Ok(routes) => all_routes.extend(routes),
            Err(e) => {
                eprintln!("{}Error parsing {}: {}{}", RED, file, e, NC);
                process::exit(1);
            }
        }
    }

    if all_routes.is_empty() {
        eprintln!("{}No routes found in controllers{}", YELLOW, NC);

        return;
    }

    // Validate routes
    if let Err(e) = validator::validate_routes(&all_routes) {
        eprintln!("{}{}{}", RED, e, NC);
        process::exit(1);
    }

    let mut total_routes = 0;

    // Compile each group
    for group in &groups {
        let count = generator::compile_group(group, &all_routes, &groups, verbose);

        total_routes += count;
    }

    if verbose {
        eprintln!("{}Routes compiled successfully{}", GREEN, NC);
    } else {
        println!("{}Compiled {} routes{}", GREEN, total_routes, NC);
    }
}
