mod config;
mod generator;
mod parser;
mod validator;
mod patterns;

use std::env;
use std::process;

const GREEN: &str = "\x1b[0;32m";
const RED: &str = "\x1b[0;31m";
const YELLOW: &str = "\x1b[0;33m";
const NC: &str = "\x1b[0m";

fn main() {
    let args: Vec<String> = env::args().collect();

    // Parse command and flags
    let command = args.get(1).map(|s| s.as_str()).unwrap_or("");
    let verbose = args.iter().any(|a| a == "-v" || a == "--verbose");
    let check_only = args.iter().any(|a| a == "--check-only");

    match command {
        "routes" => {
            if check_only {
                // Check auto_compile setting and staleness
                if config::read_auto_compile() && config::routes_are_stale() {
                    process::exit(0); // Should compile
                } else {
                    process::exit(1); // Skip
                }
            }
            compile_routes(verbose);
        }
        "routes:check" => {
            // Check staleness
            if config::routes_are_stale() {
                println!("stale");
            } else {
                println!("fresh");
            }
        }
        "--help" | "-h" | "" => {
            println!("glimr-compiler - Fast compiler for Glimr framework");
            println!();
            println!("Usage: glimr-compiler <command> [options]");
            println!();
            println!("Commands:");
            println!("  routes          Compile controller routes");
            println!("  routes:check    Check if routes need recompilation");
            println!();
            println!("Options:");
            println!("  -v, --verbose   Show detailed output");
            println!("  --check-only    Exit 0 if stale, 1 if fresh");
        }
        _ => {
            eprintln!("{}Unknown command: {}{}", RED, command, NC);
            process::exit(1);
        }
    }
}

fn compile_routes(verbose: bool) {
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

    // Parse all controllers
    let mut all_routes = Vec::new();
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

    // Compile each group
    let mut total_routes = 0;
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
