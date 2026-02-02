//! Loom Template Compiler
//!
//! Compiles Loom template files (.loom.html) to executable Gleam
//! code. Templates are parsed, validated, and transformed into
//! type-safe render functions that integrate with Wisp's server.
//! Handles dependency tracking between components and their
//! consumers for incremental compilation during watch mode.
//!

mod config;
mod generator;
mod gleam_parser;
mod lexer;
mod parser;
mod validator;

use crate::common::colors::{GREEN, NC, RED, YELLOW};
use config::VIEWS_PATH;
use generator::{ComponentDataMap, ComponentSlotMap};
use std::collections::HashSet;
use std::fs;
use std::process::Command;

// ------------------------------------------------------------- Public Functions

/// Compiles all loom template files in the project. Discovers
/// templates in the views directory, partitions them into
/// components and pages, then compiles in dependency order.
/// Components are compiled first since pages may reference them.
///
pub fn compile(verbose: bool) {
    let all_files = config::find_loom_files(VIEWS_PATH);

    if all_files.is_empty() {
        println!("{}No loom templates found{}", YELLOW, NC);
        return;
    }

    if verbose {
        println!("{}Compiling loom templates...{}", YELLOW, NC);
    }

    let component_data = build_component_data_map();
    let (component_files, page_files) = partition_files(all_files);
    let component_slots = build_component_slot_map(&component_files);

    // Compile components first (they may depend on other components), then pages
    let mut compiled_files =
        compile_files(&component_files, &component_data, &component_slots, verbose);
    compiled_files.extend(compile_files(
        &page_files,
        &component_data,
        &component_slots,
        verbose,
    ));

    format_generated_files();
    validator::validate_generated_files(&compiled_files);
    print_compiled_count(compiled_files.len());
}

/// Compiles only stale loom files for incremental builds. When
/// a component changes, this also recompiles all templates that
/// depend on it. This enables fast iteration during development
/// by avoiding full recompilation of unchanged templates.
///
pub fn compile_stale(verbose: bool) {
    let stale_files = config::find_stale_templates();

    if stale_files.is_empty() {
        return;
    }

    if verbose {
        let count = stale_files.len();
        if count == 1 {
            println!("{}Compiling 1 stale loom template...{}", YELLOW, NC);
        } else {
            println!(
                "{}Compiling {} stale loom templates...{}",
                YELLOW, count, NC
            );
        }
    }

    // Build set of templates to compile (stale + dependents of stale components)
    let mut to_compile_set: HashSet<String> = HashSet::new();

    for path in &stale_files {
        to_compile_set.insert(path.clone());

        if config::is_component_path(path) {
            // Find templates that use this component
            let component_name = config::component_name_from_template_path(path);
            let dependents = config::find_templates_using_component(&component_name);
            to_compile_set.extend(dependents);
        }
    }

    let templates_to_compile: Vec<_> = to_compile_set.into_iter().collect();

    let component_data = build_component_data_map();
    let component_slots = build_component_slot_map_all();
    let (component_files, page_files) = partition_files(templates_to_compile);

    // Compile components first, then pages
    let mut compiled_files =
        compile_files(&component_files, &component_data, &component_slots, verbose);
    compiled_files.extend(compile_files(
        &page_files,
        &component_data,
        &component_slots,
        verbose,
    ));

    if !compiled_files.is_empty() {
        format_generated_files();
        validator::validate_generated_files(&compiled_files);
        print_compiled_count(compiled_files.len());
    }
}

/// Checks if loom compilation is needed for watch mode. Returns
/// true if any source template file is newer than its compiled
/// Gleam output. Returns false early if auto_compile is disabled
/// in the project config, avoiding unnecessary file stat checks.
///
pub fn should_auto_compile() -> bool {
    if !config::read_auto_compile() {
        return false;
    }
    config::loom_is_stale()
}

// ------------------------------------------------------------- Private Functions

/// Partitions template files into components and pages. This
/// separation is needed because components must be compiled
/// before pages to ensure slot information is available when
/// pages reference those components.
///
fn partition_files(files: Vec<String>) -> (Vec<String>, Vec<String>) {
    files.into_iter().partition(|f| f.contains("components/"))
}

/// Compiles a list of template files and collects the results.
/// Failed compilations are filtered out with errors printed to
/// stderr. Returns compiled file info for validation and
/// potential rollback if the generated code fails type checking.
///
fn compile_files(
    files: &[String],
    component_data: &ComponentDataMap,
    component_slots: &ComponentSlotMap,
    verbose: bool,
) -> Vec<validator::CompiledFile> {
    files
        .iter()
        .filter_map(|file| compile_file(file, component_data, component_slots, verbose))
        .collect()
}

/// Prints the compilation success message with proper grammar.
/// Uses singular "template" for count of 1 and plural otherwise.
/// This provides user feedback that the compilation completed
/// and shows how many files were processed.
///
fn print_compiled_count(count: usize) {
    if count == 1 {
        println!("{}Compiled 1 loom template{}", GREEN, NC);
    } else {
        println!("{}Compiled {} loom templates{}", GREEN, count, NC);
    }
}

/// Compiles a single template file through the full pipeline.
/// Reads the source, tokenizes, parses, generates Gleam code,
/// and writes the output. Returns compiled file info on success
/// for validation, or None if any stage fails with error output.
///
fn compile_file(
    path: &str,
    component_data: &ComponentDataMap,
    component_slots: &ComponentSlotMap,
    verbose: bool,
) -> Option<validator::CompiledFile> {
    let is_component = path.contains("components/");

    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(_) => {
            eprintln!("{}Failed to read file: {}{}", RED, path, NC);
            return None;
        }
    };

    let tokens = match lexer::tokenize(&content) {
        Ok(t) => t,
        Err(err) => {
            eprintln!(
                "{}Lexer error in {}: {}{}",
                RED,
                path,
                lexer::error_to_string(&err),
                NC
            );
            return None;
        }
    };

    let template = match parser::parse(tokens) {
        Ok(t) => t,
        Err(err) => {
            eprintln!(
                "{}Parser error in {}: {}{}",
                RED,
                path,
                parser::error_to_string(&err),
                NC
            );
            return None;
        }
    };

    let module_name = config::path_to_module_name(path);
    let view_file_path = config::path_to_view_file(path);
    let view_file = gleam_parser::parse_view_file(&view_file_path).ok();

    let generated = generator::generate(
        &template,
        &module_name,
        is_component,
        view_file.as_ref(),
        component_data,
        component_slots,
    );

    let output_file = config::path_to_output_path(path);
    config::ensure_directory_exists(&output_file);

    // Read previous content for potential revert
    let previous_content = fs::read_to_string(&output_file).unwrap_or_default();

    if fs::write(&output_file, &generated.code).is_err() {
        eprintln!("{}Failed to write: {}{}", RED, output_file, NC);
        return None;
    }

    if verbose {
        println!("  {} -> {}{}{}", path, GREEN, output_file, NC);
    }

    Some(validator::CompiledFile {
        output: output_file,
        previous: previous_content,
    })
}

/// Formats all generated files using the gleam formatter. This
/// ensures consistent code style and makes the generated output
/// readable for debugging. Failures are silently ignored since
/// formatting is not critical to compilation correctness.
///
fn format_generated_files() {
    let _ = Command::new("gleam")
        .args(["format", config::OUTPUT_PATH])
        .output();
}

/// Builds a map of component names to their Data type fields.
/// Parses each component's view file to extract field info that
/// the generator needs to produce correct component invocations.
/// Components without parseable view files are excluded.
///
fn build_component_data_map() -> ComponentDataMap {
    config::find_component_view_files()
        .into_iter()
        .filter_map(|path| {
            let parsed = gleam_parser::parse_view_file(&path).ok()?;
            let name = config::component_name_from_view_path(&path);
            Some((name, parsed.fields))
        })
        .collect()
}

/// Builds a map of component names to their slot definitions.
/// Parses each component template to extract which slots exist
/// and whether they are required or optional. Used by the
/// generator to validate slot usage in pages and components.
///
fn build_component_slot_map(component_files: &[String]) -> ComponentSlotMap {
    component_files
        .iter()
        .filter_map(|path| {
            let content = fs::read_to_string(path).ok()?;
            let tokens = lexer::tokenize(&content).ok()?;
            let template = parser::parse(tokens).ok()?;
            let name = config::component_name_from_template_path(path);
            Some((name, generator::extract_slot_info(&template)))
        })
        .collect()
}

/// Builds component slot map for all components in the project.
/// Used during incremental compilation when we need slot info
/// for components that weren't in the stale set but may be
/// referenced by stale pages being recompiled.
///
fn build_component_slot_map_all() -> ComponentSlotMap {
    let all_files = config::find_loom_files(VIEWS_PATH);
    let component_files: Vec<_> = all_files
        .into_iter()
        .filter(|f| f.contains("components/"))
        .collect();
    build_component_slot_map(&component_files)
}
