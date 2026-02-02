//! Configuration and Path Utilities
//!
//! Handles reading loom settings from glimr.toml, path
//! transformations between source and output files, and
//! staleness detection for incremental compilation.
//!

use crate::common::toml;
use std::collections::HashSet;
use std::fs;
use std::path::Path;
use std::time::SystemTime;

// ------------------------------------------------------------- Public Consts

/// Root directory containing loom template files (.loom.html).
/// Templates are organized with components in subdirectories
/// and pages at the root level for routing.
///
pub const VIEWS_PATH: &str = "src/resources/views/";

/// Directory containing view files (.gleam) that define Data
/// types for templates. Mirrors the views directory structure
/// so each template has a corresponding view file.
///
pub const APP_LOOM_PATH: &str = "src/app/loom/";

/// Output directory for compiled Gleam files. Generated files
/// mirror the source directory structure to maintain clear
/// relationships between templates and their compiled output.
///
pub const OUTPUT_PATH: &str = "src/compiled/loom/";

// ------------------------------------------------------------- Private Consts

/// User's project config file where loom settings are stored.
/// Checked for auto_compile setting under [loom] section
/// to determine if templates should compile automatically.
///
const GLIMR_TOML: &str = "glimr.toml";

// ------------------------------------------------------------- Public Functions

/// Checks if the user has enabled auto_compile in glimr.toml.
/// Defaults to true so loom compiles unless explicitly disabled.
///
pub fn read_auto_compile() -> bool {
    fs::read_to_string(GLIMR_TOML)
        .ok()
        .and_then(|content| toml::parse_section_bool(&content, "loom", "auto_compile"))
        .unwrap_or(true)
}

/// Compares source file mtimes against compiled output to
/// detect if recompilation is needed. Returns true if any
/// template or view file is newer than its compiled output.
///
pub fn loom_is_stale() -> bool {
    !find_stale_templates().is_empty()
}

/// Finds all template paths that need recompilation.
/// Checks both .loom.html templates and .gleam view files,
/// returning the template path for any that are stale.
///
pub fn find_stale_templates() -> Vec<String> {
    let mut stale = HashSet::new();

    if !Path::new(OUTPUT_PATH).exists() {
        // No output directory - everything is stale
        return find_loom_files(VIEWS_PATH);
    }

    // Check templates
    let templates = find_loom_files(VIEWS_PATH);
    for template in templates {
        let output = path_to_output_path(&template);
        if is_source_newer(&template, &output) {
            stale.insert(template);
        }
    }

    // Check view files - if view file is newer, its template is stale
    let view_files = find_view_files(APP_LOOM_PATH);
    for view_file in &view_files {
        let template = view_file_to_template_path(view_file);
        let output = path_to_output_path(&template);
        if is_source_newer(view_file, &output)
            && !stale.contains(&template)
            && Path::new(&template).exists()
        {
            stale.insert(template);
        }
    }

    stale.into_iter().collect()
}

/// Recursively finds all .loom.html files in a directory.
/// Used to discover templates for compilation and to scan
/// for component usage across the project.
///
pub fn find_loom_files(dir: &str) -> Vec<String> {
    let mut files = Vec::new();
    collect_files_with_ext(dir, ".loom.html", &mut files);
    files
}

/// Finds all view files (.gleam) in the app/loom directory.
/// View files define the Data types that templates use
/// for type-safe data binding.
///
pub fn find_view_files(dir: &str) -> Vec<String> {
    let mut files = Vec::new();
    collect_files_with_ext(dir, ".gleam", &mut files);
    files
}

/// Finds all component view files in app/loom/components.
/// Component view files define Data types and props for
/// reusable template components.
///
pub fn find_component_view_files() -> Vec<String> {
    let components_dir = format!("{}components/", APP_LOOM_PATH);
    find_view_files(&components_dir)
}

/// Finds all templates that use a specific component.
/// Scans all .loom.html files for `<x-{component_name}`.
///
pub fn find_templates_using_component(component_name: &str) -> Vec<String> {
    let tag = format!("<x-{}", component_name);
    let all_files = find_loom_files(VIEWS_PATH);

    all_files
        .into_iter()
        .filter(|path| {
            fs::read_to_string(path)
                .map(|content| content.contains(&tag))
                .unwrap_or(false)
        })
        .collect()
}

/// Converts a template path to its output path.
/// Transforms views/home.loom.html to compiled/loom/home.gleam
/// maintaining the same directory structure.
///
pub fn path_to_output_path(path: &str) -> String {
    let stripped = path
        .strip_prefix(VIEWS_PATH)
        .unwrap_or(path)
        .strip_suffix(".loom.html")
        .unwrap_or(path);

    format!("{}{}.gleam", OUTPUT_PATH, stripped)
}

/// Converts a template path to its view file path.
/// Transforms views/home.loom.html to app/loom/home.gleam
/// to locate the Data type definition for a template.
///
pub fn path_to_view_file(path: &str) -> String {
    let stripped = path
        .strip_prefix(VIEWS_PATH)
        .unwrap_or(path)
        .strip_suffix(".loom.html")
        .unwrap_or(path);

    format!("{}{}.gleam", APP_LOOM_PATH, stripped)
}

/// Converts a view file path to its template path.
/// Transforms app/loom/home.gleam to views/home.loom.html
/// to find the template associated with a view file.
///
pub fn view_file_to_template_path(path: &str) -> String {
    let stripped = path
        .strip_prefix(APP_LOOM_PATH)
        .unwrap_or(path)
        .strip_suffix(".gleam")
        .unwrap_or(path);

    format!("{}{}.loom.html", VIEWS_PATH, stripped)
}

/// Converts a template path to its Gleam module name.
/// Transforms views/home.loom.html to home and
/// views/components/btn.loom.html to components/btn.
///
pub fn path_to_module_name(path: &str) -> String {
    path.strip_prefix(VIEWS_PATH)
        .unwrap_or(path)
        .strip_suffix(".loom.html")
        .unwrap_or(path)
        .to_string()
}

/// Converts a template path to component name using colon
/// notation for nested paths. Transforms
/// components/forms/input.loom.html to forms:input.
///
pub fn component_name_from_template_path(path: &str) -> String {
    let module = path_to_module_name(path);
    let stripped = module.strip_prefix("components/").unwrap_or(&module);
    stripped.replace('/', ":")
}

/// Converts a view file path to component name using colon
/// notation for nested paths. Transforms
/// app/loom/components/forms/input.gleam to forms:input.
///
pub fn component_name_from_view_path(path: &str) -> String {
    let stripped = path
        .strip_prefix(APP_LOOM_PATH)
        .unwrap_or(path)
        .strip_prefix("components/")
        .unwrap_or(path)
        .strip_suffix(".gleam")
        .unwrap_or(path);
    stripped.replace('/', ":")
}

/// Checks if a path refers to a component file by looking
/// for "components/" in the path. Used to distinguish
/// between page templates and reusable components.
///
pub fn is_component_path(path: &str) -> bool {
    path.contains("components/")
}

/// Ensures the parent directory exists for a file path.
/// Creates all intermediate directories as needed before
/// writing compiled output files.
///
pub fn ensure_directory_exists(file_path: &str) {
    if let Some(parent) = Path::new(file_path).parent() {
        let _ = fs::create_dir_all(parent);
    }
}

// ------------------------------------------------------------- Private Functions

/// Recursively collects files with a specific extension.
/// Walks the directory tree depth-first and accumulates
/// matching files into the provided vector.
///
fn collect_files_with_ext(dir: &str, ext: &str, files: &mut Vec<String>) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.filter_map(|e| e.ok()) {
            let path = entry.path();

            if path.is_dir() {
                collect_files_with_ext(path.to_str().unwrap(), ext, files);
            } else if let Some(path_str) = path.to_str()
                && path_str.ends_with(ext)
            {
                files.push(path_str.to_string());
            }
        }
    }
}

/// Checks if a source file is newer than its output.
/// Returns true if the source exists but output doesn't,
/// or if source mtime is greater than output mtime.
///
fn is_source_newer(source: &str, output: &str) -> bool {
    let source_mtime = get_mtime(source);
    let output_mtime = get_mtime(output);

    match (source_mtime, output_mtime) {
        (Some(s), Some(o)) => s > o,
        (Some(_), None) => true, // Source exists but output doesn't
        _ => false,
    }
}

/// Gets the modification time of a file.
/// Returns None if the file doesn't exist or metadata
/// cannot be read.
///
fn get_mtime(path: &str) -> Option<SystemTime> {
    fs::metadata(path).ok().and_then(|m| m.modified().ok())
}

// ------------------------------------------------------------- Unit Tests

#[cfg(test)]
mod tests {
    use super::*;

    // ----------------------------------------- path conversion tests

    #[test]
    fn test_path_to_output_path() {
        assert_eq!(
            path_to_output_path("src/resources/views/home.loom.html"),
            "src/compiled/loom/home.gleam"
        );
    }

    #[test]
    fn test_path_to_output_path_nested() {
        assert_eq!(
            path_to_output_path("src/resources/views/components/forms/input.loom.html"),
            "src/compiled/loom/components/forms/input.gleam"
        );
    }

    #[test]
    fn test_path_to_view_file() {
        assert_eq!(
            path_to_view_file("src/resources/views/home.loom.html"),
            "src/app/loom/home.gleam"
        );
    }

    #[test]
    fn test_view_file_to_template_path() {
        assert_eq!(
            view_file_to_template_path("src/app/loom/home.gleam"),
            "src/resources/views/home.loom.html"
        );
    }

    #[test]
    fn test_path_to_module_name() {
        assert_eq!(
            path_to_module_name("src/resources/views/home.loom.html"),
            "home"
        );
        assert_eq!(
            path_to_module_name("src/resources/views/components/btn.loom.html"),
            "components/btn"
        );
    }

    #[test]
    fn test_component_name_from_template_path() {
        assert_eq!(
            component_name_from_template_path("src/resources/views/components/btn.loom.html"),
            "btn"
        );
        assert_eq!(
            component_name_from_template_path(
                "src/resources/views/components/forms/input.loom.html"
            ),
            "forms:input"
        );
    }

    #[test]
    fn test_component_name_from_view_path() {
        assert_eq!(
            component_name_from_view_path("src/app/loom/components/btn.gleam"),
            "btn"
        );
        assert_eq!(
            component_name_from_view_path("src/app/loom/components/forms/input.gleam"),
            "forms:input"
        );
    }

    #[test]
    fn test_is_component_path() {
        assert!(is_component_path(
            "src/resources/views/components/btn.loom.html"
        ));
        assert!(is_component_path(
            "src/resources/views/components/layouts/app.loom.html"
        ));
        assert!(!is_component_path("src/resources/views/home.loom.html"));
    }
}
