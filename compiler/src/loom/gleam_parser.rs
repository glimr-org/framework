//! Gleam Source Parser
//!
//! Parses Gleam source files to extract type definitions and
//! imports. Used by the template compiler to understand view
//! data structures for code generation.
//!

use std::fs;

// ------------------------------------------------------------- Public Types

/// Represents the parsed contents of a view file. Contains the
/// extracted Data type fields and import statements needed for
/// code generation.
///
#[derive(Debug, Clone)]
pub struct ParsedViewFile {
    /// Name-type pairs extracted from the Data type definition.
    /// Used to generate function parameters for the compiled
    /// template's html function.
    ///
    pub fields: Vec<(String, String)>,
    /// Import statements from the view file to be included in
    /// the generated module. Allows templates to use custom
    /// types defined elsewhere.
    ///
    pub imports: Vec<String>,
}

// ------------------------------------------------------------- Public Functions

/// Parses a view file to extract its Data type fields and
/// imports. Returns the parsed structure or Error if the
/// file cannot be read.
///
pub fn parse_view_file(path: &str) -> Result<ParsedViewFile, ()> {
    let content = fs::read_to_string(path).map_err(|_| ())?;

    let imports = extract_imports(&content);
    let fields = extract_type_fields(&content, "Data").unwrap_or_default();

    Ok(ParsedViewFile { fields, imports })
}

// ------------------------------------------------------------- Private Functions

/// Extracts all import statements from the source content.
/// Filters lines that start with "import " to capture both
/// qualified and aliased imports.
///
fn extract_imports(content: &str) -> Vec<String> {
    content
        .lines()
        .filter_map(|line| {
            let trimmed = line.trim();
            trimmed.starts_with("import ").then(|| trimmed.to_string())
        })
        .collect()
}

/// Extracts fields from a named type definition. Locates the
/// type by name and parses its constructor arguments into
/// field name-type pairs.
///
fn extract_type_fields(content: &str, type_name: &str) -> Option<Vec<(String, String)>> {
    let pattern = format!("pub type {} {{", type_name);

    let (_, after_type) = content.split_once(&pattern)?;

    // Find the constructor
    let constructor_pattern = format!("{}(", type_name);
    let (_, after_constructor) = after_type.split_once(&constructor_pattern)?;

    // Extract until closing paren
    let fields_str = find_matching_paren(after_constructor)?;

    Some(parse_fields(&fields_str))
}

/// Finds content within balanced parentheses. Tracks nesting
/// depth to correctly handle nested type expressions like
/// List(Option(String)).
///
fn find_matching_paren(input: &str) -> Option<String> {
    let mut depth = 0;
    let mut acc = String::new();

    for c in input.chars() {
        match c {
            '(' => {
                depth += 1;
                acc.push(c);
            }
            ')' => {
                if depth == 0 {
                    return Some(acc);
                }
                depth -= 1;
                acc.push(c);
            }
            _ => acc.push(c),
        }
    }

    None
}

/// Parses a comma-separated field string into name-type pairs.
/// Splits on depth-zero commas and extracts name:type from
/// each segment.
///
fn parse_fields(fields_str: &str) -> Vec<(String, String)> {
    split_on_commas_at_depth_zero(fields_str)
        .into_iter()
        .filter_map(|field| {
            let field = field.trim();
            let (name, type_str) = field.split_once(':')?;
            let name = name.trim();
            let type_str = type_str.trim();

            if name.is_empty() || type_str.is_empty() {
                None
            } else {
                Some((name.to_string(), type_str.to_string()))
            }
        })
        .collect()
}

/// Splits a string on commas, but only at nesting depth zero.
/// Prevents incorrect splitting within nested type parameters
/// like List(Int, String).
///
fn split_on_commas_at_depth_zero(input: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut current = String::new();
    let mut depth = 0;

    for c in input.chars() {
        match c {
            '(' => {
                depth += 1;
                current.push(c);
            }
            ')' => {
                depth -= 1;
                current.push(c);
            }
            ',' if depth == 0 => {
                let field = std::mem::take(&mut current);
                if !field.trim().is_empty() {
                    result.push(field);
                }
            }
            _ => current.push(c),
        }
    }

    if !current.trim().is_empty() {
        result.push(current);
    }

    result
}

// ------------------------------------------------------------- Unit Tests

#[cfg(test)]
mod tests {
    use super::*;

    // ----------------------------------------- extract_imports tests

    #[test]
    fn test_extract_imports() {
        let content = r#"
import gleam/string
import gleam/list

pub type Data {
  Data
}
"#;
        let imports = extract_imports(content);
        assert_eq!(imports.len(), 2);
        assert_eq!(imports[0], "import gleam/string");
        assert_eq!(imports[1], "import gleam/list");
    }

    // ----------------------------------------- extract_type_fields tests

    #[test]
    fn test_extract_type_fields_empty() {
        let content = r#"
pub type Data {
  Data
}
"#;
        let fields = extract_type_fields(content, "Data");
        assert_eq!(fields, None);
    }

    #[test]
    fn test_extract_type_fields_simple() {
        let content = r#"
pub type Data {
  Data(name: String, age: Int)
}
"#;
        let fields = extract_type_fields(content, "Data").unwrap();
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0], ("name".to_string(), "String".to_string()));
        assert_eq!(fields[1], ("age".to_string(), "Int".to_string()));
    }

    #[test]
    fn test_extract_type_fields_nested_types() {
        let content = r#"
pub type Data {
  Data(
    items: List(String),
    user: Option(#(String, Int)),
  )
}
"#;
        let fields = extract_type_fields(content, "Data").unwrap();
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0], ("items".to_string(), "List(String)".to_string()));
        assert_eq!(
            fields[1],
            ("user".to_string(), "Option(#(String, Int))".to_string())
        );
    }

    // ----------------------------------------- split_on_commas_at_depth_zero tests

    #[test]
    fn test_split_on_commas_at_depth_zero() {
        let input = "a: String, b: List(Int, String), c: Bool";
        let parts = split_on_commas_at_depth_zero(input);
        assert_eq!(parts.len(), 3);
        assert_eq!(parts[0].trim(), "a: String");
        assert_eq!(parts[1].trim(), "b: List(Int, String)");
        assert_eq!(parts[2].trim(), "c: Bool");
    }

    // ----------------------------------------- find_matching_paren tests

    #[test]
    fn test_find_matching_paren() {
        let input = "a: String, b: List(Int)) rest";
        let result = find_matching_paren(input).unwrap();
        assert_eq!(result, "a: String, b: List(Int)");
    }
}
