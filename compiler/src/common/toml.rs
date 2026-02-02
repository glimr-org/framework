//! TOML Parsing
//!
//! Lightweight parsing for glimr.toml configuration. Handles
//! comments, sections, and basic value types without requiring
//! a full TOML parsing library.
//!

// ------------------------------------------------------------- Public Functions

/// Extracts a boolean value from a named section in TOML
/// content. Searches for the specified key within the target
/// section only, ignoring keys with the same name in other
/// sections.
///
pub fn parse_section_bool(content: &str, section: &str, key: &str) -> Option<bool> {
    let section_header = format!("[{}]", section);
    let mut in_section = false;

    for line in content.lines() {
        let trimmed = line.trim();

        // Skip empty lines and comments
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }

        // Check for section headers
        if trimmed.starts_with('[') {
            in_section = trimmed == section_header;
            continue;
        }

        if in_section && let Some(value) = parse_bool(trimmed, key) {
            return Some(value);
        }
    }

    None
}

// ------------------------------------------------------------- Private Functions

/// Parses a boolean value from a single TOML line if the key
/// matches. Strips inline comments and normalizes whitespace
/// before comparing key names and parsing the value.
///
fn parse_bool(line: &str, key: &str) -> Option<bool> {
    // Strip inline comments
    let line = line.split('#').next()?.trim();

    // Split on '=' and check the key
    let mut parts = line.splitn(2, '=');
    let line_key = parts.next()?.trim();
    let value = parts.next()?.trim();

    if line_key != key {
        return None;
    }

    match value {
        "true" => Some(true),
        "false" => Some(false),
        _ => None,
    }
}

// ------------------------------------------------------------- Unit Tests

#[cfg(test)]
mod tests {
    use super::*;

    // ----------------------------------------- parse_section_bool tests

    #[test]
    fn test_parse_section_bool_found() {
        let content = r#"
[loom]
auto_compile = false
"#;
        assert_eq!(
            parse_section_bool(content, "loom", "auto_compile"),
            Some(false)
        );
    }

    #[test]
    fn test_parse_section_bool_not_found() {
        let content = r#"
[loom]
other_key = true
"#;
        assert_eq!(parse_section_bool(content, "loom", "auto_compile"), None);
    }

    #[test]
    fn test_parse_section_bool_wrong_section() {
        let content = r#"
[routes]
auto_compile = false
"#;
        assert_eq!(parse_section_bool(content, "loom", "auto_compile"), None);
    }

    #[test]
    fn test_parse_section_bool_ignores_comments() {
        let content = r#"
[loom]
# auto_compile = false
auto_compile = true
"#;
        assert_eq!(
            parse_section_bool(content, "loom", "auto_compile"),
            Some(true)
        );
    }

    #[test]
    fn test_parse_section_bool_ignores_inline_comments() {
        let content = r#"
[loom]
auto_compile = true  # false would disable
"#;
        assert_eq!(
            parse_section_bool(content, "loom", "auto_compile"),
            Some(true)
        );
    }

    #[test]
    fn test_parse_section_bool_multiple_sections() {
        let content = r#"
[routes]
auto_compile = true

[loom]
auto_compile = false
"#;
        assert_eq!(
            parse_section_bool(content, "loom", "auto_compile"),
            Some(false)
        );
        assert_eq!(
            parse_section_bool(content, "routes", "auto_compile"),
            Some(true)
        );
    }
}
