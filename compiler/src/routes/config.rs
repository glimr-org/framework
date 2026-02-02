use super::patterns;
use std::fs;
use std::path::Path;
use std::time::SystemTime;

// ------------------------------------------------------------- Public Consts

/// Shared with parser.rs so it can find controller files
/// in the same location we check for staleness.
///
pub(super) const CONTROLLER_DIR: &str = "src/app/http/controllers";

/// Where compiled route files are written. Used to check
/// if recompilation is needed by comparing mtimes.
///
pub(super) const COMPILED_DIR: &str = "src/compiled/routes";

// ------------------------------------------------------------- Private Consts

/// Gleam config file that defines route groups. Parsed to
/// determine how routes are split across output files.
///
const CONFIG_ROUTE_FILE: &str = "src/config/config_route.gleam";

/// User's project config file. Checked for auto_compile
/// setting to decide if routes should compile on run/build.
///
const GLIMR_TOML: &str = "glimr.toml";

// ------------------------------------------------------------- Public Types

/// Determines which output file a route belongs to based on
/// path prefix matching. Routes with path `/api/users` and
/// a group with prefix `/api` go into `api.gleam`.
///
#[derive(Debug, Clone)]
pub struct RouteGroup {
    pub name: String,
    pub prefix: String,
}

// ------------------------------------------------------------- Public Functions

/// Entry point for the CLI's --check-only flag. Returns true
/// if routes should be compiled (auto_compile enabled AND
/// source files are newer than compiled output).
///
pub fn should_auto_compile() -> bool {
    has_auto_compile() && are_stale()
}

/// Checks if the user has disabled auto_compile in their
/// glimr.toml. Defaults to true so routes compile unless
/// explicitly disabled with `auto_compile = false`.
///
pub fn has_auto_compile() -> bool {
    let content = match fs::read_to_string(GLIMR_TOML) {
        Ok(content) => content,
        Err(_) => return true,
    };

    let mut in_routes_section = false;

    for line in content.lines() {
        let trimmed = line.trim();

        if trimmed.starts_with('[') {
            in_routes_section = trimmed == "[routes]";
        }

        if in_routes_section && trimmed.contains("auto_compile") && trimmed.contains("false") {
            return false;
        }
    }

    true
}

/// Reads route group configuration to determine how routes
/// should be split across output files. Falls back to a
/// single "web" group if no config exists.
///
pub fn read_route_groups() -> Vec<RouteGroup> {
    let content = match fs::read_to_string(CONFIG_ROUTE_FILE) {
        Ok(content) => content,
        Err(_) => {
            return vec![RouteGroup {
                name: "web".to_string(),
                prefix: "".to_string(),
            }];
        }
    };

    let mut groups = Vec::new();
    let mut current_name: Option<String> = None;
    let mut current_prefix: Option<String> = None;
    let mut in_config = false;

    for line in content.lines() {
        if line.contains("RouteGroupConfig(") {
            in_config = true;
            current_name = None;
            current_prefix = None;
        }

        if in_config {
            if let Some(caps) = patterns::CONFIG_NAME.captures(line) {
                current_name = Some(caps[1].to_string());
            }
            if let Some(caps) = patterns::CONFIG_PREFIX.captures(line) {
                current_prefix = Some(caps[1].to_string());
            }
            if line.contains(')') {
                if let Some(name) = current_name.take() {
                    groups.push(RouteGroup {
                        name,
                        prefix: current_prefix.take().unwrap_or_default(),
                    });
                }
                in_config = false;
            }
        }
    }

    if groups.is_empty() {
        vec![RouteGroup {
            name: "web".to_string(),
            prefix: "".to_string(),
        }]
    } else {
        groups
    }
}

/// Compares controller mtimes against compiled output to
/// avoid unnecessary recompilation. Returns true if any
/// controller is newer than the oldest compiled file.
///
pub fn are_stale() -> bool {
    if !Path::new(COMPILED_DIR).exists() {
        return true;
    }

    let compiled_files: Vec<_> = match fs::read_dir(COMPILED_DIR) {
        Ok(entries) => entries
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().map(|x| x == "gleam").unwrap_or(false))
            .collect(),
        Err(_) => return true,
    };

    if compiled_files.is_empty() {
        return true;
    }

    if !Path::new(CONTROLLER_DIR).exists() {
        return false;
    }

    let newest_controller = get_newest_mtime(CONTROLLER_DIR);
    let oldest_compiled = get_oldest_mtime_in_dir(COMPILED_DIR);

    match (newest_controller, oldest_compiled) {
        (Some(ctrl), Some(comp)) => ctrl > comp,
        (Some(_), None) => true,
        (None, _) => false,
    }
}

// ------------------------------------------------------------- Private Functions

/// Recursively finds the newest mtime among all .gleam files
/// in a directory tree. Used to find the most recently
/// modified controller file.
///
fn get_newest_mtime(dir: &str) -> Option<SystemTime> {
    let mut newest: Option<SystemTime> = None;

    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.filter_map(|e| e.ok()) {
            let path = entry.path();

            if path.is_dir() {
                if let Some(sub_newest) = get_newest_mtime(path.to_str().unwrap()) {
                    newest = match newest {
                        Some(n) if sub_newest > n => Some(sub_newest),
                        None => Some(sub_newest),
                        _ => newest,
                    };
                }
            } else if path.extension().map(|x| x == "gleam").unwrap_or(false)
                && let Ok(meta) = fs::metadata(&path)
                && let Ok(mtime) = meta.modified()
            {
                newest = match newest {
                    Some(n) if mtime > n => Some(mtime),
                    None => Some(mtime),
                    _ => newest,
                };
            }
        }
    }

    newest
}

/// Finds the oldest mtime in the compiled directory (non-
/// recursive). If any compiled file is older than the newest
/// controller, we need to recompile.
///
fn get_oldest_mtime_in_dir(dir: &str) -> Option<SystemTime> {
    let mut oldest: Option<SystemTime> = None;

    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.filter_map(|e| e.ok()) {
            let path = entry.path();

            if path.extension().map(|x| x == "gleam").unwrap_or(false)
                && let Ok(meta) = fs::metadata(&path)
                && let Ok(mtime) = meta.modified()
            {
                oldest = match oldest {
                    Some(o) if mtime < o => Some(mtime),
                    None => Some(mtime),
                    _ => oldest,
                };
            }
        }
    }

    oldest
}

/// Testable version of has_auto_compile that takes content
/// directly instead of reading from the filesystem.
///
#[cfg(test)]
fn parse_auto_compile(content: &str) -> bool {
    let mut in_routes_section = false;

    for line in content.lines() {
        let trimmed = line.trim();

        if trimmed.starts_with('[') {
            in_routes_section = trimmed == "[routes]";
        }

        if in_routes_section && trimmed.contains("auto_compile") && trimmed.contains("false") {
            return false;
        }
    }

    true
}

/// Testable version of read_route_groups that takes content
/// directly instead of reading from the filesystem.
///
#[cfg(test)]
fn parse_route_groups(content: &str) -> Vec<RouteGroup> {
    let mut groups = Vec::new();
    let mut current_name: Option<String> = None;
    let mut current_prefix: Option<String> = None;
    let mut in_config = false;

    for line in content.lines() {
        if line.contains("RouteGroupConfig(") {
            in_config = true;
            current_name = None;
            current_prefix = None;
        }

        if in_config {
            if let Some(caps) = super::patterns::CONFIG_NAME.captures(line) {
                current_name = Some(caps[1].to_string());
            }
            if let Some(caps) = super::patterns::CONFIG_PREFIX.captures(line) {
                current_prefix = Some(caps[1].to_string());
            }
            if line.contains(')') {
                if let Some(name) = current_name.take() {
                    groups.push(RouteGroup {
                        name,
                        prefix: current_prefix.take().unwrap_or_default(),
                    });
                }
                in_config = false;
            }
        }
    }

    if groups.is_empty() {
        vec![RouteGroup {
            name: "web".to_string(),
            prefix: "".to_string(),
        }]
    } else {
        groups
    }
}

// ------------------------------------------------------------- Unit Tests

#[cfg(test)]
mod tests {
    use super::*;

    // ----------------------------------------- parse_auto_compile tests

    #[test]
    fn test_parse_auto_compile_default_true() {
        let content = "";
        assert!(parse_auto_compile(content));
    }

    #[test]
    fn test_parse_auto_compile_explicit_true() {
        let content = r#"
[routes]
auto_compile = true
"#;
        assert!(parse_auto_compile(content));
    }

    #[test]
    fn test_parse_auto_compile_false() {
        let content = r#"
[routes]
auto_compile = false
"#;
        assert!(!parse_auto_compile(content));
    }

    #[test]
    fn test_parse_auto_compile_different_section() {
        let content = r#"
[other]
auto_compile = false

[routes]
auto_compile = true
"#;
        assert!(parse_auto_compile(content));
    }

    #[test]
    fn test_parse_auto_compile_false_in_routes_section() {
        let content = r#"
[other]
auto_compile = true

[routes]
auto_compile = false
"#;
        assert!(!parse_auto_compile(content));
    }

    // ----------------------------------------- parse_route_groups tests

    #[test]
    fn test_parse_route_groups_default() {
        let content = "";
        let groups = parse_route_groups(content);
        assert_eq!(groups.len(), 1);
        assert_eq!(groups[0].name, "web");
        assert_eq!(groups[0].prefix, "");
    }

    #[test]
    fn test_parse_route_groups_single() {
        let content = r#"
pub fn groups() {
  [
    RouteGroupConfig(
      name: "web",
      prefix: "",
    ),
  ]
}
"#;
        let groups = parse_route_groups(content);
        assert_eq!(groups.len(), 1);
        assert_eq!(groups[0].name, "web");
        assert_eq!(groups[0].prefix, "");
    }

    #[test]
    fn test_parse_route_groups_multiple() {
        let content = r#"
pub fn groups() {
  [
    RouteGroupConfig(
      name: "web",
      prefix: "",
    ),
    RouteGroupConfig(
      name: "api",
      prefix: "/api",
    ),
  ]
}
"#;
        let groups = parse_route_groups(content);
        assert_eq!(groups.len(), 2);
        assert_eq!(groups[0].name, "web");
        assert_eq!(groups[0].prefix, "");
        assert_eq!(groups[1].name, "api");
        assert_eq!(groups[1].prefix, "/api");
    }

    #[test]
    fn test_parse_route_groups_with_v2() {
        let content = r#"
pub fn groups() {
  [
    RouteGroupConfig(
      name: "web",
      prefix: "",
    ),
    RouteGroupConfig(
      name: "api",
      prefix: "/api",
    ),
    RouteGroupConfig(
      name: "api_v2",
      prefix: "/api/v2",
    ),
  ]
}
"#;
        let groups = parse_route_groups(content);
        assert_eq!(groups.len(), 3);
        assert_eq!(groups[2].name, "api_v2");
        assert_eq!(groups[2].prefix, "/api/v2");
    }

    #[test]
    fn test_parse_route_groups_single_line() {
        let content = r#"
RouteGroupConfig(name: "api", prefix: "/api")
"#;
        let groups = parse_route_groups(content);
        assert_eq!(groups.len(), 1);
        assert_eq!(groups[0].name, "api");
        assert_eq!(groups[0].prefix, "/api");
    }
}
