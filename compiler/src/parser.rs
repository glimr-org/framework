use regex::Regex;
use std::fs;
use std::io;
use std::path::Path;

const CONTROLLER_DIR: &str = "src/app/http/controllers";

#[derive(Debug, Clone)]
pub struct Route {
    pub method: String,
    pub path: String,
    pub handler: String,
    pub controller_path: String,
    pub middleware: Vec<String>,
    pub validator: Option<String>,
    pub redirect: Option<String>,
    pub redirect_permanent: bool,
    pub params: Vec<FunctionParam>,
    pub group_middleware: Vec<String>,
    pub return_type: String,
    pub has_wisp_response_import: bool,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub name: String,
    pub param_type: String,
}

/// Find all controller files recursively
pub fn find_controller_files() -> io::Result<Vec<String>> {
    let mut files = Vec::new();
    find_gleam_files(CONTROLLER_DIR, &mut files)?;
    Ok(files)
}

fn find_gleam_files(dir: &str, files: &mut Vec<String>) -> io::Result<()> {
    if !Path::new(dir).exists() {
        return Ok(());
    }

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            find_gleam_files(path.to_str().unwrap(), files)?;
        } else if path.extension().map(|x| x == "gleam").unwrap_or(false) {
            files.push(path.to_string_lossy().to_string());
        }
    }
    Ok(())
}

/// Parse a controller file and extract routes
pub fn parse_controller(path: &str) -> Result<Vec<Route>, String> {
    let content =
        fs::read_to_string(path).map_err(|e| format!("Failed to read {}: {}", path, e))?;

    let mut routes = Vec::new();

    // Check if Response is imported from wisp
    let has_wisp_response_import = check_wisp_response_import(&content);

    // Parse group-level middleware
    let group_middleware = parse_group_middleware(&content);

    // Regex patterns
    let method_re =
        Regex::new(r#"///\s*@(get|post|put|patch|delete|head|options)\s+"([^"]+)""#).unwrap();
    let middleware_re = Regex::new(r#"///\s*@middleware\s+"([^"]+)""#).unwrap();
    let validator_re = Regex::new(r#"///\s*@validator\s+"([^"]+)""#).unwrap();
    let redirect_re = Regex::new(r#"///\s*@redirect\s+"([^"]+)""#).unwrap();
    let redirect_perm_re = Regex::new(r#"///\s*@redirect_permanent\s+"([^"]+)""#).unwrap();
    let fn_re = Regex::new(r"pub\s+fn\s+(\w+)\s*\(").unwrap();

    let lines: Vec<&str> = content.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i];

        // Look for route annotation
        if let Some(caps) = method_re.captures(line) {
            let method = caps[1].to_uppercase();
            let route_path = caps[2].to_string();

            // Collect annotations until we hit the function
            let mut middleware = Vec::new();
            let mut validator = None;
            let mut redirect = None;
            let mut redirect_permanent = false;

            let _start = i;
            i += 1;

            // Collect middleware, validator, redirect annotations
            while i < lines.len() {
                let current = lines[i];

                if let Some(caps) = middleware_re.captures(current) {
                    middleware.push(caps[1].to_string());
                    i += 1;
                    continue;
                }
                if let Some(caps) = validator_re.captures(current) {
                    validator = Some(caps[1].to_string());
                    i += 1;
                    continue;
                }
                if let Some(caps) = redirect_re.captures(current) {
                    redirect = Some(caps[1].to_string());
                    i += 1;
                    continue;
                }
                if let Some(caps) = redirect_perm_re.captures(current) {
                    redirect = Some(caps[1].to_string());
                    redirect_permanent = true;
                    i += 1;
                    continue;
                }
                if current.trim().starts_with("///") || current.trim().is_empty() {
                    i += 1;
                    continue;
                }
                break;
            }

            // Find the function definition
            if i < lines.len() {
                // Collect function signature (may span multiple lines)
                let mut sig_lines = Vec::new();
                while i < lines.len() && !lines[i].contains('{') {
                    sig_lines.push(lines[i]);
                    i += 1;
                }
                if i < lines.len() {
                    sig_lines.push(lines[i]);
                }

                let signature = sig_lines.join("\n");

                if let Some(caps) = fn_re.captures(&signature) {
                    let handler = caps[1].to_string();
                    let params = parse_function_params(&signature);
                    let return_type = parse_return_type(&signature);

                    routes.push(Route {
                        method,
                        path: route_path,
                        handler,
                        controller_path: path.to_string(),
                        middleware,
                        validator,
                        redirect,
                        redirect_permanent,
                        params,
                        group_middleware: group_middleware.clone(),
                        return_type,
                        has_wisp_response_import,
                    });
                }
            }
        } else {
            i += 1;
        }
    }

    Ok(routes)
}

/// Parse group-level middleware from file comments
fn parse_group_middleware(content: &str) -> Vec<String> {
    let re = Regex::new(r#"//\s*@group_middleware\s+"([^"]+)""#).unwrap();
    re.captures_iter(content)
        .map(|c| c[1].to_string())
        .collect()
}

/// Parse function parameters from signature
fn parse_function_params(signature: &str) -> Vec<FunctionParam> {
    // Find content between ( and )
    let start = match signature.find('(') {
        Some(i) => i + 1,
        None => return Vec::new(),
    };

    // Find matching closing paren (handle nested parens in types)
    let chars: Vec<char> = signature.chars().collect();
    let mut depth = 1;
    let mut end = start;
    for i in start..chars.len() {
        match chars[i] {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    end = i;
                    break;
                }
            }
            _ => {}
        }
    }

    let params_str: String = chars[start..end].iter().collect();
    if params_str.trim().is_empty() {
        return Vec::new();
    }

    // Split by comma, but respect nested parens
    let mut params = Vec::new();
    let mut current = String::new();
    let mut depth = 0;

    for c in params_str.chars() {
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
                if !current.trim().is_empty() {
                    if let Some(param) = parse_single_param(&current) {
                        params.push(param);
                    }
                }
                current.clear();
            }
            _ => current.push(c),
        }
    }

    // Don't forget the last param
    if !current.trim().is_empty() {
        if let Some(param) = parse_single_param(&current) {
            params.push(param);
        }
    }

    params
}

/// Parse a single parameter "name: Type" or just "name"
fn parse_single_param(param: &str) -> Option<FunctionParam> {
    let trimmed = param.trim();
    if trimmed.is_empty() {
        return None;
    }

    // Handle "name: Type" format
    if let Some(colon_pos) = trimmed.find(':') {
        let name = trimmed[..colon_pos].trim();
        let param_type = trimmed[colon_pos + 1..].trim();

        // Strip leading underscore for matching purposes
        let clean_name = name.trim_start_matches('_').to_string();

        Some(FunctionParam {
            name: clean_name,
            param_type: param_type.to_string(),
        })
    } else {
        // Just a name without type
        let clean_name = trimmed.trim_start_matches('_').to_string();
        Some(FunctionParam {
            name: clean_name,
            param_type: String::new(),
        })
    }
}

/// Check if the file imports Response from wisp
/// Matches patterns like:
/// - import wisp.{type Response}
/// - import wisp.{type Request, type Response}
/// - import wisp.{Response}
fn check_wisp_response_import(content: &str) -> bool {
    let re = Regex::new(r"import\s+wisp\s*\.\s*\{[^}]*\bResponse\b[^}]*\}").unwrap();
    for line in content.lines() {
        let trimmed = line.trim();
        // Skip commented lines
        if trimmed.starts_with("//") {
            continue;
        }
        if re.is_match(line) {
            return true;
        }
    }
    false
}

/// Parse the return type from a function signature
/// e.g., "pub fn show() -> Response {" returns "Response"
/// e.g., "pub fn show() -> wisp.Response {" returns "wisp.Response"
fn parse_return_type(signature: &str) -> String {
    // Look for -> followed by the return type
    let re = Regex::new(r"->\s*([A-Za-z_][A-Za-z0-9_\.]*(?:\([^)]*\))?)").unwrap();

    if let Some(caps) = re.captures(signature) {
        caps[1].trim().to_string()
    } else {
        String::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ============================================================
    // parse_single_param tests
    // ============================================================

    #[test]
    fn test_parse_single_param_with_type() {
        let param = parse_single_param("name: String").unwrap();
        assert_eq!(param.name, "name");
        assert_eq!(param.param_type, "String");
    }

    #[test]
    fn test_parse_single_param_with_underscore() {
        let param = parse_single_param("_req: Request").unwrap();
        assert_eq!(param.name, "req");
        assert_eq!(param.param_type, "Request");
    }

    #[test]
    fn test_parse_single_param_without_type() {
        let param = parse_single_param("name").unwrap();
        assert_eq!(param.name, "name");
        assert_eq!(param.param_type, "");
    }

    #[test]
    fn test_parse_single_param_complex_type() {
        let param = parse_single_param("data: Option(String)").unwrap();
        assert_eq!(param.name, "data");
        assert_eq!(param.param_type, "Option(String)");
    }

    #[test]
    fn test_parse_single_param_qualified_type() {
        let param = parse_single_param("req: wisp.Request").unwrap();
        assert_eq!(param.name, "req");
        assert_eq!(param.param_type, "wisp.Request");
    }

    #[test]
    fn test_parse_single_param_empty() {
        assert!(parse_single_param("").is_none());
        assert!(parse_single_param("   ").is_none());
    }

    // ============================================================
    // parse_function_params tests
    // ============================================================

    #[test]
    fn test_parse_function_params_empty() {
        let params = parse_function_params("pub fn show() -> Response {");
        assert!(params.is_empty());
    }

    #[test]
    fn test_parse_function_params_single() {
        let params = parse_function_params("pub fn show(req: Request) -> Response {");
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].name, "req");
        assert_eq!(params[0].param_type, "Request");
    }

    #[test]
    fn test_parse_function_params_multiple() {
        let params = parse_function_params("pub fn show(req: Request, ctx: Context) -> Response {");
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].name, "req");
        assert_eq!(params[1].name, "ctx");
    }

    #[test]
    fn test_parse_function_params_nested_parens() {
        let params =
            parse_function_params("pub fn show(data: Option(String), req: Request) -> Response {");
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].name, "data");
        assert_eq!(params[0].param_type, "Option(String)");
        assert_eq!(params[1].name, "req");
    }

    #[test]
    fn test_parse_function_params_multiline() {
        let sig = "pub fn show(\n  req: Request,\n  ctx: Context\n) -> Response {";
        let params = parse_function_params(sig);
        assert_eq!(params.len(), 2);
    }

    // ============================================================
    // parse_return_type tests
    // ============================================================

    #[test]
    fn test_parse_return_type_simple() {
        let rt = parse_return_type("pub fn show() -> Response {");
        assert_eq!(rt, "Response");
    }

    #[test]
    fn test_parse_return_type_qualified() {
        let rt = parse_return_type("pub fn show() -> wisp.Response {");
        assert_eq!(rt, "wisp.Response");
    }

    #[test]
    fn test_parse_return_type_none() {
        let rt = parse_return_type("pub fn show() {");
        assert_eq!(rt, "");
    }

    #[test]
    fn test_parse_return_type_with_params() {
        let rt = parse_return_type("pub fn show(req: Request) -> Response {");
        assert_eq!(rt, "Response");
    }

    // ============================================================
    // check_wisp_response_import tests
    // ============================================================

    #[test]
    fn test_check_wisp_response_import_type() {
        let content = "import wisp.{type Response}\n\npub fn show() -> Response {";
        assert!(check_wisp_response_import(content));
    }

    #[test]
    fn test_check_wisp_response_import_value() {
        let content = "import wisp.{Response}\n\npub fn show() -> Response {";
        assert!(check_wisp_response_import(content));
    }

    #[test]
    fn test_check_wisp_response_import_multiple() {
        let content = "import wisp.{type Request, type Response}\n\npub fn show() -> Response {";
        assert!(check_wisp_response_import(content));
    }

    #[test]
    fn test_check_wisp_response_import_commented() {
        let content = "// import wisp.{type Response}\n\npub fn show() -> Response {";
        assert!(!check_wisp_response_import(content));
    }

    #[test]
    fn test_check_wisp_response_import_not_present() {
        let content = "import gleam/io\n\npub fn show() -> Response {";
        assert!(!check_wisp_response_import(content));
    }

    #[test]
    fn test_check_wisp_response_import_wrong_module() {
        let content = "import other.{type Response}\n\npub fn show() -> Response {";
        assert!(!check_wisp_response_import(content));
    }

    // ============================================================
    // parse_group_middleware tests
    // ============================================================

    #[test]
    fn test_parse_group_middleware_single() {
        let content = "// @group_middleware \"auth\"\n\npub fn show() {}";
        let mw = parse_group_middleware(content);
        assert_eq!(mw, vec!["auth"]);
    }

    #[test]
    fn test_parse_group_middleware_multiple() {
        let content = "// @group_middleware \"auth\"\n// @group_middleware \"logger\"\n";
        let mw = parse_group_middleware(content);
        assert_eq!(mw, vec!["auth", "logger"]);
    }

    #[test]
    fn test_parse_group_middleware_none() {
        let content = "pub fn show() {}";
        let mw = parse_group_middleware(content);
        assert!(mw.is_empty());
    }

    #[test]
    fn test_parse_group_middleware_wrong_format() {
        // Missing @ symbol
        let content = "// group_middleware \"auth\"\n";
        let mw = parse_group_middleware(content);
        assert!(mw.is_empty());
    }
}
