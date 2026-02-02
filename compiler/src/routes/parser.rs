use regex::Regex;
use std::collections::HashSet;
use std::fs;
use std::io;
use std::path::Path;

use super::config::CONTROLLER_DIR;
use super::patterns;

// ------------------------------------------------------------- Public Types

/// Used by the generator to produce Gleam routing code.
/// Contains all the information extracted from controller
/// annotations needed to generate route matching, middleware
/// chains, and handler calls.
///
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

/// Used to track handler function parameters so the generator
/// knows what arguments to pass when calling the handler (e.g.,
/// req, ctx, path params, validated data).
///
#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub name: String,
    pub param_type: String,
}

// ------------------------------------------------------------- Private Types

/// Holds file-level information that applies to all routes in a
/// controller, such as group middleware and whether Response is
/// imported from wisp.
///
struct FileMetadata {
    has_wisp_response_import: bool,
    group_middleware: Vec<String>,
}

/// Represents a single parsed annotation comment from the source.
/// Tracks the line number so we can match it to the function
/// definition that follows.
///
struct Annotation {
    line_num: usize,
    kind: AnnotationKind,
}

/// Holds parsed information about a public function definition.
/// Used to match annotations to their handler functions and
/// extract parameter/return type info.
///
#[derive(Clone)]
struct FunctionDef {
    line_num: usize,
    name: String,
    params: Vec<FunctionParam>,
    return_type: String,
}

/// Groups a route annotation with its associated middleware,
/// validator, and redirect annotations. Built by scanning
/// consecutive annotations before a function.
///
struct AnnotationBlock {
    route: Annotation,
    middleware: Vec<String>,
    validator: Option<String>,
    redirect: Option<String>,
    redirect_permanent: bool,
}

// ------------------------------------------------------------- Private Enums

/// Represents the different annotation types we can parse from
/// controller comments. Each variant corresponds to an `@`
/// directive like `@get`, `@middleware`, etc.
///
enum AnnotationKind {
    Route { method: String, path: String },
    Middleware(String),
    Validator(String),
    Redirect(String),
    RedirectPermanent(String),
    GroupMiddleware(String),
}

// ------------------------------------------------------------- Public Functions

/// Recursively searches the controllers directory for all
/// `.gleam` files. Returns a list of file paths that can be
/// passed to `parse_controller` for route extraction.
///
pub fn find_controller_files() -> io::Result<Vec<String>> {
    let mut files = Vec::new();

    find_gleam_files(CONTROLLER_DIR, &mut files)?;

    Ok(files)
}

/// Parses a controller file and extracts all annotated routes.
/// Looks for route annotations like `@get`, `@post`, etc. and
/// matches them to their corresponding handler functions to
/// build a complete Route struct.
///
pub fn parse_controller(path: &str) -> Result<Vec<Route>, String> {
    let content =
        fs::read_to_string(path).map_err(|e| format!("Failed to read {}: {}", path, e))?;

    let annotations = find_annotations(&content);
    let functions = find_functions(&content);

    let metadata = FileMetadata {
        has_wisp_response_import: has_import(&content, &patterns::WISP_RESPONSE_IMPORT),
        group_middleware: find_group_middleware(&annotations),
    };

    let blocks = group_annotations(annotations);
    let matched = match_blocks_to_functions(blocks, &functions);

    Ok(build_routes(matched, &metadata, path))
}

// ------------------------------------------------------------- Private Functions

/// Recursively walks a directory tree collecting paths to all
/// `.gleam` files. Populates the provided vector with absolute
/// paths to each file found.
///
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

/// Attempts to parse a single line as a route annotation. Returns
/// None if the line isn't a comment or doesn't match any known
/// annotation pattern.
///
fn parse_line(line: &str, line_num: usize) -> Option<Annotation> {
    let trimmed: &str = line.trim();

    if !trimmed.starts_with("//") {
        return None;
    }

    if let Some(caps) = patterns::METHOD.captures(trimmed) {
        return Some(Annotation {
            line_num,
            kind: AnnotationKind::Route {
                method: caps[1].to_uppercase(),
                path: caps[2].to_string(),
            },
        });
    }

    if let Some(caps) = patterns::MIDDLEWARE.captures(trimmed) {
        return Some(Annotation {
            line_num,
            kind: AnnotationKind::Middleware(caps[1].to_string()),
        });
    }

    if let Some(caps) = patterns::VALIDATOR.captures(trimmed) {
        return Some(Annotation {
            line_num,
            kind: AnnotationKind::Validator(caps[1].to_string()),
        });
    }

    if let Some(caps) = patterns::REDIRECT.captures(trimmed) {
        return Some(Annotation {
            line_num,
            kind: AnnotationKind::Redirect(caps[1].to_string()),
        });
    }

    if let Some(caps) = patterns::REDIRECT_PERM.captures(trimmed) {
        return Some(Annotation {
            line_num,
            kind: AnnotationKind::RedirectPermanent(caps[1].to_string()),
        });
    }

    if let Some(caps) = patterns::GROUP_MIDDLEWARE.captures(trimmed) {
        return Some(Annotation {
            line_num,
            kind: AnnotationKind::GroupMiddleware(caps[1].to_string()),
        });
    }

    None
}

/// Scans all lines in the file content and collects any that
/// contain route annotations. Returns them with their line
/// numbers for later matching to functions.
///
fn find_annotations(content: &str) -> Vec<Annotation> {
    content
        .lines()
        .enumerate()
        .filter_map(|(i, line)| parse_line(line, i + 1))
        .collect()
}

/// Checks if the file contains an import matching the given
/// regex pattern. Ignores commented lines so we don't get
/// false positives from commented-out imports.
///
fn has_import(content: &str, pattern: &Regex) -> bool {
    content
        .lines()
        .filter(|line| !line.trim().starts_with("//"))
        .any(|line| pattern.is_match(line))
}

/// Finds all public function definitions in the file. Extracts
/// the function name, parameters, and return type for each one
/// so we can match them to route annotations.
///
fn find_functions(content: &str) -> Vec<FunctionDef> {
    let mut functions = Vec::new();
    let lines: Vec<&str> = content.lines().collect();
    let mut i: usize = 0;

    while i < lines.len() {
        if let Some(caps) = patterns::PUB_FN.captures(lines[i]) {
            let line_num = i + 1; // 1-indexed to match find_annotations
            let name = caps[1].to_string();

            let signature = get_signature(&lines, &mut i);
            let params = get_params(&signature);
            let return_type = get_return_type(&signature);

            functions.push(FunctionDef {
                line_num,
                name,
                params,
                return_type,
            });
        }

        i += 1;
    }

    functions
}

/// Collects lines starting from the current index until we hit
/// an opening brace. Handles multi-line function signatures by
/// joining all the lines together.
///
fn get_signature(lines: &[&str], index: &mut usize) -> String {
    let mut sig_lines = Vec::new();

    while *index < lines.len() && !lines[*index].contains('{') {
        sig_lines.push(lines[*index]);
        *index += 1;
    }

    if *index < lines.len() {
        sig_lines.push(lines[*index]);
    }

    sig_lines.join("\n")
}

/// Extracts function parameters from a signature string. Handles
/// nested parentheses in types like `Option(String)` by tracking
/// depth so we don't split on commas inside type expressions.
///
fn get_params(signature: &str) -> Vec<FunctionParam> {
    let start = match signature.find('(') {
        Some(i) => i + 1,
        None => return Vec::new(),
    };

    let chars: Vec<char> = signature.chars().collect();
    let mut depth: usize = 1;
    let mut end: usize = start;

    for (i, &c) in chars.iter().enumerate().skip(start) {
        match c {
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

    let mut params: Vec<FunctionParam> = Vec::new();
    let mut current: String = String::new();
    let mut depth: usize = 0;

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
                if !current.trim().is_empty()
                    && let Some(param) = get_param(&current)
                {
                    params.push(param);
                }
                current.clear();
            }
            _ => current.push(c),
        }
    }

    if !current.trim().is_empty()
        && let Some(param) = get_param(&current)
    {
        params.push(param);
    }

    params
}

/// Parses a single parameter string like "name: Type" into a
/// FunctionParam. Strips leading underscores from names since
/// they're just Gleam's way of marking unused params.
///
fn get_param(param: &str) -> Option<FunctionParam> {
    let trimmed = param.trim();

    if trimmed.is_empty() {
        return None;
    }

    let (name, param_type) = match trimmed.find(':') {
        Some(pos) => (trimmed[..pos].trim(), trimmed[pos + 1..].trim()),
        None => (trimmed, ""),
    };

    Some(FunctionParam {
        name: name.trim_start_matches('_').to_string(),
        param_type: param_type.to_string(),
    })
}

/// Extracts the return type from a function signature using a
/// regex pattern. Returns an empty string if no return type
/// annotation is found.
///
fn get_return_type(signature: &str) -> String {
    if let Some(caps) = patterns::RETURN_TYPE.captures(signature) {
        return caps[1].trim().to_string();
    }

    String::new()
}

/// Groups consecutive annotations into blocks. Each block starts
/// with a route annotation and includes any middleware, validator,
/// or redirect annotations that follow before the next route.
///
fn group_annotations(annotations: Vec<Annotation>) -> Vec<AnnotationBlock> {
    let mut blocks = Vec::new();
    let mut current_route: Option<Annotation> = None;
    let mut current_middleware: Vec<String> = Vec::new();
    let mut current_validator: Option<String> = None;
    let mut current_redirect: Option<String> = None;
    let mut current_redirect_perm = false;

    for ann in annotations {
        match ann.kind {
            AnnotationKind::Route { .. } => {
                if let Some(route) = current_route.take() {
                    blocks.push(AnnotationBlock {
                        route,
                        middleware: std::mem::take(&mut current_middleware),
                        validator: current_validator.take(),
                        redirect: current_redirect.take(),
                        redirect_permanent: current_redirect_perm,
                    });
                }

                current_route = Some(ann);
                current_middleware.clear();
                current_validator = None;
                current_redirect = None;
                current_redirect_perm = false;
            }

            AnnotationKind::Middleware(name) => {
                if current_route.is_some() {
                    current_middleware.push(name);
                }
            }

            AnnotationKind::Validator(name) => {
                if current_route.is_some() {
                    current_validator = Some(name);
                }
            }

            AnnotationKind::Redirect(path) => {
                if current_route.is_some() {
                    current_redirect = Some(path);
                }
            }

            AnnotationKind::RedirectPermanent(path) => {
                if current_route.is_some() {
                    current_redirect = Some(path);
                    current_redirect_perm = true;
                }
            }

            AnnotationKind::GroupMiddleware(_) => {
                // Handled separately via find_group_middleware
            }
        }
    }

    if let Some(route) = current_route {
        blocks.push(AnnotationBlock {
            route,
            middleware: current_middleware,
            validator: current_validator,
            redirect: current_redirect,
            redirect_permanent: current_redirect_perm,
        });
    }

    blocks
}

/// Extracts all group middleware annotations from the file.
/// These are applied to every route in the controller rather
/// than individual handlers.
///
fn find_group_middleware(annotations: &[Annotation]) -> Vec<String> {
    annotations
        .iter()
        .filter_map(|ann| match &ann.kind {
            AnnotationKind::GroupMiddleware(name) => Some(name.clone()),
            _ => None,
        })
        .collect()
}

/// Pairs each annotation block with the function that follows it.
/// Uses line numbers to find the nearest function after each route
/// annotation, ensuring each function is only matched once.
///
fn match_blocks_to_functions(
    blocks: Vec<AnnotationBlock>,
    functions: &[FunctionDef],
) -> Vec<(AnnotationBlock, FunctionDef)> {
    let mut used_functions: HashSet<usize> = HashSet::new();
    let mut result = Vec::new();

    let mut blocks = blocks;
    blocks.sort_by_key(|b| b.route.line_num);

    for block in blocks {
        let block_line = block.route.line_num;

        if let Some(func) = functions
            .iter()
            .filter(|f| f.line_num > block_line && !used_functions.contains(&f.line_num))
            .min_by_key(|f| f.line_num)
        {
            used_functions.insert(func.line_num);
            result.push((block, func.clone()));
        }
    }

    result
}

/// Converts matched annotation/function pairs into Route structs.
/// Combines information from the annotation block, function def,
/// and file metadata into the final route representation.
///
fn build_routes(
    matched: Vec<(AnnotationBlock, FunctionDef)>,
    metadata: &FileMetadata,
    controller_path: &str,
) -> Vec<Route> {
    matched
        .into_iter()
        .map(|(block, func)| {
            let (method, path) = match block.route.kind {
                AnnotationKind::Route { method, path } => (method, path),
                _ => unreachable!(),
            };

            Route {
                method,
                path,
                handler: func.name,
                controller_path: controller_path.to_string(),
                middleware: block.middleware,
                validator: block.validator,
                redirect: block.redirect,
                redirect_permanent: block.redirect_permanent,
                params: func.params,
                group_middleware: metadata.group_middleware.clone(),
                return_type: func.return_type,
                has_wisp_response_import: metadata.has_wisp_response_import,
            }
        })
        .collect()
}

// ------------------------------------------------------------- Unit Tests

#[cfg(test)]
mod tests {
    use super::*;

    // ----------------------------------------- get_param tests

    #[test]
    fn test_get_param_with_type() {
        let param = get_param("name: String").unwrap();
        assert_eq!(param.name, "name");
        assert_eq!(param.param_type, "String");
    }

    #[test]
    fn test_get_param_with_underscore() {
        let param = get_param("_req: Request").unwrap();
        assert_eq!(param.name, "req");
        assert_eq!(param.param_type, "Request");
    }

    #[test]
    fn test_get_param_without_type() {
        let param = get_param("name").unwrap();
        assert_eq!(param.name, "name");
        assert_eq!(param.param_type, "");
    }

    #[test]
    fn test_get_param_complex_type() {
        let param = get_param("data: Option(String)").unwrap();
        assert_eq!(param.name, "data");
        assert_eq!(param.param_type, "Option(String)");
    }

    #[test]
    fn test_get_param_qualified_type() {
        let param = get_param("req: wisp.Request").unwrap();
        assert_eq!(param.name, "req");
        assert_eq!(param.param_type, "wisp.Request");
    }

    #[test]
    fn test_get_param_empty() {
        assert!(get_param("").is_none());
        assert!(get_param("   ").is_none());
    }

    // ----------------------------------------- get_params tests

    #[test]
    fn test_get_params_empty() {
        let params = get_params("pub fn show() -> Response {");
        assert!(params.is_empty());
    }

    #[test]
    fn test_get_params_single() {
        let params = get_params("pub fn show(req: Request) -> Response {");
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].name, "req");
        assert_eq!(params[0].param_type, "Request");
    }

    #[test]
    fn test_get_params_multiple() {
        let params = get_params("pub fn show(req: Request, ctx: Context) -> Response {");
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].name, "req");
        assert_eq!(params[1].name, "ctx");
    }

    #[test]
    fn test_get_params_nested_parens() {
        let params = get_params("pub fn show(data: Option(String), req: Request) -> Response {");
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].name, "data");
        assert_eq!(params[0].param_type, "Option(String)");
        assert_eq!(params[1].name, "req");
    }

    #[test]
    fn test_get_params_multiline() {
        let sig = "pub fn show(\n  req: Request,\n  ctx: Context\n) -> Response {";
        let params = get_params(sig);
        assert_eq!(params.len(), 2);
    }

    // ----------------------------------------- get_return_type tests

    #[test]
    fn test_get_return_type_simple() {
        let rt = get_return_type("pub fn show() -> Response {");
        assert_eq!(rt, "Response");
    }

    #[test]
    fn test_get_return_type_qualified() {
        let rt = get_return_type("pub fn show() -> wisp.Response {");
        assert_eq!(rt, "wisp.Response");
    }

    #[test]
    fn test_get_return_type_none() {
        let rt = get_return_type("pub fn show() {");
        assert_eq!(rt, "");
    }

    #[test]
    fn test_get_return_type_with_params() {
        let rt = get_return_type("pub fn show(req: Request) -> Response {");
        assert_eq!(rt, "Response");
    }

    // ----------------------------------------- has_import tests (wisp response import)

    #[test]
    fn test_has_import_wisp_response_type() {
        let content = "import wisp.{type Response}\n\npub fn show() -> Response {";
        assert!(has_import(content, &super::patterns::WISP_RESPONSE_IMPORT));
    }

    #[test]
    fn test_has_import_wisp_response_value() {
        let content = "import wisp.{Response}\n\npub fn show() -> Response {";
        assert!(has_import(content, &super::patterns::WISP_RESPONSE_IMPORT));
    }

    #[test]
    fn test_has_import_wisp_response_multiple() {
        let content = "import wisp.{type Request, type Response}\n\npub fn show() -> Response {";
        assert!(has_import(content, &super::patterns::WISP_RESPONSE_IMPORT));
    }

    #[test]
    fn test_has_import_wisp_response_commented() {
        let content = "// import wisp.{type Response}\n\npub fn show() -> Response {";
        assert!(!has_import(content, &super::patterns::WISP_RESPONSE_IMPORT));
    }

    #[test]
    fn test_has_import_wisp_response_not_present() {
        let content = "import gleam/io\n\npub fn show() -> Response {";
        assert!(!has_import(content, &super::patterns::WISP_RESPONSE_IMPORT));
    }

    #[test]
    fn test_has_import_wisp_response_wrong_module() {
        let content = "import other.{type Response}\n\npub fn show() -> Response {";
        assert!(!has_import(content, &super::patterns::WISP_RESPONSE_IMPORT));
    }

    // ----------------------------------------- find_group_middleware tests

    #[test]
    fn test_find_group_middleware_single() {
        let content = "// @group_middleware \"auth\"\n\npub fn show() {}";
        let annotations = find_annotations(content);
        let mw = find_group_middleware(&annotations);
        assert_eq!(mw, vec!["auth"]);
    }

    #[test]
    fn test_find_group_middleware_multiple() {
        let content = "// @group_middleware \"auth\"\n// @group_middleware \"logger\"\n";
        let annotations = find_annotations(content);
        let mw = find_group_middleware(&annotations);
        assert_eq!(mw, vec!["auth", "logger"]);
    }

    #[test]
    fn test_find_group_middleware_none() {
        let content = "pub fn show() {}";
        let annotations = find_annotations(content);
        let mw = find_group_middleware(&annotations);
        assert!(mw.is_empty());
    }

    #[test]
    fn test_find_group_middleware_wrong_format() {
        // Missing @ symbol
        let content = "// group_middleware \"auth\"\n";
        let annotations = find_annotations(content);
        let mw = find_group_middleware(&annotations);
        assert!(mw.is_empty());
    }

    #[test]
    fn test_parse_line_route_with_param() {
        let line = r#"/// @get "/users/:id""#;
        let ann = parse_line(line, 1);
        assert!(ann.is_some(), "Should parse the annotation");
        let ann = ann.unwrap();
        match ann.kind {
            AnnotationKind::Route { method, path } => {
                assert_eq!(method, "GET");
                assert_eq!(path, "/users/:id");
            }
            _ => panic!("Expected Route annotation"),
        }
    }
}
