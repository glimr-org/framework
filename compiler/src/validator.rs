use crate::parser::Route;
use regex::Regex;
use std::fs;
use std::path::Path;

const MIDDLEWARE_DIRS: &[&str] = &[
    "src/app/http/middleware",
    "test/fixtures/app/http/middleware",
];

const VALIDATOR_DIRS: &[&str] = &[
    "src/app/http/validators",
    "test/fixtures/app/http/validators",
];

/// Validate all routes
pub fn validate_routes(routes: &[Route]) -> Result<(), String> {
    for route in routes {
        let controller = get_controller_name(&route.controller_path);
        let handler = format!("{}.{}", controller, route.handler);

        validate_path(&route.path)?;
        validate_path_params(&route.path)?;

        // Validate group middleware (applied to controller)
        for mw in &route.group_middleware {
            validate_middleware(mw, &controller, true)?;
        }

        // Validate route middleware (applied to handler)
        for mw in &route.middleware {
            validate_middleware(mw, &handler, false)?;
        }

        // Validate validator
        if let Some(ref v) = route.validator {
            validate_validator(v, &handler)?;
        }

        // Validate handler params
        validate_handler_params(route, &handler)?;

        // Validate return type
        validate_return_type(route, &handler)?;
    }

    Ok(())
}

/// Get controller name from path for error messages
fn get_controller_name(path: &str) -> String {
    path.trim_start_matches("src/app/http/controllers/")
        .trim_end_matches(".gleam")
        .replace('/', ".")
}

/// Validate path format
fn validate_path(path: &str) -> Result<(), String> {
    if !path.starts_with('/') {
        return Err(format!("Route path must start with '/': {}", path));
    }

    // Valid chars: letters, numbers, hyphens, underscores, slashes, :params
    let re = Regex::new(r"^(/([a-zA-Z0-9_-]+|:[a-zA-Z_][a-zA-Z0-9_]*))*/?$|^/$").unwrap();
    if !re.is_match(path) {
        return Err(format!("Invalid route path format: {}", path));
    }

    Ok(())
}

/// Validate reserved path params
fn validate_path_params(path: &str) -> Result<(), String> {
    let reserved = &["req", "_req", "ctx", "_ctx"];
    let param_re = Regex::new(r":([a-zA-Z_][a-zA-Z0-9_]*)").unwrap();

    for cap in param_re.captures_iter(path) {
        let param = &cap[1];
        if reserved.contains(&param) {
            return Err(format!(
                "Reserved param name '{}' cannot be used as a path parameter in: {}",
                param, path
            ));
        }
    }

    Ok(())
}

/// Validate middleware exists and has run function
fn validate_middleware(name: &str, applied_to: &str, _is_group: bool) -> Result<(), String> {
    let file_path = match find_file(MIDDLEWARE_DIRS, name) {
        Ok(path) => path,
        Err(_) => {
            return Err(format!(
                "Middleware \"{}\" applied to {} does not exist",
                name, applied_to
            ));
        }
    };

    let content = fs::read_to_string(&file_path).map_err(|_| {
        format!(
            "Middleware \"{}\" applied to {} does not exist",
            name, applied_to
        )
    })?;

    if !content.contains("pub fn run(") {
        return Err(format!(
            "Middleware \"{}\" applied to {} doesn't have a public \"run\" function",
            name, applied_to
        ));
    }

    Ok(())
}

/// Validate validator exists and has validate function
fn validate_validator(name: &str, handler: &str) -> Result<(), String> {
    let file_path = match find_file(VALIDATOR_DIRS, name) {
        Ok(path) => path,
        Err(_) => {
            return Err(format!(
                "Validator \"{}\" applied to {} does not exist",
                name, handler
            ));
        }
    };

    let content = fs::read_to_string(&file_path).map_err(|_| {
        format!(
            "Validator \"{}\" applied to {} does not exist",
            name, handler
        )
    })?;

    if !content.contains("pub fn validate(") {
        return Err(format!(
            "Validator \"{}\" applied to {} doesn't have a public \"validate\" function",
            name, handler
        ));
    }

    Ok(())
}

/// Find file in one of the given directories
fn find_file(dirs: &[&str], name: &str) -> Result<String, String> {
    for dir in dirs {
        let path = format!("{}/{}.gleam", dir, name);
        if Path::new(&path).exists() {
            return Ok(path);
        }
    }
    Err(format!("File not found: {}", name))
}

/// Validate handler params match route
fn validate_handler_params(route: &Route, handler: &str) -> Result<(), String> {
    // Extract path params from route
    let param_re = Regex::new(r":([a-zA-Z_][a-zA-Z0-9_]*)").unwrap();
    let path_params: Vec<String> = param_re
        .captures_iter(&route.path)
        .map(|c| c[1].to_string())
        .collect();

    // Validate req/ctx param types
    for param in &route.params {
        if param.name == "req" {
            if param.param_type.is_empty() {
                return Err(format!(
                    "{} param 'req' must have a type annotation",
                    handler
                ));
            }
            if !is_valid_request_type(&param.param_type, &route.controller_path) {
                return Err(format!(
                    "{} param 'req' must be wisp.Request, got {}\n\
                     Use 'import wisp.{{type Request}}' or use wisp.Request",
                    handler, param.param_type
                ));
            }
        }
        if param.name == "ctx" {
            if param.param_type.is_empty() {
                return Err(format!(
                    "{} param 'ctx' must have a type annotation",
                    handler
                ));
            }
            if !is_valid_context_type(&param.param_type, &route.controller_path) {
                return Err(format!(
                    "{} param 'ctx' must be ctx.Context, got {}\n\
                     Use 'import app/http/context/ctx.{{type Context}}' or use ctx.Context",
                    handler, param.param_type
                ));
            }
        }
    }

    // Check that all path params are in handler
    for path_param in &path_params {
        let found = route.params.iter().any(|p| &p.name == path_param);
        if !found {
            return Err(format!(
                "{} is missing param '{}' from route path '{}'",
                handler, path_param, route.path
            ));
        }
    }

    // Check for extra params not in route (excluding req, ctx, and valid data params)
    let special_params = &["req", "ctx"];
    for param in &route.params {
        if special_params.contains(&param.name.as_str()) {
            continue;
        }
        if !path_params.contains(&param.name) {
            // Check if it's a valid validator data param
            if let Some(ref validator) = route.validator {
                if is_valid_data_param(&param.param_type, validator, &route.controller_path) {
                    continue;
                }
            }
            // Better error message if validator is present and param is named data/validated
            if route.validator.is_some() && (param.name == "data" || param.name == "validated") {
                return Err(format!(
                    "{} has extra param '{}' not in route path '{}'\n\
                     Did you mean to give {} the Data type from your validator?",
                    handler, param.name, route.path, param.name
                ));
            }
            return Err(format!(
                "{} has extra param '{}' not in route path '{}'",
                handler, param.name, route.path
            ));
        }
    }

    // Check if @validator is used but no valid Data param
    if let Some(ref validator) = route.validator {
        let has_data_param = route
            .params
            .iter()
            .any(|p| is_valid_data_param(&p.param_type, validator, &route.controller_path));
        if !has_data_param {
            return Err(format!(
                "{} uses @validator \"{}\" but has no param with type {}.Data",
                handler, validator, validator
            ));
        }
    }

    Ok(())
}

/// Check if a param type is a valid Data type for the given validator
fn is_valid_data_param(param_type: &str, validator: &str, controller_path: &str) -> bool {
    let param_type = param_type.trim();

    // Check for fully qualified type: validator_name.Data
    if param_type == format!("{}.Data", validator) {
        return true;
    }

    // Check for just "Data" with proper import
    if param_type == "Data" {
        return check_validator_data_import(controller_path, validator);
    }

    false
}

/// Check if the controller imports Data from the given validator
fn check_validator_data_import(controller_path: &str, validator: &str) -> bool {
    let content = match fs::read_to_string(controller_path) {
        Ok(c) => c,
        Err(_) => return false,
    };

    // Match patterns like:
    // - import app/http/validators/user_validator.{type Data}
    // - import app/http/validators/user_validator.{Data}
    // - import app/http/validators/user_validator.{type Data, validate}
    let pattern = format!(
        r"import\s+app/http/validators/{}\s*\.\s*\{{[^}}]*\bData\b[^}}]*\}}",
        regex::escape(validator)
    );
    let re = match Regex::new(&pattern) {
        Ok(r) => r,
        Err(_) => return false,
    };

    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("//") {
            continue;
        }
        if re.is_match(line) {
            return true;
        }
    }

    false
}

/// Check if a param type is a valid Request type (wisp.Request or imported from wisp)
fn is_valid_request_type(param_type: &str, controller_path: &str) -> bool {
    let param_type = param_type.trim();

    // wisp.Request is always valid
    if param_type == "wisp.Request" {
        return true;
    }

    // Request is valid only if imported from wisp
    if param_type == "Request" {
        return check_wisp_request_import(controller_path);
    }

    false
}

/// Check if the controller imports Request from wisp
fn check_wisp_request_import(controller_path: &str) -> bool {
    let content = match fs::read_to_string(controller_path) {
        Ok(c) => c,
        Err(_) => return false,
    };

    let re = match Regex::new(r"import\s+wisp\s*\.\s*\{[^}]*\bRequest\b[^}]*\}") {
        Ok(r) => r,
        Err(_) => return false,
    };

    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("//") {
            continue;
        }
        if re.is_match(line) {
            return true;
        }
    }

    false
}

/// Check if a param type is a valid Context type (ctx.Context or imported from ctx)
fn is_valid_context_type(param_type: &str, controller_path: &str) -> bool {
    let param_type = param_type.trim();

    // ctx.Context is always valid
    if param_type == "ctx.Context" {
        return true;
    }

    // Context is valid only if imported from app/http/context/ctx
    if param_type == "Context" {
        return check_ctx_context_import(controller_path);
    }

    false
}

/// Check if the controller imports Context from app/http/context/ctx
fn check_ctx_context_import(controller_path: &str) -> bool {
    let content = match fs::read_to_string(controller_path) {
        Ok(c) => c,
        Err(_) => return false,
    };

    let re = match Regex::new(r"import\s+app/http/context/ctx\s*\.\s*\{[^}]*\bContext\b[^}]*\}") {
        Ok(r) => r,
        Err(_) => return false,
    };

    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("//") {
            continue;
        }
        if re.is_match(line) {
            return true;
        }
    }

    false
}

/// Validate that route handler returns wisp.Response
fn validate_return_type(route: &Route, handler: &str) -> Result<(), String> {
    let return_type = route.return_type.trim();

    // Empty return type means no explicit return type annotation
    if return_type.is_empty() {
        return Err(format!(
            "{} must have a return type annotation of wisp.Response",
            handler
        ));
    }

    // wisp.Response is always valid
    if return_type == "wisp.Response" {
        return Ok(());
    }

    // Response is valid only if imported from wisp
    if return_type == "Response" {
        if route.has_wisp_response_import {
            return Ok(());
        } else {
            return Err(format!(
                "{} returns Response but it's not imported from wisp.\n\
                 Use 'import wisp.{{type Response}}' or return wisp.Response",
                handler
            ));
        }
    }

    // Any other return type is invalid
    Err(format!(
        "{} must return wisp.Response, got {}",
        handler, return_type
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::FunctionParam;

    // Helper to create a minimal route for testing
    fn make_route(
        path: &str,
        handler: &str,
        params: Vec<(&str, &str)>,
        return_type: &str,
        has_wisp_response_import: bool,
    ) -> Route {
        Route {
            method: "GET".to_string(),
            path: path.to_string(),
            handler: handler.to_string(),
            controller_path: "test/controller.gleam".to_string(),
            middleware: vec![],
            validator: None,
            redirect: None,
            redirect_permanent: false,
            params: params
                .into_iter()
                .map(|(name, typ)| FunctionParam {
                    name: name.to_string(),
                    param_type: typ.to_string(),
                })
                .collect(),
            group_middleware: vec![],
            return_type: return_type.to_string(),
            has_wisp_response_import,
        }
    }

    // ============================================================
    // validate_path tests
    // ============================================================

    #[test]
    fn test_validate_path_root() {
        assert!(validate_path("/").is_ok());
    }

    #[test]
    fn test_validate_path_simple() {
        assert!(validate_path("/users").is_ok());
        assert!(validate_path("/users/profile").is_ok());
    }

    #[test]
    fn test_validate_path_with_params() {
        assert!(validate_path("/users/:id").is_ok());
        assert!(validate_path("/users/:user_id/posts/:post_id").is_ok());
    }

    #[test]
    fn test_validate_path_with_hyphens() {
        assert!(validate_path("/user-profile").is_ok());
        assert!(validate_path("/api/v1/user-settings").is_ok());
    }

    #[test]
    fn test_validate_path_trailing_slash() {
        assert!(validate_path("/users/").is_ok());
    }

    #[test]
    fn test_validate_path_missing_leading_slash() {
        let result = validate_path("users");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("must start with '/'"));
    }

    #[test]
    fn test_validate_path_invalid_chars() {
        assert!(validate_path("/users?query=1").is_err());
        assert!(validate_path("/users#anchor").is_err());
    }

    // ============================================================
    // validate_path_params tests
    // ============================================================

    #[test]
    fn test_validate_path_params_valid() {
        assert!(validate_path_params("/users/:id").is_ok());
        assert!(validate_path_params("/users/:user_id").is_ok());
    }

    #[test]
    fn test_validate_path_params_reserved_req() {
        let result = validate_path_params("/users/:req");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Reserved param name 'req'"));
    }

    #[test]
    fn test_validate_path_params_reserved_ctx() {
        let result = validate_path_params("/users/:ctx");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Reserved param name 'ctx'"));
    }

    #[test]
    fn test_validate_path_params_reserved_with_underscore() {
        assert!(validate_path_params("/users/:_req").is_err());
        assert!(validate_path_params("/users/:_ctx").is_err());
    }

    // ============================================================
    // validate_return_type tests
    // ============================================================

    #[test]
    fn test_validate_return_type_wisp_response() {
        let route = make_route("/", "show", vec![], "wisp.Response", false);
        assert!(validate_return_type(&route, "controller.show").is_ok());
    }

    #[test]
    fn test_validate_return_type_response_with_import() {
        let route = make_route("/", "show", vec![], "Response", true);
        assert!(validate_return_type(&route, "controller.show").is_ok());
    }

    #[test]
    fn test_validate_return_type_response_without_import() {
        let route = make_route("/", "show", vec![], "Response", false);
        let result = validate_return_type(&route, "controller.show");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not imported from wisp"));
    }

    #[test]
    fn test_validate_return_type_wrong_type() {
        let route = make_route("/", "show", vec![], "String", false);
        let result = validate_return_type(&route, "controller.show");
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .contains("must return wisp.Response, got String")
        );
    }

    #[test]
    fn test_validate_return_type_missing() {
        let route = make_route("/", "show", vec![], "", false);
        let result = validate_return_type(&route, "controller.show");
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .contains("must have a return type annotation")
        );
    }

    // ============================================================
    // validate_handler_params tests
    // ============================================================

    #[test]
    fn test_validate_handler_params_no_params() {
        let route = make_route("/", "show", vec![], "Response", true);
        assert!(validate_handler_params(&route, "controller.show").is_ok());
    }

    #[test]
    fn test_validate_handler_params_path_param_present() {
        let route = make_route(
            "/users/:id",
            "show",
            vec![("id", "String")],
            "Response",
            true,
        );
        assert!(validate_handler_params(&route, "controller.show").is_ok());
    }

    #[test]
    fn test_validate_handler_params_path_param_missing() {
        let route = make_route("/users/:id", "show", vec![], "Response", true);
        let result = validate_handler_params(&route, "controller.show");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("missing param 'id'"));
    }

    #[test]
    fn test_validate_handler_params_extra_param() {
        let route = make_route("/", "show", vec![("extra", "String")], "Response", true);
        let result = validate_handler_params(&route, "controller.show");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("extra param 'extra'"));
    }

    #[test]
    fn test_validate_handler_params_req_without_type() {
        let route = make_route("/", "show", vec![("req", "")], "Response", true);
        let result = validate_handler_params(&route, "controller.show");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("must have a type annotation"));
    }

    #[test]
    fn test_validate_handler_params_ctx_without_type() {
        let route = make_route("/", "show", vec![("ctx", "")], "Response", true);
        let result = validate_handler_params(&route, "controller.show");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("must have a type annotation"));
    }

    // ============================================================
    // get_controller_name tests
    // ============================================================

    #[test]
    fn test_get_controller_name_simple() {
        let name = get_controller_name("src/app/http/controllers/home_controller.gleam");
        assert_eq!(name, "home_controller");
    }

    #[test]
    fn test_get_controller_name_nested() {
        let name = get_controller_name("src/app/http/controllers/api/users_controller.gleam");
        assert_eq!(name, "api.users_controller");
    }

    #[test]
    fn test_get_controller_name_deeply_nested() {
        let name =
            get_controller_name("src/app/http/controllers/api/v1/admin/users_controller.gleam");
        assert_eq!(name, "api.v1.admin.users_controller");
    }

    // ============================================================
    // is_valid_data_param tests (requires filesystem, so limited)
    // ============================================================

    #[test]
    fn test_is_valid_data_param_qualified() {
        // Qualified type doesn't need filesystem check
        assert!(is_valid_data_param(
            "user_validator.Data",
            "user_validator",
            "nonexistent.gleam"
        ));
    }

    #[test]
    fn test_is_valid_data_param_wrong_validator() {
        assert!(!is_valid_data_param(
            "other_validator.Data",
            "user_validator",
            "nonexistent.gleam"
        ));
    }

    #[test]
    fn test_is_valid_data_param_wrong_type() {
        assert!(!is_valid_data_param(
            "String",
            "user_validator",
            "nonexistent.gleam"
        ));
    }

    // ============================================================
    // is_valid_request_type tests
    // ============================================================

    #[test]
    fn test_is_valid_request_type_qualified() {
        assert!(is_valid_request_type("wisp.Request", "nonexistent.gleam"));
    }

    #[test]
    fn test_is_valid_request_type_wrong() {
        assert!(!is_valid_request_type("String", "nonexistent.gleam"));
        assert!(!is_valid_request_type("other.Request", "nonexistent.gleam"));
    }

    // ============================================================
    // is_valid_context_type tests
    // ============================================================

    #[test]
    fn test_is_valid_context_type_qualified() {
        assert!(is_valid_context_type("ctx.Context", "nonexistent.gleam"));
    }

    #[test]
    fn test_is_valid_context_type_wrong() {
        assert!(!is_valid_context_type("String", "nonexistent.gleam"));
        assert!(!is_valid_context_type("other.Context", "nonexistent.gleam"));
    }
}
