use super::parser::Route;
use super::patterns;
use regex::Regex;
use std::fs;
use std::path::Path;

// ------------------------------------------------------------- Private Consts

/// Directories to search when resolving middleware names.
/// Includes both production and test paths so validation
/// works correctly in both contexts.
///
const MIDDLEWARE_DIRS: &[&str] = &[
    "src/app/http/middleware",
    "test/fixtures/app/http/middleware",
];

/// Directories to search when resolving validator names.
/// Mirrors middleware search paths to maintain consistent
/// resolution behavior across the codebase.
///
const VALIDATOR_DIRS: &[&str] = &[
    "src/app/http/validators",
    "test/fixtures/app/http/validators",
];

// ------------------------------------------------------------- Public Functions

/// Entry point for route validation. Checks all routes for
/// correctness before code generation, providing helpful
/// error messages when issues are found.
///
pub fn validate_routes(routes: &[Route]) -> Result<(), String> {
    for route in routes {
        let controller = get_controller_name(&route.controller_path);
        let handler = format!("{}.{}", controller, route.handler);

        validate_path(&route.path)?;
        validate_path_params(&route.path)?;

        for mw in &route.group_middleware {
            validate_middleware(mw, &controller, true)?;
        }

        for mw in &route.middleware {
            validate_middleware(mw, &handler, false)?;
        }

        if let Some(ref v) = route.validator {
            validate_validator(v, &handler)?;
        }

        validate_handler_params(route, &handler)?;
        validate_return_type(route, &handler)?;
    }

    Ok(())
}

// ------------------------------------------------------------- Private Functions

/// Extracts a readable controller name from its file path.
/// Converts `src/app/http/controllers/api/users.gleam` to
/// `api.users` for use in error messages.
///
fn get_controller_name(path: &str) -> String {
    path.trim_start_matches("src/app/http/controllers/")
        .trim_end_matches(".gleam")
        .replace('/', ".")
}

/// Ensures route paths follow URL conventions. Paths must
/// start with `/` and contain only valid characters to
/// prevent runtime routing failures.
///
fn validate_path(path: &str) -> Result<(), String> {
    if !path.starts_with('/') {
        return Err(format!("Route path must start with '/': {}", path));
    }

    if !patterns::VALID_PATH.is_match(path) {
        return Err(format!("Invalid route path format: {}", path));
    }

    Ok(())
}

/// Prevents conflicts between path params and special names.
/// Names like `req` and `ctx` are reserved for request and
/// context injection, so they can't be used as URL params.
///
fn validate_path_params(path: &str) -> Result<(), String> {
    let reserved = &["req", "_req", "ctx", "_ctx"];

    for cap in patterns::PATH_PARAM.captures_iter(path) {
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

/// Verifies middleware exists and has the required `run`
/// function. Catches typos in middleware names and missing
/// implementations before runtime.
///
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

/// Verifies validators exist and have the required `validate`
/// function. Similar to middleware validation but checks for
/// the validation-specific entry point.
///
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

/// Searches multiple directories for a named Gleam file.
/// Enables consistent resolution of middleware and validators
/// across production and test environments.
///
fn find_file(dirs: &[&str], name: &str) -> Result<String, String> {
    for dir in dirs {
        let path = format!("{}/{}.gleam", dir, name);
        if Path::new(&path).exists() {
            return Ok(path);
        }
    }
    Err(format!("File not found: {}", name))
}

/// Validates handler function signatures match route paths.
/// Ensures path params like `:id` have corresponding function
/// params, and special params have correct types.
///
fn validate_handler_params(route: &Route, handler: &str) -> Result<(), String> {
    let path_params: Vec<String> = patterns::PATH_PARAM
        .captures_iter(&route.path)
        .map(|c| c[1].to_string())
        .collect();

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

    for path_param in &path_params {
        let found = route.params.iter().any(|p| &p.name == path_param);
        if !found {
            return Err(format!(
                "{} is missing param '{}' from route path '{}'",
                handler, path_param, route.path
            ));
        }
    }

    let special_params = &["req", "ctx"];
    for param in &route.params {
        if special_params.contains(&param.name.as_str()) {
            continue;
        }
        if !path_params.contains(&param.name) {
            if let Some(ref validator) = route.validator
                && is_valid_data_param(&param.param_type, validator, &route.controller_path)
            {
                continue;
            }
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

/// Checks if a param type matches the validator's Data type.
/// Supports both qualified (`validator.Data`) and imported
/// (`Data`) forms for flexibility in handler signatures.
///
fn is_valid_data_param(param_type: &str, validator: &str, controller_path: &str) -> bool {
    let param_type = param_type.trim();

    if param_type == format!("{}.Data", validator) {
        return true;
    }

    if param_type == "Data" {
        return check_validator_data_import(controller_path, validator);
    }

    false
}

/// Verifies the controller imports Data from the validator.
/// Required when using unqualified `Data` type to ensure
/// it refers to the correct validator's type.
///
fn check_validator_data_import(controller_path: &str, validator: &str) -> bool {
    let content = match fs::read_to_string(controller_path) {
        Ok(c) => c,
        Err(_) => return false,
    };

    let pattern = format!(
        r"import\s+app/http/validators/{}\s*\.\s*\{{[^}}]*\bData\b[^}}]*\}}",
        regex::escape(validator)
    );
    let re = match Regex::new(&pattern) {
        Ok(r) => r,
        Err(_) => return false,
    };

    content
        .lines()
        .filter(|line| !line.trim().starts_with("//"))
        .any(|line| re.is_match(line))
}

/// Validates `req` param has correct wisp.Request type.
/// Accepts both qualified and imported forms to support
/// different coding styles in controllers.
///
fn is_valid_request_type(param_type: &str, controller_path: &str) -> bool {
    let param_type = param_type.trim();

    if param_type == "wisp.Request" {
        return true;
    }

    if param_type == "Request" {
        return check_wisp_request_import(controller_path);
    }

    false
}

/// Verifies the controller imports Request from wisp.
/// Required when using unqualified `Request` type to
/// ensure it's the correct wisp type.
///
fn check_wisp_request_import(controller_path: &str) -> bool {
    let content = match fs::read_to_string(controller_path) {
        Ok(c) => c,
        Err(_) => return false,
    };

    content
        .lines()
        .filter(|line| !line.trim().starts_with("//"))
        .any(|line| patterns::WISP_REQUEST_IMPORT.is_match(line))
}

/// Validates `ctx` param has correct ctx.Context type.
/// Accepts both qualified and imported forms to support
/// different coding styles in controllers.
///
fn is_valid_context_type(param_type: &str, controller_path: &str) -> bool {
    let param_type = param_type.trim();

    if param_type == "ctx.Context" {
        return true;
    }

    if param_type == "Context" {
        return check_ctx_context_import(controller_path);
    }

    false
}

/// Verifies the controller imports Context from ctx module.
/// Required when using unqualified `Context` type to
/// ensure it's the correct application context type.
///
fn check_ctx_context_import(controller_path: &str) -> bool {
    let content = match fs::read_to_string(controller_path) {
        Ok(c) => c,
        Err(_) => return false,
    };

    content
        .lines()
        .filter(|line| !line.trim().starts_with("//"))
        .any(|line| patterns::CTX_CONTEXT_IMPORT.is_match(line))
}

/// Ensures handlers return wisp.Response type. All route
/// handlers must return a Response so the framework can
/// send it back to the client.
///
fn validate_return_type(route: &Route, handler: &str) -> Result<(), String> {
    let return_type = route.return_type.trim();

    if return_type.is_empty() {
        return Err(format!(
            "{} must have a return type annotation of wisp.Response",
            handler
        ));
    }

    if return_type == "wisp.Response" {
        return Ok(());
    }

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

    Err(format!(
        "{} must return wisp.Response, got {}",
        handler, return_type
    ))
}

// ------------------------------------------------------------- Unit Tests

#[cfg(test)]
mod tests {
    use super::*;
    use crate::routes::parser::FunctionParam;

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

    // ----------------------------------------- validate_path tests

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

    // ----------------------------------------- validate_path_params tests

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

    // ----------------------------------------- validate_return_type tests

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

    // ----------------------------------------- validate_handler_params tests

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

    // ----------------------------------------- get_controller_name tests

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

    // ----------------------------------------- is_valid_data_param tests

    #[test]
    fn test_is_valid_data_param_qualified() {
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

    // ----------------------------------------- is_valid_request_type tests

    #[test]
    fn test_is_valid_request_type_qualified() {
        assert!(is_valid_request_type("wisp.Request", "nonexistent.gleam"));
    }

    #[test]
    fn test_is_valid_request_type_wrong() {
        assert!(!is_valid_request_type("String", "nonexistent.gleam"));
        assert!(!is_valid_request_type("other.Request", "nonexistent.gleam"));
    }

    // ----------------------------------------- is_valid_context_type tests

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
