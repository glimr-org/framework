//! Route Regex Patterns
//!
//! Static regex patterns for parsing controller annotations and
//! function signatures. Uses LazyLock for one-time compilation
//! at first access, avoiding repeated regex parsing overhead.
//!

use regex::Regex;
use std::sync::LazyLock;

// ------------------------------------------------------------- Public Consts

/// Captures HTTP method annotations like `@get "/path"`.
/// Used by the parser to identify route handlers and extract
/// their HTTP method and URL path in a single match.
///
pub static METHOD: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"///\s*@(get|post|put|delete|patch|head|options)\s+"([^"]+)""#).unwrap()
});

/// Captures middleware annotations like `@middleware "name"`.
/// Middleware runs before a handler and can short-circuit the
/// request or modify context before reaching the controller.
///
pub static MIDDLEWARE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"///\s*@middleware\s+"([^"]+)""#).unwrap());

/// Captures validator annotations like `@validator "name"`.
/// Validators run before handlers to ensure request data
/// meets requirements before processing begins.
///
pub static VALIDATOR: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"///\s*@validator\s+"([^"]+)""#).unwrap());

/// Captures temporary redirect annotations (302 status).
/// Used for routes that should redirect but may change
/// destination in the future.
///
pub static REDIRECT: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"///\s*@redirect\s+"([^"]+)""#).unwrap());

/// Captures permanent redirect annotations (301 status).
/// Used for routes that have permanently moved, allowing
/// browsers and search engines to cache the redirect.
///
pub static REDIRECT_PERM: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"///\s*@redirect_permanent\s+"([^"]+)""#).unwrap());

/// Captures public function declarations to find handlers.
/// Route annotations must precede a `pub fn` to be valid,
/// so we use this to associate annotations with functions.
///
pub static PUB_FN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"pub\s+fn\s+(\w+)\s*\(").unwrap());

/// Captures group-level middleware from file headers.
/// Unlike per-route middleware, group middleware applies
/// to all routes in a controller file.
///
pub static GROUP_MIDDLEWARE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"//\s*@group_middleware\s+"([^"]+)""#).unwrap());

/// Detects if a file imports wisp.Response type.
/// Used to validate that handlers returning Response have
/// the necessary import, catching errors at compile time.
///
pub static WISP_RESPONSE_IMPORT: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"import\s+wisp\s*\.\s*\{[^}]*\bResponse\b[^}]*\}").unwrap());

/// Captures function return types from signatures.
/// Needed to verify handlers return appropriate types
/// and to generate correct routing code.
///
pub static RETURN_TYPE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"->\s*([A-Za-z_][A-Za-z0-9_\.]*(?:\([^)]*\))?)").unwrap());

/// Validates URL path format for route annotations.
/// Ensures paths start with `/`, use valid characters,
/// and have properly formed path parameters like `:id`.
///
pub static VALID_PATH: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(/([a-zA-Z0-9_-]+|:[a-zA-Z_][a-zA-Z0-9_]*))*/?$|^/$").unwrap());

/// Extracts path parameter names from URL patterns.
/// Converts `:user_id` to `user_id` so the generator can
/// create code that binds URL segments to variables.
///
pub static PATH_PARAM: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r":([a-zA-Z_][a-zA-Z0-9_]*)").unwrap());

/// Detects if a file imports wisp.Request type.
/// Handlers receiving Request must have this import,
/// allowing early detection of missing dependencies.
///
pub static WISP_REQUEST_IMPORT: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"import\s+wisp\s*\.\s*\{[^}]*\bRequest\b[^}]*\}").unwrap());

/// Detects if a file imports the Context type.
/// Context provides request state and is required by
/// handlers that access session, auth, or other state.
///
pub static CTX_CONTEXT_IMPORT: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"import\s+app/http/context/ctx\s*\.\s*\{[^}]*\bContext\b[^}]*\}").unwrap()
});

/// Extracts group name from route config definitions.
/// Used when parsing config_route.gleam to determine
/// how routes should be split across output files.
///
pub static CONFIG_NAME: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"name:\s*"([^"]+)""#).unwrap());

/// Extracts prefix from route config definitions.
/// The prefix determines which routes belong to a group
/// by matching the start of their URL paths.
///
pub static CONFIG_PREFIX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"prefix:\s*"([^"]*)""#).unwrap());
