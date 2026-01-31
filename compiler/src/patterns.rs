use regex::Regex;
use std::sync::LazyLock;
/*
Moving al parser regex patterns here
*/
#[allow(dead_code)]
pub static METHOD: LazyLock<Regex> = LazyLock::new(||{
    Regex::new(r#"///\s*@(get|post|put|delete|patch|head|options)\s+"([^"]+)""#).unwrap()
});

pub static MIDDLEWARE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"///\s*@middleware\s+"([^"]+)""#).unwrap()
});

pub static VALIDATOR: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"///\s*@validator\s+"([^"]+)""#).unwrap()
});

pub static REDIRECT: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"///\s*@redirect\s+"([^"]+)""#).unwrap()
});

pub static REDIRECT_PERM: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"///\s*@redirect_permanent\s+"([^"]+)""#).unwrap()
});

pub static PUB_FN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"pub\s+fn\s+(\w+)\s*\(").unwrap()
});

pub static GROUP_MIDDLEWARE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"//\s*@group_middleware\s+"([^"]+)""#).unwrap()
});

pub static WISP_RESPONSE_IMPORT: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"import\s+wisp\s*\.\s*\{[^}]*\bResponse\b[^}]*\}").unwrap()
});

pub static RETURN_TYPE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"->\s*([A-Za-z_][A-Za-z0-9_\.]*(?:\([^)]*\))?)").unwrap()
});


/*
Moving all validator regex patterns here 
*/

pub static VALID_PATH: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"^(/([a-zA-Z0-9_-]+|:[a-zA-Z_][a-zA-Z0-9_]*))*/?$|^/$").unwrap()
});

pub static PATH_PARAM: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r":([a-zA-Z_][a-zA-Z0-9_]*)").unwrap()
});

pub static WISP_REQUEST_IMPORT: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"import\s+wisp\s*\.\s*\{[^}]*\bRequest\b[^}]*\}").unwrap()
});

pub static CTX_CONTEXT_IMPORT: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"import\s+app/http/context/ctx\s*\.\s*\{[^}]*\bContext\b[^}]*\}").unwrap()
});

/*
Moving all config regex patterns here
*/

pub static CONFIG_NAME: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"name:\s*"([^"]+)""#).unwrap()
});

pub static CONFIG_PREFIX: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"prefix:\s*"([^"]*)""#).unwrap()
});