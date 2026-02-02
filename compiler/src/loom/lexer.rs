//! Template Lexer
//!
//! Converts template source text into a stream of tokens.
//! Recognizes variables, directives, components, and plain
//! text for the parser to process.
//!

// ------------------------------------------------------------- Public Types

/// Represents a token produced by the lexer. Each variant
/// corresponds to a template syntax element like variables,
/// directives, or component tags.
///
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Text(String),
    Variable(String),
    RawVariable(String),
    Slot(Option<String>),
    SlotDef(Option<String>),
    SlotDefEnd,
    Attributes,
    Component {
        name: String,
        attributes: Vec<ComponentAttr>,
        self_closing: bool,
    },
    ComponentEnd(String),
    Element {
        tag: String,
        attributes: Vec<ComponentAttr>,
        self_closing: bool,
    },
    ElementEnd(String),
}

/// Represents an attribute on a component tag. Can be a
/// string literal, an expression to evaluate, a boolean
/// attribute with no value, or conditional class/style
/// expressions.
///
#[derive(Debug, Clone, PartialEq)]
pub enum ComponentAttr {
    StringAttr {
        name: String,
        value: String,
    },
    ExprAttr {
        name: String,
        value: String,
    },
    BoolAttr(String),
    ClassAttr(String),
    StyleAttr(String),
    LmIf(String),
    LmElseIf(String),
    LmElse,
    LmFor {
        collection: String,
        items: Vec<String>,
        loop_var: Option<String>,
    },
}

/// Errors that can occur during lexical analysis. Includes
/// position information to help locate the error in the
/// source template.
///
#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
    /// A variable expression {{ or {{{ was opened but never
    /// closed with }} or }}}.
    ///
    UnterminatedVariable(usize),
    /// A variable name contained invalid characters or was
    /// empty.
    ///
    InvalidVariableName(String, usize),
    /// A component or slot tag was opened but never properly
    /// closed with > or />.
    ///
    UnterminatedComponent(usize),
}

// ------------------------------------------------------------- Private Types

/// Parsed component tag result tuple containing the tag name,
/// list of parsed attributes, whether self-closing, and how
/// many bytes were consumed from input.
///
type ComponentTag = (String, Vec<ComponentAttr>, bool, usize);

/// Internal lexer state tracking the remaining input, current
/// position for error reporting, and accumulated tokens.
///
struct Lexer<'a> {
    /// Remaining input to be processed. Advanced as tokens are
    /// consumed.
    ///
    input: &'a str,
    /// Current byte position in the original input for error
    /// messages.
    ///
    position: usize,
    /// Accumulated tokens produced during lexing.
    ///
    tokens: Vec<Token>,
}

// ------------------------------------------------------------- Private Implementations

impl<'a> Lexer<'a> {
    /// Creates a new lexer for the given input string. Initializes
    /// position to zero and creates an empty token buffer ready
    /// to accumulate parsed tokens.
    ///
    fn new(input: &'a str) -> Self {
        Self {
            input,
            position: 0,
            tokens: Vec::new(),
        }
    }

    /// Main tokenization loop. Processes input until exhausted,
    /// dispatching to specialized parsers based on what syntax
    /// element is detected at the current position.
    ///
    fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        while !self.input.is_empty() {
            if self.input.starts_with("{{{") {
                self.parse_raw_variable()?;
            } else if self.input.starts_with("{{") {
                self.parse_variable()?;
            } else if self.input.starts_with("@attributes") {
                self.parse_attributes_directive()?;
            } else if self.input.starts_with("<slot") {
                self.parse_slot_element()?;
            } else if self.input.starts_with("</slot>") {
                self.advance(7);
                self.tokens.push(Token::SlotDefEnd);
            } else if self.input.starts_with("<x-") {
                self.parse_component()?;
            } else if self.input.starts_with("</x-") {
                self.parse_component_end()?;
            } else if self.input.starts_with("</") {
                self.parse_element_end()?;
            } else if self.input.starts_with('<') && self.is_potential_element_start() {
                self.try_parse_element()?;
            } else {
                self.consume_text()?;
            }
        }

        Ok(std::mem::take(&mut self.tokens))
    }

    /// Advances the lexer position by n bytes. Updates both the
    /// input slice and position counter to keep them in sync
    /// for accurate error reporting.
    ///
    fn advance(&mut self, n: usize) {
        self.input = &self.input[n..];
        self.position += n;
    }

    /// Returns the next character without consuming it. Used to
    /// look ahead when deciding how to parse without modifying
    /// the lexer state.
    ///
    fn peek_char(&self) -> Option<char> {
        self.input.chars().next()
    }

    /// Checks if current position could be start of an HTML
    /// element by verifying < is followed by a letter. Prevents
    /// treating comparison operators as element starts.
    ///
    fn is_potential_element_start(&self) -> bool {
        self.input
            .chars()
            .nth(1)
            .map(|c| c.is_ascii_alphabetic())
            .unwrap_or(false)
    }

    /// Parses a raw variable {{{ expr }}} that outputs unescaped
    /// HTML. Validates the expression name before emitting the
    /// RawVariable token.
    ///
    fn parse_raw_variable(&mut self) -> Result<(), LexerError> {
        let start_pos = self.position;
        self.advance(3); // Skip "{{{"

        if let Some(end) = self.input.find("}}}") {
            let name = self.input[..end].trim().to_string();
            if is_valid_variable_name(&name) {
                self.advance(end + 3);
                self.tokens.push(Token::RawVariable(name));
                Ok(())
            } else {
                Err(LexerError::InvalidVariableName(name, start_pos))
            }
        } else {
            Err(LexerError::UnterminatedVariable(start_pos))
        }
    }

    /// Parses an escaped variable {{ expr }} that outputs HTML-
    /// escaped content. Validates the expression name before
    /// emitting the Variable token.
    ///
    fn parse_variable(&mut self) -> Result<(), LexerError> {
        let start_pos = self.position;
        self.advance(2); // Skip "{{"

        if let Some(end) = self.input.find("}}") {
            let name = self.input[..end].trim().to_string();
            if is_valid_variable_name(&name) {
                self.advance(end + 2);
                self.tokens.push(Token::Variable(name));
                Ok(())
            } else {
                Err(LexerError::InvalidVariableName(name, start_pos))
            }
        } else {
            Err(LexerError::UnterminatedVariable(start_pos))
        }
    }

    /// Parses the @attributes directive for attribute pass-
    /// through in components. Checks it's not part of a longer
    /// word.
    ///
    fn parse_attributes_directive(&mut self) -> Result<(), LexerError> {
        self.advance(11); // Skip "@attributes"

        // Check if followed by alphanumeric (would be part of a longer word)
        if let Some(c) = self.peek_char()
            && c.is_alphanumeric()
        {
            // Not a directive, treat as text
            self.append_text("@attributes");
            return Ok(());
        }

        self.tokens.push(Token::Attributes);
        Ok(())
    }

    /// Parses a <slot> element which can be self-closing (slot
    /// usage) or have content (slot definition with fallback
    /// content).
    ///
    fn parse_slot_element(&mut self) -> Result<(), LexerError> {
        let start_pos = self.position;
        self.advance(5); // Skip "<slot"

        match self.input.chars().next() {
            Some('/') if self.input.starts_with("/>") => {
                self.advance(2);
                self.tokens.push(Token::Slot(None));
                Ok(())
            }
            Some('>') => {
                self.advance(1);
                self.tokens.push(Token::SlotDef(None));
                Ok(())
            }
            Some(' ') | Some('\t') | Some('\n') => self.parse_slot_with_attrs(start_pos),
            _ => Err(LexerError::UnterminatedComponent(start_pos)),
        }
    }

    /// Parses a slot element that has attributes. Extracts the
    /// optional name attribute and determines if self-closing
    /// to emit the correct token type.
    ///
    fn parse_slot_with_attrs(&mut self, start_pos: usize) -> Result<(), LexerError> {
        // Find the end of the tag
        if let Some((attrs_part, self_closing, rest_start)) = self.find_slot_tag_end() {
            let name = extract_slot_name(&attrs_part);
            self.advance(rest_start);

            if self_closing {
                self.tokens.push(Token::Slot(name));
            } else {
                self.tokens.push(Token::SlotDef(name));
            }
            Ok(())
        } else {
            Err(LexerError::UnterminatedComponent(start_pos))
        }
    }

    /// Finds where the slot tag ends, returning the attributes
    /// string, whether self-closing, and bytes consumed. Scans
    /// for > or /> to determine the tag boundary.
    ///
    fn find_slot_tag_end(&self) -> Option<(String, bool, usize)> {
        for (i, c) in self.input.char_indices() {
            if c == '/' && self.input[i..].starts_with("/>") {
                let attrs = self.input[..i].to_string();
                return Some((attrs, true, i + 2));
            }
            if c == '>' {
                let attrs = self.input[..i].to_string();
                return Some((attrs, false, i + 1));
            }
        }
        None
    }

    /// Parses a component tag <x-name>. Extracts the component
    /// name and any attributes from the opening tag to build
    /// the Component token.
    ///
    fn parse_component(&mut self) -> Result<(), LexerError> {
        let start_pos = self.position;
        self.advance(3); // Skip "<x-"

        if let Some((name, attrs, self_closing, consumed)) = self.parse_component_tag()? {
            self.advance(consumed);
            self.tokens.push(Token::Component {
                name,
                attributes: attrs,
                self_closing,
            });
            Ok(())
        } else {
            Err(LexerError::UnterminatedComponent(start_pos))
        }
    }

    /// Parses a component closing tag </x-name>. Extracts the
    /// component name for matching with the opening tag during
    /// parsing to build the AST.
    ///
    fn parse_component_end(&mut self) -> Result<(), LexerError> {
        let start_pos = self.position;
        self.advance(4); // Skip "</x-"

        if let Some(end) = self.input.find('>') {
            let name = self.input[..end].trim().to_string();
            self.advance(end + 1);
            self.tokens.push(Token::ComponentEnd(name));
            Ok(())
        } else {
            Err(LexerError::UnterminatedComponent(start_pos))
        }
    }

    /// Attempts to parse an HTML element with dynamic attributes.
    /// Only emits Element token if l-if, l-for, or expression
    /// attributes are present.
    ///
    fn try_parse_element(&mut self) -> Result<(), LexerError> {
        if let Some((tag, attrs, self_closing, consumed)) = self.parse_element_tag()? {
            if has_dynamic_attrs(&attrs) {
                self.advance(consumed);
                self.tokens.push(Token::Element {
                    tag,
                    attributes: attrs,
                    self_closing,
                });
            } else {
                // No dynamic attributes - emit "<" as text
                self.append_text("<");
                self.advance(1);
            }
        } else {
            // Not a valid element, consume "<" as text
            self.append_text("<");
            self.advance(1);
        }
        Ok(())
    }

    /// Parses an element closing tag </tag>. Only emits
    /// ElementEnd if there's a matching Element token, otherwise
    /// treats it as plain text.
    ///
    fn parse_element_end(&mut self) -> Result<(), LexerError> {
        self.advance(2); // Skip "</"

        if let Some(end) = self.input.find('>') {
            let tag = self.input[..end].trim().to_string();

            if self.has_matching_element(&tag) {
                self.advance(end + 1);
                self.tokens.push(Token::ElementEnd(tag));
            } else {
                // No matching Element, emit as text
                let text = format!("</{}>", tag);
                self.advance(end + 1);
                self.append_text(&text);
            }
        } else {
            // Invalid closing tag, treat as text
            self.append_text("</");
        }
        Ok(())
    }

    /// Checks if there's an unmatched Element token for this tag.
    /// Counts Element and ElementEnd tokens to track nesting
    /// depth and determine if a closing tag is valid.
    ///
    fn has_matching_element(&self, tag: &str) -> bool {
        let mut element_count = 0;
        let mut end_count = 0;

        for token in &self.tokens {
            match token {
                Token::Element {
                    tag: t,
                    self_closing: false,
                    ..
                } if t == tag => {
                    element_count += 1;
                }
                Token::ElementEnd(t) if t == tag => {
                    end_count += 1;
                }
                _ => {}
            }
        }

        element_count > end_count
    }

    /// Parses a component tag after the <x- prefix. Extracts
    /// name and attributes, determines if self-closing. Returns
    /// None if the tag is malformed.
    ///
    fn parse_component_tag(&mut self) -> Result<Option<ComponentTag>, LexerError> {
        let (name, name_len) = take_component_name(self.input);
        if name.is_empty() {
            return Ok(None);
        }

        let rest = &self.input[name_len..];
        let (attrs, attrs_len) = parse_component_attrs(rest);
        let rest = &rest[attrs_len..];
        let rest = skip_whitespace(rest);
        let ws_len = attrs_len
            + (rest.as_ptr() as usize - self.input[name_len + attrs_len..].as_ptr() as usize);

        if rest.starts_with("/>") {
            Ok(Some((name, attrs, true, name_len + ws_len + 2)))
        } else if rest.starts_with('>') {
            Ok(Some((name, attrs, false, name_len + ws_len + 1)))
        } else {
            Ok(None)
        }
    }

    /// Parses an HTML element tag. Similar to component parsing
    /// but for regular HTML elements with dynamic attributes.
    /// Returns None for invalid or non-element tags.
    ///
    fn parse_element_tag(&mut self) -> Result<Option<ComponentTag>, LexerError> {
        // Skip the '<' we already checked
        let input = &self.input[1..];
        let (tag, tag_len) = take_component_name(input);

        if tag.is_empty() || tag.starts_with("x-") {
            return Ok(None);
        }

        let rest = &input[tag_len..];
        let (attrs, attrs_len) = parse_element_attrs(rest);
        let rest = &rest[attrs_len..];
        let rest = skip_whitespace(rest);
        let total_len = 1
            + tag_len
            + attrs_len
            + (rest.as_ptr() as usize - input[tag_len + attrs_len..].as_ptr() as usize);

        if rest.starts_with("/>") {
            Ok(Some((tag, attrs, true, total_len + 2)))
        } else if rest.starts_with('>') {
            Ok(Some((tag, attrs, false, total_len + 1)))
        } else {
            Ok(None)
        }
    }

    /// Consumes plain text until a special syntax marker is
    /// found. Appends to the last Text token if possible to
    /// minimize token count and improve parsing efficiency.
    ///
    fn consume_text(&mut self) -> Result<(), LexerError> {
        let (text, consumed) = take_until_special(self.input);

        if consumed == 0 {
            // Take single character
            if let Some(c) = self.peek_char() {
                self.append_text(&c.to_string());
                self.advance(c.len_utf8());
            }
        } else {
            self.append_text(&text);
            self.advance(consumed);
        }
        Ok(())
    }

    /// Appends text to the token list, merging with previous
    /// Text token if one exists. Reduces token count and avoids
    /// fragmenting text content unnecessarily.
    ///
    fn append_text(&mut self, text: &str) {
        if let Some(Token::Text(prev)) = self.tokens.last_mut() {
            prev.push_str(text);
        } else {
            self.tokens.push(Token::Text(text.to_string()));
        }
    }
}

// ------------------------------------------------------------- Public Functions

/// Tokenizes template source into a list of tokens. This is
/// the main entry point for lexing, creating a Lexer instance
/// and processing the entire input.
///
pub fn tokenize(input: &str) -> Result<Vec<Token>, LexerError> {
    let mut lexer = Lexer::new(input);
    lexer.tokenize()
}

/// Converts a lexer error to a human-readable string. Used
/// for displaying error messages to users when template
/// compilation fails.
///
pub fn error_to_string(err: &LexerError) -> String {
    match err {
        LexerError::UnterminatedVariable(pos) => {
            format!("Unterminated variable at position {}", pos)
        }
        LexerError::InvalidVariableName(name, pos) => {
            format!("Invalid variable name '{}' at position {}", name, pos)
        }
        LexerError::UnterminatedComponent(pos) => {
            format!("Unterminated component at position {}", pos)
        }
    }
}

// ------------------------------------------------------------- Private Functions

/// Validates a variable name contains only allowed characters.
/// Permits alphanumerics, underscores, and dots for field
/// access.
///
fn is_valid_variable_name(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }
    name.chars()
        .all(|c| c.is_alphanumeric() || c == '_' || c == '.')
}

/// Extracts a component or tag name from input. Stops at
/// whitespace, >, /, or = characters that mark the end of
/// a name.
///
fn take_component_name(input: &str) -> (String, usize) {
    let mut end = 0;
    for c in input.chars() {
        match c {
            ' ' | '\t' | '\n' | '>' | '/' => break,
            _ => end += c.len_utf8(),
        }
    }
    (input[..end].to_string(), end)
}

/// Skips leading whitespace characters from input. Returns
/// a slice starting at the first non-whitespace character.
///
fn skip_whitespace(input: &str) -> &str {
    input.trim_start_matches([' ', '\t', '\n', '\r'])
}

/// Consumes input until a special template syntax marker is
/// found. Stops at {{, {{{, @attributes, <slot, <x-, </,
/// or potential element starts.
///
fn take_until_special(input: &str) -> (String, usize) {
    let mut end = 0;

    while end < input.len() {
        let remaining = &input[end..];

        if remaining.starts_with("{{{")
            || remaining.starts_with("{{")
            || remaining.starts_with("@attributes")
            || remaining.starts_with("</slot>")
            || remaining.starts_with("<slot")
            || remaining.starts_with("<x-")
            || remaining.starts_with("</x-")
            || remaining.starts_with("</")
        {
            break;
        }

        if remaining.starts_with('<') {
            // Check if potential element start
            if remaining
                .chars()
                .nth(1)
                .map(|c| c.is_ascii_alphabetic())
                .unwrap_or(false)
            {
                break;
            }
        }

        if let Some(c) = remaining.chars().next() {
            end += c.len_utf8();
        } else {
            break;
        }
    }

    (input[..end].to_string(), end)
}

/// Checks if any attribute requires runtime evaluation.
/// Elements with only static string attributes are left as
/// plain text.
///
fn has_dynamic_attrs(attrs: &[ComponentAttr]) -> bool {
    attrs.iter().any(|attr| {
        matches!(
            attr,
            ComponentAttr::LmIf(_)
                | ComponentAttr::LmElseIf(_)
                | ComponentAttr::LmElse
                | ComponentAttr::LmFor { .. }
                | ComponentAttr::ClassAttr(_)
                | ComponentAttr::StyleAttr(_)
                | ComponentAttr::ExprAttr { .. }
        )
    })
}

/// Extracts the name attribute value from a slot element's
/// attribute string. Handles both single and double quoted
/// values.
///
fn extract_slot_name(attrs: &str) -> Option<String> {
    // Try name="value"
    if let Some(start) = attrs.find("name=\"") {
        let rest = &attrs[start + 6..];
        if let Some(end) = rest.find('"') {
            return Some(rest[..end].to_string());
        }
    }
    // Try name='value'
    if let Some(start) = attrs.find("name='") {
        let rest = &attrs[start + 6..];
        if let Some(end) = rest.find('\'') {
            return Some(rest[..end].to_string());
        }
    }
    None
}

/// Parses all attributes from a component or element tag.
/// Handles :expr, l-if, l-else-if, l-else, l-for, and
/// regular string/boolean attributes.
///
fn parse_component_attrs(input: &str) -> (Vec<ComponentAttr>, usize) {
    let mut attrs = Vec::new();
    let mut pos = 0;
    let original_len = input.len();
    let mut input = skip_whitespace(input);
    pos += original_len - input.len();

    loop {
        let before_ws_len = input.len();
        input = skip_whitespace(input);
        pos += before_ws_len - input.len();

        if input.is_empty() || input.starts_with('>') || input.starts_with("/>") {
            break;
        }

        if input.starts_with(':')
            && let Some((attr, consumed)) = parse_expr_attr(&input[1..])
        {
            attrs.push(attr);
            input = &input[1 + consumed..];
            pos += 1 + consumed;
            continue;
        }

        if input.starts_with("l-if=")
            && let Some((condition, consumed)) = parse_lm_condition_attr(&input[5..])
        {
            attrs.push(ComponentAttr::LmIf(condition));
            input = &input[5 + consumed..];
            pos += 5 + consumed;
            continue;
        }

        if input.starts_with("l-else-if=")
            && let Some((condition, consumed)) = parse_lm_condition_attr(&input[10..])
        {
            attrs.push(ComponentAttr::LmElseIf(condition));
            input = &input[10 + consumed..];
            pos += 10 + consumed;
            continue;
        }

        if input.starts_with("l-else") {
            attrs.push(ComponentAttr::LmElse);
            input = skip_whitespace(&input[6..]);
            pos += 6;
            continue;
        }

        if input.starts_with("l-for=")
            && let Some((collection, items, loop_var, consumed)) = parse_lm_for_attr(&input[6..])
        {
            attrs.push(ComponentAttr::LmFor {
                collection,
                items,
                loop_var,
            });
            input = &input[6 + consumed..];
            pos += 6 + consumed;
            continue;
        }

        if let Some((attr, consumed)) = parse_string_or_bool_attr(input) {
            attrs.push(attr);
            input = &input[consumed..];
            pos += consumed;
            continue;
        }

        // Skip unknown character
        if let Some(c) = input.chars().next() {
            input = &input[c.len_utf8()..];
            pos += c.len_utf8();
        } else {
            break;
        }
    }

    (attrs, pos)
}

/// Parses attributes for regular HTML elements. Currently
/// delegates to component attribute parsing as they share
/// the same syntax.
///
fn parse_element_attrs(input: &str) -> (Vec<ComponentAttr>, usize) {
    // Same as component attrs for now
    parse_component_attrs(input)
}

/// Parses an expression attribute :name="value". Special-
/// cases :class and :style to their own attribute types
/// for conditional styling support.
///
fn parse_expr_attr(input: &str) -> Option<(ComponentAttr, usize)> {
    let (name, name_len) = take_attr_name(input);
    if name.is_empty() {
        return None;
    }

    let rest = &input[name_len..];

    if let Some(value_start) = rest.strip_prefix("=\"")
        && let Some(end) = value_start.find('"')
    {
        let value = normalize_quotes(&value_start[..end]);
        let attr = make_expr_attr(&name, &value);
        return Some((attr, name_len + 3 + end));
    }

    if let Some(value_start) = rest.strip_prefix("='")
        && let Some(end) = value_start.find('\'')
    {
        let value = normalize_quotes(&value_start[..end]);
        let attr = make_expr_attr(&name, &value);
        return Some((attr, name_len + 3 + end));
    }

    None
}

/// Creates the appropriate expression attribute type. Routes
/// class and style to their specialized types for conditional
/// list handling.
///
fn make_expr_attr(name: &str, value: &str) -> ComponentAttr {
    match name {
        "class" => ComponentAttr::ClassAttr(value.to_string()),
        "style" => ComponentAttr::StyleAttr(value.to_string()),
        _ => ComponentAttr::ExprAttr {
            name: name.to_string(),
            value: value.to_string(),
        },
    }
}

/// Normalizes single quotes to double quotes in attribute
/// values. Ensures consistent quote style in generated Gleam
/// code.
///
fn normalize_quotes(value: &str) -> String {
    value.replace('\'', "\"")
}

/// Parses a regular string attribute name="value" or boolean
/// attribute name. Boolean attributes have no value and are
/// true by presence.
///
fn parse_string_or_bool_attr(input: &str) -> Option<(ComponentAttr, usize)> {
    let (name, name_len) = take_attr_name(input);
    if name.is_empty() {
        return None;
    }

    let rest = &input[name_len..];

    if let Some(value_start) = rest.strip_prefix("=\"")
        && let Some(end) = value_start.find('"')
    {
        let value = value_start[..end].to_string();
        return Some((
            ComponentAttr::StringAttr { name, value },
            name_len + 3 + end,
        ));
    }

    if let Some(value_start) = rest.strip_prefix("='")
        && let Some(end) = value_start.find('\'')
    {
        let value = value_start[..end].to_string();
        return Some((
            ComponentAttr::StringAttr { name, value },
            name_len + 3 + end,
        ));
    }

    Some((ComponentAttr::BoolAttr(name), name_len))
}

/// Extracts an attribute name from input. Stops at whitespace,
/// equals sign, or tag closing characters.
///
fn take_attr_name(input: &str) -> (String, usize) {
    let mut end = 0;
    for c in input.chars() {
        match c {
            ' ' | '\t' | '\n' | '=' | '>' | '/' => break,
            _ => end += c.len_utf8(),
        }
    }
    (input[..end].to_string(), end)
}

/// Parses the quoted value of an l-if or l-else-if attribute.
/// Handles both single and double quoted condition
/// expressions.
///
fn parse_lm_condition_attr(input: &str) -> Option<(String, usize)> {
    if let Some(value_start) = input.strip_prefix('"')
        && let Some(end) = value_start.find('"')
    {
        return Some((value_start[..end].to_string(), 2 + end));
    }
    if let Some(value_start) = input.strip_prefix('\'')
        && let Some(end) = value_start.find('\'')
    {
        return Some((value_start[..end].to_string(), 2 + end));
    }
    None
}

/// Parses an l-for attribute value like "item in items" or
/// "(key, val) in items, loop". Extracts collection, item
/// variables, and optional loop variable.
///
fn parse_lm_for_attr(input: &str) -> Option<(String, Vec<String>, Option<String>, usize)> {
    let (value, consumed) = if let Some(value_start) = input.strip_prefix('"') {
        let end = value_start.find('"')?;
        (value_start[..end].to_string(), 2 + end)
    } else if let Some(value_start) = input.strip_prefix('\'') {
        let end = value_start.find('\'')?;
        (value_start[..end].to_string(), 2 + end)
    } else {
        return None;
    };

    // Parse "item in collection" or "(key, val) in items, loop"
    let parts: Vec<&str> = value.splitn(2, " in ").collect();
    if parts.len() != 2 {
        return None;
    }

    let item_part = parts[0].trim();
    let collection_part = parts[1].trim();

    // Parse items
    let items = parse_item_pattern(item_part);

    // Parse collection and optional loop variable
    let (collection, loop_var) = parse_collection_and_loop(collection_part);

    Some((collection, items, loop_var, consumed))
}

/// Parses the item pattern from an l-for attribute. Handles
/// both single items and tuple destructuring like (key, val).
///
fn parse_item_pattern(pattern: &str) -> Vec<String> {
    if let Some(inner) = pattern.strip_prefix('(').and_then(|s| s.strip_suffix(')')) {
        inner.split(',').map(|s| s.trim().to_string()).collect()
    } else {
        vec![pattern.to_string()]
    }
}

/// Parses the collection expression and optional loop variable
/// from an l-for attribute. Loop variable provides iteration
/// metadata like index.
///
fn parse_collection_and_loop(input: &str) -> (String, Option<String>) {
    if let Some((collection, loop_var)) = input.split_once(',') {
        (
            collection.trim().to_string(),
            Some(loop_var.trim().to_string()),
        )
    } else {
        (input.to_string(), None)
    }
}

// ------------------------------------------------------------- Unit Tests

#[cfg(test)]
mod tests {
    use super::*;

    // ----------------------------------------- tokenize tests

    #[test]
    fn test_tokenize_text() {
        let tokens = tokenize("Hello World").unwrap();
        assert_eq!(tokens, vec![Token::Text("Hello World".to_string())]);
    }

    #[test]
    fn test_tokenize_variable() {
        let tokens = tokenize("{{ name }}").unwrap();
        assert_eq!(tokens, vec![Token::Variable("name".to_string())]);
    }

    #[test]
    fn test_tokenize_raw_variable() {
        let tokens = tokenize("{{{ html }}}").unwrap();
        assert_eq!(tokens, vec![Token::RawVariable("html".to_string())]);
    }

    #[test]
    fn test_tokenize_mixed() {
        let tokens = tokenize("Hello {{ name }}!").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Text("Hello ".to_string()),
                Token::Variable("name".to_string()),
                Token::Text("!".to_string()),
            ]
        );
    }

    #[test]
    fn test_tokenize_attributes() {
        let tokens = tokenize("@attributes").unwrap();
        assert_eq!(tokens, vec![Token::Attributes]);
    }

    #[test]
    fn test_tokenize_slot() {
        let tokens = tokenize("<slot />").unwrap();
        assert_eq!(tokens, vec![Token::Slot(None)]);
    }

    #[test]
    fn test_tokenize_named_slot() {
        let tokens = tokenize("<slot name=\"header\" />").unwrap();
        assert_eq!(tokens, vec![Token::Slot(Some("header".to_string()))]);
    }

    #[test]
    fn test_tokenize_slot_def() {
        let tokens = tokenize("<slot>fallback</slot>").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::SlotDef(None),
                Token::Text("fallback".to_string()),
                Token::SlotDefEnd,
            ]
        );
    }

    #[test]
    fn test_tokenize_component() {
        let tokens = tokenize("<x-button />").unwrap();
        assert_eq!(
            tokens,
            vec![Token::Component {
                name: "button".to_string(),
                attributes: vec![],
                self_closing: true,
            }]
        );
    }

    #[test]
    fn test_tokenize_component_with_attrs() {
        let tokens = tokenize("<x-button class=\"btn\" :disabled=\"true\" />").unwrap();
        assert_eq!(
            tokens,
            vec![Token::Component {
                name: "button".to_string(),
                attributes: vec![
                    ComponentAttr::StringAttr {
                        name: "class".to_string(),
                        value: "btn".to_string()
                    },
                    ComponentAttr::ExprAttr {
                        name: "disabled".to_string(),
                        value: "true".to_string()
                    },
                ],
                self_closing: true,
            }]
        );
    }

    #[test]
    fn test_tokenize_component_end() {
        let tokens = tokenize("<x-button></x-button>").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Component {
                    name: "button".to_string(),
                    attributes: vec![],
                    self_closing: false,
                },
                Token::ComponentEnd("button".to_string()),
            ]
        );
    }

    #[test]
    fn test_tokenize_element_with_l_if() {
        let tokens = tokenize("<div l-if=\"show\">content</div>").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Element {
                    tag: "div".to_string(),
                    attributes: vec![ComponentAttr::LmIf("show".to_string())],
                    self_closing: false,
                },
                Token::Text("content".to_string()),
                Token::ElementEnd("div".to_string()),
            ]
        );
    }

    #[test]
    fn test_tokenize_l_for() {
        let tokens = tokenize("<li l-for=\"item in items\">{{ item }}</li>").unwrap();
        assert_eq!(tokens.len(), 3);
        match &tokens[0] {
            Token::Element {
                tag, attributes, ..
            } => {
                assert_eq!(tag, "li");
                match &attributes[0] {
                    ComponentAttr::LmFor {
                        collection,
                        items,
                        loop_var,
                    } => {
                        assert_eq!(collection, "items");
                        assert_eq!(items, &vec!["item".to_string()]);
                        assert!(loop_var.is_none());
                    }
                    _ => panic!("Expected LmFor"),
                }
            }
            _ => panic!("Expected Element"),
        }
    }

    #[test]
    fn test_tokenize_l_for_with_tuple() {
        let tokens = tokenize("<li l-for=\"(key, val) in items\">test</li>").unwrap();
        match &tokens[0] {
            Token::Element { attributes, .. } => match &attributes[0] {
                ComponentAttr::LmFor {
                    collection, items, ..
                } => {
                    assert_eq!(collection, "items");
                    assert_eq!(items, &vec!["key".to_string(), "val".to_string()]);
                }
                _ => panic!("Expected LmFor"),
            },
            _ => panic!("Expected Element"),
        }
    }

    #[test]
    fn test_tokenize_nested_component() {
        let tokens = tokenize("<x-layouts:app>content</x-layouts:app>").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Component {
                    name: "layouts:app".to_string(),
                    attributes: vec![],
                    self_closing: false,
                },
                Token::Text("content".to_string()),
                Token::ComponentEnd("layouts:app".to_string()),
            ]
        );
    }

    #[test]
    fn test_tokenize_class_attr() {
        let tokens =
            tokenize("<div :class=\"['btn', #('active', is_active)]\">test</div>").unwrap();
        match &tokens[0] {
            Token::Element { attributes, .. } => match &attributes[0] {
                ComponentAttr::ClassAttr(value) => {
                    assert!(value.contains("btn"));
                    assert!(value.contains("active"));
                }
                _ => panic!("Expected ClassAttr"),
            },
            _ => panic!("Expected Element"),
        }
    }
}
