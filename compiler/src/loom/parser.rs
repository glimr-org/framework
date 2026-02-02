//! Template Parser
//!
//! Transforms token streams from the lexer into a typed AST
//! representation. The parser handles nested structures like
//! conditionals, loops, components, and slot definitions while
//! maintaining correct scope boundaries.
//!

use super::lexer::{ComponentAttr, Token};

// ------------------------------------------------------------- Public Types

/// Root container for a parsed template's abstract syntax tree.
/// This is the main output of the parsing phase, containing all
/// top-level nodes that make up the template structure.
///
#[derive(Debug, Clone, PartialEq)]
pub struct Template {
    /// The ordered sequence of nodes at the template root level.
    ///
    pub nodes: Vec<Node>,
}

/// Represents a single node in the template AST. Each variant
/// captures a distinct template construct, from simple text to
/// complex nested structures like conditionals and loops.
///
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    /// Static text content rendered verbatim in output.
    ///
    Text(String),
    /// Variable interpolation with HTML escaping applied.
    ///
    Variable(String),
    /// Variable interpolation without HTML escaping, used for
    /// trusted content that contains HTML markup.
    ///
    RawVariable(String),
    /// Slot output point where parent-provided content appears.
    /// Falls back to fallback content when no slot content given.
    ///
    Slot {
        /// Named slot identifier, None for the default slot.
        ///
        name: Option<String>,
        /// Content rendered when no slot content is provided.
        ///
        fallback: Vec<Node>,
    },
    /// Slot definition inside a component, providing content to
    /// fill a specific slot in the child component.
    ///
    SlotDef {
        /// Named slot to fill, None targets the default slot.
        ///
        name: Option<String>,
        /// Content to insert into the targeted slot.
        ///
        children: Vec<Node>,
    },
    /// Spread attributes placeholder, replaced during generation
    /// with dynamically passed attributes.
    ///
    Attributes(Vec<ComponentAttr>),
    /// Conditional rendering with support for if/else-if/else
    /// chains. Branches evaluated in order, first match wins.
    ///
    If {
        /// Condition-body pairs; None condition means else branch.
        ///
        branches: Vec<(Option<String>, Vec<Node>)>,
    },
    /// Loop construct for iterating over collections. Supports
    /// destructuring and optional loop index variable.
    ///
    Each {
        /// Expression identifying the collection to iterate.
        ///
        collection: String,
        /// Binding names for each item (supports destructuring).
        ///
        items: Vec<String>,
        /// Optional loop index variable name.
        ///
        loop_var: Option<String>,
        /// Template content rendered for each iteration.
        ///
        body: Vec<Node>,
    },
    /// Custom component invocation with attributes and children.
    /// Components are identified by x- prefix in templates.
    ///
    Component {
        /// Component name without the x- prefix.
        ///
        name: String,
        /// Attributes passed to the component.
        ///
        attributes: Vec<ComponentAttr>,
        /// Child content available via slots in the component.
        ///
        children: Vec<Node>,
    },
    /// Standard HTML element with attributes and nested content.
    ///
    Element {
        /// HTML tag name like div, span, input, etc.
        ///
        tag: String,
        /// Element attributes including dynamic bindings.
        ///
        attributes: Vec<ComponentAttr>,
        /// Nested child elements and content.
        ///
        children: Vec<Node>,
    },
}

/// Errors that can occur during template parsing. Each variant
/// captures a specific structural problem in the template that
/// prevents successful AST construction.
///
#[derive(Debug, Clone, PartialEq)]
pub enum ParserError {
    /// l-else directive without a preceding l-if block.
    ///
    UnexpectedLmElse,
    /// l-else-if directive without a preceding l-if block.
    ///
    UnexpectedLmElseIf,
    /// l-else directive appearing after another l-else.
    ///
    LmElseAfterLmElse,
    /// l-else-if directive appearing after l-else in chain.
    ///
    LmElseIfAfterLmElse,
    /// Closing component tag without matching opening tag.
    ///
    UnexpectedComponentEnd(String),
    /// Closing element tag without matching opening tag.
    ///
    UnexpectedElementEnd(String),
    /// Closing slot tag without matching opening slot tag.
    ///
    UnexpectedSlotDefEnd,
    /// Component opened but never closed before end of input.
    ///
    UnclosedComponent(String),
    /// Element opened but never closed before end of input.
    ///
    UnclosedElement(String),
    /// Slot tag opened but never closed before end of input.
    ///
    UnclosedSlot(Option<String>),
    /// Token found in position where it cannot be processed.
    ///
    UnexpectedToken,
}

// ------------------------------------------------------------- Private Types

/// Accumulator for building if/else-if/else chains across
/// sibling elements. When an l-if is encountered, a pending
/// state is created and subsequent l-else-if/l-else siblings
/// are accumulated until a non-conditional element ends the chain.
///
struct PendingIf {
    /// Condition expression from the initial l-if directive.
    ///
    first_condition: String,
    /// Body nodes from the initial l-if branch.
    ///
    first_body: Vec<Node>,
    /// Accumulated else-if and else branches in reverse order.
    ///
    branches: Vec<(Option<String>, Vec<Node>)>,
    /// Tracks whether an l-else has been seen, preventing
    /// duplicate else branches or else-if after else.
    ///
    has_else: bool,
}

/// Stateful parser that walks through tokens sequentially
/// while tracking position. Provides the core parsing logic
/// for converting tokens into nested AST structures.
///
struct Parser {
    /// Complete list of tokens to parse.
    ///
    tokens: Vec<Token>,
    /// Current position in the token stream.
    ///
    position: usize,
}

// ------------------------------------------------------------- Private Implementations

impl Parser {
    /// Creates a new parser positioned at the start of the
    /// token stream, ready to begin parsing.
    ///
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    /// Checks if all tokens have been consumed. Used to detect
    /// premature end-of-input for unclosed structures.
    ///
    fn is_at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }

    /// Returns the current token without consuming it. Used to
    /// peek ahead and decide which parsing path to take.
    ///
    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    /// Consumes and returns the current token, advancing the
    /// position. Returns None when at end of token stream.
    ///
    fn advance(&mut self) -> Option<Token> {
        if self.is_at_end() {
            None
        } else {
            let token = self.tokens[self.position].clone();
            self.position += 1;
            Some(token)
        }
    }

    /// Parses top-level nodes until end of input. Handles the
    /// full range of token types and manages conditional chains
    /// that span across sibling elements.
    ///
    fn parse_nodes(&mut self) -> Result<Vec<Node>, ParserError> {
        let mut acc = Vec::new();
        let mut pending_if: Option<PendingIf> = None;

        while !self.is_at_end() {
            match self.current() {
                Some(Token::Text(content)) => {
                    let content = content.clone();
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    acc.push(Node::Text(content));
                }

                Some(Token::Variable(name)) => {
                    let name = name.clone();
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    acc.push(Node::Variable(name));
                }

                Some(Token::RawVariable(name)) => {
                    let name = name.clone();
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    acc.push(Node::RawVariable(name));
                }

                Some(Token::Slot(name)) => {
                    let name = name.clone();
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    acc.push(Node::Slot {
                        name,
                        fallback: vec![],
                    });
                }

                Some(Token::Attributes) => {
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    acc.push(Node::Attributes(vec![]));
                }

                Some(Token::SlotDef(name)) => {
                    let name = name.clone();
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    let children = self.parse_slot_fallback_body(name.clone())?;
                    acc.push(Node::Slot {
                        name,
                        fallback: children,
                    });
                }

                Some(Token::Component {
                    name,
                    attributes,
                    self_closing,
                }) => {
                    let name = name.clone();
                    let attrs = attributes.clone();
                    let self_closing = *self_closing;
                    self.advance();

                    let (node, new_pending) = self.parse_component_with_attrs(
                        name,
                        attrs,
                        self_closing,
                        pending_if.take(),
                    )?;

                    if let Some(n) = node {
                        flush_pending_if(&mut acc, &mut pending_if);
                        acc.push(n);
                    }
                    pending_if = new_pending;
                }

                Some(Token::Element {
                    tag,
                    attributes,
                    self_closing,
                }) => {
                    let tag = tag.clone();
                    let attrs = attributes.clone();
                    let self_closing = *self_closing;
                    self.advance();

                    let (node, new_pending) =
                        self.parse_element_with_attrs(tag, attrs, self_closing, pending_if.take())?;

                    if let Some(n) = node {
                        flush_pending_if(&mut acc, &mut pending_if);
                        acc.push(n);
                    }
                    pending_if = new_pending;
                }

                Some(Token::ComponentEnd(name)) => {
                    let name = name.clone();
                    flush_pending_if(&mut acc, &mut pending_if);
                    return Err(ParserError::UnexpectedComponentEnd(name));
                }

                Some(Token::ElementEnd(tag)) => {
                    let tag = tag.clone();
                    flush_pending_if(&mut acc, &mut pending_if);
                    return Err(ParserError::UnexpectedElementEnd(tag));
                }

                Some(Token::SlotDefEnd) => {
                    flush_pending_if(&mut acc, &mut pending_if);
                    return Err(ParserError::UnexpectedSlotDefEnd);
                }

                None => break,
            }
        }

        flush_pending_if(&mut acc, &mut pending_if);
        Ok(acc)
    }

    /// Parses a component with its attributes, handling control
    /// flow directives (l-if, l-else-if, l-else, l-for) and
    /// building the appropriate AST structure.
    ///
    fn parse_component_with_attrs(
        &mut self,
        name: String,
        attrs: Vec<ComponentAttr>,
        self_closing: bool,
        pending_if: Option<PendingIf>,
    ) -> Result<(Option<Node>, Option<PendingIf>), ParserError> {
        let lm_if = find_lm_if(&attrs);
        let lm_else_if = find_lm_else_if(&attrs);
        let has_lm_else = has_lm_else(&attrs);
        let lm_for = find_lm_for(&attrs);

        let clean_attrs = filter_lm_attrs(attrs);

        let children = if self_closing {
            vec![]
        } else {
            self.parse_component_body(&name)?
        };

        let base_node = Node::Component {
            name,
            attributes: clean_attrs,
            children,
        };

        let wrapped_node = if let Some((collection, items, loop_var)) = lm_for {
            Node::Each {
                collection,
                items,
                loop_var,
                body: vec![base_node],
            }
        } else {
            base_node
        };

        handle_conditional_chain(wrapped_node, lm_if, lm_else_if, has_lm_else, pending_if)
    }

    /// Parses an HTML element with its attributes, handling
    /// control flow directives and template unwrapping for
    /// attribute-free template elements.
    ///
    fn parse_element_with_attrs(
        &mut self,
        tag: String,
        attrs: Vec<ComponentAttr>,
        self_closing: bool,
        pending_if: Option<PendingIf>,
    ) -> Result<(Option<Node>, Option<PendingIf>), ParserError> {
        let lm_if = find_lm_if(&attrs);
        let lm_else_if = find_lm_else_if(&attrs);
        let has_lm_else = has_lm_else(&attrs);
        let lm_for = find_lm_for(&attrs);

        let clean_attrs = filter_lm_attrs(attrs);

        let children = if self_closing {
            vec![]
        } else {
            self.parse_element_body(&tag)?
        };

        // For <template>, use children directly if no attributes
        let base_nodes = if tag == "template" && clean_attrs.is_empty() {
            children
        } else {
            vec![Node::Element {
                tag,
                attributes: clean_attrs,
                children,
            }]
        };

        let wrapped_node = match (lm_for, base_nodes) {
            (Some((collection, items, loop_var)), body) => Node::Each {
                collection,
                items,
                loop_var,
                body,
            },
            (None, mut nodes) if nodes.len() == 1 => nodes.pop().unwrap(),
            (None, children) => Node::Element {
                tag: "template".to_string(),
                attributes: vec![],
                children,
            },
        };

        handle_conditional_chain(wrapped_node, lm_if, lm_else_if, has_lm_else, pending_if)
    }

    /// Parses the body content of a component until its closing
    /// tag. Slot definitions within component bodies become
    /// SlotDef nodes that provide content to child slots.
    ///
    fn parse_component_body(&mut self, component_name: &str) -> Result<Vec<Node>, ParserError> {
        let mut acc = Vec::new();
        let mut pending_if: Option<PendingIf> = None;

        loop {
            match self.current() {
                None => return Err(ParserError::UnclosedComponent(component_name.to_string())),

                Some(Token::ComponentEnd(name)) if name == component_name => {
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    return Ok(acc);
                }

                Some(Token::ComponentEnd(name)) => {
                    let name = name.clone();
                    return Err(ParserError::UnexpectedComponentEnd(name));
                }

                Some(Token::Text(content)) => {
                    let content = content.clone();
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    acc.push(Node::Text(content));
                }

                Some(Token::Variable(name)) => {
                    let name = name.clone();
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    acc.push(Node::Variable(name));
                }

                Some(Token::RawVariable(name)) => {
                    let name = name.clone();
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    acc.push(Node::RawVariable(name));
                }

                Some(Token::Slot(name)) => {
                    let name = name.clone();
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    // In component body, <slot /> is SlotDefNode (defining content for slot)
                    acc.push(Node::SlotDef {
                        name,
                        children: vec![],
                    });
                }

                Some(Token::Attributes) => {
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    acc.push(Node::Attributes(vec![]));
                }

                Some(Token::SlotDef(name)) => {
                    let name = name.clone();
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    let children = self.parse_slot_fallback_body(name.clone())?;
                    // In component body, <slot name="x">content</slot> is SlotDefNode
                    acc.push(Node::SlotDef { name, children });
                }

                Some(Token::Component {
                    name,
                    attributes,
                    self_closing,
                }) => {
                    let name = name.clone();
                    let attrs = attributes.clone();
                    let self_closing = *self_closing;
                    self.advance();

                    let (node, new_pending) = self.parse_component_with_attrs(
                        name,
                        attrs,
                        self_closing,
                        pending_if.take(),
                    )?;

                    if let Some(n) = node {
                        flush_pending_if(&mut acc, &mut pending_if);
                        acc.push(n);
                    }
                    pending_if = new_pending;
                }

                Some(Token::Element {
                    tag,
                    attributes,
                    self_closing,
                }) => {
                    let tag = tag.clone();
                    let attrs = attributes.clone();
                    let self_closing = *self_closing;
                    self.advance();

                    let (node, new_pending) =
                        self.parse_element_with_attrs(tag, attrs, self_closing, pending_if.take())?;

                    if let Some(n) = node {
                        flush_pending_if(&mut acc, &mut pending_if);
                        acc.push(n);
                    }
                    pending_if = new_pending;
                }

                Some(Token::ElementEnd(tag)) => {
                    let tag = tag.clone();
                    return Err(ParserError::UnexpectedElementEnd(tag));
                }

                Some(Token::SlotDefEnd) => {
                    return Err(ParserError::UnexpectedSlotDefEnd);
                }
            }
        }
    }

    /// Parses the body content of an HTML element until its
    /// closing tag. Slot tags within elements become Slot nodes
    /// that output slot content with optional fallback.
    ///
    fn parse_element_body(&mut self, element_tag: &str) -> Result<Vec<Node>, ParserError> {
        let mut acc = Vec::new();
        let mut pending_if: Option<PendingIf> = None;

        loop {
            match self.current() {
                None => return Err(ParserError::UnclosedElement(element_tag.to_string())),

                Some(Token::ElementEnd(tag)) if tag == element_tag => {
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    return Ok(acc);
                }

                Some(Token::ElementEnd(tag)) => {
                    let tag = tag.clone();
                    return Err(ParserError::UnexpectedElementEnd(tag));
                }

                Some(Token::Text(content)) => {
                    let content = content.clone();
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    acc.push(Node::Text(content));
                }

                Some(Token::Variable(name)) => {
                    let name = name.clone();
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    acc.push(Node::Variable(name));
                }

                Some(Token::RawVariable(name)) => {
                    let name = name.clone();
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    acc.push(Node::RawVariable(name));
                }

                Some(Token::Slot(name)) => {
                    let name = name.clone();
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    // In element body, <slot /> is SlotNode (slot output)
                    acc.push(Node::Slot {
                        name,
                        fallback: vec![],
                    });
                }

                Some(Token::Attributes) => {
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    acc.push(Node::Attributes(vec![]));
                }

                Some(Token::SlotDef(name)) => {
                    let name = name.clone();
                    self.advance();
                    flush_pending_if(&mut acc, &mut pending_if);
                    let children = self.parse_slot_fallback_body(name.clone())?;
                    // In element body, <slot>fallback</slot> is SlotNode with fallback
                    acc.push(Node::Slot {
                        name,
                        fallback: children,
                    });
                }

                Some(Token::Component {
                    name,
                    attributes,
                    self_closing,
                }) => {
                    let name = name.clone();
                    let attrs = attributes.clone();
                    let self_closing = *self_closing;
                    self.advance();

                    let (node, new_pending) = self.parse_component_with_attrs(
                        name,
                        attrs,
                        self_closing,
                        pending_if.take(),
                    )?;

                    if let Some(n) = node {
                        flush_pending_if(&mut acc, &mut pending_if);
                        acc.push(n);
                    }
                    pending_if = new_pending;
                }

                Some(Token::Element {
                    tag,
                    attributes,
                    self_closing,
                }) => {
                    let tag = tag.clone();
                    let attrs = attributes.clone();
                    let self_closing = *self_closing;
                    self.advance();

                    let (node, new_pending) =
                        self.parse_element_with_attrs(tag, attrs, self_closing, pending_if.take())?;

                    if let Some(n) = node {
                        flush_pending_if(&mut acc, &mut pending_if);
                        acc.push(n);
                    }
                    pending_if = new_pending;
                }

                Some(Token::ComponentEnd(name)) => {
                    let name = name.clone();
                    return Err(ParserError::UnexpectedComponentEnd(name));
                }

                Some(Token::SlotDefEnd) => {
                    return Err(ParserError::UnexpectedSlotDefEnd);
                }
            }
        }
    }

    /// Parses slot fallback content until the closing slot tag.
    /// This content is rendered when no slot content is provided
    /// by the parent component.
    ///
    fn parse_slot_fallback_body(
        &mut self,
        slot_name: Option<String>,
    ) -> Result<Vec<Node>, ParserError> {
        let mut acc = Vec::new();

        loop {
            match self.current() {
                None => return Err(ParserError::UnclosedSlot(slot_name)),

                Some(Token::SlotDefEnd) => {
                    self.advance();
                    return Ok(acc);
                }

                Some(Token::Text(content)) => {
                    let content = content.clone();
                    self.advance();
                    acc.push(Node::Text(content));
                }

                Some(Token::Variable(name)) => {
                    let name = name.clone();
                    self.advance();
                    acc.push(Node::Variable(name));
                }

                Some(Token::RawVariable(name)) => {
                    let name = name.clone();
                    self.advance();
                    acc.push(Node::RawVariable(name));
                }

                Some(Token::Attributes) => {
                    self.advance();
                    acc.push(Node::Attributes(vec![]));
                }

                Some(Token::Component {
                    name,
                    attributes,
                    self_closing,
                }) => {
                    let name = name.clone();
                    let attrs = attributes.clone();
                    let self_closing = *self_closing;
                    self.advance();

                    let (node, _) =
                        self.parse_component_with_attrs(name, attrs, self_closing, None)?;
                    if let Some(n) = node {
                        acc.push(n);
                    }
                }

                Some(Token::Element {
                    tag,
                    attributes,
                    self_closing,
                }) => {
                    let tag = tag.clone();
                    let attrs = attributes.clone();
                    let self_closing = *self_closing;
                    self.advance();

                    let (node, _) =
                        self.parse_element_with_attrs(tag, attrs, self_closing, None)?;
                    if let Some(n) = node {
                        acc.push(n);
                    }
                }

                Some(Token::ComponentEnd(name)) => {
                    let name = name.clone();
                    return Err(ParserError::UnexpectedComponentEnd(name));
                }

                Some(Token::ElementEnd(tag)) => {
                    let tag = tag.clone();
                    return Err(ParserError::UnexpectedElementEnd(tag));
                }

                Some(Token::Slot(_)) | Some(Token::SlotDef(_)) => {
                    return Err(ParserError::UnexpectedToken);
                }
            }
        }
    }
}

// ------------------------------------------------------------- Public Functions

/// Converts a token stream into a complete template AST. This
/// is the main entry point for parsing, handling all template
/// constructs including nested components and control flow.
///
pub fn parse(tokens: Vec<Token>) -> Result<Template, ParserError> {
    let mut parser = Parser::new(tokens);
    let nodes = parser.parse_nodes()?;
    Ok(Template { nodes })
}

/// Formats a parser error as a human-readable message suitable
/// for display to developers. Messages explain what went wrong
/// and include relevant context like tag names.
///
pub fn error_to_string(err: &ParserError) -> String {
    match err {
        ParserError::UnexpectedLmElse => "Unexpected l-else without matching l-if".to_string(),
        ParserError::UnexpectedLmElseIf => "Unexpected l-else-if without matching l-if".to_string(),
        ParserError::LmElseAfterLmElse => "l-else-if cannot appear after l-else".to_string(),
        ParserError::LmElseIfAfterLmElse => "l-else-if cannot appear after l-else".to_string(),
        ParserError::UnexpectedComponentEnd(name) => {
            format!("Unexpected closing tag </x-{}>", name)
        }
        ParserError::UnexpectedElementEnd(tag) => {
            format!("Unexpected closing tag </{}>", tag)
        }
        ParserError::UnexpectedSlotDefEnd => "Unexpected </slot> tag".to_string(),
        ParserError::UnclosedComponent(name) => format!("Unclosed component <x-{}>", name),
        ParserError::UnclosedElement(tag) => format!("Unclosed element <{}>", tag),
        ParserError::UnclosedSlot(name) => match name {
            Some(n) => format!("Unclosed slot <slot name=\"{}\">", n),
            None => "Unclosed slot <slot>".to_string(),
        },
        ParserError::UnexpectedToken => "Unexpected token".to_string(),
    }
}

// ------------------------------------------------------------- Private Functions

/// Finalizes a pending if-chain by assembling branches and
/// pushing the completed If node to the accumulator. Called when
/// a non-conditional element ends the chain or at scope end.
///
fn flush_pending_if(acc: &mut Vec<Node>, pending_if: &mut Option<PendingIf>) {
    if let Some(p) = pending_if.take() {
        let mut branches = vec![(Some(p.first_condition), p.first_body)];
        branches.extend(p.branches.into_iter().rev());
        acc.push(Node::If { branches });
    }
}

/// Determines how to handle a node based on its conditional
/// attributes. Starts new if-chains, extends existing ones, or
/// returns the node directly for non-conditional elements.
///
fn handle_conditional_chain(
    node: Node,
    lm_if: Option<String>,
    lm_else_if: Option<String>,
    has_lm_else: bool,
    pending_if: Option<PendingIf>,
) -> Result<(Option<Node>, Option<PendingIf>), ParserError> {
    match (lm_if, lm_else_if, has_lm_else, pending_if) {
        // l-if: start new chain
        (Some(condition), _, _, _) => Ok((
            None,
            Some(PendingIf {
                first_condition: condition,
                first_body: vec![node],
                branches: vec![],
                has_else: false,
            }),
        )),

        // l-else-if: add to existing chain
        (_, Some(condition), _, Some(mut p)) => {
            if p.has_else {
                return Err(ParserError::LmElseIfAfterLmElse);
            }
            p.branches.push((Some(condition), vec![node]));
            Ok((None, Some(p)))
        }

        // l-else-if without pending chain
        (_, Some(_), _, None) => Err(ParserError::UnexpectedLmElseIf),

        // l-else: add else branch
        (_, _, true, Some(mut p)) => {
            if p.has_else {
                return Err(ParserError::LmElseAfterLmElse);
            }
            p.branches.push((None, vec![node]));
            p.has_else = true;
            Ok((None, Some(p)))
        }

        // l-else without pending chain
        (_, _, true, None) => Err(ParserError::UnexpectedLmElse),

        // No l-* conditional attributes - emit node normally
        (_, _, _, _) => Ok((Some(node), None)),
    }
}

/// Extracts the l-if condition from an attribute list if
/// present. Returns the condition expression string for
/// conditional rendering evaluation.
///
fn find_lm_if(attrs: &[ComponentAttr]) -> Option<String> {
    attrs.iter().find_map(|attr| {
        if let ComponentAttr::LmIf(condition) = attr {
            Some(condition.clone())
        } else {
            None
        }
    })
}

/// Extracts the l-else-if condition from an attribute list if
/// present. Used to extend an existing conditional chain with
/// an additional branch.
///
fn find_lm_else_if(attrs: &[ComponentAttr]) -> Option<String> {
    attrs.iter().find_map(|attr| {
        if let ComponentAttr::LmElseIf(condition) = attr {
            Some(condition.clone())
        } else {
            None
        }
    })
}

/// Checks whether an l-else directive is present in the
/// attribute list. Else branches have no condition and render
/// when no prior branch conditions matched.
///
fn has_lm_else(attrs: &[ComponentAttr]) -> bool {
    attrs
        .iter()
        .any(|attr| matches!(attr, ComponentAttr::LmElse))
}

/// Extracts l-for loop parameters from an attribute list if
/// present. Returns the collection expression, item bindings,
/// and optional loop index variable.
///
fn find_lm_for(attrs: &[ComponentAttr]) -> Option<(String, Vec<String>, Option<String>)> {
    attrs.iter().find_map(|attr| {
        if let ComponentAttr::LmFor {
            collection,
            items,
            loop_var,
        } = attr
        {
            Some((collection.clone(), items.clone(), loop_var.clone()))
        } else {
            None
        }
    })
}

/// Removes control flow directives from an attribute list,
/// returning only the attributes that should appear on the
/// rendered element or component.
///
fn filter_lm_attrs(attrs: Vec<ComponentAttr>) -> Vec<ComponentAttr> {
    attrs
        .into_iter()
        .filter(|attr| {
            !matches!(
                attr,
                ComponentAttr::LmIf(_)
                    | ComponentAttr::LmElseIf(_)
                    | ComponentAttr::LmElse
                    | ComponentAttr::LmFor { .. }
            )
        })
        .collect()
}

// ------------------------------------------------------------- Unit Tests

#[cfg(test)]
mod tests {
    use super::*;
    use crate::loom::lexer;

    // ----------------------------------------- parse tests

    #[test]
    fn test_parse_text() {
        let tokens = lexer::tokenize("Hello World").unwrap();
        let template = parse(tokens).unwrap();
        assert_eq!(template.nodes, vec![Node::Text("Hello World".to_string())]);
    }

    #[test]
    fn test_parse_variable() {
        let tokens = lexer::tokenize("{{ name }}").unwrap();
        let template = parse(tokens).unwrap();
        assert_eq!(template.nodes, vec![Node::Variable("name".to_string())]);
    }

    #[test]
    fn test_parse_component() {
        let tokens = lexer::tokenize("<x-button>Click</x-button>").unwrap();
        let template = parse(tokens).unwrap();
        assert_eq!(template.nodes.len(), 1);
        match &template.nodes[0] {
            Node::Component { name, children, .. } => {
                assert_eq!(name, "button");
                assert_eq!(children.len(), 1);
            }
            _ => panic!("Expected ComponentNode"),
        }
    }

    #[test]
    fn test_parse_if_else() {
        let tokens = lexer::tokenize("<div l-if=\"show\">yes</div><div l-else>no</div>").unwrap();
        let template = parse(tokens).unwrap();
        assert_eq!(template.nodes.len(), 1);
        match &template.nodes[0] {
            Node::If { branches } => {
                assert_eq!(branches.len(), 2);
                assert_eq!(branches[0].0, Some("show".to_string()));
                assert!(branches[1].0.is_none()); // else branch
            }
            _ => panic!("Expected IfNode"),
        }
    }

    #[test]
    fn test_parse_for() {
        let tokens = lexer::tokenize("<li l-for=\"item in items\">{{ item }}</li>").unwrap();
        let template = parse(tokens).unwrap();
        assert_eq!(template.nodes.len(), 1);
        match &template.nodes[0] {
            Node::Each {
                collection,
                items,
                body,
                ..
            } => {
                assert_eq!(collection, "items");
                assert_eq!(items, &vec!["item".to_string()]);
                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected EachNode"),
        }
    }

    #[test]
    fn test_parse_slot_with_fallback() {
        let tokens = lexer::tokenize("<slot>fallback</slot>").unwrap();
        let template = parse(tokens).unwrap();
        match &template.nodes[0] {
            Node::Slot { name, fallback } => {
                assert!(name.is_none());
                assert_eq!(fallback.len(), 1);
            }
            _ => panic!("Expected SlotNode"),
        }
    }

    #[test]
    fn test_parse_nested_components() {
        let tokens = lexer::tokenize("<x-outer><x-inner /></x-outer>").unwrap();
        let template = parse(tokens).unwrap();
        match &template.nodes[0] {
            Node::Component { name, children, .. } => {
                assert_eq!(name, "outer");
                assert_eq!(children.len(), 1);
                match &children[0] {
                    Node::Component { name, .. } => assert_eq!(name, "inner"),
                    _ => panic!("Expected inner ComponentNode"),
                }
            }
            _ => panic!("Expected outer ComponentNode"),
        }
    }
}
