//// Template Parser
////
//// Converts a stream of lexer tokens into an abstract syntax
//// tree. Handles nested structures like conditionals, loops,
//// components, and slot definitions.
////

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import glimr/loom/lexer.{type ComponentAttr, type Token}

// ------------------------------------------------------------- Public Types

/// Represents a parsed template as a list of content nodes.
/// The root structure returned from parsing, containing all
/// AST nodes that make up the template.
///
pub type Template {
  Template(
    /// Import statements from @import directives
    imports: List(String),
    /// Props from @props directive (name, type pairs)
    props: List(#(String, String)),
    /// Content nodes
    nodes: List(Node),
  )
}

/// Represents a node in the template AST. Each variant maps
/// to a different template construct like text, variables,
/// control flow, or components.
///
pub type Node {
  TextNode(String)
  VariableNode(name: String, line: Int)
  RawVariableNode(name: String, line: Int)
  /// SlotNode outputs slot content with optional fallback.
  /// Used in component templates: <slot />, <slot>fallback</slot>
  SlotNode(name: Option(String), fallback: List(Node))
  /// SlotDefNode defines content to pass to a slot when using a component.
  /// Used inside <x-component>: <slot name="header">content</slot>
  SlotDefNode(name: Option(String), children: List(Node))
  /// AttributesNode holds optional base attributes that will be merged with props.attributes
  AttributesNode(base_attributes: List(ComponentAttr))
  IfNode(branches: List(#(Option(String), Int, List(Node))))
  EachNode(
    collection: String,
    items: List(String),
    loop_var: Option(String),
    body: List(Node),
    line: Int,
  )
  ComponentNode(
    name: String,
    attributes: List(ComponentAttr),
    children: List(Node),
  )
  ElementNode(
    tag: String,
    attributes: List(ComponentAttr),
    children: List(Node),
  )
}

/// Errors that can occur during template parsing. Includes
/// unexpected closing tags, unclosed blocks, and invalid
/// token sequences.
///
pub type ParserError {
  UnexpectedLmElse
  UnexpectedLmElseIf
  LmElseAfterLmElse
  LmElseIfAfterLmElse
  UnexpectedComponentEnd(name: String)
  UnexpectedElementEnd(tag: String)
  UnexpectedSlotDefEnd
  UnclosedComponent(name: String)
  UnclosedElement(tag: String)
  UnclosedSlot(name: Option(String))
  UnexpectedToken(token: Token)
  /// @props or @import directive appeared after template content
  DirectiveAfterContent(directive: String, line: Int)
  /// Multiple @props directives found
  DuplicatePropsDirective(line: Int)
}

// ------------------------------------------------------------- Public Functions

/// Parses a list of tokens into a Template AST. Extracts
/// @import and @props directives first, then recursively
/// parses all nodes including nested structures like
/// conditionals, loops, and component hierarchies.
///
pub fn parse(tokens: List(Token)) -> Result(Template, ParserError) {
  // First extract @import and @props directives from the beginning
  use #(imports, props, remaining_tokens) <- try_parse(extract_directives(
    tokens,
    [],
    None,
  ))
  // Then parse the content nodes
  use nodes <- try_parse(parse_nodes(remaining_tokens, [], None))
  Ok(Template(imports: imports, props: props, nodes: nodes))
}

/// Extracts @import and @props directives from the beginning of tokens.
/// Directives must appear before any content. Returns the imports,
/// props, and remaining tokens.
///
fn extract_directives(
  tokens: List(Token),
  imports_acc: List(String),
  props_acc: Option(List(#(String, String))),
) -> Result(#(List(String), List(#(String, String)), List(Token)), ParserError) {
  case tokens {
    [] -> Ok(#(list.reverse(imports_acc), option.unwrap(props_acc, []), []))

    // Skip leading whitespace text (newlines, spaces between directives)
    [lexer.Text(content), ..rest] -> {
      case is_whitespace_only(content) {
        True -> extract_directives(rest, imports_acc, props_acc)
        False ->
          // Non-whitespace text - directives phase is over
          Ok(#(list.reverse(imports_acc), option.unwrap(props_acc, []), tokens))
      }
    }

    [lexer.ImportDirective(import_str, _line), ..rest] ->
      extract_directives(rest, [import_str, ..imports_acc], props_acc)

    [lexer.PropsDirective(props, line), ..rest] -> {
      case props_acc {
        Some(_) -> Error(DuplicatePropsDirective(line))
        None -> extract_directives(rest, imports_acc, Some(props))
      }
    }

    // Any other token - directives phase is over
    _ -> Ok(#(list.reverse(imports_acc), option.unwrap(props_acc, []), tokens))
  }
}

/// Checks if a string contains only whitespace.
///
fn is_whitespace_only(s: String) -> Bool {
  s
  |> string.to_graphemes
  |> list.all(fn(c) { c == " " || c == "\n" || c == "\t" || c == "\r" })
}

// ------------------------------------------------------------- Private Functions

/// State for tracking conditional chains across sibling elements.
/// When we see l-if, we start a pending chain. When we see l-else-if
/// or l-else, we add to it. When we see something else or end of
/// current context, we emit the completed IfNode.
///
type PendingIf {
  PendingIf(
    first_condition: String,
    first_line: Int,
    first_body: List(Node),
    branches: List(#(Option(String), Int, List(Node))),
    has_else: Bool,
  )
}

/// Parses top-level nodes from the token stream. Handles all
/// node types and delegates to specialized parsers for nested
/// structures. Tracks conditional chains across siblings.
///
fn parse_nodes(
  tokens: List(Token),
  acc: List(Node),
  pending_if: Option(PendingIf),
) -> Result(List(Node), ParserError) {
  case tokens {
    [] -> {
      // End of input - emit any pending conditional
      let acc = flush_pending_if(acc, pending_if)
      Ok(list.reverse(acc))
    }

    [lexer.Text(content), ..rest] -> {
      // Text breaks a conditional chain
      let acc = flush_pending_if(acc, pending_if)
      parse_nodes(rest, [TextNode(content), ..acc], None)
    }

    [lexer.Variable(name, line), ..rest] -> {
      let acc = flush_pending_if(acc, pending_if)
      parse_nodes(rest, [VariableNode(name, line), ..acc], None)
    }

    [lexer.RawVariable(name, line), ..rest] -> {
      let acc = flush_pending_if(acc, pending_if)
      parse_nodes(rest, [RawVariableNode(name, line), ..acc], None)
    }

    [lexer.Slot(name), ..rest] -> {
      // Self-closing <slot /> or <slot name="x" /> - no fallback
      let acc = flush_pending_if(acc, pending_if)
      parse_nodes(rest, [SlotNode(name, []), ..acc], None)
    }

    [lexer.Attributes, ..rest] -> {
      let acc = flush_pending_if(acc, pending_if)
      parse_nodes(rest, [AttributesNode([]), ..acc], None)
    }

    [lexer.SlotDef(name), ..rest] -> {
      // Opening <slot> or <slot name="x"> - parse children as fallback
      // At top level/element body, this is a slot OUTPUT with fallback content
      let acc = flush_pending_if(acc, pending_if)
      use #(children, remaining) <- try_parse(
        parse_slot_fallback_body(rest, name, []),
      )
      parse_nodes(remaining, [SlotNode(name, children), ..acc], None)
    }

    [lexer.Component(name, attrs, self_closing), ..rest] -> {
      // Check for l-* attributes on component
      use #(node, remaining, new_pending) <- try_parse(
        parse_component_with_attrs(name, attrs, self_closing, rest, pending_if),
      )
      case node {
        Some(n) -> {
          let acc = flush_pending_if(acc, pending_if)
          parse_nodes(remaining, [n, ..acc], new_pending)
        }
        None -> {
          // Node was absorbed into pending_if chain
          parse_nodes(remaining, acc, new_pending)
        }
      }
    }

    [lexer.Element(tag, attrs, self_closing), ..rest] -> {
      use #(node, remaining, new_pending) <- try_parse(parse_element_with_attrs(
        tag,
        attrs,
        self_closing,
        rest,
        pending_if,
      ))
      case node {
        Some(n) -> {
          let acc = flush_pending_if(acc, pending_if)
          parse_nodes(remaining, [n, ..acc], new_pending)
        }
        None -> {
          // Node was absorbed into pending_if chain
          parse_nodes(remaining, acc, new_pending)
        }
      }
    }

    [lexer.ComponentEnd(name), ..] -> {
      let _acc = flush_pending_if(acc, pending_if)
      Error(UnexpectedComponentEnd(name))
    }

    [lexer.ElementEnd(tag), ..] -> {
      let _acc = flush_pending_if(acc, pending_if)
      Error(UnexpectedElementEnd(tag))
    }

    [lexer.SlotDefEnd, ..] -> {
      let _ = flush_pending_if(acc, pending_if)
      Error(UnexpectedSlotDefEnd)
    }

    [lexer.ImportDirective(_, line), ..] ->
      Error(DirectiveAfterContent("@import", line))

    [lexer.PropsDirective(_, line), ..] ->
      Error(DirectiveAfterContent("@props", line))
  }
}

/// Flushes any pending conditional chain to the accumulator.
/// Converts the accumulated branches into an IfNode and
/// prepends it to the node list.
///
fn flush_pending_if(
  acc: List(Node),
  pending_if: Option(PendingIf),
) -> List(Node) {
  case pending_if {
    None -> acc
    Some(p) -> {
      let first_branch = #(Some(p.first_condition), p.first_line, p.first_body)
      // Branches are accumulated in reverse order, so reverse them
      // Then prepend first_branch to get correct order
      let all_branches = [first_branch, ..list.reverse(p.branches)]
      [IfNode(all_branches), ..acc]
    }
  }
}

/// Parses a component with potential l-* attributes.
/// Returns the node (or None if absorbed into chain), remaining tokens,
/// and the new pending_if state.
///
fn parse_component_with_attrs(
  name: String,
  attrs: List(ComponentAttr),
  self_closing: Bool,
  rest: List(Token),
  pending_if: Option(PendingIf),
) -> Result(#(Option(Node), List(Token), Option(PendingIf)), ParserError) {
  // Extract l-* attributes
  let lm_if = find_lm_if(attrs)
  let lm_else_if = find_lm_else_if(attrs)
  let has_lm_else = has_lm_else(attrs)
  let lm_for = find_lm_for(attrs)

  // Filter out l-* attributes from the component's attributes
  let clean_attrs = filter_lm_attrs(attrs)

  // Get component children
  use #(children, remaining) <- try_parse(case self_closing {
    True -> Ok(#([], rest))
    False -> parse_component_body(rest, name, [], None)
  })

  // Build the base node
  let base_node = ComponentNode(name, clean_attrs, children)

  // Wrap with l-for if present
  let wrapped_node = case lm_for {
    Some(#(collection, items, loop_var, line)) ->
      EachNode(collection, items, loop_var, [base_node], line)
    None -> base_node
  }

  // Handle conditional chain
  handle_conditional_chain(
    wrapped_node,
    lm_if,
    lm_else_if,
    has_lm_else,
    pending_if,
    remaining,
  )
}

/// Parses an element with potential l-* attributes. Handles
/// l-if, l-else-if, l-else, and l-for directives that wrap
/// the element in conditional or loop structures.
///
fn parse_element_with_attrs(
  tag: String,
  attrs: List(ComponentAttr),
  self_closing: Bool,
  rest: List(Token),
  pending_if: Option(PendingIf),
) -> Result(#(Option(Node), List(Token), Option(PendingIf)), ParserError) {
  // Extract l-* attributes
  let lm_if = find_lm_if(attrs)
  let lm_else_if = find_lm_else_if(attrs)
  let has_lm_else = has_lm_else(attrs)
  let lm_for = find_lm_for(attrs)

  // Filter out l-* attributes
  let clean_attrs = filter_lm_attrs(attrs)

  // Get element children
  use #(children, remaining) <- try_parse(case self_closing {
    True -> Ok(#([], rest))
    False -> parse_element_body(rest, tag, [], None)
  })

  // Build the base node
  // For <template>, we just use children directly (no wrapper)
  let base_node = case tag {
    "template" -> {
      case children {
        [single] -> single
        _ -> ElementNode(tag, clean_attrs, children)
      }
    }
    _ -> ElementNode(tag, clean_attrs, children)
  }

  // Handle template specially - if it has no non-l-* attributes and
  // multiple children, we want to use children directly
  let base_nodes = case tag, clean_attrs {
    "template", [] -> children
    _, _ -> [base_node]
  }

  // When both l-for and l-if are on the same element, l-if goes INSIDE the loop
  // so it can access loop variables (e.g., l-for="item in items" l-if="item.active")
  case lm_for, lm_if {
    Some(#(collection, items, loop_var, for_line)), Some(#(condition, if_line)) -> {
      // Create: EachNode(IfNode(base_nodes))
      let if_node = IfNode([#(Some(condition), if_line, base_nodes)])
      let each_node = EachNode(collection, items, loop_var, [if_node], for_line)
      Ok(#(Some(each_node), remaining, pending_if))
    }
    Some(#(collection, items, loop_var, line)), None -> {
      // Just l-for, no l-if
      let each_node = EachNode(collection, items, loop_var, base_nodes, line)
      // Handle any l-else-if/l-else (shouldn't happen but be safe)
      handle_conditional_chain(
        each_node,
        None,
        lm_else_if,
        has_lm_else,
        pending_if,
        remaining,
      )
    }
    None, _ -> {
      // No l-for, handle conditional chain normally
      let wrapped_node = case base_nodes {
        [single] -> single
        multiple -> ElementNode("template", [], multiple)
      }
      handle_conditional_chain(
        wrapped_node,
        lm_if,
        lm_else_if,
        has_lm_else,
        pending_if,
        remaining,
      )
    }
  }
}

/// Handles conditional chain logic for an element or component.
/// Manages l-if/l-else-if/l-else chains across sibling elements
/// and returns the appropriate pending state.
///
fn handle_conditional_chain(
  node: Node,
  lm_if: Option(#(String, Int)),
  lm_else_if: Option(#(String, Int)),
  has_lm_else: Bool,
  pending_if: Option(PendingIf),
  remaining: List(Token),
) -> Result(#(Option(Node), List(Token), Option(PendingIf)), ParserError) {
  case lm_if, lm_else_if, has_lm_else, pending_if {
    // l-if: start new chain
    Some(#(condition, line)), _, _, _ -> {
      // If there's a pending chain, it will be flushed by caller
      Ok(#(
        None,
        remaining,
        Some(PendingIf(
          first_condition: condition,
          first_line: line,
          first_body: [node],
          branches: [],
          has_else: False,
        )),
      ))
    }

    // l-else-if: add to existing chain
    _, Some(#(condition, line)), _, Some(p) -> {
      case p.has_else {
        True -> Error(LmElseIfAfterLmElse)
        False -> {
          // Keep first as-is, append new branch
          let new_branch = #(Some(condition), line, [node])
          let new_pending =
            PendingIf(
              first_condition: p.first_condition,
              first_line: p.first_line,
              first_body: p.first_body,
              branches: [new_branch, ..p.branches],
              has_else: False,
            )
          Ok(#(None, remaining, Some(new_pending)))
        }
      }
    }

    // l-else-if without pending chain
    _, Some(_), _, None -> Error(UnexpectedLmElseIf)

    // l-else: add else branch (line 0 since there's no condition to validate)
    _, _, True, Some(p) -> {
      case p.has_else {
        True -> Error(LmElseAfterLmElse)
        False -> {
          // Add else branch - first_condition/first_body stay as the "head"
          let else_branch = #(None, 0, [node])
          let new_pending =
            PendingIf(
              first_condition: p.first_condition,
              first_line: p.first_line,
              first_body: p.first_body,
              branches: [else_branch, ..p.branches],
              has_else: True,
            )
          Ok(#(None, remaining, Some(new_pending)))
        }
      }
    }

    // l-else without pending chain
    _, _, True, None -> Error(UnexpectedLmElse)

    // No l-* conditional attributes - emit node normally
    _, _, _, _ -> Ok(#(Some(node), remaining, None))
  }
}

/// Finds l-if attribute value in an attribute list. Returns
/// the condition string if present, None otherwise. Used to
/// detect the start of a conditional chain.
///
fn find_lm_if(attrs: List(ComponentAttr)) -> Option(#(String, Int)) {
  list.find_map(attrs, fn(attr) {
    case attr {
      lexer.LmIf(condition, line) -> Ok(#(condition, line))
      _ -> Error(Nil)
    }
  })
  |> option.from_result
}

/// Finds l-else-if attribute value in an attribute list.
/// Returns the condition string if present, None otherwise.
/// Used to continue a conditional chain.
///
fn find_lm_else_if(attrs: List(ComponentAttr)) -> Option(#(String, Int)) {
  list.find_map(attrs, fn(attr) {
    case attr {
      lexer.LmElseIf(condition, line) -> Ok(#(condition, line))
      _ -> Error(Nil)
    }
  })
  |> option.from_result
}

/// Checks if l-else is present in an attribute list. Returns
/// true if the else directive is found, indicating the final
/// branch of a conditional chain.
///
fn has_lm_else(attrs: List(ComponentAttr)) -> Bool {
  list.any(attrs, fn(attr) {
    case attr {
      lexer.LmElse -> True
      _ -> False
    }
  })
}

/// Finds l-for attribute value in an attribute list. Returns
/// collection name, item pattern, and optional loop variable
/// if the for directive is present.
///
fn find_lm_for(
  attrs: List(ComponentAttr),
) -> Option(#(String, List(String), Option(String), Int)) {
  list.find_map(attrs, fn(attr) {
    case attr {
      lexer.LmFor(collection, items, loop_var, line) ->
        Ok(#(collection, items, loop_var, line))
      _ -> Error(Nil)
    }
  })
  |> option.from_result
}

/// Filters out l-* attributes from attribute list. Removes
/// directive attributes (l-if, l-else-if, l-else, l-for)
/// leaving only standard HTML/component attributes.
///
fn filter_lm_attrs(attrs: List(ComponentAttr)) -> List(ComponentAttr) {
  list.filter(attrs, fn(attr) {
    case attr {
      lexer.LmIf(_, _)
      | lexer.LmElseIf(_, _)
      | lexer.LmElse
      | lexer.LmFor(_, _, _, _) -> False
      _ -> True
    }
  })
}

/// Parses the children of a component until its closing tag.
/// Handles nested components, elements, slots, and maintains
/// conditional chain state across siblings.
///
fn parse_component_body(
  tokens: List(Token),
  component_name: String,
  acc: List(Node),
  pending_if: Option(PendingIf),
) -> Result(#(List(Node), List(Token)), ParserError) {
  case tokens {
    [] -> Error(UnclosedComponent(component_name))

    [lexer.ComponentEnd(name), ..rest] if name == component_name -> {
      let acc = flush_pending_if(acc, pending_if)
      Ok(#(list.reverse(acc), rest))
    }

    [lexer.ComponentEnd(name), ..] -> Error(UnexpectedComponentEnd(name))

    [lexer.Text(content), ..rest] -> {
      let acc = flush_pending_if(acc, pending_if)
      parse_component_body(
        rest,
        component_name,
        [TextNode(content), ..acc],
        None,
      )
    }

    [lexer.Variable(name, line), ..rest] -> {
      let acc = flush_pending_if(acc, pending_if)
      parse_component_body(
        rest,
        component_name,
        [VariableNode(name, line), ..acc],
        None,
      )
    }

    [lexer.RawVariable(name, line), ..rest] -> {
      let acc = flush_pending_if(acc, pending_if)
      parse_component_body(
        rest,
        component_name,
        [RawVariableNode(name, line), ..acc],
        None,
      )
    }

    [lexer.Slot(name), ..rest] -> {
      // Self-closing <slot /> - creates SlotDefNode inside component body
      // This is defining content to pass to the component's slot
      let acc = flush_pending_if(acc, pending_if)
      parse_component_body(
        rest,
        component_name,
        [SlotDefNode(name, []), ..acc],
        None,
      )
    }

    [lexer.Attributes, ..rest] -> {
      let acc = flush_pending_if(acc, pending_if)
      parse_component_body(
        rest,
        component_name,
        [AttributesNode([]), ..acc],
        None,
      )
    }

    [lexer.SlotDef(name), ..rest] -> {
      // Opening <slot name="x"> inside component - defines content for named slot
      let acc = flush_pending_if(acc, pending_if)
      use #(children, remaining) <- try_parse(
        parse_slot_fallback_body(rest, name, []),
      )
      parse_component_body(
        remaining,
        component_name,
        [SlotDefNode(name, children), ..acc],
        None,
      )
    }

    [lexer.Component(name, attrs, self_closing), ..rest] -> {
      use #(node, remaining, new_pending) <- try_parse(
        parse_component_with_attrs(name, attrs, self_closing, rest, pending_if),
      )
      case node {
        Some(n) -> {
          let acc = flush_pending_if(acc, pending_if)
          parse_component_body(
            remaining,
            component_name,
            [n, ..acc],
            new_pending,
          )
        }
        None ->
          parse_component_body(remaining, component_name, acc, new_pending)
      }
    }

    [lexer.Element(tag, attrs, self_closing), ..rest] -> {
      use #(node, remaining, new_pending) <- try_parse(parse_element_with_attrs(
        tag,
        attrs,
        self_closing,
        rest,
        pending_if,
      ))
      case node {
        Some(n) -> {
          let acc = flush_pending_if(acc, pending_if)
          parse_component_body(
            remaining,
            component_name,
            [n, ..acc],
            new_pending,
          )
        }
        None ->
          parse_component_body(remaining, component_name, acc, new_pending)
      }
    }

    [lexer.ElementEnd(tag), ..] -> Error(UnexpectedElementEnd(tag))

    [lexer.SlotDefEnd, ..] -> Error(UnexpectedSlotDefEnd)

    [lexer.ImportDirective(_, line), ..] ->
      Error(DirectiveAfterContent("@import", line))

    [lexer.PropsDirective(_, line), ..] ->
      Error(DirectiveAfterContent("@props", line))
  }
}

/// Parses the children of an element until its closing tag.
/// Handles nested elements, components, slots, and maintains
/// conditional chain state across siblings.
///
fn parse_element_body(
  tokens: List(Token),
  element_tag: String,
  acc: List(Node),
  pending_if: Option(PendingIf),
) -> Result(#(List(Node), List(Token)), ParserError) {
  case tokens {
    [] -> Error(UnclosedElement(element_tag))

    [lexer.ElementEnd(tag), ..rest] if tag == element_tag -> {
      let acc = flush_pending_if(acc, pending_if)
      Ok(#(list.reverse(acc), rest))
    }

    [lexer.ElementEnd(tag), ..] -> Error(UnexpectedElementEnd(tag))

    [lexer.Text(content), ..rest] -> {
      let acc = flush_pending_if(acc, pending_if)
      parse_element_body(rest, element_tag, [TextNode(content), ..acc], None)
    }

    [lexer.Variable(name, line), ..rest] -> {
      let acc = flush_pending_if(acc, pending_if)
      parse_element_body(
        rest,
        element_tag,
        [VariableNode(name, line), ..acc],
        None,
      )
    }

    [lexer.RawVariable(name, line), ..rest] -> {
      let acc = flush_pending_if(acc, pending_if)
      parse_element_body(
        rest,
        element_tag,
        [RawVariableNode(name, line), ..acc],
        None,
      )
    }

    [lexer.Slot(name), ..rest] -> {
      // Self-closing <slot /> in element body - slot output with no fallback
      let acc = flush_pending_if(acc, pending_if)
      parse_element_body(rest, element_tag, [SlotNode(name, []), ..acc], None)
    }

    [lexer.Attributes, ..rest] -> {
      let acc = flush_pending_if(acc, pending_if)
      parse_element_body(rest, element_tag, [AttributesNode([]), ..acc], None)
    }

    [lexer.SlotDef(name), ..rest] -> {
      // Opening <slot> in element body - slot output with fallback
      let acc = flush_pending_if(acc, pending_if)
      use #(children, remaining) <- try_parse(
        parse_slot_fallback_body(rest, name, []),
      )
      parse_element_body(
        remaining,
        element_tag,
        [SlotNode(name, children), ..acc],
        None,
      )
    }

    [lexer.Component(name, attrs, self_closing), ..rest] -> {
      use #(node, remaining, new_pending) <- try_parse(
        parse_component_with_attrs(name, attrs, self_closing, rest, pending_if),
      )
      case node {
        Some(n) -> {
          let acc = flush_pending_if(acc, pending_if)
          parse_element_body(remaining, element_tag, [n, ..acc], new_pending)
        }
        None -> parse_element_body(remaining, element_tag, acc, new_pending)
      }
    }

    [lexer.Element(tag, attrs, self_closing), ..rest] -> {
      use #(node, remaining, new_pending) <- try_parse(parse_element_with_attrs(
        tag,
        attrs,
        self_closing,
        rest,
        pending_if,
      ))
      case node {
        Some(n) -> {
          let acc = flush_pending_if(acc, pending_if)
          parse_element_body(remaining, element_tag, [n, ..acc], new_pending)
        }
        None -> parse_element_body(remaining, element_tag, acc, new_pending)
      }
    }

    [lexer.ComponentEnd(name), ..] -> Error(UnexpectedComponentEnd(name))

    [lexer.SlotDefEnd, ..] -> Error(UnexpectedSlotDefEnd)

    [lexer.ImportDirective(_, line), ..] ->
      Error(DirectiveAfterContent("@import", line))

    [lexer.PropsDirective(_, line), ..] ->
      Error(DirectiveAfterContent("@props", line))
  }
}

/// Parses slot content until the closing </slot> tag. Used
/// for both slot fallback content in components and slot
/// definition content when passing to a component.
///
fn parse_slot_fallback_body(
  tokens: List(Token),
  slot_name: Option(String),
  acc: List(Node),
) -> Result(#(List(Node), List(Token)), ParserError) {
  case tokens {
    [] -> Error(UnclosedSlot(slot_name))

    [lexer.SlotDefEnd, ..rest] -> Ok(#(list.reverse(acc), rest))

    [lexer.Text(content), ..rest] ->
      parse_slot_fallback_body(rest, slot_name, [TextNode(content), ..acc])

    [lexer.Variable(name, line), ..rest] ->
      parse_slot_fallback_body(rest, slot_name, [
        VariableNode(name, line),
        ..acc
      ])

    [lexer.RawVariable(name, line), ..rest] ->
      parse_slot_fallback_body(rest, slot_name, [
        RawVariableNode(name, line),
        ..acc
      ])

    [lexer.Attributes, ..rest] ->
      parse_slot_fallback_body(rest, slot_name, [AttributesNode([]), ..acc])

    [lexer.Component(name, attrs, self_closing), ..rest] -> {
      use #(node, remaining, _) <- try_parse(parse_component_with_attrs(
        name,
        attrs,
        self_closing,
        rest,
        None,
      ))
      case node {
        Some(n) -> parse_slot_fallback_body(remaining, slot_name, [n, ..acc])
        None -> parse_slot_fallback_body(remaining, slot_name, acc)
      }
    }

    [lexer.Element(tag, attrs, self_closing), ..rest] -> {
      use #(node, remaining, _) <- try_parse(parse_element_with_attrs(
        tag,
        attrs,
        self_closing,
        rest,
        None,
      ))
      case node {
        Some(n) -> parse_slot_fallback_body(remaining, slot_name, [n, ..acc])
        None -> parse_slot_fallback_body(remaining, slot_name, acc)
      }
    }

    [lexer.ComponentEnd(name), ..] -> Error(UnexpectedComponentEnd(name))

    [lexer.ElementEnd(tag), ..] -> Error(UnexpectedElementEnd(tag))

    // Nested slots not allowed
    [lexer.Slot(_), ..] -> Error(UnexpectedToken(lexer.Slot(None)))

    [lexer.SlotDef(_), ..] -> Error(UnexpectedToken(lexer.SlotDef(None)))

    [lexer.ImportDirective(_, line), ..] ->
      Error(DirectiveAfterContent("@import", line))

    [lexer.PropsDirective(_, line), ..] ->
      Error(DirectiveAfterContent("@props", line))
  }
}

/// Chains parser results using use syntax. Propagates errors
/// automatically, allowing sequential parsing operations to
/// be written in a clean, linear style.
///
fn try_parse(
  result: Result(a, ParserError),
  next: fn(a) -> Result(b, ParserError),
) -> Result(b, ParserError) {
  case result {
    Ok(value) -> next(value)
    Error(e) -> Error(e)
  }
}
