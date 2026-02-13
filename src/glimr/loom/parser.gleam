//// Template Parser
////
//// The lexer produces a flat stream of tokens, but templates
//// are inherently hierarchical — components nest inside each
//// other, conditionals span siblings, and slots cross
//// component boundaries. This module transforms that flat
//// token stream into an AST that preserves these structural
//// relationships so downstream code generation can emit
//// correct, nested output without re-discovering structure.
////

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import glimr/loom/lexer.{type ComponentAttr, type Token}
import glimr/utils/string as string_utils

// ------------------------------------------------------------- Public Types

/// Code generation needs a single entry point that carries
/// all the information extracted from a template file —
/// imports, props, content nodes, and liveness — so it can
/// emit a complete module without re-parsing or requiring
/// multiple passes over the token stream.
///
pub type Template {
  Template(
    /// Import statements from @import directives
    imports: List(String),
    /// Props from @props directive (name, type pairs)
    props: List(#(String, String)),
    /// Content nodes
    nodes: List(Node),
    /// Whether this template contains l-on:* or l-model attributes
    /// and should be treated as a "live" template with WebSocket connection
    is_live: Bool,
  )
}

/// Different template constructs need distinct code
/// generation strategies — text is emitted verbatim,
/// variables require escaping or lookups, control flow
/// needs branching logic, and components trigger recursive
/// rendering. A sum type lets the code generator pattern
/// match exhaustively on every construct.
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

/// Template authors need actionable error messages when their
/// markup is malformed. Structured error variants let the
/// compiler surface the specific problem — a mismatched
/// closing tag, a dangling else branch, or a directive in
/// the wrong position — rather than a generic parse failure.
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

// ------------------------------------------------------------- Private Types

/// Conditional chains (l-if / l-else-if / l-else) span
/// multiple sibling elements, but the parser processes
/// tokens one at a time. This accumulator collects branches
/// as they appear so the chain can be emitted as a single
/// IfNode once a non-conditional sibling or end-of-context
/// signals the chain is complete.
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

/// Components, elements, and slots all contain children but
/// differ in their closing tags, slot semantics, and error
/// messages. This context type lets a single parse_body
/// function handle all three without duplicating the shared
/// child-parsing logic.
///
type BodyContext {
  ComponentBody(name: String)
  ElementBody(tag: String)
  SlotBody(name: Option(String))
}

// ------------------------------------------------------------- Public Functions

/// Entry point for the parser. Directives are extracted
/// first because they affect how the generated module is
/// structured (imports, function signatures) and must not
/// appear mixed with content. The remaining tokens are then
/// parsed into the node tree that drives code generation.
///
pub fn parse(tokens: List(Token)) -> Result(Template, ParserError) {
  // First extract @import and @props directives from the beginning
  use #(imports, props, remaining_tokens) <- result.try(extract_directives(
    tokens,
    [],
    None,
  ))

  // Then parse the content nodes
  use nodes <- result.try(parse_nodes(remaining_tokens, [], None))

  // Detect if template contains l-on:* or l-model (makes it "live")
  let is_live = has_live_attributes(nodes)

  Ok(Template(imports: imports, props: props, nodes: nodes, is_live: is_live))
}

// ------------------------------------------------------------- Private Functions

/// Directives control module-level concerns (what to import,
/// what props the template accepts) so they must be resolved
/// before content parsing begins. Enforcing a "directives
/// first" rule keeps template structure predictable and
/// avoids ambiguity about directive scope.
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

/// Whitespace-only text nodes between directives should be
/// ignored so that newlines and indentation between @import
/// and @props lines don't prematurely end the directive
/// extraction phase.
///
fn is_whitespace_only(string: String) -> Bool {
  string |> string.to_graphemes |> list.all(string_utils.is_whitespace)
}

/// The recursive backbone of the parser. Each token is
/// dispatched to the appropriate node constructor while a 
/// pending conditional accumulator tracks l-if chains across 
/// siblings — necessary because conditional branches are 
/// separate tokens that must coalesce into one IfNode.
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
      // Whitespace-only text between l-if/l-else-if/l-else siblings
      // should not break the conditional chain
      case pending_if, string.trim(content) {
        Some(_), "" -> parse_nodes(rest, acc, pending_if)
        _, _ -> {
          let acc = flush_pending_if(acc, pending_if)
          parse_nodes(rest, [TextNode(content), ..acc], None)
        }
      }
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
      use #(children, remaining) <- result.try(parse_body(
        rest,
        SlotBody(name),
        [],
        None,
      ))
      parse_nodes(remaining, [SlotNode(name, children), ..acc], None)
    }

    [lexer.Component(name, attrs, self_closing), ..rest] -> {
      // Check for l-* attributes on component
      use #(node, remaining, new_pending) <- result.try(
        parse_component_with_attrs(name, attrs, self_closing, rest, pending_if),
      )
      case node {
        Some(n) -> {
          // When new_pending is Some, the node is a flushed IfNode from
          // a replaced pending chain — don't double-flush
          let acc = case new_pending {
            Some(_) -> acc
            None -> flush_pending_if(acc, pending_if)
          }
          parse_nodes(remaining, [n, ..acc], new_pending)
        }
        None -> {
          // Node was absorbed into pending_if chain
          parse_nodes(remaining, acc, new_pending)
        }
      }
    }

    [lexer.Element(tag, attrs, self_closing), ..rest] -> {
      use #(node, remaining, new_pending) <- result.try(
        parse_element_with_attrs(tag, attrs, self_closing, rest, pending_if),
      )
      case node {
        Some(n) -> {
          let acc = case new_pending {
            Some(_) -> acc
            None -> flush_pending_if(acc, pending_if)
          }
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

/// A conditional chain only becomes a complete IfNode once a 
/// non-conditional sibling or end-of-context appears. This 
/// function materializes the accumulated branches at that 
/// boundary, ensuring the IfNode lands in the correct position 
/// relative to surrounding nodes.
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

/// Components can carry l-* directives that wrap them in
/// conditional or loop structures. This function separates
/// those directives from the component's own attributes so the 
/// generated AST nests correctly — the component node lives 
/// inside the control flow node, not alongside it.
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
  use #(children, remaining) <- result.try(case self_closing {
    True -> Ok(#([], rest))
    False -> parse_body(rest, ComponentBody(name), [], None)
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

/// Elements with both l-for and l-if need special nesting order: 
/// l-if goes inside the loop so loop variables are in scope for 
/// the condition. This function also unwraps <template> 
/// elements into their children to avoid emitting unnecessary 
/// wrapper nodes in the output.
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
  use #(children, remaining) <- result.try(case self_closing {
    True -> Ok(#([], rest))
    False -> parse_body(rest, ElementBody(tag), [], None)
  })

  // <template> with no remaining attrs is an invisible wrapper,
  // so unwrap its children directly to avoid an extra DOM node
  let base_nodes = case tag, clean_attrs {
    "template", [] -> children
    _, _ -> [ElementNode(tag, clean_attrs, children)]
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

/// Both components and elements can participate in conditional 
/// chains, so this shared function avoids duplicating the chain 
/// state machine. It validates ordering constraints (no else-if 
/// after else, no orphaned else) and returns the updated 
/// pending state.
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
    // l-if: start new chain (no existing pending)
    Some(#(condition, line)), _, _, None -> {
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

    // l-if: start new chain, flush existing pending as a completed IfNode
    Some(#(condition, line)), _, _, Some(p) -> {
      let first_branch = #(Some(p.first_condition), p.first_line, p.first_body)
      let all_branches = [first_branch, ..list.reverse(p.branches)]
      let flushed = IfNode(all_branches)
      Ok(#(
        Some(flushed),
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

/// l-if attributes are mixed in with regular HTML attributes by
/// the lexer. Extracting them separately lets the parser treat 
/// them as control flow directives rather than as attributes to 
/// pass through to the rendered output.
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

/// l-else-if must be extracted separately from l-if because it 
/// continues an existing conditional chain rather than starting '
/// a new one, requiring different state machine transitions in 
/// the parser.
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

/// Unlike l-if and l-else-if which carry condition strings,
/// l-else is a boolean presence check — it has no value. A 
/// simple Bool return suffices because the parser only needs to 
/// know whether the else branch exists, not extract any 
/// associated data.
///
fn has_lm_else(attrs: List(ComponentAttr)) -> Bool {
  list.any(attrs, fn(attr) {
    case attr {
      lexer.LmElse -> True
      _ -> False
    }
  })
}

/// l-for carries structured data (collection, destructured 
/// items, optional loop variable) that the parser needs to
/// build an EachNode. Extracting it here keeps the main parsing 
/// functions focused on nesting logic rather than attribute 
/// inspection.
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

/// Directive attributes control parsing structure but must not 
/// appear in the rendered output. Stripping them here ensures 
/// that generated HTML elements and component invocations only 
/// carry the attributes that belong in the final markup.
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

/// Child parsing is shared across components, elements, and
/// slots because they all consume tokens the same way — only 
/// the termination condition and slot semantics differ. 
/// Centralizing this avoids three near-identical recursive 
/// functions and ensures consistent conditional chain handling 
/// at every nesting level.
///
fn parse_body(
  tokens: List(Token),
  context: BodyContext,
  acc: List(Node),
  pending_if: Option(PendingIf),
) -> Result(#(List(Node), List(Token)), ParserError) {
  case tokens {
    // Empty input means unclosed tag
    [] ->
      case context {
        ComponentBody(name) -> Error(UnclosedComponent(name))
        ElementBody(tag) -> Error(UnclosedElement(tag))
        SlotBody(name) -> Error(UnclosedSlot(name))
      }

    // Closing tags - match against context
    [lexer.ComponentEnd(name), ..rest] ->
      case context {
        ComponentBody(expected) if name == expected -> {
          let acc = flush_pending_if(acc, pending_if)
          Ok(#(list.reverse(acc), rest))
        }
        _ -> Error(UnexpectedComponentEnd(name))
      }

    [lexer.ElementEnd(tag), ..rest] ->
      case context {
        ElementBody(expected) if tag == expected -> {
          let acc = flush_pending_if(acc, pending_if)
          Ok(#(list.reverse(acc), rest))
        }
        _ -> Error(UnexpectedElementEnd(tag))
      }

    [lexer.SlotDefEnd, ..rest] ->
      case context {
        SlotBody(_) -> {
          let acc = flush_pending_if(acc, pending_if)
          Ok(#(list.reverse(acc), rest))
        }
        _ -> Error(UnexpectedSlotDefEnd)
      }

    // Common content nodes
    [lexer.Text(content), ..rest] -> {
      // Whitespace-only text between l-if/l-else-if/l-else siblings
      // should not break the conditional chain
      case pending_if, string.trim(content) {
        Some(_), "" -> parse_body(rest, context, acc, pending_if)
        _, _ -> {
          let acc = flush_pending_if(acc, pending_if)
          parse_body(rest, context, [TextNode(content), ..acc], None)
        }
      }
    }

    [lexer.Variable(name, line), ..rest] -> {
      let acc = flush_pending_if(acc, pending_if)
      parse_body(rest, context, [VariableNode(name, line), ..acc], None)
    }

    [lexer.RawVariable(name, line), ..rest] -> {
      let acc = flush_pending_if(acc, pending_if)
      parse_body(rest, context, [RawVariableNode(name, line), ..acc], None)
    }

    [lexer.Attributes, ..rest] -> {
      let acc = flush_pending_if(acc, pending_if)
      parse_body(rest, context, [AttributesNode([]), ..acc], None)
    }

    // Slot handling differs by context: components define slot
    // content, elements output it, and nested slots are invalid
    [lexer.Slot(name), ..rest] ->
      case context {
        ComponentBody(_) -> {
          let acc = flush_pending_if(acc, pending_if)
          parse_body(rest, context, [SlotDefNode(name, []), ..acc], None)
        }
        ElementBody(_) -> {
          let acc = flush_pending_if(acc, pending_if)
          parse_body(rest, context, [SlotNode(name, []), ..acc], None)
        }
        SlotBody(_) -> Error(UnexpectedToken(lexer.Slot(None)))
      }

    [lexer.SlotDef(name), ..rest] ->
      case context {
        ComponentBody(_) -> {
          let acc = flush_pending_if(acc, pending_if)
          use #(children, remaining) <- result.try(parse_body(
            rest,
            SlotBody(name),
            [],
            None,
          ))
          parse_body(
            remaining,
            context,
            [SlotDefNode(name, children), ..acc],
            None,
          )
        }
        ElementBody(_) -> {
          let acc = flush_pending_if(acc, pending_if)
          use #(children, remaining) <- result.try(parse_body(
            rest,
            SlotBody(name),
            [],
            None,
          ))
          parse_body(
            remaining,
            context,
            [SlotNode(name, children), ..acc],
            None,
          )
        }
        SlotBody(_) -> Error(UnexpectedToken(lexer.SlotDef(None)))
      }

    // Nested components and elements
    [lexer.Component(name, attrs, self_closing), ..rest] -> {
      use #(node, remaining, new_pending) <- result.try(
        parse_component_with_attrs(name, attrs, self_closing, rest, pending_if),
      )
      case node {
        Some(n) -> {
          let acc = case new_pending {
            Some(_) -> acc
            None -> flush_pending_if(acc, pending_if)
          }
          parse_body(remaining, context, [n, ..acc], new_pending)
        }
        None -> parse_body(remaining, context, acc, new_pending)
      }
    }

    [lexer.Element(tag, attrs, self_closing), ..rest] -> {
      use #(node, remaining, new_pending) <- result.try(
        parse_element_with_attrs(tag, attrs, self_closing, rest, pending_if),
      )
      case node {
        Some(n) -> {
          let acc = case new_pending {
            Some(_) -> acc
            None -> flush_pending_if(acc, pending_if)
          }
          parse_body(remaining, context, [n, ..acc], new_pending)
        }
        None -> parse_body(remaining, context, acc, new_pending)
      }
    }

    [lexer.ImportDirective(_, line), ..] ->
      Error(DirectiveAfterContent("@import", line))

    [lexer.PropsDirective(_, line), ..] ->
      Error(DirectiveAfterContent("@props", line))
  }
}

/// Templates with event handlers or model bindings need a
/// WebSocket connection for interactivity. Detecting this at 
/// parse time lets the framework decide upfront whether to wire 
/// up live infrastructure, avoiding unnecessary overhead for 
/// purely static templates.
///
fn has_live_attributes(nodes: List(Node)) -> Bool {
  list.any(nodes, fn(node) {
    case node {
      ElementNode(_, attributes, children) ->
        attrs_have_live(attributes) || has_live_attributes(children)
      ComponentNode(_, attributes, children) ->
        attrs_have_live(attributes) || has_live_attributes(children)
      IfNode(branches) ->
        list.any(branches, fn(branch) { has_live_attributes(branch.2) })
      EachNode(_, _, _, body, _) -> has_live_attributes(body)
      SlotNode(_, fallback) -> has_live_attributes(fallback)
      SlotDefNode(_, children) -> has_live_attributes(children)
      TextNode(_)
      | VariableNode(_, _)
      | RawVariableNode(_, _)
      | AttributesNode(_) -> False
    }
  })
}

/// Leaf-level check for the recursive has_live_attributes
/// traversal. Separated out so the tree walker only needs to 
/// concern itself with node structure, not with how individual 
/// attributes are classified as "live".
///
fn attrs_have_live(attrs: List(ComponentAttr)) -> Bool {
  list.any(attrs, fn(attr) {
    case attr {
      lexer.LmOn(_, _, _, _) | lexer.LmModel(_, _) -> True
      _ -> False
    }
  })
}
