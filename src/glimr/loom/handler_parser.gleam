//// Handler Expression Parser
////
//// Template event handlers (l-on:click, l-on:input) are
//// written as inline assignment strings that need to become
//// typed Gleam code. This module bridges that gap by parsing
//// handler expressions into structured data so the code
//// generator can emit correct function bodies with proper prop
//// assignments, tuple destructuring, and special variable
//// injection ($value, $key, etc.).
////
//// *Example:*
////
//// "count = count + 1" "count = counter.increment(count)"
//// "#(count, total) = counter.increment_both(count, total)"
////

import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import glimr/loom/lexer
import glimr/loom/parser.{type Node, type Template}
import glimr/utils/string as string_util

// ------------------------------------------------------------- Public Types

/// Code generation needs to know which props a handler updates
/// and what expression produces the new values. Bundling event
/// metadata (name, modifiers), assignment targets, and the
/// expression together lets the generator emit a complete
/// handler function in one pass without re-parsing the original
/// string.
///
pub type Handler {
  Handler(
    /// The event name (click, input, submit, etc.)
    event: String,
    /// Event modifiers (prevent, stop, immediate, etc.)
    modifiers: List(String),
    /// Props being assigned to (single or tuple destructure)
    targets: List(String),
    /// The Gleam expression that produces the new value(s)
    expression: String,
    /// Original handler string for error messages
    original: String,
    /// Line number in source
    line: Int,
  )
}

/// Handler expressions are user-authored strings that can
/// easily contain syntax mistakes. Structured error variants
/// let the compiler point to the exact problem — a missing
/// assignment operator, an empty target, or malformed tuple
/// syntax — so the user can fix their template quickly.
///
pub type HandlerError {
  /// Empty target in assignment
  EmptyTarget(handler: String, line: Int)
  /// Empty expression in assignment
  EmptyExpression(handler: String, line: Int)
  /// Invalid tuple syntax
  InvalidTupleSyntax(handler: String, line: Int)
}

// ------------------------------------------------------------- Public Functions

/// Entry point for turning a raw handler string into a
/// structured Handler. Supports both assignment expressions
/// (prop = expr) and side-effect expressions (expr) that
/// execute code without updating props.
///
pub fn parse(
  event: String,
  modifiers: List(String),
  handler_str: String,
  line: Int,
) -> Result(Handler, HandlerError) {
  let handler_str = string.trim(handler_str)

  case find_assignment(handler_str) {
    // Assignment expression: "count = count + 1"
    Ok(#(target_part, expr_part)) -> {
      let target_part = string.trim(target_part)
      let expr_part = string.trim(expr_part)

      case target_part, expr_part {
        "", _ -> Error(EmptyTarget(handler_str, line))
        _, "" -> Error(EmptyExpression(handler_str, line))
        _, _ -> {
          use targets <- result.try(
            parse_targets(target_part)
            |> result.replace_error(InvalidTupleSyntax(handler_str, line)),
          )
          Ok(Handler(
            event: event,
            modifiers: modifiers,
            targets: targets,
            expression: expr_part,
            original: handler_str,
            line: line,
          ))
        }
      }
    }

    // Side-effect expression: "process.sleep(5)"
    Error(_) -> {
      case handler_str {
        "" -> Error(EmptyExpression(handler_str, line))
        _ ->
          Ok(Handler(
            event: event,
            modifiers: modifiers,
            targets: [],
            expression: handler_str,
            original: handler_str,
            line: line,
          ))
      }
    }
  }
}

/// Handler expressions can reference browser event data via
/// special variables ($value, $checked, $key). The generated
/// handler function needs to extract these from the JS event
/// object and pass them as arguments, so we scan for their
/// presence to know which extractions to emit.
///
pub fn get_special_vars(handler: Handler) -> List(String) {
  let expr = handler.expression
  []
  |> maybe_add_var(expr, "$value")
  |> maybe_add_var(expr, "$checked")
  |> maybe_add_var(expr, "$key")
}

/// Multiple handlers of the same event type can exist in a
/// single template. A deterministic ID based on event name and
/// index ensures each generated handler function has a unique
/// name that the client-side runtime can resolve.
///
pub fn handler_id(event: String, index: Int) -> String {
  "handle_" <> event <> "_" <> int.to_string(index)
}

/// The code generator needs a flat list of all handlers in the
/// template to emit handler functions and a dispatch map.
/// Walking the AST here centralizes that collection so the
/// generator doesn't need its own traversal logic. l-model
/// attributes are desugared into input handlers to keep the
/// generator's output uniform.
///
pub fn collect_handlers(
  template: Template,
) -> Result(List(#(String, Handler)), HandlerError) {
  collect_handlers_from_nodes(template.nodes, [], 0)
  |> result.map(fn(pair) { list.reverse(pair.0) })
}

// ------------------------------------------------------------- Private Functions

/// Handlers can appear at any depth in the node tree, so a
/// recursive walk is needed. The index counter threads through
/// the traversal to ensure every handler gets a globally unique
/// ID regardless of nesting depth.
///
fn collect_handlers_from_nodes(
  nodes: List(Node),
  acc: List(#(String, Handler)),
  index: Int,
) -> Result(#(List(#(String, Handler)), Int), HandlerError) {
  case nodes {
    [] -> Ok(#(acc, index))
    [node, ..rest] -> {
      use #(new_acc, new_index) <- result.try(collect_handlers_from_node(
        node,
        acc,
        index,
      ))
      collect_handlers_from_nodes(rest, new_acc, new_index)
    }
  }
}

/// Each node type contributes handlers differently — elements
/// and components carry attributes directly, while control flow
/// nodes (if, each) contain handlers in their child branches.
/// Pattern matching here keeps the per-node dispatch clear and
/// exhaustive.
///
fn collect_handlers_from_node(
  node: Node,
  acc: List(#(String, Handler)),
  index: Int,
) -> Result(#(List(#(String, Handler)), Int), HandlerError) {
  case node {
    parser.ElementNode(_, attrs, children)
    | parser.ComponentNode(_, attrs, children) -> {
      use #(new_acc, new_index) <- result.try(collect_handlers_from_attrs(
        attrs,
        acc,
        index,
      ))
      collect_handlers_from_nodes(children, new_acc, new_index)
    }

    parser.IfNode(branches) ->
      collect_handlers_from_branches(branches, acc, index)

    parser.EachNode(_, _, _, body, _) ->
      collect_handlers_from_nodes(body, acc, index)

    parser.SlotNode(_, fallback) ->
      collect_handlers_from_nodes(fallback, acc, index)

    parser.SlotDefNode(_, children) ->
      collect_handlers_from_nodes(children, acc, index)

    // These nodes don't contain handlers
    parser.TextNode(_)
    | parser.VariableNode(_, _)
    | parser.RawVariableNode(_, _)
    | parser.AttributesNode(_) -> Ok(#(acc, index))
  }
}

/// IfNode branches each contain independent node trees that may
/// hold handlers. Iterating through all branches ensures
/// handlers inside conditional blocks are still collected and
/// assigned unique IDs.
///
fn collect_handlers_from_branches(
  branches: List(#(Option(String), Int, List(Node))),
  acc: List(#(String, Handler)),
  index: Int,
) -> Result(#(List(#(String, Handler)), Int), HandlerError) {
  case branches {
    [] -> Ok(#(acc, index))
    [#(_, _, body), ..rest] -> {
      use #(new_acc, new_index) <- result.try(collect_handlers_from_nodes(
        body,
        acc,
        index,
      ))
      collect_handlers_from_branches(rest, new_acc, new_index)
    }
  }
}

/// This is where handlers are actually discovered — in the
/// attribute lists of elements and components. l-on:*
/// attributes are parsed into Handler structs, and l-model is
/// desugared into an equivalent l-on:input so both produce
/// uniform output for the code generator.
///
fn collect_handlers_from_attrs(
  attrs: List(lexer.ComponentAttr),
  acc: List(#(String, Handler)),
  index: Int,
) -> Result(#(List(#(String, Handler)), Int), HandlerError) {
  case attrs {
    [] -> Ok(#(acc, index))
    [attr, ..rest] -> {
      case attr {
        lexer.LmOn(event, modifiers, handler_str, line) -> {
          use handler <- result.try(parse(event, modifiers, handler_str, line))
          let id = handler_id(event, index)
          collect_handlers_from_attrs(rest, [#(id, handler), ..acc], index + 1)
        }

        // l-model is sugar for l-on:input="prop = $value"
        lexer.LmModel(prop, line) -> {
          use handler <- result.try(parse(
            "input",
            [],
            prop <> " = $value",
            line,
          ))
          let id = handler_id("input", index)
          collect_handlers_from_attrs(rest, [#(id, handler), ..acc], index + 1)
        }

        // Other attributes don't contain handlers
        _ -> collect_handlers_from_attrs(rest, acc, index)
      }
    }
  }
}

/// Handler expressions like "count = inc(count)" contain an
/// assignment, but the expression side may also contain =
/// inside parenthesized function calls or comparisons.
/// Splitting only at the top-level = avoids misinterpreting
/// nested equality or named arguments as the assignment.
///
fn find_assignment(input: String) -> Result(#(String, String), Nil) {
  find_assignment_loop(input, 0, 0, "")
}

/// Walks the string character by character tracking nesting
/// depth so that = inside parentheses or brackets is skipped.
/// Also distinguishes = (assignment) from == (equality) to
/// avoid false splits on comparisons.
///
fn find_assignment_loop(
  input: String,
  paren_depth: Int,
  bracket_depth: Int,
  acc: String,
) -> Result(#(String, String), Nil) {
  case string.pop_grapheme(input) {
    Error(_) -> Error(Nil)
    Ok(#(char, rest)) -> {
      case char {
        "(" ->
          find_assignment_loop(
            rest,
            paren_depth + 1,
            bracket_depth,
            acc <> char,
          )
        ")" ->
          find_assignment_loop(
            rest,
            paren_depth - 1,
            bracket_depth,
            acc <> char,
          )
        "[" ->
          find_assignment_loop(
            rest,
            paren_depth,
            bracket_depth + 1,
            acc <> char,
          )
        "]" ->
          find_assignment_loop(
            rest,
            paren_depth,
            bracket_depth - 1,
            acc <> char,
          )
        "=" if paren_depth == 0 && bracket_depth == 0 -> {
          // Check it's not == (equality)
          case string.first(rest) {
            Ok("=") ->
              find_assignment_loop(
                rest,
                paren_depth,
                bracket_depth,
                acc <> char,
              )
            _ -> Ok(#(acc, rest))
          }
        }
        _ -> find_assignment_loop(rest, paren_depth, bracket_depth, acc <> char)
      }
    }
  }
}

/// Handlers can update one prop or multiple props via tuple
/// destructuring. Returning a list of targets in both cases
/// gives the code generator a uniform interface — it always
/// iterates over targets regardless of cardinality.
///
fn parse_targets(target: String) -> Result(List(String), Nil) {
  let target = string.trim(target)
  case string.starts_with(target, "#(") {
    True -> parse_tuple_targets(target)
    False -> {
      // Single target - validate it's a valid identifier
      case is_valid_identifier(target) {
        True -> Ok([target])
        False -> Error(Nil)
      }
    }
  }
}

/// Tuple destructuring lets a single handler expression update
/// multiple props at once (e.g. a function returning #(count,
/// total)). Parsing the #() syntax here validates that each
/// element is a valid identifier before the code generator
/// tries to emit assignments for them.
///
fn parse_tuple_targets(target: String) -> Result(List(String), Nil) {
  // Remove "#(" prefix and ")" suffix
  let inner = {
    target
    |> string.drop_start(2)
    |> string.trim
  }

  case string.ends_with(inner, ")") {
    False -> Error(Nil)
    True -> {
      let inner = string.drop_end(inner, 1)
      let parts =
        inner
        |> string.split(",")
        |> list.map(string.trim)

      // Validate all parts are valid identifiers
      case list.all(parts, is_valid_identifier) {
        True -> Ok(parts)
        False -> Error(Nil)
      }
    }
  }
}

/// Assignment targets become variable names in generated Gleam
/// code. Validating identifier syntax here catches typos and
/// invalid characters at parse time rather than producing
/// broken generated code that fails later with confusing
/// compiler errors.
///
fn is_valid_identifier(s: String) -> Bool {
  case string.to_graphemes(s) {
    [] -> False
    [first, ..rest] -> {
      { first == "_" || string_util.is_lowercase_letter(first) }
      && list.all(rest, string_util.is_alphanumeric)
    }
  }
}

/// Conditionally includes a special variable only if it appears
/// in the expression, so the generated handler function doesn't
/// extract unused values from the JS event object.
///
fn maybe_add_var(vars: List(String), expr: String, var: String) -> List(String) {
  case string.contains(expr, var) {
    True -> [var, ..vars]
    False -> vars
  }
}
