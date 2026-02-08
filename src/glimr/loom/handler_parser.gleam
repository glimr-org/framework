//// Handler Expression Parser
////
//// Parses l-on:* handler expressions into structured data for code generation.
//// Handles single assignments, tuple destructuring, and expression extraction.
////
//// Examples:
////   "count = count + 1"
////   "count = counter.increment(count)"
////   "#(count, total) = counter.increment_both(count, total)"
////

import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import glimr/loom/lexer
import glimr/loom/parser.{type Node, type Template}

// ------------------------------------------------------------- Public Types

/// Represents a parsed handler expression.
/// Contains the targets (props to update) and the expression to evaluate.
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

/// Errors that can occur during handler parsing
pub type HandlerError {
  /// Handler string doesn't contain an assignment
  MissingAssignment(handler: String, line: Int)
  /// Empty target in assignment
  EmptyTarget(handler: String, line: Int)
  /// Empty expression in assignment
  EmptyExpression(handler: String, line: Int)
  /// Invalid tuple syntax
  InvalidTupleSyntax(handler: String, line: Int)
}

// ------------------------------------------------------------- Public Functions

/// Parses a handler expression string into a Handler structure.
/// Supports:
/// - Simple: "count = count + 1"
/// - Function call: "count = counter.increment(count)"
/// - Tuple destructure: "#(count, total) = counter.increment_both(count, total)"
///
pub fn parse(
  event: String,
  modifiers: List(String),
  handler_str: String,
  line: Int,
) -> Result(Handler, HandlerError) {
  let handler_str = string.trim(handler_str)

  // Find the assignment operator
  case find_assignment(handler_str) {
    Error(_) -> Error(MissingAssignment(handler_str, line))
    Ok(#(target_part, expr_part)) -> {
      let target_part = string.trim(target_part)
      let expr_part = string.trim(expr_part)

      case target_part, expr_part {
        "", _ -> Error(EmptyTarget(handler_str, line))
        _, "" -> Error(EmptyExpression(handler_str, line))
        _, _ -> {
          // Parse targets (single or tuple)
          case parse_targets(target_part) {
            Error(_) -> Error(InvalidTupleSyntax(handler_str, line))
            Ok(targets) ->
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
    }
  }
}

/// Checks if a handler expression uses any special variables.
/// Returns the list of special variables used ($value, $checked, $key).
///
pub fn get_special_vars(handler: Handler) -> List(String) {
  let expr = handler.expression
  []
  |> maybe_add_var(expr, "$value")
  |> maybe_add_var(expr, "$checked")
  |> maybe_add_var(expr, "$key")
}

/// Generates a unique handler ID from event and index.
///
pub fn handler_id(event: String, index: Int) -> String {
  "handle_" <> event <> "_" <> int.to_string(index)
}

/// Collects all handlers from a template AST.
/// Returns a list of (handler_id, Handler) tuples with parsed handler info.
/// l-model attributes are converted to input handlers.
///
pub fn collect_handlers(
  template: Template,
) -> Result(List(#(String, Handler)), HandlerError) {
  case collect_handlers_from_nodes(template.nodes, [], 0) {
    Ok(#(handlers, _)) -> Ok(list.reverse(handlers))
    Error(e) -> Error(e)
  }
}

/// Recursively collects handlers from nodes.
/// Returns (accumulated handlers, next index) without reversing.
///
fn collect_handlers_from_nodes(
  nodes: List(Node),
  acc: List(#(String, Handler)),
  index: Int,
) -> Result(#(List(#(String, Handler)), Int), HandlerError) {
  case nodes {
    [] -> Ok(#(acc, index))
    [node, ..rest] -> {
      case collect_handlers_from_node(node, acc, index) {
        Error(e) -> Error(e)
        Ok(#(new_acc, new_index)) ->
          collect_handlers_from_nodes(rest, new_acc, new_index)
      }
    }
  }
}

/// Collects handlers from a single node.
///
fn collect_handlers_from_node(
  node: Node,
  acc: List(#(String, Handler)),
  index: Int,
) -> Result(#(List(#(String, Handler)), Int), HandlerError) {
  case node {
    parser.ElementNode(_, attrs, children) -> {
      case collect_handlers_from_attrs(attrs, acc, index) {
        Error(e) -> Error(e)
        Ok(#(new_acc, new_index)) ->
          collect_handlers_from_nodes(children, new_acc, new_index)
      }
    }

    parser.ComponentNode(_, attrs, children) -> {
      case collect_handlers_from_attrs(attrs, acc, index) {
        Error(e) -> Error(e)
        Ok(#(new_acc, new_index)) ->
          collect_handlers_from_nodes(children, new_acc, new_index)
      }
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

/// Collects handlers from if node branches.
///
fn collect_handlers_from_branches(
  branches: List(#(Option(String), Int, List(Node))),
  acc: List(#(String, Handler)),
  index: Int,
) -> Result(#(List(#(String, Handler)), Int), HandlerError) {
  case branches {
    [] -> Ok(#(acc, index))
    [#(_, _, body), ..rest] -> {
      case collect_handlers_from_nodes(body, acc, index) {
        Error(e) -> Error(e)
        Ok(#(new_acc, new_index)) ->
          collect_handlers_from_branches(rest, new_acc, new_index)
      }
    }
  }
}

/// Collects handlers from attribute list.
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
          case parse(event, modifiers, handler_str, line) {
            Error(e) -> Error(e)
            Ok(handler) -> {
              let id = handler_id(event, index)
              collect_handlers_from_attrs(
                rest,
                [#(id, handler), ..acc],
                index + 1,
              )
            }
          }
        }

        lexer.LmModel(prop, line) -> {
          // l-model is sugar for l-on:input="prop = $value"
          case parse("input", [], prop <> " = $value", line) {
            Error(e) -> Error(e)
            Ok(handler) -> {
              let id = handler_id("input", index)
              collect_handlers_from_attrs(
                rest,
                [#(id, handler), ..acc],
                index + 1,
              )
            }
          }
        }

        // Other attributes don't contain handlers
        _ -> collect_handlers_from_attrs(rest, acc, index)
      }
    }
  }
}

// ------------------------------------------------------------- Private Functions

/// Finds the assignment operator (=) at the top level (not inside parens/brackets).
/// Returns the target and expression parts.
///
fn find_assignment(input: String) -> Result(#(String, String), Nil) {
  find_assignment_loop(input, 0, 0, "")
}

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

/// Parses the target part of an assignment.
/// Returns a list of target variable names.
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

/// Parses tuple destructuring like "#(count, total)".
///
fn parse_tuple_targets(target: String) -> Result(List(String), Nil) {
  // Remove "#(" prefix and ")" suffix
  let inner =
    target
    |> string.drop_start(2)
    |> string.trim

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

/// Checks if a string is a valid Gleam identifier.
///
fn is_valid_identifier(s: String) -> Bool {
  case string.first(s) {
    Error(_) -> False
    Ok(first) -> {
      // Must start with lowercase letter or underscore
      case is_lowercase_letter(first) || first == "_" {
        False -> False
        True -> {
          // Rest must be alphanumeric or underscore
          s
          |> string.to_graphemes
          |> list.drop(1)
          |> list.all(fn(c) {
            is_lowercase_letter(c)
            || is_uppercase_letter(c)
            || result.is_ok(int.parse(c))
            || c == "_"
          })
        }
      }
    }
  }
}

fn is_lowercase_letter(c: String) -> Bool {
  case c {
    "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" ->
      True
    "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" ->
      True
    _ -> False
  }
}

fn is_uppercase_letter(c: String) -> Bool {
  case c {
    "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" ->
      True
    "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" ->
      True
    _ -> False
  }
}

fn maybe_add_var(vars: List(String), expr: String, var: String) -> List(String) {
  case string.contains(expr, var) {
    True -> [var, ..vars]
    False -> vars
  }
}
