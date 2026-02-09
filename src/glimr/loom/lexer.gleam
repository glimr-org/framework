//// Template Lexer
////
//// Converts template source text into a stream of tokens.
//// Recognizes variables, directives, components, and plain
//// text for the parser to process.
////

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import glimr/utils/string as string_utils

// ------------------------------------------------------------- Public Types

/// Represents a token produced by the lexer. Each variant
/// corresponds to a template syntax element like variables,
/// directives, or component tags.
///
pub type Token {
  Text(String)
  Variable(name: String, line: Int)
  RawVariable(name: String, line: Int)
  Slot(name: Option(String))
  SlotDef(name: Option(String))
  SlotDefEnd
  Attributes
  Component(name: String, attributes: List(ComponentAttr), self_closing: Bool)
  ComponentEnd(name: String)
  Element(tag: String, attributes: List(ComponentAttr), self_closing: Bool)
  ElementEnd(tag: String)
  /// @import directive for importing types/modules
  ImportDirective(import_str: String, line: Int)
  /// @props directive declaring template parameters
  PropsDirective(props: List(#(String, String)), line: Int)
}

/// Represents an attribute on a component tag. Can be a
/// string literal, an expression to evaluate, a boolean
/// attribute with no value, or conditional class/style
/// expressions.
///
pub type ComponentAttr {
  StringAttr(name: String, value: String)
  ExprAttr(name: String, value: String)
  BoolAttr(name: String)
  ClassAttr(value: String)
  StyleAttr(value: String)
  LmIf(condition: String, line: Int)
  LmElseIf(condition: String, line: Int)
  LmElse
  LmFor(
    collection: String,
    items: List(String),
    loop_var: Option(String),
    line: Int,
  )
  /// l-on:event handler (e.g., l-on:click="count = count + 1")
  LmOn(event: String, modifiers: List(String), handler: String, line: Int)
  /// l-model two-way binding (e.g., l-model="name")
  LmModel(prop: String, line: Int)
}

/// Errors that can occur during lexical analysis. Includes
/// unterminated constructs, invalid names, and malformed
/// directive syntax.
///
pub type LexerError {
  UnterminatedExpression(position: Int)
  EmptyExpression(position: Int)
  UnterminatedDirective(position: Int)
  InvalidDirective(directive: String, position: Int)
  InvalidLmForSyntax(content: String, position: Int)
  UnterminatedComponent(position: Int)
  InvalidComponentSyntax(content: String, position: Int)
  InvalidPropsDirective(content: String, line: Int)
  InvalidImportDirective(content: String, line: Int)
  UnterminatedPropsDirective(line: Int)
  UnterminatedImportDirective(line: Int)
}

// ------------------------------------------------------------- Public Functions

/// Tokenizes template source into a list of tokens. Scans the
/// input character by character, recognizing template syntax
/// and returning errors for malformed constructs.
///
pub fn tokenize(input: String) -> Result(List(Token), LexerError) {
  // tag_stack tracks open HTML tags in LIFO order. Each entry is
  // (tag_name, is_dynamic) where is_dynamic=True means we emitted an
  // Element token, False means it was plain HTML emitted as text.
  // This ensures closing tags match the correct opening tag.
  do_tokenize(input, 0, 1, [], [])
}

// ------------------------------------------------------------- Private Functions

/// Main tokenization loop. Pattern matches on input prefixes
/// to identify token types and delegates to specialized
/// parsers for complex constructs. Tracks line numbers for
/// error reporting.
///
/// tag_stack: Stack of open tags as (tag_name, is_dynamic) tuples.
///
fn do_tokenize(
  input: String,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  case input {
    "" -> Ok(list.reverse(tokens))

    "{{{" <> rest -> {
      case string.split_once(rest, "}}}") {
        Ok(#(expr, remaining)) -> {
          let expr = string.trim(expr)
          case expr {
            "" -> Error(EmptyExpression(position))
            _ -> {
              let new_pos = position + 6 + string.length(expr)
              let new_line = line + count_newlines("{{{" <> expr <> "}}}")
              do_tokenize(
                remaining,
                new_pos,
                new_line,
                [RawVariable(expr, line), ..tokens],
                tag_stack,
              )
            }
          }
        }
        Error(_) -> Error(UnterminatedExpression(position))
      }
    }

    "{{" <> rest -> {
      case string.split_once(rest, "}}") {
        Ok(#(expr, remaining)) -> {
          let expr = string.trim(expr)
          case expr {
            "" -> Error(EmptyExpression(position))
            _ -> {
              let new_pos = position + 4 + string.length(expr)
              let new_line = line + count_newlines("{{" <> expr <> "}}")
              do_tokenize(
                remaining,
                new_pos,
                new_line,
                [Variable(expr, line), ..tokens],
                tag_stack,
              )
            }
          }
        }
        Error(_) -> Error(UnterminatedExpression(position))
      }
    }

    // Skip HTML comments entirely - don't parse their contents
    "<!--" <> rest -> {
      case string.split_once(rest, "-->") {
        Ok(#(comment_content, remaining)) -> {
          let full_comment = "<!--" <> comment_content <> "-->"
          let new_pos = position + string.length(full_comment)
          let new_line = line + count_newlines(full_comment)
          // Emit the comment as plain text (so it appears in output)
          case tokens {
            [Text(prev), ..rest_tokens] ->
              do_tokenize(
                remaining,
                new_pos,
                new_line,
                [Text(prev <> full_comment), ..rest_tokens],
                tag_stack,
              )
            _ ->
              do_tokenize(
                remaining,
                new_pos,
                new_line,
                [Text(full_comment), ..tokens],
                tag_stack,
              )
          }
        }
        // Unclosed comment - just treat "<!--" as text
        Error(_) -> {
          case tokens {
            [Text(prev), ..rest_tokens] ->
              do_tokenize(
                rest,
                position + 4,
                line,
                [Text(prev <> "<!--"), ..rest_tokens],
                tag_stack,
              )
            _ ->
              do_tokenize(
                rest,
                position + 4,
                line,
                [Text("<!--"), ..tokens],
                tag_stack,
              )
          }
        }
      }
    }

    "@import(" <> rest ->
      parse_import_directive(rest, position, line, tokens, tag_stack)

    "@props(" <> rest ->
      parse_props_directive(rest, position, line, tokens, tag_stack)

    "@attributes" <> rest ->
      parse_simple_directive(
        rest,
        "@attributes",
        Attributes,
        position,
        line,
        tokens,
        tag_stack,
      )

    "<slot" <> rest ->
      parse_slot_element(rest, position, line, tokens, tag_stack)

    "</slot>" <> rest ->
      do_tokenize(rest, position + 7, line, [SlotDefEnd, ..tokens], tag_stack)

    "<x-" <> rest -> parse_component(rest, position, line, tokens, tag_stack)

    "</x-" <> rest ->
      parse_component_end(rest, position, line, tokens, tag_stack)

    "</" <> rest -> parse_element_end(rest, position, line, tokens, tag_stack)

    "<" <> rest ->
      try_parse_element(rest, position, line, tokens, input, tag_stack)

    _ -> consume_text(input, position, line, tokens, tag_stack)
  }
}

/// Counts newline characters in a string.
///
fn count_newlines(s: String) -> Int {
  s
  |> string.to_graphemes
  |> list.filter(fn(c) { c == "\n" })
  |> list.length
}

/// Parses a component opening tag starting after "<x-".
/// Extracts the name, attributes, and whether it's self
/// closing, then continues tokenization.
///
fn parse_component(
  input: String,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  case parse_component_tag(input, line) {
    Ok(#(name, attrs, self_closing, rest)) -> {
      let token = Component(name, attrs, self_closing)
      let consumed =
        "<x-"
        <> string.slice(input, 0, string.length(input) - string.length(rest))
      let len = string.length(input) - string.length(rest) + 3
      let new_line = line + count_newlines(consumed)
      do_tokenize(rest, position + len, new_line, [token, ..tokens], tag_stack)
    }
    Error(_) -> Error(UnterminatedComponent(position))
  }
}

/// Parses a component closing tag starting after "</x-".
/// Extracts the component name and continues tokenization
/// with the remaining input.
///
fn parse_component_end(
  input: String,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  case string.split_once(input, ">") {
    Ok(#(name, rest)) -> {
      let name = string.trim(name)
      let len = string.length(name) + 5
      do_tokenize(
        rest,
        position + len,
        line,
        [ComponentEnd(name), ..tokens],
        tag_stack,
      )
    }
    Error(_) -> Error(UnterminatedComponent(position))
  }
}

/// Tries to parse an HTML element. If it has l-* attributes,
/// emits an Element token. Otherwise, emits as text. Both cases
/// push to tag_stack for proper closing tag matching.
///
fn try_parse_element(
  input: String,
  position: Int,
  line: Int,
  tokens: List(Token),
  _full_input: String,
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  case parse_element_tag(input, line) {
    Ok(#(tag, attrs, self_closing, rest)) -> {
      // Check if element has any dynamic attributes (l-*, :class, :style, :*)
      case has_dynamic_attrs(attrs) {
        True -> {
          let token = Element(tag, attrs, self_closing)
          let consumed =
            "<"
            <> string.slice(
              input,
              0,
              string.length(input) - string.length(rest),
            )
          let len = string.length(input) - string.length(rest) + 1
          let new_line = line + count_newlines(consumed)
          // Push dynamic tag onto stack (only non-self-closing)
          let new_stack = case self_closing {
            True -> tag_stack
            False -> [#(tag, True), ..tag_stack]
          }
          do_tokenize(
            rest,
            position + len,
            new_line,
            [token, ..tokens],
            new_stack,
          )
        }
        False -> {
          // No l-* attributes - emit as text, push plain tag onto stack
          let new_stack = case self_closing {
            True -> tag_stack
            False -> [#(tag, False), ..tag_stack]
          }
          case tokens {
            [Text(prev), ..rest_tokens] ->
              do_tokenize(
                input,
                position + 1,
                line,
                [Text(prev <> "<"), ..rest_tokens],
                new_stack,
              )
            _ ->
              do_tokenize(
                input,
                position + 1,
                line,
                [Text("<"), ..tokens],
                new_stack,
              )
          }
        }
      }
    }
    Error(_) -> {
      // Not a valid element, just consume the "<" and continue
      case tokens {
        [Text(prev), ..rest_tokens] ->
          do_tokenize(
            input,
            position + 1,
            line,
            [Text(prev <> "<"), ..rest_tokens],
            tag_stack,
          )
        _ ->
          do_tokenize(
            input,
            position + 1,
            line,
            [Text("<"), ..tokens],
            tag_stack,
          )
      }
    }
  }
}

/// Parses an HTML element closing tag starting after "</".
/// Pops from tag_stack to determine if this closes a plain
/// HTML tag (emit as text) or a dynamic Element (emit ElementEnd).
///
fn parse_element_end(
  input: String,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  case string.split_once(input, ">") {
    Ok(#(tag, rest)) -> {
      let tag = string.trim(tag)
      let text = "</" <> tag <> ">"
      let len = string.length(text)

      // Pop from stack to find the matching opening tag
      case pop_matching_tag(tag_stack, tag) {
        // Found matching tag on stack
        Ok(#(is_dynamic, new_stack)) -> {
          case is_dynamic {
            True -> {
              // Closing a dynamic Element - emit ElementEnd
              do_tokenize(
                rest,
                position + len,
                line,
                [ElementEnd(tag), ..tokens],
                new_stack,
              )
            }
            False -> {
              // Closing a plain HTML tag - emit as text
              case tokens {
                [Text(prev), ..rest_tokens] ->
                  do_tokenize(
                    rest,
                    position + len,
                    line,
                    [Text(prev <> text), ..rest_tokens],
                    new_stack,
                  )
                _ ->
                  do_tokenize(
                    rest,
                    position + len,
                    line,
                    [Text(text), ..tokens],
                    new_stack,
                  )
              }
            }
          }
        }
        // No matching tag on stack - emit as text
        Error(_) -> {
          case tokens {
            [Text(prev), ..rest_tokens] ->
              do_tokenize(
                rest,
                position + len,
                line,
                [Text(prev <> text), ..rest_tokens],
                tag_stack,
              )
            _ ->
              do_tokenize(
                rest,
                position + len,
                line,
                [Text(text), ..tokens],
                tag_stack,
              )
          }
        }
      }
    }
    Error(_) -> {
      // Invalid closing tag, treat as text
      consume_text("</" <> input, position, line, tokens, tag_stack)
    }
  }
}

/// Pops the first matching tag from the stack.
/// Returns Ok(is_dynamic, remaining_stack) if found, Error otherwise.
///
fn pop_matching_tag(
  stack: List(#(String, Bool)),
  tag: String,
) -> Result(#(Bool, List(#(String, Bool))), Nil) {
  case stack {
    [] -> Error(Nil)
    [#(t, is_dynamic), ..rest] if t == tag -> Ok(#(is_dynamic, rest))
    [_, ..rest] -> {
      // Tag doesn't match - this is malformed HTML, but we handle it
      // by continuing to search. In practice, browsers are lenient.
      case pop_matching_tag(rest, tag) {
        Ok(#(is_dynamic, remaining)) -> Ok(#(is_dynamic, remaining))
        Error(_) -> Error(Nil)
      }
    }
  }
}

/// Checks if attribute list contains any dynamic attributes.
/// Includes l-* directives, :class, :style, and expression
/// bindings that require runtime evaluation.
///
fn has_dynamic_attrs(attrs: List(ComponentAttr)) -> Bool {
  list.any(attrs, fn(attr) {
    case attr {
      LmIf(_, _) | LmElseIf(_, _) | LmElse | LmFor(_, _, _, _) -> True
      LmOn(_, _, _, _) | LmModel(_, _) -> True
      ClassAttr(_) | StyleAttr(_) | ExprAttr(_, _) -> True
      _ -> False
    }
  })
}

/// Parses an HTML element tag. Returns tag name, attributes,
/// self-closing flag, and remaining input after the closing
/// bracket.
///
fn parse_element_tag(
  input: String,
  line: Int,
) -> Result(#(String, List(ComponentAttr), Bool, String), Nil) {
  let #(tag, rest) = take_component_name(input, "")
  case tag {
    "" -> Error(Nil)
    // Don't parse x- prefixed tags as elements (they're components)
    "x-" <> _ -> Error(Nil)
    _ -> {
      let #(attrs, rest) = parse_element_attrs(rest, [], line)
      let rest = skip_whitespace(rest)
      case rest {
        "/>" <> remaining -> Ok(#(tag, attrs, True, remaining))
        ">" <> remaining -> Ok(#(tag, attrs, False, remaining))
        _ -> Error(Nil)
      }
    }
  }
}

/// Parses attributes from an HTML element tag. Handles regular
/// string and boolean attributes as well as l-* directive
/// attributes for conditionals and loops.
///
fn parse_element_attrs(
  input: String,
  acc: List(ComponentAttr),
  line: Int,
) -> #(List(ComponentAttr), String) {
  let input = skip_whitespace(input)
  case input {
    ">" <> _ -> #(list.reverse(acc), input)
    "/>" <> _ -> #(list.reverse(acc), input)
    "" -> #(list.reverse(acc), input)
    ":" <> rest -> {
      case parse_expr_attr(rest) {
        Ok(#(attr, remaining)) ->
          parse_element_attrs(remaining, [attr, ..acc], line)
        Error(_) -> #(list.reverse(acc), input)
      }
    }
    "l-if=" <> rest -> {
      case parse_lm_condition_attr(rest) {
        Ok(#(condition, remaining)) ->
          parse_element_attrs(remaining, [LmIf(condition, line), ..acc], line)
        Error(_) -> #(list.reverse(acc), input)
      }
    }
    "l-else-if=" <> rest -> {
      case parse_lm_condition_attr(rest) {
        Ok(#(condition, remaining)) ->
          parse_element_attrs(
            remaining,
            [LmElseIf(condition, line), ..acc],
            line,
          )
        Error(_) -> #(list.reverse(acc), input)
      }
    }
    "l-else" <> rest -> {
      let rest = skip_whitespace(rest)
      parse_element_attrs(rest, [LmElse, ..acc], line)
    }
    "l-for=" <> rest -> {
      case parse_lm_for_attr(rest) {
        Ok(#(collection, items, loop_var, remaining)) ->
          parse_element_attrs(
            remaining,
            [LmFor(collection, items, loop_var, line), ..acc],
            line,
          )
        Error(_) -> #(list.reverse(acc), input)
      }
    }
    "l-on:" <> rest -> {
      case parse_lm_on_attr(rest, line) {
        Ok(#(attr, remaining)) ->
          parse_element_attrs(remaining, [attr, ..acc], line)
        Error(_) -> #(list.reverse(acc), input)
      }
    }
    "l-model=" <> rest -> {
      case parse_lm_model_attr(rest, line) {
        Ok(#(attr, remaining)) ->
          parse_element_attrs(remaining, [attr, ..acc], line)
        Error(_) -> #(list.reverse(acc), input)
      }
    }
    _ -> {
      case parse_string_or_bool_attr(input) {
        Ok(#(attr, remaining)) ->
          parse_element_attrs(remaining, [attr, ..acc], line)
        Error(_) -> #(list.reverse(acc), input)
      }
    }
  }
}

/// Parses a condition value from l-if or l-else-if. Extracts
/// the quoted condition string supporting both single and
/// double quote delimiters.
///
fn parse_lm_condition_attr(input: String) -> Result(#(String, String), Nil) {
  case input {
    "\"" <> rest -> {
      case take_until_quote(rest, "\"", "") {
        Ok(#(value, remaining)) -> Ok(#(value, remaining))
        Error(_) -> Error(Nil)
      }
    }
    "'" <> rest -> {
      case take_until_quote(rest, "'", "") {
        Ok(#(value, remaining)) -> Ok(#(value, remaining))
        Error(_) -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

/// Parses an l-for attribute value. Supports syntax like
/// "item in collection", "(key, val) in items", and optional
/// loop variable: "item in collection, loop".
///
fn parse_lm_for_attr(
  input: String,
) -> Result(#(String, List(String), Option(String), String), Nil) {
  case input {
    "\"" <> rest -> {
      case take_until_quote(rest, "\"", "") {
        Ok(#(value, remaining)) -> {
          case parse_lm_for_syntax(value) {
            Ok(#(collection, items, loop_var)) ->
              Ok(#(collection, items, loop_var, remaining))
            Error(_) -> Error(Nil)
          }
        }
        Error(_) -> Error(Nil)
      }
    }
    "'" <> rest -> {
      case take_until_quote(rest, "'", "") {
        Ok(#(value, remaining)) -> {
          case parse_lm_for_syntax(value) {
            Ok(#(collection, items, loop_var)) ->
              Ok(#(collection, items, loop_var, remaining))
            Error(_) -> Error(Nil)
          }
        }
        Error(_) -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

/// Parses the inner l-for syntax after quotes are removed.
/// Splits on " in " to separate item pattern from collection,
/// then extracts optional loop variable after comma.
///
fn parse_lm_for_syntax(
  value: String,
) -> Result(#(String, List(String), Option(String)), Nil) {
  case string.split_once(value, " in ") {
    Ok(#(item_part, collection_part)) -> {
      let item_part = string.trim(item_part)
      let collection_part = string.trim(collection_part)

      // Parse items (single or tuple)
      let items = parse_item_pattern(item_part)

      // Parse collection and optional loop variable
      let #(collection, loop_var) = parse_collection_and_loop(collection_part)

      Ok(#(collection, items, loop_var))
    }
    Error(_) -> Error(Nil)
  }
}

/// Parses the item pattern from l-for.
/// "item" -> ["item"]
/// "(key, value)" -> ["key", "value"]
///
fn parse_item_pattern(pattern: String) -> List(String) {
  case string.starts_with(pattern, "(") {
    True -> {
      // Tuple: (key, value)
      let inner =
        pattern
        |> string.drop_start(1)
        |> string.drop_end(1)
      inner
      |> string.split(",")
      |> list.map(string.trim)
    }
    False -> [pattern]
  }
}

/// Parses collection name and optional loop variable.
/// "items" -> ("items", None)
/// "items, loop" -> ("items", Some("loop"))
///
fn parse_collection_and_loop(input: String) -> #(String, Option(String)) {
  case string.split_once(input, ",") {
    Ok(#(collection, loop_var)) -> #(
      string.trim(collection),
      Some(string.trim(loop_var)),
    )
    Error(_) -> #(input, None)
  }
}

/// Parses an l-on:event attribute. Extracts the event name,
/// modifiers, and handler expression.
/// Examples:
///   l-on:click="count = count + 1"
///   l-on:click.prevent="handler()"
///   l-on:input.debounce-300="name = $value"
///
fn parse_lm_on_attr(
  input: String,
  line: Int,
) -> Result(#(ComponentAttr, String), Nil) {
  // Parse event name and modifiers (e.g., "click.prevent.stop")
  let #(event_part, rest) = take_until_equals_or_space(input, "")
  case event_part, rest {
    "", _ -> Error(Nil)
    _, "=" <> remaining -> {
      // Parse the handler value
      case parse_quoted_value(remaining) {
        Ok(#(handler, rest2)) -> {
          let #(event, modifiers) = parse_event_and_modifiers(event_part)
          Ok(#(LmOn(event, modifiers, handler, line), rest2))
        }
        Error(_) -> Error(Nil)
      }
    }
    _, _ -> Error(Nil)
  }
}

/// Parses an l-model attribute. Extracts the prop name.
/// Example: l-model="name"
///
fn parse_lm_model_attr(
  input: String,
  line: Int,
) -> Result(#(ComponentAttr, String), Nil) {
  case parse_quoted_value(input) {
    Ok(#(prop, rest)) -> Ok(#(LmModel(prop, line), rest))
    Error(_) -> Error(Nil)
  }
}

/// Takes characters until = or whitespace is found.
///
fn take_until_equals_or_space(input: String, acc: String) -> #(String, String) {
  case string.pop_grapheme(input) {
    Ok(#(c, rest)) -> {
      case c {
        "=" | " " | "\t" | "\n" | ">" | "/" -> #(acc, input)
        _ -> take_until_equals_or_space(rest, acc <> c)
      }
    }
    Error(_) -> #(acc, input)
  }
}

/// Parses event name and modifiers from a string like "click.prevent.stop".
/// Returns (event_name, [modifiers]).
///
fn parse_event_and_modifiers(input: String) -> #(String, List(String)) {
  case string.split(input, ".") {
    [event, ..modifiers] -> #(event, modifiers)
    [] -> #(input, [])
  }
}

/// Parses a quoted value (single or double quotes).
/// Returns the value and remaining input.
///
fn parse_quoted_value(input: String) -> Result(#(String, String), Nil) {
  case input {
    "\"" <> rest -> {
      case take_until_quote(rest, "\"", "") {
        Ok(#(value, remaining)) -> Ok(#(value, remaining))
        Error(_) -> Error(Nil)
      }
    }
    "'" <> rest -> {
      case take_until_quote(rest, "'", "") {
        Ok(#(value, remaining)) -> Ok(#(value, remaining))
        Error(_) -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

/// Takes characters until a specific quote character is found.
/// Used to extract attribute values enclosed in matching
/// quote delimiters. Handles escaped quotes (\" or \').
///
fn take_until_quote(
  input: String,
  quote: String,
  acc: String,
) -> Result(#(String, String), Nil) {
  case string.pop_grapheme(input) {
    Ok(#("\\", rest)) -> {
      // Check if this is an escaped quote
      case string.pop_grapheme(rest) {
        Ok(#(next, rest2)) if next == quote -> {
          // Escaped quote - include the quote (not the backslash) and continue
          take_until_quote(rest2, quote, acc <> quote)
        }
        Ok(#(next, rest2)) -> {
          // Backslash followed by something else - keep both
          take_until_quote(rest2, quote, acc <> "\\" <> next)
        }
        Error(_) -> Error(Nil)
      }
    }
    Ok(#(c, rest)) -> {
      case c == quote {
        True -> Ok(#(acc, rest))
        False -> take_until_quote(rest, quote, acc <> c)
      }
    }
    Error(_) -> Error(Nil)
  }
}

/// Parses the full component tag structure. Returns the name,
/// list of attributes, self-closing flag, and remaining
/// input after the closing bracket.
///
fn parse_component_tag(
  input: String,
  line: Int,
) -> Result(#(String, List(ComponentAttr), Bool, String), Nil) {
  let #(name, rest) = take_component_name(input, "")
  case name {
    "" -> Error(Nil)
    _ -> {
      let #(attrs, rest) = parse_component_attrs(rest, [], line)
      let rest = skip_whitespace(rest)
      case rest {
        "/>" <> remaining -> Ok(#(name, attrs, True, remaining))
        ">" <> remaining -> Ok(#(name, attrs, False, remaining))
        _ -> Error(Nil)
      }
    }
  }
}

/// Extracts the component name from the input. Reads until
/// whitespace, closing bracket, or slash is encountered,
/// returning the name and remaining input.
///
fn take_component_name(input: String, acc: String) -> #(String, String) {
  case string.pop_grapheme(input) {
    Ok(#(c, rest)) -> {
      case c {
        " " | "\t" | "\n" | ">" | "/" -> #(acc, input)
        _ -> take_component_name(rest, acc <> c)
      }
    }
    Error(_) -> #(acc, input)
  }
}

/// Parses all attributes from a component tag. Handles
/// expression attributes (prefixed with :), l-* directive
/// attributes, and string/boolean attributes.
///
fn parse_component_attrs(
  input: String,
  acc: List(ComponentAttr),
  line: Int,
) -> #(List(ComponentAttr), String) {
  let input = skip_whitespace(input)
  case input {
    ">" <> _ -> #(list.reverse(acc), input)
    "/>" <> _ -> #(list.reverse(acc), input)
    "" -> #(list.reverse(acc), input)
    ":" <> rest -> {
      case parse_expr_attr(rest) {
        Ok(#(attr, remaining)) ->
          parse_component_attrs(remaining, [attr, ..acc], line)
        Error(_) -> #(list.reverse(acc), input)
      }
    }
    "l-if=" <> rest -> {
      case parse_lm_condition_attr(rest) {
        Ok(#(condition, remaining)) ->
          parse_component_attrs(remaining, [LmIf(condition, line), ..acc], line)
        Error(_) -> #(list.reverse(acc), input)
      }
    }
    "l-else-if=" <> rest -> {
      case parse_lm_condition_attr(rest) {
        Ok(#(condition, remaining)) ->
          parse_component_attrs(
            remaining,
            [LmElseIf(condition, line), ..acc],
            line,
          )
        Error(_) -> #(list.reverse(acc), input)
      }
    }
    "l-else" <> rest -> {
      let rest = skip_whitespace(rest)
      parse_component_attrs(rest, [LmElse, ..acc], line)
    }
    "l-for=" <> rest -> {
      case parse_lm_for_attr(rest) {
        Ok(#(collection, items, loop_var, remaining)) ->
          parse_component_attrs(
            remaining,
            [LmFor(collection, items, loop_var, line), ..acc],
            line,
          )
        Error(_) -> #(list.reverse(acc), input)
      }
    }
    "l-on:" <> rest -> {
      case parse_lm_on_attr(rest, line) {
        Ok(#(attr, remaining)) ->
          parse_component_attrs(remaining, [attr, ..acc], line)
        Error(_) -> #(list.reverse(acc), input)
      }
    }
    "l-model=" <> rest -> {
      case parse_lm_model_attr(rest, line) {
        Ok(#(attr, remaining)) ->
          parse_component_attrs(remaining, [attr, ..acc], line)
        Error(_) -> #(list.reverse(acc), input)
      }
    }
    _ -> {
      case parse_string_or_bool_attr(input) {
        Ok(#(attr, remaining)) ->
          parse_component_attrs(remaining, [attr, ..acc], line)
        Error(_) -> #(list.reverse(acc), input)
      }
    }
  }
}

/// Parses an expression attribute (starts with :). Extracts
/// the attribute name and expression value from quoted
/// string, supporting both single and double quotes.
/// Special-cases :class and :style to return ClassAttr/StyleAttr.
///
fn parse_expr_attr(input: String) -> Result(#(ComponentAttr, String), Nil) {
  let #(name, rest) = take_attr_name(input, "")
  case name, rest {
    "", _ -> Error(Nil)
    // Double-quoted value
    _, "=\"" <> remaining -> {
      case string.split_once(remaining, "\"") {
        Ok(#(value, rest2)) -> Ok(#(make_expr_attr(name, value), rest2))
        Error(_) -> Error(Nil)
      }
    }
    // Single-quoted value
    _, "='" <> remaining -> {
      case string.split_once(remaining, "'") {
        Ok(#(value, rest2)) -> Ok(#(make_expr_attr(name, value), rest2))
        Error(_) -> Error(Nil)
      }
    }
    _, _ -> Error(Nil)
  }
}

/// Creates an expression attribute, converting single quotes
/// to double quotes so users can write :class="[#('x', True)]"
/// which produces valid Gleam code with double-quoted strings.
///
fn make_expr_attr(name: String, value: String) -> ComponentAttr {
  let normalized_value = normalize_quotes(value)
  case name {
    "class" -> ClassAttr(normalized_value)
    "style" -> StyleAttr(normalized_value)
    _ -> ExprAttr(name, normalized_value)
  }
}

/// Converts single quotes to double quotes in expression values.
/// This allows users to write :class="[#('active', True)]" with
/// single quotes inside double quotes, producing valid Gleam.
///
fn normalize_quotes(value: String) -> String {
  value
  |> string.replace("'", "\"")
}

/// Parses a string or boolean attribute. If an equals sign
/// and quoted value follow the name, returns StringAttr.
/// Otherwise returns BoolAttr with just the name.
///
fn parse_string_or_bool_attr(
  input: String,
) -> Result(#(ComponentAttr, String), Nil) {
  let #(name, rest) = take_attr_name(input, "")
  case name, rest {
    "", _ -> Error(Nil)
    // Double-quoted value
    _, "=\"" <> remaining -> {
      case string.split_once(remaining, "\"") {
        Ok(#(value, rest2)) -> Ok(#(StringAttr(name, value), rest2))
        Error(_) -> Error(Nil)
      }
    }
    // Single-quoted value
    _, "='" <> remaining -> {
      case string.split_once(remaining, "'") {
        Ok(#(value, rest2)) -> Ok(#(StringAttr(name, value), rest2))
        Error(_) -> Error(Nil)
      }
    }
    _, _ -> Ok(#(BoolAttr(name), rest))
  }
}

/// Extracts an attribute name from input. Reads characters
/// until whitespace, equals, closing bracket, or slash is
/// found, returning name and remaining input.
///
fn take_attr_name(input: String, acc: String) -> #(String, String) {
  case string.pop_grapheme(input) {
    Ok(#(c, rest)) -> {
      case c {
        " " | "\t" | "\n" | "=" | ">" | "/" -> #(acc, input)
        _ -> take_attr_name(rest, acc <> c)
      }
    }
    Error(_) -> #(acc, input)
  }
}

/// Skips leading whitespace characters from the input.
/// Consumes spaces, tabs, newlines, and carriage returns,
/// returning the remaining non-whitespace content.
///
fn skip_whitespace(input: String) -> String {
  case input {
    " " <> rest -> skip_whitespace(rest)
    "\t" <> rest -> skip_whitespace(rest)
    "\n" <> rest -> skip_whitespace(rest)
    "\r" <> rest -> skip_whitespace(rest)
    _ -> input
  }
}

/// Parses a simple directive without arguments. Verifies
/// the directive isn't part of a longer word, then emits
/// the token and continues tokenization.
///
fn parse_simple_directive(
  rest: String,
  directive: String,
  token: Token,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  let directive_len = string.length(directive)
  case rest {
    "" ->
      do_tokenize(
        "",
        position + directive_len,
        line,
        [token, ..tokens],
        tag_stack,
      )
    _ -> {
      case string.first(rest) {
        Ok(c) -> {
          case string_utils.is_alphanumeric(c) {
            True ->
              consume_text(directive <> rest, position, line, tokens, tag_stack)
            False ->
              do_tokenize(
                rest,
                position + directive_len,
                line,
                [token, ..tokens],
                tag_stack,
              )
          }
        }
        _ ->
          do_tokenize(
            rest,
            position + directive_len,
            line,
            [token, ..tokens],
            tag_stack,
          )
      }
    }
  }
}

/// Parses an @import directive. Extracts the content between
/// parentheses and emits an ImportDirective token.
/// Example: @import(app/models/user.{type User})
///
fn parse_import_directive(
  input: String,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  case find_matching_paren(input, 0, "") {
    Ok(#(import_str, rest)) -> {
      let import_str = string.trim(import_str)
      case import_str {
        "" -> Error(InvalidImportDirective("empty import", line))
        _ -> {
          // @import( + content + )
          let len = 8 + string.length(import_str) + 1
          let new_line = line + count_newlines(import_str)
          do_tokenize(
            rest,
            position + len,
            new_line,
            [ImportDirective(import_str, line), ..tokens],
            tag_stack,
          )
        }
      }
    }
    Error(_) -> Error(UnterminatedImportDirective(line))
  }
}

/// Parses a @props directive. Extracts prop name:type pairs
/// from the parentheses content.
/// Example: @props(name: String, items: List(User))
///
fn parse_props_directive(
  input: String,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  case find_matching_paren(input, 0, "") {
    Ok(#(props_str, rest)) -> {
      case parse_props_content(props_str) {
        Ok(props) -> {
          // @props( + content + )
          let len = 7 + string.length(props_str) + 1
          let new_line = line + count_newlines(props_str)
          do_tokenize(
            rest,
            position + len,
            new_line,
            [PropsDirective(props, line), ..tokens],
            tag_stack,
          )
        }
        Error(reason) -> Error(InvalidPropsDirective(reason, line))
      }
    }
    Error(_) -> Error(UnterminatedPropsDirective(line))
  }
}

/// Finds the matching closing parenthesis, handling nested parens.
/// Returns the content inside and the remaining input after ')'.
///
fn find_matching_paren(
  input: String,
  depth: Int,
  acc: String,
) -> Result(#(String, String), Nil) {
  case string.pop_grapheme(input) {
    Error(_) -> Error(Nil)
    Ok(#(char, rest)) -> {
      case char {
        "(" -> find_matching_paren(rest, depth + 1, acc <> char)
        ")" -> {
          case depth {
            0 -> Ok(#(acc, rest))
            _ -> find_matching_paren(rest, depth - 1, acc <> char)
          }
        }
        _ -> find_matching_paren(rest, depth, acc <> char)
      }
    }
  }
}

/// Parses the content of @props() into a list of (name, type) pairs.
/// Handles nested types like List(#(String, Int)).
///
fn parse_props_content(
  content: String,
) -> Result(List(#(String, String)), String) {
  let content = string.trim(content)
  case content {
    "" -> Ok([])
    _ -> {
      let parts = split_props_at_commas(content)
      parse_prop_parts(parts, [])
    }
  }
}

/// Splits props on commas at depth 0, handling nested parens.
///
fn split_props_at_commas(input: String) -> List(String) {
  split_props_helper(input, 0, "", [])
}

fn split_props_helper(
  input: String,
  depth: Int,
  current: String,
  acc: List(String),
) -> List(String) {
  case string.pop_grapheme(input) {
    Error(_) -> {
      case string.trim(current) {
        "" -> list.reverse(acc)
        trimmed -> list.reverse([trimmed, ..acc])
      }
    }
    Ok(#(char, rest)) -> {
      case char {
        "(" -> split_props_helper(rest, depth + 1, current <> char, acc)
        ")" -> split_props_helper(rest, depth - 1, current <> char, acc)
        "," if depth == 0 -> {
          let trimmed = string.trim(current)
          split_props_helper(rest, 0, "", [trimmed, ..acc])
        }
        _ -> split_props_helper(rest, depth, current <> char, acc)
      }
    }
  }
}

/// Parses individual prop parts (name: Type) into tuples.
///
fn parse_prop_parts(
  parts: List(String),
  acc: List(#(String, String)),
) -> Result(List(#(String, String)), String) {
  case parts {
    [] -> Ok(list.reverse(acc))
    [part, ..rest] -> {
      case parse_single_prop(part) {
        Ok(prop) -> parse_prop_parts(rest, [prop, ..acc])
        Error(e) -> Error(e)
      }
    }
  }
}

/// Parses a single prop "name: Type" into a tuple.
///
fn parse_single_prop(part: String) -> Result(#(String, String), String) {
  case string.split_once(part, ":") {
    Error(_) ->
      Error("Expected ':' between prop name and type in '" <> part <> "'")
    Ok(#(name, type_str)) -> {
      let name = string.trim(name)
      let type_str = string.trim(type_str)
      case name, type_str {
        "", _ -> Error("Missing prop name before ':'")
        _, "" -> Error("Missing type after ':' for prop '" <> name <> "'")
        _, _ -> Ok(#(name, type_str))
      }
    }
  }
}

/// Parses a <slot> element. Handles:
/// - <slot /> → Slot(None)
/// - <slot name="x" /> → Slot(Some("x"))
/// - <slot> → SlotDef(None)
/// - <slot name="x"> → SlotDef(Some("x"))
///
fn parse_slot_element(
  input: String,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  // Input starts after "<slot", could be " ", ">", or "/"
  case input {
    // Self-closing without attributes: <slot/>
    "/>" <> rest -> {
      let len = 7
      // position + len for "<slot/>"
      do_tokenize(rest, position + len, line, [Slot(None), ..tokens], tag_stack)
    }
    // Opening without attributes: <slot>
    ">" <> rest -> {
      let len = 6
      // position + len for "<slot>"
      do_tokenize(
        rest,
        position + len,
        line,
        [SlotDef(None), ..tokens],
        tag_stack,
      )
    }
    // Has attributes (space after <slot)
    " " <> rest ->
      parse_slot_with_attrs(rest, position, line, tokens, tag_stack)
    // Invalid
    _ -> Error(UnterminatedComponent(position))
  }
}

/// Parses slot attributes and determines if self-closing or
/// opening. Extracts the name attribute if present and emits
/// the appropriate Slot or SlotDef token.
///
fn parse_slot_with_attrs(
  input: String,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  // Find the end of the tag (either /> or >)
  case find_slot_tag_end(input) {
    Ok(#(attrs_part, self_closing, rest)) -> {
      let name = extract_slot_name(attrs_part)
      // <slot + space + attrs + ("/> " or ">")
      let len =
        5
        + 1
        + string.length(attrs_part)
        + case self_closing {
          True -> 2
          False -> 1
        }
      case self_closing {
        True ->
          do_tokenize(
            rest,
            position + len,
            line,
            [Slot(name), ..tokens],
            tag_stack,
          )
        False ->
          do_tokenize(
            rest,
            position + len,
            line,
            [SlotDef(name), ..tokens],
            tag_stack,
          )
      }
    }
    Error(_) -> Error(UnterminatedComponent(position))
  }
}

/// Finds the end of a slot tag. Returns the attribute string,
/// whether it's self-closing (/> vs >), and the remaining
/// input after the tag.
///
fn find_slot_tag_end(input: String) -> Result(#(String, Bool, String), Nil) {
  find_slot_tag_end_loop(input, "")
}

/// Recursive helper for finding slot tag end. Accumulates
/// characters until /> or > is found, tracking whether the
/// tag is self-closing.
///
fn find_slot_tag_end_loop(
  input: String,
  acc: String,
) -> Result(#(String, Bool, String), Nil) {
  case input {
    "/>" <> rest -> Ok(#(acc, True, rest))
    ">" <> rest -> Ok(#(acc, False, rest))
    "" -> Error(Nil)
    _ -> {
      case string.pop_grapheme(input) {
        Ok(#(c, rest)) -> find_slot_tag_end_loop(rest, acc <> c)
        Error(_) -> Error(Nil)
      }
    }
  }
}

/// Extracts the name value from slot tag attributes. Looks
/// for the name="value" or name='value' pattern and returns
/// the extracted name or None if not present.
///
fn extract_slot_name(attrs: String) -> Option(String) {
  // Look for name="value" pattern
  case string.split_once(attrs, "name=\"") {
    Ok(#(_, rest)) -> {
      case string.split_once(rest, "\"") {
        Ok(#(name, _)) -> Some(name)
        Error(_) -> None
      }
    }
    Error(_) -> {
      // Try single quotes
      case string.split_once(attrs, "name='") {
        Ok(#(_, rest)) -> {
          case string.split_once(rest, "'") {
            Ok(#(name, _)) -> Some(name)
            Error(_) -> None
          }
        }
        Error(_) -> None
      }
    }
  }
}

/// Consumes plain text until a special sequence is found.
/// Accumulates characters into a Text token, stopping when
/// template syntax markers are encountered. Merges with
/// previous Text token if present.
///
fn consume_text(
  input: String,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  let #(text, rest) = take_until_special(input, "")

  case text {
    "" -> {
      case string.pop_grapheme(input) {
        Ok(#(char, remaining)) -> {
          let new_line = case char {
            "\n" -> line + 1
            _ -> line
          }
          // Merge with previous text token if exists
          case tokens {
            [Text(prev), ..rest_tokens] ->
              do_tokenize(
                remaining,
                position + 1,
                new_line,
                [Text(prev <> char), ..rest_tokens],
                tag_stack,
              )
            _ ->
              do_tokenize(
                remaining,
                position + 1,
                new_line,
                [Text(char), ..tokens],
                tag_stack,
              )
          }
        }
        Error(_) -> Ok(list.reverse(tokens))
      }
    }
    _ -> {
      let new_pos = position + string.length(text)
      let new_line = line + count_newlines(text)
      // Merge with previous text token if exists
      case tokens {
        [Text(prev), ..rest_tokens] ->
          do_tokenize(
            rest,
            new_pos,
            new_line,
            [Text(prev <> text), ..rest_tokens],
            tag_stack,
          )
        _ ->
          do_tokenize(
            rest,
            new_pos,
            new_line,
            [Text(text), ..tokens],
            tag_stack,
          )
      }
    }
  }
}

/// Scans input until a template syntax marker is found.
/// Returns accumulated text and remaining input when a
/// variable, directive, or component tag is encountered.
///
fn take_until_special(input: String, accumulated: String) -> #(String, String) {
  case input {
    "" -> #(accumulated, "")
    "{{{" <> _ -> #(accumulated, input)
    "{{" <> _ -> #(accumulated, input)
    "<!--" <> _ -> #(accumulated, input)
    "@import(" <> _ -> #(accumulated, input)
    "@props(" <> _ -> #(accumulated, input)
    "@attributes" <> _ -> #(accumulated, input)
    "</slot>" <> _ -> #(accumulated, input)
    "<slot" <> _ -> #(accumulated, input)
    "<x-" <> _ -> #(accumulated, input)
    "</x-" <> _ -> #(accumulated, input)
    "</" <> _ -> #(accumulated, input)
    "<" <> _ -> {
      // Check if this might be an HTML element (not a comparison operator)
      case is_potential_element_start(input) {
        True -> #(accumulated, input)
        False -> {
          case string.pop_grapheme(input) {
            Ok(#(char, rest)) -> take_until_special(rest, accumulated <> char)
            Error(_) -> #(accumulated, "")
          }
        }
      }
    }
    _ -> {
      case string.pop_grapheme(input) {
        Ok(#(char, rest)) -> take_until_special(rest, accumulated <> char)
        Error(_) -> #(accumulated, "")
      }
    }
  }
}

/// Checks if input starting with "<" is a potential HTML
/// element. Returns true if followed by a letter, indicating
/// a tag name rather than a comparison operator.
///
fn is_potential_element_start(input: String) -> Bool {
  case string.drop_start(input, 1) |> string.first {
    Ok(c) -> is_letter(c)
    Error(_) -> False
  }
}

/// Checks if a character is a letter. Returns true for
/// uppercase A-Z and lowercase a-z characters used to
/// identify the start of HTML tag names.
///
fn is_letter(char: String) -> Bool {
  case char {
    "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" ->
      True
    "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" ->
      True
    "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" ->
      True
    "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" ->
      True
    _ -> False
  }
}
