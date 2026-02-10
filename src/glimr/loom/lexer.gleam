//// Template Lexer
////
//// Converts template source text into a stream of tokens.
//// Recognizes variables, directives, components, and plain
//// text for the parser to process.
////

import gleam/bool
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import glimr/utils/string as string_utils

// ------------------------------------------------------------- Public Types

/// The parser needs a structured representation of template
/// syntax to build the AST. Each variant captures a distinct
/// construct so the parser can pattern match on it directly.
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

/// Component and element tags support several attribute
/// flavors that need different code generation strategies.
/// Separating them here lets the generator handle each
/// without re-parsing attribute strings.
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

/// Surfacing specific error variants allows the compiler
/// to produce actionable messages with position info,
/// so users can quickly locate malformed template syntax.
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

/// Entry point for the lexer. Wraps the recursive loop so
/// callers don't need to supply internal state like position,
/// line number, or the tag stack.
///
pub fn tokenize(input: String) -> Result(List(Token), LexerError) {
  // tag_stack tracks open HTML tags in LIFO order. Each entry is
  // (tag_name, is_dynamic) where is_dynamic=True means we emitted an
  // Element token, False means it was plain HTML emitted as text.
  // This ensures closing tags match the correct opening tag.
  do_tokenize(input, 0, 1, [], [])
}

// ------------------------------------------------------------- Private Functions

/// Recursive dispatch loop. Order of pattern matches matters
/// because longer prefixes (e.g. "{{{") must be checked
/// before shorter ones ("{{") to avoid mis-tokenizing.
/// tag_stack tracks open HTML tags so closing tags can
/// determine whether to emit ElementEnd or plain text.
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
      parse_variable(
        rest,
        "{{{",
        "}}}",
        RawVariable,
        position,
        line,
        tokens,
        tag_stack,
      )
    }

    "{{" <> rest -> {
      parse_variable(
        rest,
        "{{",
        "}}",
        Variable,
        position,
        line,
        tokens,
        tag_stack,
      )
    }

    "<!--" <> rest -> parse_comment(rest, position, line, tokens, tag_stack)

    "@import(" <> rest -> {
      parse_import_directive(rest, position, line, tokens, tag_stack)
    }

    "@props(" <> rest -> {
      parse_props_directive(rest, position, line, tokens, tag_stack)
    }

    "@attributes" <> rest -> {
      parse_simple_directive(
        rest,
        "@attributes",
        Attributes,
        position,
        line,
        tokens,
        tag_stack,
      )
    }

    "<slot" <> rest -> {
      parse_slot_element(rest, position, line, tokens, tag_stack)
    }

    "</slot>" <> rest -> {
      do_tokenize(rest, position + 7, line, [SlotDefEnd, ..tokens], tag_stack)
    }

    "<x-" <> rest -> parse_component(rest, position, line, tokens, tag_stack)

    "</x-" <> rest -> {
      parse_component_end(rest, position, line, tokens, tag_stack)
    }

    "</" <> rest -> parse_element_end(rest, position, line, tokens, tag_stack)

    "<" <> rest -> {
      try_parse_element(rest, position, line, tokens, tag_stack)
    }

    _ -> consume_text(input, position, line, tokens, tag_stack)
  }
}

/// Adjacent text characters should form a single Text token
/// rather than many small ones, both for parser simplicity
/// and output correctness. This coalesces them in O(1).
///
fn append_text(tokens: List(Token), text: String) -> List(Token) {
  case tokens {
    [Text(prev), ..rest] -> [Text(prev <> text), ..rest]
    _ -> [Text(text), ..tokens]
  }
}

/// Both {{ }} and {{{ }}} follow the same parsing logic,
/// differing only in delimiters and token type. Sharing
/// one implementation prevents the two paths from diverging.
///
fn parse_variable(
  rest: String,
  open: String,
  close: String,
  to_token: fn(String, Int) -> Token,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  use #(expr, remaining) <- result.try(
    string.split_once(rest, close)
    |> result.replace_error(UnterminatedExpression(position)),
  )

  let expr = string.trim(expr)

  // Throw an error if the expression is empty.
  use <- bool.lazy_guard(expr == "", fn() { Error(EmptyExpression(position)) })

  let open_close_len = string.length(open) + string.length(close)
  let new_pos = position + open_close_len + string.length(expr)
  let new_line = line + count_newlines(open <> expr <> close)

  do_tokenize(
    remaining,
    new_pos,
    new_line,
    [to_token(expr, line), ..tokens],
    tag_stack,
  )
}

/// HTML comments must pass through untouched to preserve
/// developer annotations in the rendered output. Unclosed
/// comments are treated as text to match browser behaviour.
///
fn parse_comment(
  rest: String,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  case string.split_once(rest, "-->") {
    Ok(#(comment_content, remaining)) -> {
      let full_comment = "<!--" <> comment_content <> "-->"
      let new_pos = position + string.length(full_comment)
      let new_line = line + count_newlines(full_comment)

      do_tokenize(
        remaining,
        new_pos,
        new_line,
        append_text(tokens, full_comment),
        tag_stack,
      )
    }
    // Unclosed comment - just treat "<!--" as text
    Error(_) -> {
      do_tokenize(
        rest,
        position + 4,
        line,
        append_text(tokens, "<!--"),
        tag_stack,
      )
    }
  }
}

/// Line tracking must stay accurate across multi-line
/// constructs so error messages point to the right line.
/// Called after consuming each token to advance the count.
///
fn count_newlines(string: String) -> Int {
  string
  |> string.to_graphemes
  |> list.filter(fn(c) { c == "\n" })
  |> list.length
}

/// Components use the <x-name> convention to distinguish
/// them from plain HTML. This extracts the full tag so
/// the generator can resolve the component module.
///
fn parse_component(
  input: String,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  use #(name, attrs, self_closing, rest) <- result.try(
    parse_component_tag(input, line)
    |> result.replace_error(UnterminatedComponent(position)),
  )

  let token = Component(name, attrs, self_closing)
  let consumed_len = string.length(input) - string.length(rest)
  let consumed = "<x-" <> string.slice(input, 0, consumed_len)
  let new_line = line + count_newlines(consumed)

  do_tokenize(
    rest,
    position + consumed_len + 3,
    new_line,
    [token, ..tokens],
    tag_stack,
  )
}

/// Closing component tags must be paired with their opener
/// so the generator knows where component content ends.
/// The name is extracted to validate matching pairs later.
///
fn parse_component_end(
  input: String,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  use #(name, rest) <- result.try(
    string.split_once(input, ">")
    |> result.replace_error(UnterminatedComponent(position)),
  )

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

/// Only elements with dynamic attributes (l-*, :class, etc.)
/// need dedicated tokens for code generation. Plain HTML is
/// emitted as text for efficiency, but both push to tag_stack
/// so their closing tags can be matched correctly.
///
fn try_parse_element(
  input: String,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  case parse_element_tag(input, line) {
    Ok(#(tag, attrs, self_closing, rest)) -> {
      let is_dynamic = has_dynamic_attrs(attrs)

      let new_stack = case self_closing {
        True -> tag_stack
        False -> [#(tag, is_dynamic), ..tag_stack]
      }

      case is_dynamic {
        True -> {
          let token = Element(tag, attrs, self_closing)
          let consumed_len = string.length(input) - string.length(rest)
          let consumed = "<" <> string.slice(input, 0, consumed_len)
          let new_line = line + count_newlines(consumed)

          do_tokenize(
            rest,
            position + consumed_len + 1,
            new_line,
            [token, ..tokens],
            new_stack,
          )
        }

        // No l-* attributes - emit "<" as text
        False -> {
          do_tokenize(
            input,
            position + 1,
            line,
            append_text(tokens, "<"),
            new_stack,
          )
        }
      }
    }

    // Not a valid element - emit "<" as text
    Error(_) -> {
      do_tokenize(
        input,
        position + 1,
        line,
        append_text(tokens, "<"),
        tag_stack,
      )
    }
  }
}

/// Closing tags must consult the tag_stack to determine
/// whether the matching opener was dynamic or plain HTML,
/// because each requires a different token in the output.
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

      case pop_matching_tag(tag_stack, tag) {
        // Closing a dynamic Element - emit ElementEnd
        Ok(#(True, new_stack)) -> {
          do_tokenize(
            rest,
            position + len,
            line,
            [ElementEnd(tag), ..tokens],
            new_stack,
          )
        }

        // Closing a plain HTML tag - emit as text
        Ok(#(False, new_stack)) -> {
          do_tokenize(
            rest,
            position + len,
            line,
            append_text(tokens, text),
            new_stack,
          )
        }

        // No matching tag on stack - emit as text
        Error(_) -> {
          do_tokenize(
            rest,
            position + len,
            line,
            append_text(tokens, text),
            tag_stack,
          )
        }
      }
    }

    // Invalid closing tag, treat as text
    Error(_) -> consume_text("</" <> input, position, line, tokens, tag_stack)
  }
}

/// HTML in templates can be malformed (e.g. unclosed tags),
/// so we search the stack rather than just popping the top.
/// This mirrors browser leniency and avoids false errors.
///
fn pop_matching_tag(
  stack: List(#(String, Bool)),
  tag: String,
) -> Result(#(Bool, List(#(String, Bool))), Nil) {
  case stack {
    [] -> Error(Nil)
    [#(t, is_dynamic), ..rest] if t == tag -> Ok(#(is_dynamic, rest))
    [_, ..rest] -> pop_matching_tag(rest, tag)
  }
}

/// Plain HTML elements don't need code generation, only
/// elements with dynamic attributes do. This check gates
/// whether an Element token or plain text is emitted.
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

/// Shared between element and component paths to extract
/// tag structure. Returns enough info for the caller to
/// decide how to tokenize the element.
///
fn parse_element_tag(
  input: String,
  line: Int,
) -> Result(#(String, List(ComponentAttr), Bool, String), Nil) {
  let #(tag, rest) = take_component_name(input, "")

  // Reject empty names and x- prefixed tags (those are components)
  use <- bool.lazy_guard(tag == "" || string.starts_with(tag, "x-"), fn() {
    Error(Nil)
  })

  let #(attrs, rest) = parse_element_attrs(rest, [], line)
  let rest = skip_whitespace(rest)

  case rest {
    "/>" <> remaining -> Ok(#(tag, attrs, True, remaining))
    ">" <> remaining -> Ok(#(tag, attrs, False, remaining))
    _ -> Error(Nil)
  }
}

/// Element attributes are parsed recursively because l-*
/// directives need special handling distinct from regular
/// HTML attributes. Each prefix dispatches to a sub-parser.
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
      case parse_quoted_value(rest) {
        Ok(#(condition, remaining)) ->
          parse_element_attrs(remaining, [LmIf(condition, line), ..acc], line)
        Error(_) -> #(list.reverse(acc), input)
      }
    }

    "l-else-if=" <> rest -> {
      case parse_quoted_value(rest) {
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

/// l-for needs to extract multiple pieces (items, collection,
/// optional loop var) from a single attribute value, so it
/// has its own parser separate from other attributes.
///
fn parse_lm_for_attr(
  input: String,
) -> Result(#(String, List(String), Option(String), String), Nil) {
  use #(value, remaining) <- result.try(parse_quoted_value(input))
  use #(collection, items, loop_var) <- result.try(parse_lm_for_syntax(value))

  Ok(#(collection, items, loop_var, remaining))
}

/// The " in " keyword separates the binding pattern from
/// the collection, following a familiar syntax convention.
/// An optional comma suffix provides access to loop metadata.
///
fn parse_lm_for_syntax(
  value: String,
) -> Result(#(String, List(String), Option(String)), Nil) {
  use #(item_part, collection_part) <- result.try(string.split_once(
    value,
    " in ",
  ))

  let items = parse_item_pattern(string.trim(item_part))
  let #(collection, loop_var) =
    parse_collection_and_loop(string.trim(collection_part))

  Ok(#(collection, items, loop_var))
}

/// Supports both single bindings and tuple destructuring
/// so users can iterate over key-value pairs or plain
/// lists with a unified l-for syntax.
///
fn parse_item_pattern(pattern: String) -> List(String) {
  case string.starts_with(pattern, "(") {
    True ->
      pattern
      |> string.drop_start(1)
      |> string.drop_end(1)
      |> string.split(",")
      |> list.map(string.trim)
    False -> [pattern]
  }
}

/// The optional loop variable after the comma gives
/// templates access to index/count metadata without
/// requiring a separate directive.
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

/// Event handlers need the event name, modifiers, and
/// handler expression separated so the generator can
/// produce the correct LiveView event wiring code.
///
fn parse_lm_on_attr(
  input: String,
  line: Int,
) -> Result(#(ComponentAttr, String), Nil) {
  let #(event_part, rest) = take_until_equals_or_space(input, "")
  use <- bool.lazy_guard(event_part == "", fn() { Error(Nil) })

  case rest {
    "=" <> remaining -> {
      use #(handler, rest2) <- result.try(parse_quoted_value(remaining))
      let #(event, modifiers) = parse_event_and_modifiers(event_part)

      Ok(#(LmOn(event, modifiers, handler, line), rest2))
    }
    _ -> Error(Nil)
  }
}

/// l-model provides two-way binding sugar so the generator
/// can emit both the value attribute and the corresponding
/// event handler from a single directive.
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

/// Attribute names in HTML end at = or whitespace, so we
/// consume until one of those delimiters to isolate the
/// name from its value.
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

/// Dots separate the event name from modifiers, following
/// a convention similar to Vue. Splitting here lets the
/// generator apply modifiers independently.
///
fn parse_event_and_modifiers(input: String) -> #(String, List(String)) {
  case string.split(input, ".") {
    [event, ..modifiers] -> #(event, modifiers)
    [] -> #(input, [])
  }
}

/// Attribute values may use either quote style so users
/// can pick whichever avoids escaping in their Gleam
/// expressions. Both must be supported uniformly.
///
fn parse_quoted_value(input: String) -> Result(#(String, String), Nil) {
  case input {
    "\"" <> rest -> take_until_quote(rest, "\"", "")
    "'" <> rest -> take_until_quote(rest, "'", "")
    _ -> Error(Nil)
  }
}

/// Attribute values can contain escaped quotes, so we must
/// handle backslash sequences to avoid closing the value
/// prematurely on an escaped delimiter.
///
fn take_until_quote(
  input: String,
  quote: String,
  acc: String,
) -> Result(#(String, String), Nil) {
  use #(char, rest) <- result.try(string.pop_grapheme(input))

  case char {
    "\\" -> {
      use #(next, rest2) <- result.try(string.pop_grapheme(rest))
      case next == quote {
        True -> take_until_quote(rest2, quote, acc <> quote)
        False -> take_until_quote(rest2, quote, acc <> "\\" <> next)
      }
    }
    _ -> {
      case char == quote {
        True -> Ok(#(acc, rest))
        False -> take_until_quote(rest, quote, acc <> char)
      }
    }
  }
}

/// Component tags have the same attribute grammar as HTML
/// elements but use the <x-name> prefix. Parsing them
/// separately avoids confusing them with plain HTML.
///
fn parse_component_tag(
  input: String,
  line: Int,
) -> Result(#(String, List(ComponentAttr), Bool, String), Nil) {
  let #(name, rest) = take_component_name(input, "")
  use <- bool.lazy_guard(name == "", fn() { Error(Nil) })

  let #(attrs, rest) = parse_component_attrs(rest, [], line)
  let rest = skip_whitespace(rest)

  case rest {
    "/>" <> remaining -> Ok(#(name, attrs, True, remaining))
    ">" <> remaining -> Ok(#(name, attrs, False, remaining))
    _ -> Error(Nil)
  }
}

/// Tag names end at whitespace, >, or / — the same
/// boundary rules as HTML. Isolating the name here
/// keeps the tag parser focused on attributes.
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

/// Component attributes follow the same dispatch pattern
/// as element attributes. Keeping them separate allows
/// future divergence without breaking either path.
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
      case parse_quoted_value(rest) {
        Ok(#(condition, remaining)) ->
          parse_component_attrs(remaining, [LmIf(condition, line), ..acc], line)
        Error(_) -> #(list.reverse(acc), input)
      }
    }
    "l-else-if=" <> rest -> {
      case parse_quoted_value(rest) {
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

/// Expression attributes (prefixed with :) contain Gleam
/// code rather than string literals. :class and :style
/// get dedicated variants because the generator handles
/// them differently from generic expression bindings.
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

/// Users write single quotes inside double-quoted attribute
/// values (e.g. :class="[#('x', True)]"). Normalizing to
/// double quotes produces valid Gleam string literals.
///
fn make_expr_attr(name: String, value: String) -> ComponentAttr {
  let normalized_value = normalize_quotes(value)
  case name {
    "class" -> ClassAttr(normalized_value)
    "style" -> StyleAttr(normalized_value)
    _ -> ExprAttr(name, normalized_value)
  }
}

/// Gleam only uses double-quoted strings, but HTML attribute
/// values are already double-quoted. Single-to-double quote
/// conversion bridges this syntax mismatch.
///
fn normalize_quotes(value: String) -> String {
  value
  |> string.replace("'", "\"")
}

/// HTML supports both value attributes (name="val") and
/// boolean attributes (disabled). Distinguishing them
/// lets the generator emit the correct HTML output.
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

/// Attribute names end at the same delimiters as tag names,
/// plus the equals sign. Isolating the name allows the
/// caller to determine the attribute flavour from context.
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

/// Whitespace between attributes and around tag content
/// is insignificant. Skipping it here keeps individual
/// parsers from duplicating whitespace handling.
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

/// Argument-less directives like @attributes could be a
/// prefix of a longer word (e.g. @attributesSomething).
/// The alphanumeric check prevents false matches.
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
  // If the next char is alphanumeric, this isn't a real
  // directive (e.g. @attributesSomething) — treat as text
  let is_false_match = case string.first(rest) {
    Ok(c) -> string_utils.is_alphanumeric(c)
    _ -> False
  }

  case is_false_match {
    True -> consume_text(directive <> rest, position, line, tokens, tag_stack)
    False -> {
      do_tokenize(
        rest,
        position + string.length(directive),
        line,
        [token, ..tokens],
        tag_stack,
      )
    }
  }
}

/// @import directives can contain nested parentheses in
/// type signatures (e.g. List(String)), so we use
/// find_matching_paren instead of a simple split.
///
fn parse_import_directive(
  input: String,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  use #(import_str, rest) <- result.try(
    find_matching_paren(input, 0, "")
    |> result.replace_error(UnterminatedImportDirective(line)),
  )

  let import_str = string.trim(import_str)
  use <- bool.lazy_guard(import_str == "", fn() {
    Error(InvalidImportDirective("empty import", line))
  })

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

/// @props also uses nested parentheses for types like
/// List(#(String, Int)), requiring the same balanced
/// paren extraction as @import.
///
fn parse_props_directive(
  input: String,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  use #(props_str, rest) <- result.try(
    find_matching_paren(input, 0, "")
    |> result.replace_error(UnterminatedPropsDirective(line)),
  )

  use props <- result.try(
    parse_props_content(props_str)
    |> result.map_error(InvalidPropsDirective(_, line)),
  )

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

/// Directive arguments can contain nested parens (e.g. type
/// signatures), so a simple split on ")" would break. This
/// tracks depth to find the true closing paren.
///
fn find_matching_paren(
  input: String,
  depth: Int,
  acc: String,
) -> Result(#(String, String), Nil) {
  use #(char, rest) <- result.try(string.pop_grapheme(input))

  case char, depth {
    "(", _ -> find_matching_paren(rest, depth + 1, acc <> char)
    ")", 0 -> Ok(#(acc, rest))
    ")", _ -> find_matching_paren(rest, depth - 1, acc <> char)
    _, _ -> find_matching_paren(rest, depth, acc <> char)
  }
}

/// Props content like "name: String, items: List(#(String, Int))"
/// contains commas inside nested types, so it can't be split
/// naively — it needs depth-aware comma splitting.
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

/// Only commas at paren depth 0 are true prop separators.
/// Commas inside type expressions like #(String, Int)
/// must be preserved as part of the type string.
///
fn split_props_at_commas(input: String) -> List(String) {
  split_props_helper(input, 0, "", [])
}

/// Recursive loop for split_props_at_commas. Tracks paren
/// depth so commas inside nested types are not treated
/// as prop separators.
///
fn split_props_helper(
  input: String,
  depth: Int,
  current: String,
  acc: List(String),
) -> List(String) {
  case string.pop_grapheme(input) {
    Error(_) -> {
      let trimmed = string.trim(current)
      case trimmed {
        "" -> list.reverse(acc)
        _ -> list.reverse([trimmed, ..acc])
      }
    }
    Ok(#(char, rest)) ->
      case char, depth {
        "(", _ -> split_props_helper(rest, depth + 1, current <> char, acc)
        ")", _ -> split_props_helper(rest, depth - 1, current <> char, acc)
        ",", 0 -> split_props_helper(rest, 0, "", [string.trim(current), ..acc])
        _, _ -> split_props_helper(rest, depth, current <> char, acc)
      }
  }
}

/// After splitting on commas, each part must be validated
/// as a well-formed "name: Type" pair. Errors here give
/// users specific messages about malformed props.
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

/// Validates a single prop has both a name and type with
/// a colon separator. Specific error messages help users
/// fix common mistakes like missing types or names.
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

/// Slots can be either insertion points (self-closing) or
/// default content definitions (opening). The parser needs
/// to distinguish these to handle component composition.
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

/// Named slots require attribute parsing to extract the
/// name value. Self-closing vs opening determines whether
/// this is a slot reference (Slot) or definition (SlotDef).
///
fn parse_slot_with_attrs(
  input: String,
  position: Int,
  line: Int,
  tokens: List(Token),
  tag_stack: List(#(String, Bool)),
) -> Result(List(Token), LexerError) {
  use #(attrs_part, self_closing, rest) <- result.try(
    find_slot_tag_end(input)
    |> result.replace_error(UnterminatedComponent(position)),
  )

  let name = extract_slot_name(attrs_part)
  let closing_len = case self_closing {
    True -> 2
    False -> 1
  }

  // <slot + space + attrs + ("/>" or ">")
  let len = 5 + 1 + string.length(attrs_part) + closing_len
  let token = case self_closing {
    True -> Slot(name)
    False -> SlotDef(name)
  }

  do_tokenize(rest, position + len, line, [token, ..tokens], tag_stack)
}

/// Slot tags don't use the full component attribute grammar,
/// so a simpler scan for /> or > suffices to delimit the
/// attribute content.
///
fn find_slot_tag_end(input: String) -> Result(#(String, Bool, String), Nil) {
  find_slot_tag_end_loop(input, "")
}

/// Recursive loop for find_slot_tag_end. Pattern matches
/// on /> and > first so multi-char sequences are checked
/// before consuming individual characters.
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

/// Named slots use a name attribute to target specific
/// insertion points. Both quote styles are supported
/// for consistency with other attribute parsing.
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

/// Everything that isn't a recognized template construct
/// should pass through as plain text. This fallback path
/// coalesces characters until the next special sequence.
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
          do_tokenize(
            remaining,
            position + 1,
            new_line,
            append_text(tokens, char),
            tag_stack,
          )
        }
        Error(_) -> Ok(list.reverse(tokens))
      }
    }
    _ -> {
      let new_pos = position + string.length(text)
      let new_line = line + count_newlines(text)
      do_tokenize(rest, new_pos, new_line, append_text(tokens, text), tag_stack)
    }
  }
}

/// Batch-scans text to avoid calling do_tokenize for every
/// plain character. Stops at any prefix that do_tokenize
/// would handle, keeping the two in sync.
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

/// A bare "<" in template expressions (e.g. x < 10) must
/// not be mistaken for a tag. Checking for a following
/// letter disambiguates tags from comparison operators.
///
fn is_potential_element_start(input: String) -> Bool {
  case string.drop_start(input, 1) |> string.first {
    Ok(c) -> string_utils.is_letter(c)
    Error(_) -> False
  }
}
