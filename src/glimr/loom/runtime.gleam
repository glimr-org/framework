//// Template Runtime
////
//// Provides runtime functions for compiled Loom templates.
//// Handles string concatenation, conditional rendering, loops,
//// HTML escaping, and attribute management.

import dot_env/env
import gleam/int
import gleam/list
import gleam/string
import houdini

// ------------------------------------------------------------- Public Types

/// Represents an HTML attribute. Can be a standard name-value
/// pair or a boolean attribute that renders only when the
/// condition is true.
///
pub type Attribute {
  Attribute(name: String, value: String)
  BoolAttribute(name: String, condition: Bool)
}

/// Provides loop metadata for @each iterations. Contains
/// index, count, and boolean flags for first/last/even/odd
/// to enable conditional styling in templates.
///
pub type Loop {
  Loop(
    index: Int,
    iteration: Int,
    first: Bool,
    last: Bool,
    even: Bool,
    odd: Bool,
    count: Int,
    remaining: Int,
  )
}

// ------------------------------------------------------------- Public Functions

/// Appends a value to the accumulator string. Used by compiled
/// templates to build up the output HTML through successive
/// string concatenation.
///
pub fn append(acc: String, value: String) -> String {
  acc <> value
}

/// Conditionally appends content based on a boolean. When true,
/// calls the render function to append content. When false,
/// returns the accumulator unchanged.
///
pub fn append_if(
  acc: String,
  condition: Bool,
  render_fn: fn(String) -> String,
) -> String {
  case condition {
    True -> render_fn(acc)
    False -> acc
  }
}

/// Appends content for each item in a list. Folds over the
/// items, calling the render function for each one to build
/// up repeated template sections.
///
pub fn append_each(
  acc: String,
  items: List(item),
  render_fn: fn(String, item) -> String,
) -> String {
  list.fold(items, acc, render_fn)
}

/// Appends content for each item with loop metadata. Provides
/// a Loop record containing index, count, and position flags
/// for conditional rendering based on iteration state.
///
pub fn append_each_with_loop(
  acc: String,
  items: List(item),
  render_fn: fn(String, item, Loop) -> String,
) -> String {
  let count = list.length(items)
  list.index_fold(items, acc, fn(acc, item, index) {
    let loop =
      Loop(
        index: index,
        iteration: index + 1,
        first: index == 0,
        last: index == count - 1,
        even: index % 2 == 0,
        odd: index % 2 != 0,
        count: count,
        remaining: count - index - 1,
      )
    render_fn(acc, item, loop)
  })
}

/// Escapes HTML special characters to prevent XSS attacks.
/// Converts &, <, >, ", and ' to their HTML entity
/// equivalents for safe rendering.
///
pub fn escape(value: String) -> String {
  houdini.escape(value)
}

/// Converts any value to a string for display. Handles all types
/// automatically - strings, bools, ints, floats, and custom types
/// are all converted via string.inspect().
///
pub fn to_string(value: a) -> String {
  let s = string.inspect(value)
  // string.inspect wraps strings in quotes - strip them
  case string.starts_with(s, "\"") && string.ends_with(s, "\"") {
    True -> string.slice(s, 1, string.length(s) - 2)
    False -> s
  }
}

/// Converts any value to a string and escapes it for HTML display.
/// Handles all types automatically - strings, bools, ints, floats,
/// and custom types are all converted via string.inspect().
///
pub fn display(value: a) -> String {
  to_string(value) |> escape
}

/// Builds a class string from conditional class entries. Takes
/// a list of class name and boolean pairs, including only the
/// classes where the condition is true.
///
pub fn build_classes(items: List(#(String, Bool))) -> String {
  items
  |> list.filter_map(fn(item) {
    let #(class_name, condition) = item
    case condition {
      True -> Ok(class_name)
      False -> Error(Nil)
    }
  })
  |> string.join(" ")
}

/// Wraps a static class string as an always-true conditional.
/// Use in :class lists to include static classes alongside
/// conditional ones: :class="[class('btn'), #('active', is_active)]"
///
pub fn class(value: String) -> #(String, Bool) {
  #(value, True)
}

/// Builds a style string from conditional style entries. Takes
/// a list of style value and boolean pairs, including only the
/// styles where the condition is true.
///
pub fn build_styles(items: List(#(String, Bool))) -> String {
  items
  |> list.filter_map(fn(item) {
    let #(style_value, condition) = item
    case condition {
      True -> Ok(style_value)
      False -> Error(Nil)
    }
  })
  |> string.join(" ")
}

/// Wraps a static style string as an always-true conditional.
/// Use in :style lists to include static styles alongside
/// conditional ones: :style="[style('color: red'), #('display: none', hide)]"
///
pub fn style(value: String) -> #(String, Bool) {
  #(value, True)
}

/// Renders a list of attributes to an HTML attribute string.
/// Escapes values and handles boolean attributes that only
/// render their name when the condition is true.
///
pub fn render_attributes(attrs: List(Attribute)) -> String {
  attrs
  |> list.filter_map(fn(attr) {
    case attr {
      Attribute(name, value) -> Ok(name <> "=\"" <> escape(value) <> "\"")
      BoolAttribute(name, condition) -> {
        case condition {
          True -> Ok(name)
          False -> Error(Nil)
        }
      }
    }
  })
  |> string.join(" ")
}

/// Merges extra attributes into a base list. Class and style
/// attributes are concatenated, while other attributes override
/// existing values with the same name.
///
pub fn merge_attributes(
  base: List(Attribute),
  extra: List(Attribute),
) -> List(Attribute) {
  list.fold(extra, base, fn(acc, extra_attr) {
    case extra_attr {
      Attribute("class", value) -> merge_class_attribute(acc, value)
      Attribute("style", value) -> merge_style_attribute(acc, value)
      _ -> override_attribute(acc, extra_attr)
    }
  })
}

// ------------------------------------------------------------- Private Functions

/// Merges an extra class value into the attribute list. If a
/// class attribute exists, appends the new classes with a
/// space separator. Otherwise adds a new class attribute.
///
fn merge_class_attribute(
  attrs: List(Attribute),
  extra_class: String,
) -> List(Attribute) {
  let has_class = list.any(attrs, fn(a) { attr_name(a) == "class" })
  case has_class {
    True ->
      list.map(attrs, fn(a) {
        case a {
          Attribute("class", value) ->
            Attribute("class", value <> " " <> extra_class)
          _ -> a
        }
      })
    False -> list.append(attrs, [Attribute("class", extra_class)])
  }
}

/// Merges an extra style value into the attribute list. If a
/// style attribute exists, appends the new styles ensuring
/// proper semicolon separation. Otherwise adds new attribute.
///
fn merge_style_attribute(
  attrs: List(Attribute),
  extra_style: String,
) -> List(Attribute) {
  let has_style = list.any(attrs, fn(a) { attr_name(a) == "style" })
  case has_style {
    True ->
      list.map(attrs, fn(a) {
        case a {
          Attribute("style", value) -> {
            let base = string.trim_end(value)
            let base = case string.ends_with(base, ";") {
              True -> base
              False -> base <> ";"
            }
            Attribute("style", base <> " " <> extra_style)
          }
          _ -> a
        }
      })
    False -> list.append(attrs, [Attribute("style", extra_style)])
  }
}

/// Replaces an attribute with a new value if it exists, or
/// appends the new attribute if not present. Used for all
/// attributes except class and style.
///
fn override_attribute(
  attrs: List(Attribute),
  new_attr: Attribute,
) -> List(Attribute) {
  let new_name = attr_name(new_attr)
  let has_attr = list.any(attrs, fn(a) { attr_name(a) == new_name })
  case has_attr {
    True ->
      list.map(attrs, fn(a) {
        case attr_name(a) == new_name {
          True -> new_attr
          False -> a
        }
      })
    False -> list.append(attrs, [new_attr])
  }
}

/// Extracts the name from an attribute regardless of variant.
/// Works with both standard Attribute and BoolAttribute types
/// for consistent name comparison.
///
fn attr_name(attr: Attribute) -> String {
  case attr {
    Attribute(name, _) -> name
    BoolAttribute(name, _) -> name
  }
}

/// Returns the WebSocket URL for Loom Live connections.
/// In dev mode (when DEV_PROXY_PORT env var is set), returns an
/// explicit URL pointing to the app port to bypass the dev proxy.
/// In production, returns a relative path that uses the page host.
///
pub fn live_ws_url() -> String {
  // Check if we're in dev mode by looking for DEV_PROXY_PORT
  case env.get_int("DEV_PROXY_PORT") {
    Ok(port) -> {
      // Dev mode: connect directly to app port, bypassing proxy
      "ws://localhost:" <> int.to_string(port) <> "/loom/ws"
    }
    Error(_) -> {
      // Production: use relative path (same host as page)
      "/loom/ws"
    }
  }
}

/// Injects live template wrapper into HTML by finding the body tag.
/// Adds the live container div after <body> and closing div + script before </body>.
/// This ensures proper HTML structure when using layout components.
///
pub fn inject_live_wrapper(
  html: String,
  module_name: String,
  props_json: String,
) -> String {
  let ws_url = live_ws_url()
  let open_div =
    "<div data-l-live=\""
    <> module_name
    <> "\" data-l-ws=\""
    <> ws_url
    <> "\" data-l-props='"
    <> props_json
    <> "'>"
  let close_div = "</div><script src=\"/loom.js\"></script>"

  // Find and inject after <body...>
  let html = inject_after_body_open(html, open_div)
  // Find and inject before </body>
  let html = string.replace(html, "</body>", close_div <> "</body>")

  html
}

/// Injects content after the opening body tag, handling body attributes.
///
fn inject_after_body_open(html: String, content: String) -> String {
  case string.split_once(html, "<body") {
    Ok(#(before, after)) -> {
      // Find the closing > of the body tag
      case string.split_once(after, ">") {
        Ok(#(body_attrs, rest)) ->
          before <> "<body" <> body_attrs <> ">" <> content <> rest
        Error(_) -> html
      }
    }
    Error(_) -> {
      // No body tag found - wrap the whole thing (fallback)
      content <> html <> "</div><script src=\"/loom.js\"></script>"
    }
  }
}
