//// Template Runtime
////
//// Generated Loom templates compile down to string
//// concatenation expressions, but they need a shared set of 
//// helpers for escaping, conditional rendering, loop iteration, 
//// and attribute management. This module provides those 
//// building blocks so generated code stays minimal and focused 
//// on template structure while the runtime handles the messy 
//// details of safe HTML output.
////

import dot_env/env
import gleam/int
import gleam/list
import gleam/string
import houdini

// ------------------------------------------------------------- Public Types

/// HTML has two fundamentally different attribute types:
/// standard name="value" pairs and boolean attributes (like 
/// disabled, checked) that render only their name when true and 
/// are omitted entirely when false. A sum type lets 
/// render_attributes handle both correctly.
///
pub type Attribute {
  Attribute(name: String, value: String)
  BoolAttribute(name: String, condition: Bool)
}

/// Template authors frequently need to style the first/last
/// items differently, apply zebra striping, or display counts. 
/// Pre-computing all loop metadata into a record lets templates 
/// access these values without manual index arithmetic or 
/// length checks in the template itself.
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

/// Generated templates build HTML via a chain of pipe
/// operations. This function provides a named entry point for 
/// string concatenation that integrates cleanly with Gleam's 
/// pipe syntax, keeping generated code readable and consistent.
///
pub fn append(acc: String, value: String) -> String {
  acc <> value
}

/// l-if directives generate conditional blocks that either
/// render content or skip it entirely. Using a callback for the 
/// true branch lets generated code defer rendering — the 
/// template body only executes when the condition holds, 
/// avoiding unnecessary work.
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

/// l-for loops need to render the same template body for each 
/// item while threading the accumulator through. A fold-based 
/// approach builds the output in a single pass without 
/// allocating intermediate string lists that would need joining 
/// afterward.
///
pub fn append_each(
  acc: String,
  items: List(item),
  render_fn: fn(String, item) -> String,
) -> String {
  list.fold(items, acc, render_fn)
}

/// When a template uses `loop` in its l-for body, it needs
/// metadata like index and first/last flags. Computing the Loop 
/// record once per iteration and passing it to the render 
/// callback avoids repeated list.length calls and keeps the 
/// metadata fresh per item.
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

/// User-provided data rendered into HTML can contain characters 
/// that would be interpreted as markup, enabling XSS attacks. 
/// Escaping through houdini neutralizes these characters so 
/// template output is safe by default without author 
/// intervention.
///
pub fn escape(value: String) -> String {
  houdini.escape(value)
}

/// Template variables can hold any Gleam type, but HTML output 
/// requires strings. Using string.inspect() as a universal 
/// converter means template authors don't need explicit 
/// conversions for Int, Bool, or custom types — the runtime 
/// handles it transparently.
///
pub fn to_string(value: a) -> String {
  let s = string.inspect(value)
  // string.inspect wraps strings in quotes - strip them
  case string.starts_with(s, "\"") && string.ends_with(s, "\"") {
    True -> string.slice(s, 1, string.length(s) - 2)
    False -> s
  }
}

/// The most common template operation: convert a value to a
/// string and escape it. Combining both steps here means
/// generated code for {{ variable }} is a single function call, 
/// keeping the generated output compact while ensuring XSS 
/// safety by default.
///
pub fn display(value: a) -> String {
  to_string(value) |> escape
}

/// Templates often need to toggle CSS classes based on state
/// (e.g., "active" when selected, "disabled" when locked). A 
/// list of #(name, Bool) tuples lets authors express this 
/// declaratively, and this function handles the filtering and 
/// space-joining at render time.
///
pub fn build_classes(items: List(#(String, Bool))) -> String {
  items
  |> list.filter_map(fn(item) {
    case item.1 {
      True -> Ok(item.0)
      False -> Error(Nil)
    }
  })
  |> string.join(" ")
}

/// :class lists expect uniform #(String, Bool) tuples, but
/// static classes that are always present shouldn't need a 
/// redundant True flag. This helper lets authors write
/// class("btn") instead of #("btn", True), keeping the template 
/// syntax clean for the common case.
///
pub fn class(value: String) -> #(String, Bool) {
  #(value, True)
}

/// Same pattern as build_classes but for inline styles.
/// Templates may need to toggle style rules based on state
/// (e.g., "display: none" when hidden). Filtering by boolean 
/// and joining produces a valid style attribute value without 
/// manual string manipulation.
///
pub fn build_styles(items: List(#(String, Bool))) -> String {
  items
  |> list.filter_map(fn(item) {
    case item.1 {
      True -> Ok(item.0)
      False -> Error(Nil)
    }
  })
  |> string.join(" ")
}

/// Mirrors the class() helper for :style lists. Static styles 
/// that are always applied can be written as style("color: red") 
/// instead of #("color: red", True), keeping template syntax 
/// consistent between :class and :style.
///
pub fn style(value: String) -> #(String, Bool) {
  #(value, True)
}

/// Generated code builds attribute lists as runtime values, but 
/// the final HTML needs a flat string. Rendering them here with 
/// proper escaping and boolean attribute handling centralizes 
/// the HTML output rules so generated code doesn't need to know 
/// about escaping or attribute syntax.
///
pub fn render_attributes(attrs: List(Attribute)) -> String {
  attrs
  |> list.filter_map(fn(attr) {
    case attr {
      Attribute(name, value) -> Ok(name <> "=\"" <> escape(value) <> "\"")
      BoolAttribute(name, True) -> Ok(name)
      BoolAttribute(_, False) -> Error(Nil)
    }
  })
  |> string.join(" ")
}

/// Parent-provided attributes must combine with a component's
/// base attributes, but the merge rules differ by attribute
/// type: classes and styles should concatenate (so both parent 
/// and component classes apply), while other attributes should 
/// override (parent wins for id, etc.).
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

/// Dev environments use a proxy (e.g., Vite) that doesn't
/// forward WebSocket connections properly. Detecting the
/// DEV_PROXY_PORT env var lets live templates connect directly 
/// to the app port in dev while using a relative path in 
/// production that works behind any reverse proxy.
///
pub fn live_ws_url() -> String {
  case env.get_int("DEV_PROXY_PORT") {
    // Dev mode: connect directly to app port, bypassing proxy
    Ok(port) -> "ws://localhost:" <> int.to_string(port) <> "/loom/ws"
    // Production: use relative path (same host as page)
    Error(_) -> "/loom/ws"
  }
}

/// Live templates render through layout components that produce 
/// the <html>/<body> structure. The live container div and 
/// script tag must be injected inside the body rather than 
/// wrapping the entire output, otherwise the HTML structure 
/// would be invalid with nested body tags.
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

  // Inject <script defer> before </head> for early fetch
  let html = inject_before_head_close(html)
  // Inject live container after <body...>
  let html = inject_after_body_open(html, open_div)
  // Close the container before </body>
  string.replace(html, "</body>", "</div></body>")
}

// ------------------------------------------------------------- Private Functions

/// Classes from both the component and parent should be visible 
/// in the DOM — dropping either would break styling. Appending 
/// with a space separator preserves both class lists, letting 
/// CSS specificity determine which styles win.
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

/// Like classes, inline styles from both component and parent
/// should be combined rather than overridden. Ensuring a
/// semicolon separator between the existing and new styles
/// prevents CSS parsing errors from concatenated rules like 
/// "color: red" + "display: none" without a delimiter.
///
fn merge_style_attribute(
  attrs: List(Attribute),
  extra_style: String,
) -> List(Attribute) {
  let has_style = list.any(attrs, fn(a) { attr_name(a) == "style" })
  case has_style {
    False -> list.append(attrs, [Attribute("style", extra_style)])
    True -> {
      list.map(attrs, fn(a) {
        case a {
          Attribute("style", value) ->
            Attribute("style", ensure_semicolon(value) <> " " <> extra_style)
          _ -> a
        }
      })
    }
  }
}

/// For non-class/style attributes (id, href, data-*, etc.), the 
/// parent's value should take precedence since the parent has 
/// more context about how the component is used. Replacing 
/// rather than concatenating avoids invalid duplicate values 
/// like two different ids.
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

/// Merge operations need to compare attribute names across both 
/// Attribute and BoolAttribute variants. Extracting the name 
/// uniformly avoids duplicating the pattern match at every 
/// comparison site in the merge functions.
///
fn attr_name(attr: Attribute) -> String {
  case attr {
    Attribute(name, _) | BoolAttribute(name, _) -> name
  }
}

/// Style values may or may not end with a semicolon, but
/// concatenating two style strings without one between them 
/// produces invalid CSS. Normalizing the trailing semicolon 
/// here prevents broken styles when merging component and 
/// parent style attributes.
///
fn ensure_semicolon(value: String) -> String {
  let trimmed = string.trim_end(value)
  case string.ends_with(trimmed, ";") {
    True -> trimmed
    False -> trimmed <> ";"
  }
}

/// Inserting the script tag before </head> lets the browser
/// start fetching loom.js while it still parses the body. The
/// defer attribute ensures execution waits until the DOM is
/// ready, so init() always finds the live containers.
///
fn inject_before_head_close(html: String) -> String {
  let script = "<script defer src=\"/loom.js\"></script>"
  case string.split_once(html, "</head>") {
    Ok(#(before, after)) -> before <> script <> "</head>" <> after
    Error(_) -> html
  }
}

/// The <body> tag may carry attributes (class, onload, etc.)
/// that must be preserved during injection. Splitting on "<body"
/// first and then on ">" ensures the injected content appears
/// after the complete opening tag without clobbering any
/// existing body attributes.
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
      content <> html <> "</div>"
    }
  }
}
