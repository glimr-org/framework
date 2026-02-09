//// Code Generator
////
//// Transforms parsed template ASTs into executable Gleam code.
//// Generates functions that build HTML strings using the
//// runtime helpers for escaping, conditionals, and loops.
////

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
import gleam/string
import glimr/loom/handler_parser
import glimr/loom/lexer
import glimr/loom/parser.{type Node, type Template}

// ------------------------------------------------------------- Public Types

/// Maps component names to their expected data fields. Used
/// to generate default values for missing props when
/// rendering components.
///
pub type ComponentDataMap =
  Dict(String, List(#(String, String)))

/// Information about a component's slot usage. Tracks whether
/// the component uses a default slot and what named slots
/// it accepts.
///
pub type ComponentSlotInfo {
  ComponentSlotInfo(has_default_slot: Bool, named_slots: List(String))
}

/// Maps component names to their slot information. Used when
/// generating component calls to determine which slot
/// arguments to include.
///
pub type ComponentSlotMap =
  Dict(String, ComponentSlotInfo)

/// Maps handler identifiers (event, handler_str, line) to their assigned IDs.
/// Used during code generation to look up the correct handler ID for data attributes.
///
type HandlerLookup =
  Dict(#(String, String, Int), String)

/// The result of code generation. Contains the module name
/// and the complete generated Gleam source code ready to
/// be written to a file.
///
pub type GeneratedCode {
  GeneratedCode(module_name: String, code: String)
}

// ------------------------------------------------------------- Public Functions

/// Generates Gleam code from a parsed template. Takes the AST,
/// module metadata, and type information to produce complete
/// source code with imports and the render function.
///
pub fn generate(
  template: Template,
  module_name: String,
  is_component: Bool,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
) -> GeneratedCode {
  let code =
    generate_module(
      template,
      module_name,
      is_component,
      component_data,
      component_slots,
    )
  GeneratedCode(module_name: module_name, code: code)
}

/// Extracts all named slot references from a template. Walks
/// the AST to find @slot("name") directives and returns a
/// deduplicated list of slot names.
///
pub fn extract_named_slots(template: Template) -> List(String) {
  collect_named_slots(template.nodes, set.new())
  |> set.to_list
}

/// Extracts complete slot information from a component template.
/// Returns whether it has a default slot and the list of
/// named slots it accepts.
///
pub fn extract_slot_info(template: Template) -> ComponentSlotInfo {
  ComponentSlotInfo(
    has_default_slot: has_default_slot(template.nodes),
    named_slots: collect_named_slots(template.nodes, set.new()) |> set.to_list,
  )
}

/// Validates that all variables used in the template are defined.
/// Checks against props, slot variables, and loop variables.
/// Returns Ok(Nil) if valid, Error with message if undefined variable found.
///
pub fn validate_template(
  template: Template,
  source_path: String,
) -> Result(Nil, String) {
  // Get available variables from template props (with types for validation)
  let data_fields = dict.from_list(template.props)

  // Slot variables are always available (slot, slot_xxx)
  let slot_vars =
    collect_named_slots(template.nodes, set.new())
    |> set.to_list
    |> list.map(fn(name) { "slot_" <> to_field_name(name) })
    |> set.from_list
    |> set.insert("slot")

  // Check all nodes for undefined variables
  validate_nodes(template.nodes, data_fields, slot_vars, set.new(), source_path)
}

/// Validates nodes recursively, tracking scoped loop variables.
///
fn validate_nodes(
  nodes: List(Node),
  data_fields: Dict(String, String),
  slot_vars: Set(String),
  loop_vars: Set(String),
  source_path: String,
) -> Result(Nil, String) {
  case nodes {
    [] -> Ok(Nil)
    [node, ..rest] -> {
      case validate_node(node, data_fields, slot_vars, loop_vars, source_path) {
        Error(e) -> Error(e)
        Ok(_) ->
          validate_nodes(rest, data_fields, slot_vars, loop_vars, source_path)
      }
    }
  }
}

/// Validates a single node for undefined variables.
///
fn validate_node(
  node: Node,
  data_fields: Dict(String, String),
  slot_vars: Set(String),
  loop_vars: Set(String),
  source_path: String,
) -> Result(Nil, String) {
  case node {
    parser.VariableNode(expr, line) ->
      validate_expression(
        expr,
        data_fields,
        slot_vars,
        loop_vars,
        source_path,
        line,
      )

    parser.RawVariableNode(expr, line) ->
      validate_expression(
        expr,
        data_fields,
        slot_vars,
        loop_vars,
        source_path,
        line,
      )

    parser.IfNode(branches) ->
      validate_if_branches(
        branches,
        data_fields,
        slot_vars,
        loop_vars,
        source_path,
      )

    parser.EachNode(collection, items, loop_var, body, line) -> {
      // First validate the collection is defined
      case
        validate_expression(
          collection,
          data_fields,
          slot_vars,
          loop_vars,
          source_path,
          line,
        )
      {
        Error(e) -> Error(e)
        Ok(_) -> {
          // Check tuple arity if destructuring
          case
            validate_tuple_arity(
              collection,
              items,
              data_fields,
              source_path,
              line,
            )
          {
            Error(e) -> Error(e)
            Ok(_) -> {
              // Add loop variables to scope for body validation
              let new_loop_vars =
                items
                |> list.fold(loop_vars, fn(acc, item) { set.insert(acc, item) })
              let new_loop_vars = case loop_var {
                Some(lv) -> set.insert(new_loop_vars, lv)
                None -> new_loop_vars
              }
              validate_nodes(
                body,
                data_fields,
                slot_vars,
                new_loop_vars,
                source_path,
              )
            }
          }
        }
      }
    }

    parser.ComponentNode(_, attrs, children) -> {
      case
        validate_attrs(attrs, data_fields, slot_vars, loop_vars, source_path)
      {
        Error(e) -> Error(e)
        Ok(_) ->
          validate_nodes(
            children,
            data_fields,
            slot_vars,
            loop_vars,
            source_path,
          )
      }
    }

    parser.ElementNode(_, attrs, children) -> {
      case
        validate_attrs(attrs, data_fields, slot_vars, loop_vars, source_path)
      {
        Error(e) -> Error(e)
        Ok(_) ->
          validate_nodes(
            children,
            data_fields,
            slot_vars,
            loop_vars,
            source_path,
          )
      }
    }

    parser.SlotNode(_, fallback) ->
      validate_nodes(fallback, data_fields, slot_vars, loop_vars, source_path)

    parser.SlotDefNode(_, children) ->
      validate_nodes(children, data_fields, slot_vars, loop_vars, source_path)

    parser.TextNode(_) | parser.AttributesNode(_) -> Ok(Nil)
  }
}

/// Validates tuple destructuring arity in l-for loops.
/// If the collection's element type is a tuple, checks that the number
/// of destructuring variables matches the tuple arity.
///
fn validate_tuple_arity(
  collection: String,
  items: List(String),
  data_fields: Dict(String, String),
  source_path: String,
  line: Int,
) -> Result(Nil, String) {
  // Only validate if we have multiple destructuring variables (tuple pattern)
  case list.length(items) {
    1 -> Ok(Nil)
    destructure_count -> {
      // Get the root variable name (e.g., "items" from "items" or "user.posts" -> "user")
      let root = case string.split(collection, ".") {
        [first, ..] -> first
        [] -> collection
      }

      // Look up the type in data_fields
      case dict.get(data_fields, root) {
        Error(_) -> Ok(Nil)
        // Can't validate if we don't know the type
        Ok(type_str) -> {
          // For dotted access, we can't easily infer the nested type, skip validation
          case string.contains(collection, ".") {
            True -> Ok(Nil)
            False -> {
              // Try to extract tuple arity from type like "List(#(String, String, String))"
              case extract_tuple_arity_from_list_type(type_str) {
                None -> Ok(Nil)
                // Not a List of tuples, skip validation
                Some(tuple_arity) -> {
                  case destructure_count == tuple_arity {
                    True -> Ok(Nil)
                    False -> {
                      Error(
                        "Tuple destructuring mismatch in "
                        <> source_path
                        <> ":"
                        <> int.to_string(line)
                        <> "\n\nl-for expected "
                        <> int.to_string(tuple_arity)
                        <> " variables but got "
                        <> int.to_string(destructure_count)
                        <> " ("
                        <> string.join(items, ", ")
                        <> ")\nThe type '"
                        <> type_str
                        <> "' has "
                        <> int.to_string(tuple_arity)
                        <> "-element tuples.",
                      )
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Extracts the tuple arity from a List type containing tuples.
/// For example: "List(#(String, String, String))" -> Some(3)
/// Returns None if not a List of tuples.
///
fn extract_tuple_arity_from_list_type(type_str: String) -> Option(Int) {
  let trimmed = string.trim(type_str)

  // Check if it starts with "List("
  case string.starts_with(trimmed, "List(") {
    False -> None
    True -> {
      // Remove "List(" prefix and ")" suffix
      let inner =
        trimmed
        |> string.drop_start(5)
        |> string.drop_end(1)
        |> string.trim

      // Check if inner type is a tuple "#(...)"
      case string.starts_with(inner, "#(") {
        False -> None
        True -> {
          // Extract the contents of the tuple and count elements
          let tuple_inner =
            inner
            |> string.drop_start(2)
            |> string.drop_end(1)

          // Count elements by splitting on commas at depth 0
          Some(count_tuple_elements(tuple_inner))
        }
      }
    }
  }
}

/// Counts elements in a tuple type string by counting commas at depth 0.
/// Handles nested types like "List(String), #(Int, Int)" correctly.
///
fn count_tuple_elements(tuple_inner: String) -> Int {
  count_tuple_elements_helper(tuple_inner, 0, 1)
}

fn count_tuple_elements_helper(input: String, depth: Int, count: Int) -> Int {
  case string.pop_grapheme(input) {
    Error(_) -> count
    Ok(#(char, rest)) -> {
      case char {
        "(" -> count_tuple_elements_helper(rest, depth + 1, count)
        ")" -> count_tuple_elements_helper(rest, depth - 1, count)
        "," if depth == 0 -> count_tuple_elements_helper(rest, 0, count + 1)
        _ -> count_tuple_elements_helper(rest, depth, count)
      }
    }
  }
}

/// Validates if/elseif/else branches.
///
fn validate_if_branches(
  branches: List(#(Option(String), Int, List(Node))),
  data_fields: Dict(String, String),
  slot_vars: Set(String),
  loop_vars: Set(String),
  source_path: String,
) -> Result(Nil, String) {
  case branches {
    [] -> Ok(Nil)
    [#(condition, line, body), ..rest] -> {
      // Validate condition expression if present
      case condition {
        Some(cond) -> {
          case
            validate_condition(
              cond,
              data_fields,
              slot_vars,
              loop_vars,
              source_path,
              line,
            )
          {
            Error(e) -> Error(e)
            Ok(_) -> {
              case
                validate_nodes(
                  body,
                  data_fields,
                  slot_vars,
                  loop_vars,
                  source_path,
                )
              {
                Error(e) -> Error(e)
                Ok(_) ->
                  validate_if_branches(
                    rest,
                    data_fields,
                    slot_vars,
                    loop_vars,
                    source_path,
                  )
              }
            }
          }
        }
        None -> {
          // else branch - no condition to validate
          case
            validate_nodes(body, data_fields, slot_vars, loop_vars, source_path)
          {
            Error(e) -> Error(e)
            Ok(_) ->
              validate_if_branches(
                rest,
                data_fields,
                slot_vars,
                loop_vars,
                source_path,
              )
          }
        }
      }
    }
  }
}

/// Validates a condition expression from l-if/l-else-if.
/// Expressions are passed through to Gleam - the Gleam compiler
/// will catch any errors in the expression syntax.
///
fn validate_condition(
  _condition: String,
  _data_fields: Dict(String, String),
  _slot_vars: Set(String),
  _loop_vars: Set(String),
  _source_path: String,
  _line: Int,
) -> Result(Nil, String) {
  // Allow any Gleam expression - the Gleam compiler will validate
  Ok(Nil)
}

/// Validates element/component attributes for undefined variables.
///
fn validate_attrs(
  attrs: List(lexer.ComponentAttr),
  data_fields: Dict(String, String),
  slot_vars: Set(String),
  loop_vars: Set(String),
  source_path: String,
) -> Result(Nil, String) {
  case attrs {
    [] -> Ok(Nil)
    [attr, ..rest] -> {
      case attr {
        lexer.ExprAttr(_, value) -> {
          case
            validate_expression(
              value,
              data_fields,
              slot_vars,
              loop_vars,
              source_path,
              0,
            )
          {
            Error(e) -> Error(e)
            Ok(_) ->
              validate_attrs(
                rest,
                data_fields,
                slot_vars,
                loop_vars,
                source_path,
              )
          }
        }
        lexer.LmIf(cond, line) | lexer.LmElseIf(cond, line) -> {
          case
            validate_condition(
              cond,
              data_fields,
              slot_vars,
              loop_vars,
              source_path,
              line,
            )
          {
            Error(e) -> Error(e)
            Ok(_) ->
              validate_attrs(
                rest,
                data_fields,
                slot_vars,
                loop_vars,
                source_path,
              )
          }
        }
        lexer.LmFor(collection, _, _, line) -> {
          case
            validate_expression(
              collection,
              data_fields,
              slot_vars,
              loop_vars,
              source_path,
              line,
            )
          {
            Error(e) -> Error(e)
            Ok(_) ->
              validate_attrs(
                rest,
                data_fields,
                slot_vars,
                loop_vars,
                source_path,
              )
          }
        }
        _ ->
          validate_attrs(rest, data_fields, slot_vars, loop_vars, source_path)
      }
    }
  }
}

/// Validates a single expression.
/// Expressions are passed through to Gleam - the Gleam compiler
/// will catch any errors in the expression syntax.
///
fn validate_expression(
  _expr: String,
  _data_fields: Dict(String, String),
  _slot_vars: Set(String),
  _loop_vars: Set(String),
  _source_path: String,
  _line: Int,
) -> Result(Nil, String) {
  // Allow any Gleam expression - the Gleam compiler will validate
  Ok(Nil)
}

// ------------------------------------------------------------- Private Functions

/// Generates the complete module source code. Combines the
/// header comment, imports, and html function into a single
/// string ready for writing.
///
fn generate_module(
  template: Template,
  module_name: String,
  is_component: Bool,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
) -> String {
  let header =
    "//// This file was generated by Glimr ✨
//// https://github.com/glimr-org/glimr?tab=readme-ov-file#loom
////
//// DO NOT EDIT THIS FILE DIRECTLY. Instead, edit the corresponding
//// .loom.html template file and run `./glimr loom:compile`.
////
"
  let imports = generate_imports(template)

  // Collect handlers for live templates
  let handler_lookup = case template.is_live {
    False -> dict.new()
    True -> build_handler_lookup(template)
  }

  let html_fn =
    generate_html_function(
      template,
      module_name,
      is_component,
      component_data,
      component_slots,
      handler_lookup,
    )

  // For live templates, generate additional functions
  let live_fns = case template.is_live {
    False -> ""
    True -> generate_live_functions(template)
  }

  string.join([header, imports, "", html_fn, live_fns], "\n")
}

/// Generates additional functions for live templates.
/// Includes is_live(), handler metadata, and the handle dispatch function.
///
fn generate_live_functions(template: Template) -> String {
  let handlers = case handler_parser.collect_handlers(template) {
    Error(_) -> []
    Ok(h) -> h
  }

  let handlers_list =
    handlers
    |> list.map(fn(h) {
      let #(id, handler) = h
      let modifiers_str =
        handler.modifiers
        |> list.map(fn(m) { "\"" <> m <> "\"" })
        |> string.join(", ")
      "  #(\""
      <> id
      <> "\", \""
      <> handler.event
      <> "\", ["
      <> modifiers_str
      <> "])"
    })
    |> string.join(",\n")

  let prop_names =
    template.props
    |> list.map(fn(p) { "\"" <> p.0 <> "\"" })
    |> string.join(", ")

  // Collect which special vars are actually used
  let used_vars = collect_used_special_vars(handlers)

  // Generate the handle function
  let handle_fn = generate_handle_function(template, handlers, used_vars)

  // Generate JSON wrapper functions for dynamic dispatch
  let handle_json_fn = generate_handle_json_function(template, used_vars)
  let render_json_fn = generate_render_json_function(template)

  "/// Returns True if this is a live template.\n"
  <> "///\n"
  <> "pub fn is_live() -> Bool {\n"
  <> "  True\n"
  <> "}\n"
  <> "\n"
  <> "/// Returns the list of handler IDs and their event types.\n"
  <> "/// Format: List(#(handler_id, event, modifiers))\n"
  <> "///\n"
  <> "pub fn handlers() -> List(#(String, String, List(String))) {\n"
  <> "  [\n"
  <> handlers_list
  <> "\n  ]\n"
  <> "}\n"
  <> "\n"
  <> "/// Returns the list of prop names for this template.\n"
  <> "///\n"
  <> "pub fn prop_names() -> List(String) {\n"
  <> "  ["
  <> prop_names
  <> "]\n"
  <> "}\n"
  <> "\n"
  <> handle_fn
  <> "\n"
  <> handle_json_fn
  <> "\n"
  <> render_json_fn
}

/// Represents which special variables are used in the template's handlers.
///
pub type UsedSpecialVars {
  UsedSpecialVars(value: Bool, checked: Bool, key: Bool)
}

/// Collects which special variables are used across all handlers.
///
fn collect_used_special_vars(
  handlers: List(#(String, handler_parser.Handler)),
) -> UsedSpecialVars {
  let all_vars =
    handlers
    |> list.flat_map(fn(h) { handler_parser.get_special_vars(h.1) })

  UsedSpecialVars(
    value: list.contains(all_vars, "$value"),
    checked: list.contains(all_vars, "$checked"),
    key: list.contains(all_vars, "$key"),
  )
}

/// Generates the handle function for dispatching events.
/// Takes handler_id, current prop values, and special vars.
/// Returns a tuple of updated prop values.
///
fn generate_handle_function(
  template: Template,
  handlers: List(#(String, handler_parser.Handler)),
  used_vars: UsedSpecialVars,
) -> String {
  // Generate prop parameters
  let prop_params =
    template.props
    |> list.map(fn(p) { p.0 <> ": " <> p.1 })
    |> string.join(", ")

  // Generate return type (tuple of prop types)
  let return_type = case template.props {
    [] -> "Nil"
    [single] -> single.1
    props -> "#(" <> string.join(list.map(props, fn(p) { p.1 }), ", ") <> ")"
  }

  // Generate handler cases
  let handler_cases =
    handlers
    |> list.map(fn(h) { generate_handler_case(h, template.props) })
    |> string.join("\n")

  // Generate default case (return unchanged props)
  let default_return = case template.props {
    [] -> "Nil"
    [single] -> single.0
    props -> "#(" <> string.join(list.map(props, fn(p) { p.0 }), ", ") <> ")"
  }

  // Only include special var parameters that are actually used
  let special_var_params =
    []
    |> fn(acc) {
      case used_vars.value {
        True -> ["value_: option.Option(String)", ..acc]
        False -> acc
      }
    }
    |> fn(acc) {
      case used_vars.checked {
        True -> ["checked_: option.Option(Bool)", ..acc]
        False -> acc
      }
    }
    |> fn(acc) {
      case used_vars.key {
        True -> ["key_: option.Option(String)", ..acc]
        False -> acc
      }
    }
    |> list.reverse
    |> string.join(",\n  ")

  let special_params_str = case special_var_params {
    "" -> ""
    params -> ",\n  " <> params
  }

  "/// Handles an event by dispatching to the appropriate handler.\n"
  <> "/// Returns the updated prop values.\n"
  <> "///\n"
  <> "pub fn handle(\n"
  <> "  handler_id: String,\n"
  <> "  "
  <> prop_params
  <> special_params_str
  <> ",\n"
  <> ") -> "
  <> return_type
  <> " {\n"
  <> "  case handler_id {\n"
  <> handler_cases
  <> "    _ -> "
  <> default_return
  <> "\n"
  <> "  }\n"
  <> "}\n"
}

/// Generates a single handler case for the handle function.
///
fn generate_handler_case(
  handler: #(String, handler_parser.Handler),
  props: List(#(String, String)),
) -> String {
  let #(id, h) = handler

  // Transform special vars in expression ($value -> _value, etc.)
  let expression = transform_special_vars(h.expression)

  // Generate the return tuple (all prop names - shadowed bindings will be used)
  let return_expr = case props {
    [] -> "Nil"
    [single] -> single.0
    _ -> "#(" <> string.join(list.map(props, fn(p) { p.0 }), ", ") <> ")"
  }

  // Generate the let binding(s)
  let bindings = case h.targets {
    [single] -> "      let " <> single <> " = " <> expression <> "\n"
    targets -> {
      let pattern = "#(" <> string.join(targets, ", ") <> ")"
      "      let " <> pattern <> " = " <> expression <> "\n"
    }
  }

  "    \""
  <> id
  <> "\" -> {\n"
  <> bindings
  <> "      "
  <> return_expr
  <> "\n"
  <> "    }\n"
}

/// Transforms special variables in an expression.
/// $value -> value_, $checked -> checked_, $key -> key_
/// Also handles Option unwrapping for common patterns.
///
fn transform_special_vars(expr: String) -> String {
  expr
  |> string.replace("$value", "option.unwrap(value_, \"\")")
  |> string.replace("$checked", "option.unwrap(checked_, False)")
  |> string.replace("$key", "option.unwrap(key_, \"\")")
}

/// Generates the handle_json wrapper function for dynamic dispatch.
/// Parses props from JSON, calls typed handle(), returns JSON.
/// Note: Signature is fixed for dynamic dispatch compatibility.
///
fn generate_handle_json_function(
  template: Template,
  used_vars: UsedSpecialVars,
) -> String {
  // Generate the props decoder
  let props_decoder = generate_props_decoder(template.props)

  // Generate the call to handle() with extracted props
  let handle_call = generate_handle_call(template.props, used_vars)

  // Generate the JSON encoding of the result
  let result_encoder = generate_result_encoder(template.props)

  // Only parse special vars that are used (but keep all params for fixed signature)
  // Use underscore prefix for unused params to avoid warnings
  let value_param = case used_vars.value {
    True -> "value"
    False -> "_value"
  }
  let checked_param = case used_vars.checked {
    True -> "checked"
    False -> "_checked"
  }
  let key_param = case used_vars.key {
    True -> "key"
    False -> "_key"
  }

  let parse_value = case used_vars.value {
    True ->
      "  let value_opt = case value { \"\" -> option.None  _ -> option.Some(value) }\n"
    False -> ""
  }
  let parse_checked = case used_vars.checked {
    True ->
      "  let checked_opt = case checked { \"true\" -> option.Some(True)  \"false\" -> option.Some(False)  _ -> option.None }\n"
    False -> ""
  }
  let parse_key = case used_vars.key {
    True ->
      "  let key_opt = case key { \"\" -> option.None  _ -> option.Some(key) }\n"
    False -> ""
  }
  let parse_special_vars = parse_value <> parse_checked <> parse_key

  "/// JSON wrapper for handle() - used by live_socket for dynamic dispatch.\n"
  <> "/// Parses props from JSON, calls handle(), returns updated props as JSON.\n"
  <> "///\n"
  <> "pub fn handle_json(\n"
  <> "  handler_id: String,\n"
  <> "  props_json: String,\n"
  <> "  "
  <> value_param
  <> ": String,\n"
  <> "  "
  <> checked_param
  <> ": String,\n"
  <> "  "
  <> key_param
  <> ": String,\n"
  <> ") -> String {\n"
  <> parse_special_vars
  <> "\n"
  <> "  // Decode props from JSON\n"
  <> "  let decoder = {\n"
  <> props_decoder
  <> "  }\n"
  <> "\n"
  <> "  case json.parse(props_json, decoder) {\n"
  <> "    Ok(props) -> {\n"
  <> "      // Call the typed handle function\n"
  <> "      let result = "
  <> handle_call
  <> "\n"
  <> "      // Encode result back to JSON\n"
  <> result_encoder
  <> "    }\n"
  <> "    Error(_) -> props_json  // Return unchanged on parse error\n"
  <> "  }\n"
  <> "}\n"
}

/// Generates the render_json wrapper function for dynamic dispatch.
/// Parses props from JSON, calls render(), returns HTML.
///
fn generate_render_json_function(template: Template) -> String {
  // Generate the props decoder (same as handle_json)
  let props_decoder = generate_props_decoder(template.props)

  // Generate the call to render() with extracted props
  let render_call = generate_render_call(template.props)

  "/// JSON wrapper for render() - used by live_socket for dynamic dispatch.\n"
  <> "/// Parses props from JSON, calls render(), returns HTML string.\n"
  <> "///\n"
  <> "pub fn render_json(props_json: String) -> String {\n"
  <> "  // Decode props from JSON\n"
  <> "  let decoder = {\n"
  <> props_decoder
  <> "  }\n"
  <> "\n"
  <> "  case json.parse(props_json, decoder) {\n"
  <> "    Ok(props) -> "
  <> render_call
  <> "\n"
  <> "    Error(_) -> \"<!-- JSON parse error -->\"  // Error fallback\n"
  <> "  }\n"
  <> "}\n"
}

/// Generates a decoder for the props tuple/record.
///
fn generate_props_decoder(props: List(#(String, String))) -> String {
  case props {
    [] -> "    decode.success(Nil)\n"
    _ -> {
      let field_decoders =
        props
        |> list.map(fn(p) {
          let decoder = type_to_decoder(p.1)
          "    use "
          <> p.0
          <> " <- decode.field(\""
          <> p.0
          <> "\", "
          <> decoder
          <> ")\n"
        })
        |> string.join("")

      let success_tuple = case props {
        [single] -> single.0
        _ -> "#(" <> string.join(list.map(props, fn(p) { p.0 }), ", ") <> ")"
      }

      field_decoders <> "    decode.success(" <> success_tuple <> ")\n"
    }
  }
}

/// Generates the handle() function call with props.
///
fn generate_handle_call(
  props: List(#(String, String)),
  used_vars: UsedSpecialVars,
) -> String {
  // Build special var args string based on what's used
  let special_args =
    []
    |> fn(acc) {
      case used_vars.value {
        True -> ["value_opt", ..acc]
        False -> acc
      }
    }
    |> fn(acc) {
      case used_vars.checked {
        True -> ["checked_opt", ..acc]
        False -> acc
      }
    }
    |> fn(acc) {
      case used_vars.key {
        True -> ["key_opt", ..acc]
        False -> acc
      }
    }
    |> list.reverse
    |> string.join(", ")

  let special_args_str = case special_args {
    "" -> ""
    args -> ", " <> args
  }

  case props {
    [] -> "handle(handler_id" <> special_args_str <> ")"
    [_single] -> "handle(handler_id, props" <> special_args_str <> ")"
    _ -> {
      // Destructure the tuple
      let pattern =
        "#(" <> string.join(list.map(props, fn(p) { p.0 }), ", ") <> ")"
      let args = string.join(list.map(props, fn(p) { p.0 }), ", ")
      "{ let "
      <> pattern
      <> " = props\n        handle(handler_id, "
      <> args
      <> special_args_str
      <> ") }"
    }
  }
}

/// Generates the render() function call with props.
///
fn generate_render_call(props: List(#(String, String))) -> String {
  case props {
    [] -> "render()"
    [single] -> "render(" <> single.0 <> ": props)"
    _ -> {
      // Destructure the tuple and pass as named args
      let pattern =
        "#(" <> string.join(list.map(props, fn(p) { p.0 }), ", ") <> ")"
      let args =
        string.join(list.map(props, fn(p) { p.0 <> ": " <> p.0 }), ", ")
      "{ let " <> pattern <> " = props\n      render(" <> args <> ") }"
    }
  }
}

/// Generates the JSON encoder for the result.
///
fn generate_result_encoder(props: List(#(String, String))) -> String {
  case props {
    [] -> "      \"{}\"\n"
    [single] -> {
      let encoder = type_to_encoder(single.0, single.1)
      "      json.to_string(json.object([#(\""
      <> single.0
      <> "\", "
      <> encoder
      <> ")]))\n"
    }
    _ -> {
      let pattern =
        "#(" <> string.join(list.map(props, fn(p) { p.0 }), ", ") <> ")"
      let fields =
        props
        |> list.map(fn(p) {
          let encoder = type_to_encoder(p.0, p.1)
          "#(\"" <> p.0 <> "\", " <> encoder <> ")"
        })
        |> string.join(", ")

      "      let "
      <> pattern
      <> " = result\n"
      <> "      json.to_string(json.object(["
      <> fields
      <> "]))\n"
    }
  }
}

/// Maps a Gleam type to its JSON decoder.
///
fn type_to_decoder(gleam_type: String) -> String {
  case gleam_type {
    "String" -> "decode.string"
    "Int" -> "decode.int"
    "Float" -> "decode.float"
    "Bool" -> "decode.bool"
    "List(String)" -> "decode.list(decode.string)"
    "List(Int)" -> "decode.list(decode.int)"
    "List(Float)" -> "decode.list(decode.float)"
    "List(Bool)" -> "decode.list(decode.bool)"
    _ -> "decode.string"
    // Fallback for complex types
  }
}

/// Maps a prop name and type to its JSON encoder expression.
///
fn type_to_encoder(name: String, gleam_type: String) -> String {
  case gleam_type {
    "String" -> "json.string(" <> name <> ")"
    "Int" -> "json.int(" <> name <> ")"
    "Float" -> "json.float(" <> name <> ")"
    "Bool" -> "json.bool(" <> name <> ")"
    "List(String)" -> "json.array(" <> name <> ", json.string)"
    "List(Int)" -> "json.array(" <> name <> ", json.int)"
    "List(Float)" -> "json.array(" <> name <> ", json.float)"
    "List(Bool)" -> "json.array(" <> name <> ", json.bool)"
    _ -> "json.string(" <> name <> ")"
    // Fallback
  }
}

/// Builds a lookup map from handler identifiers to their assigned IDs.
/// Used during code generation to emit data-l-* attributes.
///
fn build_handler_lookup(template: Template) -> HandlerLookup {
  case handler_parser.collect_handlers(template) {
    Error(_) -> dict.new()
    Ok(handlers) -> {
      handlers
      |> list.fold(dict.new(), fn(acc, item) {
        let #(id, handler) = item
        let key = #(handler.event, handler.original, handler.line)
        dict.insert(acc, key, id)
      })
    }
  }
}

/// Generates import statements for the module. Includes the
/// runtime, user imports from @import directives, and imports
/// for all referenced components.
///
fn generate_imports(template: Template) -> String {
  // Base imports - runtime is always needed
  let base_imports = ["import glimr/loom/runtime"]

  // For live templates, import additional modules for handle/render JSON wrappers
  let live_imports = case template.is_live {
    True -> [
      "import gleam/option",
      "import gleam/json",
      "import gleam/dynamic/decode",
    ]
    False -> []
  }

  // User imports from @import directives (copied verbatim as "import <content>")
  let user_imports =
    template.imports
    |> list.map(fn(imp) { "import " <> imp })

  let component_names = collect_component_names(template.nodes, set.new())
  let component_imports =
    component_names
    |> set.to_list
    |> list.sort(string.compare)
    |> list.map(fn(name) {
      let module_path =
        "compiled/loom/components/" <> string.replace(name, ":", "/")
      let alias = component_module_alias(name)
      "import " <> module_path <> " as " <> alias
    })

  string.join(
    list.flatten([base_imports, live_imports, user_imports, component_imports]),
    "\n",
  )
}

/// Recursively collects all component names from the AST.
/// Traverses into nested structures to find every component
/// reference for import generation.
///
fn collect_component_names(nodes: List(Node), acc: Set(String)) -> Set(String) {
  list.fold(nodes, acc, fn(acc, node) {
    case node {
      parser.ComponentNode(name, _, children) -> {
        let acc = set.insert(acc, name)
        collect_component_names(children, acc)
      }
      parser.IfNode(branches) ->
        list.fold(branches, acc, fn(acc, branch) {
          collect_component_names(branch.2, acc)
        })
      parser.EachNode(_, _, _, body, _) -> collect_component_names(body, acc)
      parser.SlotDefNode(_, children) -> collect_component_names(children, acc)
      parser.ElementNode(_, _, children) ->
        collect_component_names(children, acc)
      _ -> acc
    }
  })
}

/// Recursively collects all named slot references from nodes.
/// Finds <slot name="..."> elements throughout the template
/// including inside nested structures.
///
fn collect_named_slots(nodes: List(Node), acc: Set(String)) -> Set(String) {
  list.fold(nodes, acc, fn(acc, node) {
    case node {
      parser.SlotNode(Some(name), fallback) -> {
        let acc = set.insert(acc, name)
        collect_named_slots(fallback, acc)
      }
      parser.SlotNode(None, fallback) -> collect_named_slots(fallback, acc)
      parser.IfNode(branches) ->
        list.fold(branches, acc, fn(acc, branch) {
          collect_named_slots(branch.2, acc)
        })
      parser.EachNode(_, _, _, body, _) -> collect_named_slots(body, acc)
      parser.ComponentNode(_, _, children) -> collect_named_slots(children, acc)
      parser.SlotDefNode(_, children) -> collect_named_slots(children, acc)
      parser.ElementNode(_, _, children) -> collect_named_slots(children, acc)
      _ -> acc
    }
  })
}

/// Generates the main render function for the template. Builds
/// the function signature with data and slot parameters, then
/// generates the body from the template nodes.
///
fn generate_html_function(
  template: Template,
  module_name: String,
  is_component: Bool,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
  handler_lookup: HandlerLookup,
) -> String {
  // For components, inject @attributes into first element if not present
  let nodes = case is_component {
    True ->
      case has_attributes_node(template.nodes) {
        True -> template.nodes
        False -> inject_attributes_into_first_element(template.nodes)
      }
    False -> template.nodes
  }

  // Build function parameters
  let params = generate_function_params(template, is_component)

  let body =
    generate_nodes_code(
      nodes,
      1,
      component_data,
      component_slots,
      handler_lookup,
    )

  // For live templates (non-components), wrap with live container
  // Live wrapper includes its own "" so we don't add one
  case template.is_live && !is_component {
    True -> {
      let wrapped = generate_live_wrapper(body, module_name, template.props)
      "pub fn render(" <> params <> ") -> String {\n" <> wrapped <> "}\n"
    }
    False -> {
      "pub fn render("
      <> params
      <> ") -> String {\n"
      <> "  \"\"\n"
      <> body
      <> "}\n"
    }
  }
}

/// Wraps live template content with the necessary container and script tags.
/// Uses runtime.inject_live_wrapper to properly inject into the body tag,
/// ensuring valid HTML structure when using layout components.
///
fn generate_live_wrapper(
  body: String,
  module_name: String,
  props: List(#(String, String)),
) -> String {
  // Generate the props JSON encoder expression
  let props_json_expr = case props {
    [] -> "\"{}\""
    _ -> {
      let fields =
        props
        |> list.map(fn(p) {
          let encoder = type_to_encoder(p.0, p.1)
          "#(\"" <> p.0 <> "\", " <> encoder <> ")"
        })
        |> string.join(", ")
      "json.to_string(json.object([" <> fields <> "]))"
    }
  }

  // Wrap body in a block to ensure proper grouping, then pipe to inject_live_wrapper
  // This finds the <body> tag and injects the container there at runtime
  "  {\n  \"\"\n"
  <> body
  <> "  }\n"
  <> "  |> runtime.inject_live_wrapper(\""
  <> module_name
  <> "\", "
  <> props_json_expr
  <> ")\n"
}

/// Generates the function parameter list. Includes props from
/// @props directive, slot parameters, and attributes for
/// components.
///
fn generate_function_params(template: Template, is_component: Bool) -> String {
  // Props from @props directive
  let data_params =
    template.props
    |> list.map(fn(prop) {
      let #(name, type_str) = prop
      name <> " " <> name <> ": " <> type_str
    })

  // Slot parameters from template
  let slot_params = case has_default_slot(template.nodes) {
    True -> ["slot slot: String"]
    False -> []
  }

  let named_slots =
    collect_named_slots(template.nodes, set.new()) |> set.to_list
  let named_slot_params =
    named_slots
    |> list.sort(string.compare)
    |> list.map(fn(name) {
      "slot_"
      <> to_field_name(name)
      <> " slot_"
      <> to_field_name(name)
      <> ": String"
    })

  // Attributes parameter for components
  let attr_params = case is_component {
    True -> ["attributes attributes: List(runtime.Attribute)"]
    False -> []
  }

  list.flatten([data_params, slot_params, named_slot_params, attr_params])
  |> string.join(", ")
}

/// Checks if the template uses a default slot. Recursively
/// searches for <slot> elements without a name attribute
/// throughout the AST.
///
fn has_default_slot(nodes: List(Node)) -> Bool {
  list.any(nodes, fn(node) {
    case node {
      parser.SlotNode(None, _) -> True
      parser.SlotNode(Some(_), fallback) -> has_default_slot(fallback)
      parser.IfNode(branches) ->
        list.any(branches, fn(branch) { has_default_slot(branch.2) })
      parser.EachNode(_, _, _, body, _) -> has_default_slot(body)
      parser.ComponentNode(_, _, children) -> has_default_slot(children)
      parser.SlotDefNode(_, children) -> has_default_slot(children)
      parser.ElementNode(_, _, children) -> has_default_slot(children)
      _ -> False
    }
  })
}

/// Separates slot definitions from regular content nodes.
/// Returns default content nodes and a list of named slot
/// definitions for separate rendering.
///
fn separate_slot_defs(
  nodes: List(Node),
) -> #(List(Node), List(#(Option(String), List(Node)))) {
  list.fold(nodes, #([], []), fn(acc, node) {
    let #(default_nodes, slot_defs) = acc
    case node {
      parser.SlotDefNode(name, children) -> #(
        default_nodes,
        list.append(slot_defs, [#(name, children)]),
      )
      _ -> #(list.append(default_nodes, [node]), slot_defs)
    }
  })
}

/// Generates nested case expressions for if/elseif/else chains.
/// Uses `case condition { True -> ... False -> ... }` pattern
/// which allows function calls in conditions (unlike guards).
///
fn generate_if_branches(
  branches: List(#(Option(String), Int, List(parser.Node))),
  indent: Int,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
  loop_vars: Set(String),
  handler_lookup: HandlerLookup,
) -> String {
  let pad = string.repeat("  ", indent)
  generate_if_branches_recursive(
    branches,
    indent,
    pad,
    component_data,
    component_slots,
    loop_vars,
    handler_lookup,
  )
}

fn generate_if_branches_recursive(
  branches: List(#(Option(String), Int, List(parser.Node))),
  indent: Int,
  pad: String,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
  loop_vars: Set(String),
  handler_lookup: HandlerLookup,
) -> String {
  case branches {
    [] -> ""
    [#(None, _line, body), ..] -> {
      // Else branch - just output the body (ignore any remaining branches)
      let body_code =
        generate_nodes_code_with_loop_vars(
          body,
          indent + 2,
          component_data,
          component_slots,
          loop_vars,
          handler_lookup,
        )
      pad <> "<> {\n" <> pad <> "  \"\"\n" <> body_code <> pad <> "}\n"
    }
    [#(Some(cond), _line, body), ..rest] -> {
      let transformed_cond = transform_slot_condition(cond)
      let body_code =
        generate_nodes_code_with_loop_vars(
          body,
          indent + 2,
          component_data,
          component_slots,
          loop_vars,
          handler_lookup,
        )
      let else_code = case rest {
        [] ->
          // No else branch - output empty string
          pad <> "    False -> \"\"\n"
        [#(None, _, else_body)] -> {
          // Simple else branch
          let else_body_code =
            generate_nodes_code_with_loop_vars(
              else_body,
              indent + 2,
              component_data,
              component_slots,
              loop_vars,
              handler_lookup,
            )
          pad
          <> "    False -> {\n"
          <> pad
          <> "      \"\"\n"
          <> else_body_code
          <> pad
          <> "    }\n"
        }
        _ -> {
          // More branches (else-if) - recurse
          let nested =
            generate_if_branches_recursive(
              rest,
              indent + 2,
              pad <> "    ",
              component_data,
              component_slots,
              loop_vars,
              handler_lookup,
            )
          pad <> "    False ->\n" <> nested
        }
      }
      pad
      <> "<> case "
      <> transformed_cond
      <> " {\n"
      <> pad
      <> "    True -> {\n"
      <> pad
      <> "      \"\"\n"
      <> body_code
      <> pad
      <> "    }\n"
      <> else_code
      <> pad
      <> "  }\n"
    }
  }
}

/// Transforms slot references in conditions.
/// - `slot` alone → `slot != ""`
/// - `slot.header` → `slot_header != ""`
/// Other expressions pass through unchanged.
///
fn transform_slot_condition(condition: String) -> String {
  let trimmed = string.trim(condition)
  case trimmed {
    // Exact match for "slot" - transform to emptiness check
    "slot" -> "slot != \"\""
    // Check for slot.X pattern
    _ -> {
      case string.starts_with(trimmed, "slot.") {
        True -> {
          // Transform slot.header to slot_header != ""
          let name = string.drop_start(trimmed, 5)
          "slot_" <> to_field_name(name) <> " != \"\""
        }
        False -> condition
      }
    }
  }
}

/// Generates code for a list of nodes. Maps over each node
/// and concatenates the generated code with appropriate
/// indentation.
///
fn generate_nodes_code(
  nodes: List(Node),
  indent: Int,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
  handler_lookup: HandlerLookup,
) -> String {
  generate_nodes_code_with_loop_vars(
    nodes,
    indent,
    component_data,
    component_slots,
    set.new(),
    handler_lookup,
  )
}

/// Generates code for a list of nodes with loop variable tracking.
/// Loop variables are tracked so we can generate proper type conversions
/// for loop properties like loop.index (Int) and loop.first (Bool).
///
fn generate_nodes_code_with_loop_vars(
  nodes: List(Node),
  indent: Int,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
  loop_vars: Set(String),
  handler_lookup: HandlerLookup,
) -> String {
  nodes
  |> list.map(fn(node) {
    generate_node_code_with_loop_vars(
      node,
      indent,
      component_data,
      component_slots,
      loop_vars,
      handler_lookup,
    )
  })
  |> string.join("")
}

/// Generates code for a single AST node. Handles all node
/// types: text, variables, slots, conditionals, loops,
/// and component invocations.
///
fn generate_node_code_with_loop_vars(
  node: Node,
  indent: Int,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
  loop_vars: Set(String),
  handler_lookup: HandlerLookup,
) -> String {
  let pad = string.repeat("  ", indent)

  case node {
    parser.TextNode(text) -> {
      let escaped = escape_gleam_string(text)
      pad <> "<> \"" <> escaped <> "\"\n"
    }

    parser.VariableNode(expr, _line) -> {
      // Check if this is a loop variable property that needs type conversion
      let converted_expr = convert_loop_var_expr(expr, loop_vars)
      pad <> "<> " <> converted_expr <> "\n"
    }

    parser.RawVariableNode(expr, _line) -> {
      // For raw variables, we still need to convert loop properties to strings
      let converted_expr = convert_loop_var_expr_raw(expr, loop_vars)
      pad <> "<> " <> converted_expr <> "\n"
    }

    parser.SlotNode(None, []) -> {
      // <slot /> - no fallback, just output slot
      pad <> "<> slot\n"
    }

    parser.SlotNode(None, fallback) -> {
      // <slot>fallback</slot> - output fallback if slot is empty
      let fallback_code =
        generate_nodes_code_with_loop_vars(
          fallback,
          indent + 1,
          component_data,
          component_slots,
          loop_vars,
          handler_lookup,
        )
      pad
      <> "|> runtime.append_if(slot == \"\", fn(acc) {\n"
      <> pad
      <> "  acc\n"
      <> fallback_code
      <> pad
      <> "})\n"
      <> pad
      <> "|> runtime.append_if(slot != \"\", fn(acc) { acc <> slot })\n"
    }

    parser.SlotNode(Some(name), []) -> {
      // <slot name="x" /> - no fallback
      pad <> "<> slot_" <> to_field_name(name) <> "\n"
    }

    parser.SlotNode(Some(name), fallback) -> {
      // <slot name="x">fallback</slot>
      let slot_var = "slot_" <> to_field_name(name)
      let fallback_code =
        generate_nodes_code_with_loop_vars(
          fallback,
          indent + 1,
          component_data,
          component_slots,
          loop_vars,
          handler_lookup,
        )
      pad
      <> "|> runtime.append_if("
      <> slot_var
      <> " == \"\", fn(acc) {\n"
      <> pad
      <> "  acc\n"
      <> fallback_code
      <> pad
      <> "})\n"
      <> pad
      <> "|> runtime.append_if("
      <> slot_var
      <> " != \"\", fn(acc) { acc <> "
      <> slot_var
      <> " })\n"
    }

    parser.SlotDefNode(_, _) -> {
      // SlotDefNodes are handled separately (when using components)
      ""
    }

    parser.IfNode(branches) -> {
      // Generate nested case expressions for if/elseif/else chains
      // This allows function calls in conditions (unlike guard clauses)
      generate_if_branches(
        branches,
        indent,
        component_data,
        component_slots,
        loop_vars,
        handler_lookup,
      )
    }

    parser.EachNode(collection, items, loop_var, body, _line) -> {
      // Collection is passed through directly (e.g., "items")
      // Add loop variable to the set so we can generate proper type conversions
      let new_loop_vars = case loop_var {
        Some(lv) -> set.insert(loop_vars, lv)
        None -> loop_vars
      }
      let body_code =
        generate_nodes_code_with_loop_vars(
          body,
          indent + 2,
          component_data,
          component_slots,
          new_loop_vars,
          handler_lookup,
        )
      // For tuple destructuring, we need to use a temp var and let binding
      // since Gleam doesn't allow pattern matching in fn parameters
      let #(param_name, destructure_code) = case items {
        [single] -> #(single, "")
        multiple -> {
          let pattern = "#(" <> string.join(multiple, ", ") <> ")"
          #("item__", pad <> "  let " <> pattern <> " = item__\n")
        }
      }
      case loop_var {
        None ->
          pad
          <> "|> runtime.append_each("
          <> collection
          <> ", fn(acc, "
          <> param_name
          <> ") {\n"
          <> destructure_code
          <> pad
          <> "  acc\n"
          <> body_code
          <> pad
          <> "})\n"
        Some(loop_name) ->
          pad
          <> "|> runtime.append_each_with_loop("
          <> collection
          <> ", fn(acc, "
          <> param_name
          <> ", "
          <> loop_name
          <> ") {\n"
          <> destructure_code
          <> pad
          <> "  acc\n"
          <> body_code
          <> pad
          <> "})\n"
      }
    }

    parser.ComponentNode(name, attributes, children) -> {
      let module_alias = component_module_alias(name)
      let #(default_children, named_slots) = separate_slot_defs(children)
      let #(props_code, extra_attrs_code) =
        generate_component_attrs(
          name,
          attributes,
          indent + 2,
          component_data,
          handler_lookup,
        )
      let named_slots_code =
        generate_component_named_slots_with_loop_vars(
          name,
          named_slots,
          indent + 2,
          component_data,
          component_slots,
          loop_vars,
          handler_lookup,
        )

      // Check if the component has a default slot
      let slot_info = dict.get(component_slots, name)
      let component_has_slot = case slot_info {
        Ok(info) -> info.has_default_slot
        Error(_) -> True
        // Default to True if unknown (safe fallback)
      }

      // Only include slot argument if component has @slot
      let slot_code = case component_has_slot {
        True -> {
          let default_slot_code =
            generate_nodes_code_with_loop_vars(
              default_children,
              indent + 2,
              component_data,
              component_slots,
              loop_vars,
              handler_lookup,
            )
          pad
          <> "    slot: {\n"
          <> pad
          <> "      \"\"\n"
          <> default_slot_code
          <> pad
          <> "    },\n"
        }
        False -> ""
      }

      pad
      <> "<> "
      <> module_alias
      <> ".render(\n"
      <> props_code
      <> named_slots_code
      <> slot_code
      <> pad
      <> "    attributes: "
      <> extra_attrs_code
      <> ",\n"
      <> pad
      <> "  )\n"
    }

    parser.AttributesNode(base_attrs) -> {
      case base_attrs {
        [] -> pad <> "<> \" \" <> runtime.render_attributes(attributes)\n"
        _ -> {
          let base_attrs_code = generate_base_attrs_code(base_attrs)
          pad
          <> "<> \" \" <> runtime.render_attributes(runtime.merge_attributes("
          <> base_attrs_code
          <> ", attributes))\n"
        }
      }
    }

    parser.ElementNode(tag, attributes, children) -> {
      let attrs_code = generate_element_attrs_code(attributes, handler_lookup)
      let children_code =
        generate_nodes_code_with_loop_vars(
          children,
          indent,
          component_data,
          component_slots,
          loop_vars,
          handler_lookup,
        )

      // <template> is a phantom wrapper - only render children, not the tag itself
      case tag {
        "template" -> children_code
        _ ->
          // Self-closing tags
          case is_void_element(tag) {
            True ->
              pad
              <> "<> \"<"
              <> tag
              <> "\"\n"
              <> attrs_code
              <> pad
              <> "<> \" />\"\n"
            False ->
              pad
              <> "<> \"<"
              <> tag
              <> "\"\n"
              <> attrs_code
              <> pad
              <> "<> \">\"\n"
              <> children_code
              <> pad
              <> "<> \"</"
              <> tag
              <> ">\"\n"
          }
      }
    }
  }
}

/// Generates component attribute code. Separates props from
/// HTML attributes, generates defaults for missing props,
/// and builds the attributes list for pass-through.
///
fn generate_component_attrs(
  component_name: String,
  attributes: List(lexer.ComponentAttr),
  indent: Int,
  component_data: ComponentDataMap,
  handler_lookup: HandlerLookup,
) -> #(String, String) {
  let pad = string.repeat("  ", indent)

  // Get the expected props for this component
  let expected_prop_names = case dict.get(component_data, component_name) {
    Error(_) -> []
    Ok(props) -> list.map(props, fn(p) { p.0 })
  }

  // Get explicitly provided props from expression attributes AND boolean attributes that match prop names
  let explicit_props =
    attributes
    |> list.filter_map(fn(attr) {
      case attr {
        lexer.ExprAttr(name, _) -> {
          // Expression attribute is a prop only if it matches an expected prop name
          let field_name = to_field_name(name)
          case list.contains(expected_prop_names, field_name) {
            True -> Ok(field_name)
            False -> Error(Nil)
          }
        }
        lexer.BoolAttr(name) -> {
          // Boolean attribute is a prop if it matches an expected prop name
          let field_name = to_field_name(name)
          case list.contains(expected_prop_names, field_name) {
            True -> Ok(field_name)
            False -> Error(Nil)
          }
        }
        _ -> Error(Nil)
      }
    })

  // Generate prop field assignments for expression bindings (:prop="expr") that match prop names
  // AND boolean attributes that match prop names (set to True)
  let explicit_props_code =
    attributes
    |> list.filter_map(fn(attr) {
      case attr {
        lexer.ExprAttr(name, value) -> {
          // Only generate prop assignment if name matches an expected prop
          let field_name = to_field_name(name)
          case list.contains(expected_prop_names, field_name) {
            True -> Ok(pad <> field_name <> ": " <> value <> ",\n")
            False -> Error(Nil)
          }
        }
        lexer.BoolAttr(name) -> {
          // Boolean attribute sets prop to True if it matches an expected prop
          let field_name = to_field_name(name)
          case list.contains(expected_prop_names, field_name) {
            True -> Ok(pad <> field_name <> ": True,\n")
            False -> Error(Nil)
          }
        }
        lexer.StringAttr(_, _) -> Error(Nil)
        lexer.ClassAttr(_) -> Error(Nil)
        lexer.StyleAttr(_) -> Error(Nil)
        lexer.LmIf(_, _)
        | lexer.LmElseIf(_, _)
        | lexer.LmElse
        | lexer.LmFor(_, _, _, _)
        | lexer.LmOn(_, _, _, _)
        | lexer.LmModel(_, _) -> Error(Nil)
      }
    })
    |> string.join("")

  // Look up component's expected props and generate defaults for missing ones
  let default_props_code = case dict.get(component_data, component_name) {
    Error(_) -> ""
    Ok(expected_props) -> {
      expected_props
      |> list.filter_map(fn(prop) {
        let #(prop_name, prop_type) = prop
        // Skip if explicitly provided
        case list.contains(explicit_props, prop_name) {
          True -> Error(Nil)
          False -> {
            // Generate default value based on type
            let default_value = default_for_type(prop_type)
            Ok(pad <> prop_name <> ": " <> default_value <> ",\n")
          }
        }
      })
      |> string.join("")
    }
  }

  let props_code = explicit_props_code <> default_props_code

  // Generate attributes list for pass-through
  // Exclude expression/boolean attributes that are props (they're handled above)
  // Each attribute type handled independently (no class/style merging)
  let attrs_items =
    attributes
    |> list.filter_map(fn(attr) {
      case attr {
        lexer.StringAttr(name, value) ->
          Ok(
            "runtime.Attribute(\""
            <> name
            <> "\", \""
            <> escape_gleam_string(value)
            <> "\")",
          )
        lexer.ExprAttr(name, value) -> {
          // Only include as HTML attribute if NOT a prop
          let field_name = to_field_name(name)
          case list.contains(expected_prop_names, field_name) {
            True -> Error(Nil)
            // It's a prop, not an HTML attribute
            False -> {
              // Use BoolAttribute for known HTML boolean attributes
              case is_html_boolean_attribute(name) {
                True ->
                  Ok(
                    "runtime.BoolAttribute(\"" <> name <> "\", " <> value <> ")",
                  )
                False ->
                  Ok("runtime.Attribute(\"" <> name <> "\", " <> value <> ")")
              }
            }
          }
        }
        lexer.BoolAttr(name) -> {
          // Only include as HTML attribute if NOT a prop
          let field_name = to_field_name(name)
          case list.contains(expected_prop_names, field_name) {
            True -> Error(Nil)
            // It's a prop, not an HTML attribute
            False ->
              Ok("runtime.Attribute(\"" <> name <> "\", \"" <> name <> "\")")
          }
        }
        lexer.ClassAttr(value) ->
          Ok(
            "runtime.Attribute(\"class\", runtime.build_classes("
            <> transform_class_list(value)
            <> "))",
          )
        lexer.StyleAttr(value) ->
          Ok(
            "runtime.Attribute(\"style\", runtime.build_styles("
            <> transform_style_list(value)
            <> "))",
          )
        // Generate data-l-* attributes for event handlers
        lexer.LmOn(event, modifiers, handler, line) -> {
          case dict.get(handler_lookup, #(event, handler, line)) {
            Ok(handler_id) -> {
              // Build list of attributes: handler ID + modifiers
              let handler_attr =
                "runtime.Attribute(\"data-l-"
                <> event
                <> "\", \""
                <> handler_id
                <> "\")"

              // Add modifier attributes
              let modifier_attrs =
                modifiers
                |> list.map(fn(m) {
                  // Handle debounce specially: .debounce or .debounce-300
                  case string.starts_with(m, "debounce") {
                    True -> {
                      let value = case string.split(m, "-") {
                        ["debounce", ms] -> ms
                        _ -> "150"
                        // Default debounce
                      }
                      "runtime.Attribute(\"data-l-debounce\", \""
                      <> value
                      <> "\")"
                    }
                    False ->
                      "runtime.Attribute(\"data-l-" <> m <> "\", \"true\")"
                  }
                })
                |> string.join(", ")

              case modifier_attrs {
                "" -> Ok(handler_attr)
                _ -> Ok(handler_attr <> ", " <> modifier_attrs)
              }
            }
            Error(_) -> Error(Nil)
          }
        }
        // l-model generates data-l-input attribute + value binding is handled separately
        lexer.LmModel(prop, line) -> {
          let handler_str = prop <> " = $value"
          case dict.get(handler_lookup, #("input", handler_str, line)) {
            Ok(handler_id) ->
              Ok(
                "runtime.Attribute(\"data-l-input\", \"" <> handler_id <> "\")",
              )
            Error(_) -> Error(Nil)
          }
        }
        lexer.LmIf(_, _)
        | lexer.LmElseIf(_, _)
        | lexer.LmElse
        | lexer.LmFor(_, _, _, _) -> Error(Nil)
      }
    })

  let extra_attrs_code = case attrs_items {
    [] -> "[]"
    items -> "[" <> string.join(items, ", ") <> "]"
  }

  #(props_code, extra_attrs_code)
}

/// Returns a default value for a Gleam type. Handles Bool,
/// String, Int, Float, Option, and List types with sensible
/// defaults.
///
fn default_for_type(type_str: String) -> String {
  case type_str {
    "Bool" -> "False"
    "String" -> "\"\""
    "Int" -> "0"
    "Float" -> "0.0"
    _ -> {
      // For Option types, default to None
      case string.starts_with(type_str, "Option(") {
        True -> "None"
        False -> {
          // For List types, default to empty list
          case string.starts_with(type_str, "List(") {
            True -> "[]"
            // Unknown type - can't provide a default
            False -> "todo"
          }
        }
      }
    }
  }
}

/// Checks if an attribute name is an HTML boolean attribute.
/// These attributes render only their name when true, not
/// name="value" pairs.
///
fn is_html_boolean_attribute(name: String) -> Bool {
  case name {
    "disabled"
    | "checked"
    | "readonly"
    | "required"
    | "hidden"
    | "selected"
    | "multiple"
    | "autofocus"
    | "autoplay"
    | "controls"
    | "loop"
    | "muted"
    | "default"
    | "defer"
    | "async"
    | "novalidate"
    | "formnovalidate"
    | "ismap"
    | "nomodule"
    | "playsinline"
    | "open"
    | "reversed"
    | "allowfullscreen"
    | "inert"
    | "itemscope" -> True
    _ -> False
  }
}

/// Generates named slot arguments with loop variable tracking.
///
fn generate_component_named_slots_with_loop_vars(
  component_name: String,
  named_slots: List(#(Option(String), List(Node))),
  indent: Int,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
  loop_vars: Set(String),
  handler_lookup: HandlerLookup,
) -> String {
  let pad = string.repeat("  ", indent)

  // Filter to only named slots (Some(name)), not default slot definitions
  let actual_named_slots =
    list.filter_map(named_slots, fn(slot) {
      case slot.0 {
        Some(name) -> Ok(#(name, slot.1))
        None -> Error(Nil)
      }
    })

  // Get the names of slots that are explicitly provided
  let provided_slot_names =
    list.map(actual_named_slots, fn(slot) { slot.0 }) |> set.from_list

  // Get all named slots the component expects
  let expected_slots = case dict.get(component_slots, component_name) {
    Ok(info) -> info.named_slots
    Error(_) -> []
  }

  // Generate code for explicitly provided slots
  let provided_slots_code =
    actual_named_slots
    |> list.map(fn(slot) {
      let #(name, children) = slot
      let slot_code =
        generate_nodes_code_with_loop_vars(
          children,
          indent + 2,
          component_data,
          component_slots,
          loop_vars,
          handler_lookup,
        )
      pad
      <> "slot_"
      <> to_field_name(name)
      <> ": {\n"
      <> pad
      <> "  \"\"\n"
      <> slot_code
      <> pad
      <> "},\n"
    })
    |> string.join("")

  // Generate empty strings for missing slots
  let missing_slots_code =
    expected_slots
    |> list.filter(fn(name) { !set.contains(provided_slot_names, name) })
    |> list.map(fn(name) {
      pad <> "slot_" <> to_field_name(name) <> ": \"\",\n"
    })
    |> string.join("")

  provided_slots_code <> missing_slots_code
}

/// Generates code for element attributes. Converts lexer
/// attributes to runtime calls for rendering HTML attributes.
///
fn generate_element_attrs_code(
  attrs: List(lexer.ComponentAttr),
  handler_lookup: HandlerLookup,
) -> String {
  case attrs {
    [] -> ""
    _ -> {
      let attr_items =
        attrs
        |> list.filter_map(fn(attr) {
          case attr {
            lexer.StringAttr(name, value) ->
              Ok(
                "runtime.Attribute(\""
                <> name
                <> "\", \""
                <> escape_gleam_string(value)
                <> "\")",
              )
            lexer.ExprAttr(name, value) -> {
              case is_html_boolean_attribute(name) {
                True ->
                  Ok(
                    "runtime.BoolAttribute(\"" <> name <> "\", " <> value <> ")",
                  )
                False ->
                  Ok("runtime.Attribute(\"" <> name <> "\", " <> value <> ")")
              }
            }
            lexer.BoolAttr(name) ->
              Ok("runtime.Attribute(\"" <> name <> "\", \"" <> name <> "\")")
            lexer.ClassAttr(value) ->
              Ok(
                "runtime.Attribute(\"class\", runtime.build_classes("
                <> transform_class_list(value)
                <> "))",
              )
            lexer.StyleAttr(value) ->
              Ok(
                "runtime.Attribute(\"style\", runtime.build_styles("
                <> transform_style_list(value)
                <> "))",
              )
            // Generate data-l-* attributes for event handlers
            lexer.LmOn(event, modifiers, handler, line) -> {
              case dict.get(handler_lookup, #(event, handler, line)) {
                Ok(handler_id) -> {
                  // Build list of attributes: handler ID + modifiers
                  let handler_attr =
                    "runtime.Attribute(\"data-l-"
                    <> event
                    <> "\", \""
                    <> handler_id
                    <> "\")"

                  // Add modifier attributes
                  let modifier_attrs =
                    modifiers
                    |> list.map(fn(m) {
                      // Handle debounce specially: .debounce or .debounce-300
                      case string.starts_with(m, "debounce") {
                        True -> {
                          let value = case string.split(m, "-") {
                            ["debounce", ms] -> ms
                            _ -> "150"
                            // Default debounce
                          }
                          "runtime.Attribute(\"data-l-debounce\", \""
                          <> value
                          <> "\")"
                        }
                        False ->
                          "runtime.Attribute(\"data-l-" <> m <> "\", \"true\")"
                      }
                    })
                    |> string.join(", ")

                  case modifier_attrs {
                    "" -> Ok(handler_attr)
                    _ -> Ok(handler_attr <> ", " <> modifier_attrs)
                  }
                }
                Error(_) -> Error(Nil)
              }
            }
            // l-model generates both value binding and data-l-input attribute
            lexer.LmModel(prop, line) -> {
              let handler_str = prop <> " = $value"
              case dict.get(handler_lookup, #("input", handler_str, line)) {
                Ok(handler_id) ->
                  Ok(
                    "runtime.Attribute(\"data-l-input\", \""
                    <> handler_id
                    <> "\")",
                  )
                Error(_) -> Error(Nil)
              }
            }
            // l-* control flow attributes are handled elsewhere
            lexer.LmIf(_, _)
            | lexer.LmElseIf(_, _)
            | lexer.LmElse
            | lexer.LmFor(_, _, _, _) -> Error(Nil)
          }
        })

      // For l-model, we also need to generate a value attribute
      let lm_model_value_attrs =
        attrs
        |> list.filter_map(fn(attr) {
          case attr {
            lexer.LmModel(prop, _) ->
              Ok("runtime.Attribute(\"value\", " <> prop <> ")")
            _ -> Error(Nil)
          }
        })

      let all_items = list.append(attr_items, lm_model_value_attrs)

      case all_items {
        [] -> ""
        items ->
          "  <> \" \" <> runtime.render_attributes(["
          <> string.join(items, ", ")
          <> "])\n"
      }
    }
  }
}

/// Checks if a tag is an HTML void element. Void elements like
/// img, br, and input are self-closing and cannot contain
/// children in HTML.
///
fn is_void_element(tag: String) -> Bool {
  case tag {
    "area"
    | "base"
    | "br"
    | "col"
    | "embed"
    | "hr"
    | "img"
    | "input"
    | "link"
    | "meta"
    | "param"
    | "source"
    | "track"
    | "wbr" -> True
    _ -> False
  }
}

/// Generates code for base attributes. Converts lexer
/// attributes to runtime Attribute constructors.
///
fn generate_base_attrs_code(attrs: List(lexer.ComponentAttr)) -> String {
  let items =
    attrs
    |> list.filter_map(fn(attr) {
      case attr {
        lexer.StringAttr(name, value) ->
          Ok(
            "runtime.Attribute(\""
            <> name
            <> "\", \""
            <> escape_gleam_string(value)
            <> "\")",
          )
        lexer.BoolAttr(name) ->
          Ok("runtime.Attribute(\"" <> name <> "\", \"" <> name <> "\")")
        lexer.ExprAttr(name, value) ->
          Ok("runtime.Attribute(\"" <> name <> "\", " <> value <> ")")
        lexer.ClassAttr(value) ->
          Ok(
            "runtime.Attribute(\"class\", runtime.build_classes("
            <> transform_class_list(value)
            <> "))",
          )
        lexer.StyleAttr(value) ->
          Ok(
            "runtime.Attribute(\"style\", runtime.build_styles("
            <> transform_style_list(value)
            <> "))",
          )
        lexer.LmIf(_, _)
        | lexer.LmElseIf(_, _)
        | lexer.LmElse
        | lexer.LmFor(_, _, _, _)
        | lexer.LmOn(_, _, _, _)
        | lexer.LmModel(_, _) -> Error(Nil)
      }
    })

  "[" <> string.join(items, ", ") <> "]"
}

/// Checks if the template contains an @attributes directive.
/// Recursively searches the AST to find explicit attribute
/// injection points.
///
fn has_attributes_node(nodes: List(Node)) -> Bool {
  list.any(nodes, fn(node) {
    case node {
      parser.AttributesNode(_) -> True
      parser.IfNode(branches) ->
        list.any(branches, fn(branch) { has_attributes_node(branch.2) })
      parser.EachNode(_, _, _, body, _) -> has_attributes_node(body)
      parser.SlotDefNode(_, children) -> has_attributes_node(children)
      parser.ComponentNode(_, _, children) -> has_attributes_node(children)
      parser.ElementNode(_, _, children) -> has_attributes_node(children)
      _ -> False
    }
  })
}

/// Injects @attributes into the first HTML element. For
/// components without explicit attribute placement, adds
/// attributes to the root element automatically.
///
fn inject_attributes_into_first_element(nodes: List(Node)) -> List(Node) {
  inject_attributes_helper(nodes, False).0
}

/// Recursive helper for attribute injection. Searches for the
/// first HTML tag and inserts an AttributesNode after its
/// opening bracket and existing attributes.
///
fn inject_attributes_helper(
  nodes: List(Node),
  injected: Bool,
) -> #(List(Node), Bool) {
  case injected, nodes {
    True, _ -> #(nodes, True)
    False, [] -> #([], False)
    False, [parser.TextNode(text), ..rest] -> {
      case find_first_tag_and_extract_attrs(text) {
        Some(#(before_attrs, base_attrs, after)) -> {
          let new_nodes = [
            parser.TextNode(before_attrs),
            parser.AttributesNode(base_attrs),
            parser.TextNode(after),
            ..rest
          ]
          #(new_nodes, True)
        }
        None -> {
          let #(rest_nodes, did_inject) = inject_attributes_helper(rest, False)
          #([parser.TextNode(text), ..rest_nodes], did_inject)
        }
      }
    }
    // Recurse into IfNode to find first element inside conditionals
    False, [parser.IfNode(branches), ..rest] -> {
      let #(new_branches, did_inject) =
        inject_attributes_into_branches(branches, False)
      case did_inject {
        True -> #([parser.IfNode(new_branches), ..rest], True)
        False -> {
          let #(rest_nodes, rest_inject) = inject_attributes_helper(rest, False)
          #([parser.IfNode(new_branches), ..rest_nodes], rest_inject)
        }
      }
    }
    // Recurse into EachNode to find first element inside loops
    False, [parser.EachNode(collection, items, loop_var, body, line), ..rest] -> {
      let #(new_body, did_inject) = inject_attributes_helper(body, False)
      case did_inject {
        True -> #(
          [parser.EachNode(collection, items, loop_var, new_body, line), ..rest],
          True,
        )
        False -> {
          let #(rest_nodes, rest_inject) = inject_attributes_helper(rest, False)
          #(
            [
              parser.EachNode(collection, items, loop_var, new_body, line),
              ..rest_nodes
            ],
            rest_inject,
          )
        }
      }
    }
    // For ElementNode, inject AttributesNode into its children
    False, [parser.ElementNode(tag, attrs, children), ..rest] -> {
      let new_children = [parser.AttributesNode(attrs), ..children]
      #([parser.ElementNode(tag, [], new_children), ..rest], True)
    }
    False, [node, ..rest] -> {
      let #(rest_nodes, did_inject) = inject_attributes_helper(rest, False)
      #([node, ..rest_nodes], did_inject)
    }
  }
}

/// Injects @attributes into branches of an if/elseif/else.
/// Processes each branch body looking for the first HTML tag
/// and returns updated branches with injection status.
///
fn inject_attributes_into_branches(
  branches: List(#(Option(String), Int, List(Node))),
  injected: Bool,
) -> #(List(#(Option(String), Int, List(Node))), Bool) {
  case branches {
    [] -> #([], injected)
    [#(condition, line, body), ..rest] -> {
      let #(new_body, did_inject) = inject_attributes_helper(body, injected)
      let #(rest_branches, rest_inject) =
        inject_attributes_into_branches(rest, did_inject)
      #([#(condition, line, new_body), ..rest_branches], rest_inject)
    }
  }
}

/// Finds the first HTML tag in text and extracts attributes.
/// Returns the text before attributes, parsed attributes,
/// and the closing portion of the tag.
///
fn find_first_tag_and_extract_attrs(
  text: String,
) -> Option(#(String, List(lexer.ComponentAttr), String)) {
  case string.split_once(text, "<") {
    Error(_) -> None
    Ok(#(before_tag, after_open)) -> {
      case parse_tag_content(after_open) {
        Some(#(tag_name, attrs, closing)) -> {
          Some(#(before_tag <> "<" <> tag_name <> " ", attrs, closing))
        }
        None -> None
      }
    }
  }
}

/// Parses the content after an opening angle bracket. Extracts
/// the tag name and parses any attributes before the closing
/// bracket.
///
fn parse_tag_content(
  content: String,
) -> Option(#(String, List(lexer.ComponentAttr), String)) {
  let content = string.trim_start(content)
  let #(tag_name, rest) = take_until_space_or_close(content, "")

  case tag_name {
    "" -> None
    "/" <> _ -> None
    "!" <> _ -> None
    _ -> {
      let #(attrs, closing) = parse_html_attributes(string.trim_start(rest), [])
      Some(#(tag_name, attrs, closing))
    }
  }
}

/// Reads characters until whitespace or tag delimiter. Used
/// to extract tag names and unquoted attribute values from
/// HTML content.
///
fn take_until_space_or_close(input: String, acc: String) -> #(String, String) {
  case string.pop_grapheme(input) {
    Error(_) -> #(acc, "")
    Ok(#(char, rest)) -> {
      case char {
        " " | "\t" | "\n" | "\r" | ">" | "/" -> #(acc, input)
        _ -> take_until_space_or_close(rest, acc <> char)
      }
    }
  }
}

/// Parses HTML attributes from an opening tag. Continues until
/// the closing bracket, accumulating string and boolean
/// attributes.
///
fn parse_html_attributes(
  input: String,
  acc: List(lexer.ComponentAttr),
) -> #(List(lexer.ComponentAttr), String) {
  let input = string.trim_start(input)

  case string.pop_grapheme(input) {
    Error(_) -> #(list.reverse(acc), "")
    Ok(#(">", rest)) -> #(list.reverse(acc), ">" <> rest)
    Ok(#("/", rest)) -> {
      case string.first(rest) {
        Ok(">") -> #(list.reverse(acc), "/" <> rest)
        _ -> #(list.reverse(acc), "/" <> rest)
      }
    }
    Ok(#(_, _)) -> {
      case parse_single_html_attribute(input) {
        Some(#(attr, remaining)) ->
          parse_html_attributes(remaining, [attr, ..acc])
        None ->
          case string.pop_grapheme(input) {
            Ok(#(_, rest)) -> parse_html_attributes(rest, acc)
            Error(_) -> #(list.reverse(acc), "")
          }
      }
    }
  }
}

/// Parses a single HTML attribute. Handles name="value" with
/// single or double quotes, unquoted values, and boolean
/// attributes without values.
///
fn parse_single_html_attribute(
  input: String,
) -> Option(#(lexer.ComponentAttr, String)) {
  let input = string.trim_start(input)
  let #(name, rest) = take_attr_name(input, "")

  case name {
    "" -> None
    _ -> {
      let rest = string.trim_start(rest)
      case string.pop_grapheme(rest) {
        Ok(#("=", after_eq)) -> {
          let after_eq = string.trim_start(after_eq)
          case string.pop_grapheme(after_eq) {
            Ok(#("\"", after_quote)) -> {
              let #(value, remaining) = take_until_char(after_quote, "\"", "")
              Some(#(lexer.StringAttr(name, value), remaining))
            }
            Ok(#("'", after_quote)) -> {
              let #(value, remaining) = take_until_char(after_quote, "'", "")
              Some(#(lexer.StringAttr(name, value), remaining))
            }
            Ok(#(_, _)) -> {
              let #(value, remaining) = take_until_space_or_close(after_eq, "")
              Some(#(lexer.StringAttr(name, value), remaining))
            }
            Error(_) -> Some(#(lexer.BoolAttr(name), rest))
          }
        }
        _ -> Some(#(lexer.BoolAttr(name), rest))
      }
    }
  }
}

/// Extracts an attribute name from input. Reads until
/// whitespace, equals sign, or tag delimiter is encountered,
/// returning the name and remaining input.
///
fn take_attr_name(input: String, acc: String) -> #(String, String) {
  case string.pop_grapheme(input) {
    Error(_) -> #(acc, "")
    Ok(#(char, rest)) -> {
      case char {
        " " | "\t" | "\n" | "\r" | "=" | ">" | "/" -> #(acc, input)
        _ -> take_attr_name(rest, acc <> char)
      }
    }
  }
}

/// Reads characters until a specific stop character is found.
/// Used to extract quoted attribute values by reading until
/// the closing quote.
///
fn take_until_char(
  input: String,
  stop_char: String,
  acc: String,
) -> #(String, String) {
  case string.pop_grapheme(input) {
    Error(_) -> #(acc, "")
    Ok(#(char, rest)) -> {
      case char == stop_char {
        True -> #(acc, rest)
        False -> take_until_char(rest, stop_char, acc <> char)
      }
    }
  }
}

/// Converts a name to a valid Gleam field name. Replaces
/// dashes, colons, and spaces with underscores and converts
/// to snake_case.
///
fn to_field_name(name: String) -> String {
  name
  |> string.replace("-", "_")
  |> string.replace(":", "_")
  |> string.replace(" ", "_")
  |> to_snake_case
}

/// Converts a string to snake_case. Inserts underscores before
/// uppercase letters and lowercases them, then strips any
/// leading underscores.
///
fn to_snake_case(input: String) -> String {
  input
  |> string.to_graphemes
  |> list.fold("", fn(acc, char) {
    case is_uppercase(char) {
      True -> acc <> "_" <> string.lowercase(char)
      False -> acc <> char
    }
  })
  |> strip_leading_underscores
}

/// Removes leading underscores from a string. Used to clean
/// up snake_case conversions that produce strings starting
/// with underscores.
///
fn strip_leading_underscores(input: String) -> String {
  case input {
    "_" <> rest -> strip_leading_underscores(rest)
    _ -> input
  }
}

/// Checks if a character is uppercase. Compares the character
/// to its uppercase version to determine case, excluding
/// characters that are the same in both cases.
///
fn is_uppercase(char: String) -> Bool {
  let upper = string.uppercase(char)
  char == upper && char != string.lowercase(char)
}

/// Escapes a string for use in generated Gleam code. Handles
/// backslashes, quotes, newlines, carriage returns, and tabs
/// with proper escape sequences.
///
fn escape_gleam_string(input: String) -> String {
  input
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\r", "\\r")
  |> string.replace("\t", "\\t")
}

/// Generates a module alias for a component import. Converts
/// the component name to a valid Gleam identifier by replacing
/// colons and dashes with underscores.
///
fn component_module_alias(name: String) -> String {
  "components_"
  <> name
  |> string.replace(":", "_")
  |> string.replace("-", "_")
}

/// Transforms string literals in a :class list to use the
/// runtime.class() helper. Converts ["foo", #("bar", cond)]
/// to [runtime.class("foo"), #("bar", cond)].
///
fn transform_class_list(value: String) -> String {
  transform_list_strings(value, "runtime.class")
}

/// Transforms string literals in a :style list to use the
/// runtime.style() helper. Converts ["foo", #("bar", cond)]
/// to [runtime.style("foo"), #("bar", cond)].
///
fn transform_style_list(value: String) -> String {
  transform_list_strings(value, "runtime.style")
}

/// Transforms bare string literals at the top level of a list
/// to wrap them with a helper function. Tuples and nested
/// expressions like #("x", cond) are left unchanged.
///
fn transform_list_strings(value: String, wrapper: String) -> String {
  let chars = string.to_graphemes(value)
  transform_list_chars(chars, wrapper, 0, False, "", "")
}

/// Recursive helper for transforming list string literals.
/// Tracks nesting depth and string state to identify top-level
/// string literals that need wrapping with the helper function.
///
fn transform_list_chars(
  chars: List(String),
  wrapper: String,
  depth: Int,
  in_string: Bool,
  current: String,
  result: String,
) -> String {
  case in_string, chars {
    _, [] -> result <> current
    True, ["\\", c, ..rest] -> {
      // Escaped character in string - keep both
      transform_list_chars(
        rest,
        wrapper,
        depth,
        True,
        current <> "\\" <> c,
        result,
      )
    }
    True, ["\"", ..rest] -> {
      // End of string
      transform_list_chars(rest, wrapper, depth, False, current <> "\"", result)
    }
    True, [c, ..rest] -> {
      // Inside string - just accumulate
      transform_list_chars(rest, wrapper, depth, True, current <> c, result)
    }
    False, ["\"", ..rest] -> {
      // Start of string at current depth
      case depth {
        1 -> {
          // Top level of list - wrap with helper
          transform_list_chars(
            rest,
            wrapper,
            depth,
            True,
            wrapper <> "(\"",
            result <> current,
          )
        }
        _ -> {
          // Inside tuple or nested - don't wrap
          transform_list_chars(
            rest,
            wrapper,
            depth,
            True,
            current <> "\"",
            result,
          )
        }
      }
    }
    False, ["[", ..rest] -> {
      transform_list_chars(
        rest,
        wrapper,
        depth + 1,
        False,
        current <> "[",
        result,
      )
    }
    False, ["]", ..rest] -> {
      // Check if we need to close the wrapper before ]
      let needs_close =
        depth == 1 && string.starts_with(current, wrapper <> "(")
      let new_current = case needs_close {
        True -> current <> ")"
        False -> current
      }
      transform_list_chars(
        rest,
        wrapper,
        depth - 1,
        False,
        new_current <> "]",
        result,
      )
    }
    False, ["(", ..rest] -> {
      transform_list_chars(
        rest,
        wrapper,
        depth + 1,
        False,
        current <> "(",
        result,
      )
    }
    False, [")", ..rest] -> {
      transform_list_chars(
        rest,
        wrapper,
        depth - 1,
        False,
        current <> ")",
        result,
      )
    }
    False, [",", ..rest] -> {
      // Comma at depth 1 means end of list item - flush and check if wrapper needed
      let needs_close =
        depth == 1 && string.starts_with(current, wrapper <> "(")
      let new_current = case needs_close {
        True -> current <> ")"
        False -> current
      }
      transform_list_chars(
        rest,
        wrapper,
        depth,
        False,
        "",
        result <> new_current <> ",",
      )
    }
    False, [c, ..rest] -> {
      transform_list_chars(rest, wrapper, depth, False, current <> c, result)
    }
  }
}

/// Converts any expression to a displayable string representation.
/// Uses runtime.display() which handles all types (String, Bool, Int, etc.)
/// and escapes HTML.
///
fn convert_loop_var_expr(expr: String, _loop_vars: Set(String)) -> String {
  "runtime.display(" <> expr <> ")"
}

/// Converts an expression for raw output (no escaping).
/// Loop properties are converted to strings, but regular data is
/// passed through directly (assumed to already be a String).
///
fn convert_loop_var_expr_raw(expr: String, loop_vars: Set(String)) -> String {
  case get_loop_property(expr, loop_vars) {
    Some(_) -> "runtime.to_string(" <> expr <> ")"
    None -> expr
  }
}

/// Checks if an expression is accessing a loop variable property.
/// Returns Some(#(loop_var_name, property_name)) if it is, None otherwise.
///
fn get_loop_property(
  expr: String,
  loop_vars: Set(String),
) -> Option(#(String, String)) {
  case string.split_once(expr, ".") {
    Ok(#(root, rest)) -> {
      case set.contains(loop_vars, root) {
        True -> {
          // It's a loop variable, extract just the immediate property
          let property = case string.split_once(rest, ".") {
            Ok(#(prop, _)) -> prop
            Error(_) -> rest
          }
          Some(#(root, property))
        }
        False -> None
      }
    }
    Error(_) -> None
  }
}
