//// Code Generator
////
//// Templates are authored in HTML with directives, but the
//// server needs executable Gleam functions to render them.
//// This module closes that gap by transforming the parsed AST 
//// into string-building Gleam code that calls runtime helpers, 
//// so templates get full type-checking and compile to native 
//// BEAM code with no interpretation overhead.
////

import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import glimr/loom/handler_parser
import glimr/loom/lexer
import glimr/loom/parser.{type Node, type Template}
import glimr/utils/string as string_util

// ------------------------------------------------------------- Public Types

/// When a parent template uses a component without supplying
/// all props, the generator needs to fill in type-appropriate
/// defaults. This map provides the expected fields per
/// component so defaults can be generated at compile time
/// rather than failing at runtime.
///
pub type ComponentDataMap =
  Dict(String, List(#(String, String)))

/// Components may accept a default slot, named slots, or
/// neither. The generator needs this information to decide
/// which slot arguments to include in the generated render call
/// and which to supply as empty strings when the parent doesn't 
/// provide content.
///
pub type ComponentSlotInfo {
  ComponentSlotInfo(has_default_slot: Bool, named_slots: List(String))
}

/// Without knowing which slots each component expects, the
/// generator would either omit required slot arguments (causing
/// compile errors) or always include every possible slot 
/// (wasting code). This map lets it emit exactly the right set 
/// of slot parameters.
///
pub type ComponentSlotMap =
  Dict(String, ComponentSlotInfo)

/// The compiler pipeline needs both the module name (to
/// determine the output file path) and the generated source 
/// code (to write it). Bundling them together keeps the caller 
/// from having to track which code belongs to which module.
///
pub type GeneratedCode {
  GeneratedCode(module_name: String, code: String)
}

/// The generated handle function signature only includes
/// special variable parameters ($value, $checked, $key) that 
/// are actually referenced. Tracking usage as bools lets the 
/// generator conditionally include each parameter and avoid 
/// unused-variable warnings.
///
pub type UsedSpecialVars {
  UsedSpecialVars(value: Bool, checked: Bool, key: Bool)
}

// ------------------------------------------------------------- Private Types

/// Handlers are collected once upfront but referenced many
/// times during code generation when emitting data-l-*
/// attributes. A lookup by (event, handler_str, line) lets the 
/// generator resolve the correct handler ID in O(1) without 
/// re-scanning the handler list each time.
///
type HandlerLookup =
  Dict(#(String, String, Int), String)

// ------------------------------------------------------------- Public Functions

/// Public entry point for the code generator. Wraps the
/// internal generate_module with the GeneratedCode return type 
/// so the compiler pipeline can write the output to the correct 
/// file path based on the module name.
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

/// The compiler needs to know which named slots a component
/// template declares so it can build the ComponentSlotMap
/// before generating parent templates that reference this
/// component. Deduplication via a set avoids duplicate
/// parameters in the generated function signature.
///
pub fn extract_named_slots(template: Template) -> List(String) {
  collect_named_slots(template.nodes, set.new())
  |> set.to_list
}

/// Combines default slot detection and named slot collection
/// into a single ComponentSlotInfo. The compiler calls this
/// once per component template and caches the result so every 
/// parent template that uses the component can generate correct 
/// slot arguments.
///
pub fn extract_slot_info(template: Template) -> ComponentSlotInfo {
  ComponentSlotInfo(
    has_default_slot: has_default_slot(template.nodes),
    named_slots: collect_named_slots(template.nodes, set.new()) |> set.to_list,
  )
}

/// Generated code that references undefined variables would
/// fail to compile with confusing Gleam errors. Validating at 
/// the template level lets us produce clear, actionable error 
/// messages pointing to the template source line rather than 
/// the generated code.
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

// ------------------------------------------------------------- Private Functions

/// Loop variables (from l-for) are scoped to their loop body, 
/// so the set of valid variables changes as we descend into 
/// nested structures. Threading the loop_vars set through 
/// recursion ensures each variable reference is checked against 
/// the correct scope.
///
fn validate_nodes(
  nodes: List(Node),
  data_fields: Dict(String, String),
  slot_vars: Set(String),
  loop_vars: Set(String),
  source_path: String,
) -> Result(Nil, String) {
  list.try_each(nodes, validate_node(
    _,
    data_fields,
    slot_vars,
    loop_vars,
    source_path,
  ))
}

/// Different node types introduce variables differently —
/// EachNode adds loop variables to scope, IfNode branches are 
/// independent, and leaf nodes just need checking. Dispatching 
/// per node type keeps each validation path focused and ensures 
/// scope changes are applied correctly.
///
fn validate_node(
  node: Node,
  data_fields: Dict(String, String),
  slot_vars: Set(String),
  loop_vars: Set(String),
  source_path: String,
) -> Result(Nil, String) {
  case node {
    parser.IfNode(branches) ->
      validate_if_branches(
        branches,
        data_fields,
        slot_vars,
        loop_vars,
        source_path,
      )

    parser.EachNode(collection, items, loop_var, body, line) -> {
      use _ <- result.try(validate_tuple_arity(
        collection,
        items,
        data_fields,
        source_path,
        line,
      ))

      // Add loop variables to scope for body validation
      let new_loop_vars =
        list.fold(items, loop_vars, fn(acc, item) { set.insert(acc, item) })

      let new_loop_vars = case loop_var {
        Some(lv) -> set.insert(new_loop_vars, lv)
        None -> new_loop_vars
      }

      validate_nodes(body, data_fields, slot_vars, new_loop_vars, source_path)
    }

    parser.ComponentNode(_, _, children) | parser.ElementNode(_, _, children) ->
      validate_nodes(children, data_fields, slot_vars, loop_vars, source_path)

    parser.SlotNode(_, fallback) ->
      validate_nodes(fallback, data_fields, slot_vars, loop_vars, source_path)

    parser.SlotDefNode(_, children) ->
      validate_nodes(children, data_fields, slot_vars, loop_vars, source_path)

    parser.VariableNode(_, _)
    | parser.RawVariableNode(_, _)
    | parser.TextNode(_)
    | parser.AttributesNode(_) -> Ok(Nil)
  }
}

/// A mismatch between the number of destructuring variables and 
/// the tuple arity would produce a cryptic Gleam pattern match 
/// error in generated code. Catching this at the template level 
/// gives the author a clear message about which l-for loop has 
/// the wrong variable count.
///
fn validate_tuple_arity(
  collection: String,
  items: List(String),
  data_fields: Dict(String, String),
  source_path: String,
  line: Int,
) -> Result(Nil, String) {
  let destructure_count = list.length(items)
  // Only validate if we have multiple destructuring variables (tuple pattern)
  use <- bool.guard(destructure_count == 1, Ok(Nil))

  // Get the root variable name (e.g., "items" from "items" or "user.posts" -> "user")
  let root = case string.split(collection, ".") {
    [first, ..] -> first
    [] -> collection
  }

  // Can't validate if we don't know the type
  let type_str = dict.get(data_fields, root)
  use <- bool.guard(result.is_error(type_str), Ok(Nil))
  let assert Ok(type_str) = type_str

  // For dotted access, we can't easily infer the nested type, skip validation
  use <- bool.guard(string.contains(collection, "."), Ok(Nil))

  // Try to extract tuple arity from type like "List(#(String, String, String))"
  case extract_tuple_arity_from_list_type(type_str) {
    None -> Ok(Nil)
    Some(tuple_arity) if destructure_count == tuple_arity -> Ok(Nil)
    Some(tuple_arity) ->
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

/// Tuple arity validation only applies when the collection type 
/// is a List of tuples. Parsing the type string to extract the 
/// tuple size lets us compare it against the destructuring 
/// pattern without requiring a full type system — just enough 
/// to catch common mistakes.
///
fn extract_tuple_arity_from_list_type(type_str: String) -> Option(Int) {
  let trimmed = string.trim(type_str)
  use <- bool.guard(!string.starts_with(trimmed, "List("), None)

  // Remove "List(" prefix and ")" suffix
  let inner =
    trimmed
    |> string.drop_start(5)
    |> string.drop_end(1)
    |> string.trim

  use <- bool.guard(!string.starts_with(inner, "#("), None)

  // Extract the contents of the tuple and count elements
  let tuple_inner =
    inner
    |> string.drop_start(2)
    |> string.drop_end(1)

  Some(count_tuple_elements(tuple_inner))
}

/// Tuple types can contain nested generics with their own
/// commas (e.g., List(String), #(Int, Int)). Counting only
/// commas at nesting depth 0 ensures nested type parameters
/// don't inflate the element count.
///
fn count_tuple_elements(tuple_inner: String) -> Int {
  count_tuple_elements_helper(tuple_inner, 0, 1)
}

/// Tail-recursive walker that tracks parenthesis depth so
/// commas inside nested type parameters (e.g., List(Int)) are 
/// correctly ignored when counting tuple elements.
///
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

/// Each branch in a conditional chain has its own node tree
/// that may reference variables. All branches must be validated 
/// independently since they share the same outer scope but 
/// don't introduce variables visible to sibling branches.
///
fn validate_if_branches(
  branches: List(#(Option(String), Int, List(Node))),
  data_fields: Dict(String, String),
  slot_vars: Set(String),
  loop_vars: Set(String),
  source_path: String,
) -> Result(Nil, String) {
  list.try_each(branches, fn(branch) {
    validate_nodes(branch.2, data_fields, slot_vars, loop_vars, source_path)
  })
}

/// Orchestrates the full module output: header warning, imports, 
/// render function, and optional live functions. Keeping this 
/// as the single assembly point ensures all generated modules 
/// have a consistent structure and the "DO NOT EDIT" header is 
/// never accidentally omitted.
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
    True ->
      generate_live_functions(
        template,
        module_name,
        is_component,
        component_data,
        component_slots,
        handler_lookup,
      )
  }

  string.join([header, imports, "", html_fn, live_fns], "\n")
}

/// Live templates need server-side event handling beyond just 
/// rendering. These extra functions (is_live, handlers, handle, 
/// handle_json, render_json) provide the runtime with 
/// everything it needs for WebSocket-driven interactivity 
/// without external configuration.
///
fn generate_live_functions(
  template: Template,
  module_name: String,
  is_component: Bool,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
  handler_lookup: HandlerLookup,
) -> String {
  let handlers = handler_parser.collect_handlers(template) |> result.unwrap([])

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
  let render_json_fn = generate_render_json_function(template, is_component)

  // Generate tree functions for statics/dynamics
  let tree_fn =
    generate_tree_function(
      template,
      module_name,
      component_data,
      component_slots,
      handler_lookup,
    )
  let tree_json_fn = generate_tree_json_function(template)

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
  <> "\n"
  <> tree_fn
  <> "\n"
  <> tree_json_fn
}

/// A single pass over all handlers determines the union of
/// special variables needed. Scanning once upfront avoids
/// redundant per-handler checks when generating both the typed 
/// handle function and the JSON wrapper.
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

/// The live runtime dispatches events by handler ID and needs 
/// updated prop values back. A typed handle function with a 
/// case expression per handler gives the Gleam compiler full 
/// visibility into the handler logic, catching type errors at 
/// compile time.
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
    [
      #(used_vars.value, "value_: option.Option(String)"),
      #(used_vars.checked, "checked_: option.Option(Bool)"),
      #(used_vars.key, "key_: option.Option(String)"),
    ]
    |> list.filter_map(fn(pair) {
      case pair.0 {
        True -> Ok(pair.1)
        False -> Error(Nil)
      }
    })
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

/// Each handler case binds the expression result to the target 
/// prop(s) and returns all props — not just the changed ones. 
/// Returning the full tuple lets the runtime replace the entire 
/// prop set atomically without tracking which specific props 
/// were modified.
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

/// Template authors write $value/$checked/$key for readability, 
/// but generated Gleam code receives these as Option types from 
/// the JS runtime. Replacing them with option.unwrap calls 
/// provides sensible defaults and avoids forcing authors to 
/// handle Option explicitly.
///
fn transform_special_vars(expr: String) -> String {
  expr
  |> string.replace("$value", "option.unwrap(value_, \"\")")
  |> string.replace("$checked", "option.unwrap(checked_, False)")
  |> string.replace("$key", "option.unwrap(key_, \"\")")
}

/// The WebSocket runtime dispatches events dynamically by
/// module name, so it can't call typed functions directly. This 
/// JSON wrapper provides a uniform String->String interface 
/// that the runtime can call via dynamic dispatch while the 
/// typed handle() stays type-safe.
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

/// After handling an event, the runtime needs to re-render the 
/// template with updated props. Like handle_json, this wrapper 
/// bridges the gap between dynamic dispatch (JSON strings) and 
/// the typed render function so the runtime can trigger 
/// re-renders without knowing prop types.
///
fn generate_render_json_function(
  template: Template,
  is_component: Bool,
) -> String {
  // Generate the props decoder (same as handle_json)
  let props_decoder = generate_props_decoder(template.props)

  // Generate the call to render() with extracted props
  let render_call = generate_render_call(template.props, is_component)

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

/// JSON wrapper functions receive props as a JSON string that 
/// must be decoded into typed Gleam values before calling 
/// handle() or render(). Generating a decoder per template 
/// ensures the correct field names and types are used without 
/// manual boilerplate.
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

/// The JSON wrapper decodes props into a tuple but handle()
/// expects individual named parameters. This function generates 
/// the destructuring and call expression that bridges between 
/// the decoded tuple and the typed function signature.
///
fn generate_handle_call(
  props: List(#(String, String)),
  used_vars: UsedSpecialVars,
) -> String {
  // Build special var args string based on what's used
  let special_args =
    [
      #(used_vars.value, "value_opt"),
      #(used_vars.checked, "checked_opt"),
      #(used_vars.key, "key_opt"),
    ]
    |> list.filter_map(fn(pair) {
      case pair.0 {
        True -> Ok(pair.1)
        False -> Error(Nil)
      }
    })
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

/// Similar to generate_handle_call but for render(). The
/// decoded props tuple must be destructured into named
/// arguments matching the render function's signature so the 
/// JSON wrapper can call render() correctly.
///
fn generate_render_call(
  props: List(#(String, String)),
  is_component: Bool,
) -> String {
  let attrs_arg = case is_component {
    True -> ", attributes: []"
    False -> ""
  }

  case props {
    [] -> "render(" <> case is_component {
      True -> "attributes: []"
      False -> ""
    } <> ")"
    [single] -> "render(" <> single.0 <> ": props" <> attrs_arg <> ")"
    _ -> {
      // Destructure the tuple and pass as named args
      let pattern =
        "#(" <> string.join(list.map(props, fn(p) { p.0 }), ", ") <> ")"
      let args =
        string.join(list.map(props, fn(p) { p.0 <> ": " <> p.0 }), ", ")
      "{ let "
      <> pattern
      <> " = props\n      render("
      <> args
      <> attrs_arg
      <> ") }"
    }
  }
}

/// After handle() returns updated prop values, the JSON wrapper 
/// must serialize them back to JSON for the WebSocket runtime. 
/// Generating the encoder per template ensures each prop is 
/// encoded with the correct JSON type (string, int, bool, etc.).
///
fn generate_result_encoder(props: List(#(String, String))) -> String {
  case props {
    [] -> "      \"{}\"\n"
    [single] -> {
      let encoder = type_to_encoder("result", single.1)
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

/// The props decoder needs type-specific decoders for each
/// field. Mapping from the type annotation string avoids a full 
/// type system while covering the common prop types that 
/// templates actually use.
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

/// The result encoder needs type-specific encoding for each
/// prop. Mirroring type_to_decoder ensures the encode/decode
/// round-trip preserves types correctly between the server and 
/// client.
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

/// Handler IDs must match between the data-l-* attributes in
/// the rendered HTML and the handler dispatch function.
/// Building the lookup once before code generation ensures
/// consistent IDs across both the attribute emission and the 
/// handle function's case branches.
///
fn build_handler_lookup(template: Template) -> HandlerLookup {
  handler_parser.collect_handlers(template)
  |> result.unwrap([])
  |> list.fold(dict.new(), fn(acc, item) {
    let #(id, handler) = item
    let key = #(handler.event, handler.original, handler.line)
    dict.insert(acc, key, id)
  })
}

/// Generated modules need imports for the runtime (always),
/// JSON/decode libraries (live templates only), user-specified
/// imports (@import directives), and every referenced component. 
/// Collecting them here ensures the generated module compiles 
/// without missing dependencies.
///
fn generate_imports(template: Template) -> String {
  // Base imports - runtime is always needed
  let base_imports = ["import glimr/loom/runtime"]

  // For live templates, import additional modules for handle/render JSON wrappers
  let live_imports = case template.is_live {
    True -> {
      let handlers =
        handler_parser.collect_handlers(template) |> result.unwrap([])
      let used_vars = collect_used_special_vars(handlers)
      let needs_option = used_vars.value || used_vars.checked || used_vars.key

      let option_import = case needs_option {
        True -> ["import gleam/option"]
        False -> []
      }

      list.flatten([
        option_import,
        [
          "import gleam/json",
          "import gleam/dynamic/decode",
          "import glimr/loom/loom",
        ],
      ])
    }
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

/// Every component used anywhere in the template — including
/// inside conditionals, loops, and nested slots — needs an
/// import statement. Recursive collection ensures no component 
/// reference is missed regardless of how deeply it is nested.
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

/// Named slots at any depth in the template become parameters 
/// on the generated render function. Collecting them 
/// recursively ensures slots inside conditionals or loops are 
/// still discovered, and using a set prevents duplicate 
/// parameters when the same slot appears twice.
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

/// The render function is the primary output of code generation 
/// — it's what application code calls to turn props into HTML. 
/// Components get automatic @attributes injection into the root 
/// element so parent-provided attributes pass through without 
/// explicit placement.
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

/// Live templates need a container div with data attributes and 
/// a script tag for the WebSocket client. Injecting these via 
/// runtime.inject_live_wrapper at the body tag ensures correct 
/// HTML structure even when the template uses layout components 
/// that produce the <body> tag.
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

/// The render function's parameter list must include every
/// piece of data the template can reference: declared props,
/// slot content strings, and the attributes list (for
/// components). Building it from template metadata ensures the 
/// generated signature is always in sync with usage.
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

/// The render function only includes a `slot` parameter if the 
/// template actually uses <slot /> somewhere. Checking
/// recursively covers slots nested inside conditionals or loops, 
/// so the parameter isn't omitted when a slot appears only in a 
/// branch.
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

/// When generating a component call, slot definitions (content 
/// passed to named slots) must be rendered as separate 
/// arguments, not as part of the default child content. 
/// Splitting them here lets the generator handle default 
/// children and named slots independently.
///
fn separate_slot_defs(
  nodes: List(Node),
) -> #(List(Node), List(#(Option(String), List(Node)))) {
  let #(default_nodes, slot_defs) =
    list.fold(nodes, #([], []), fn(acc, node) {
      let #(defaults, slots) = acc
      case node {
        parser.SlotDefNode(name, children) -> #(defaults, [
          #(name, children),
          ..slots
        ])
        _ -> #([node, ..defaults], slots)
      }
    })
  #(list.reverse(default_nodes), list.reverse(slot_defs))
}

/// Gleam guards don't support function calls, but template
/// conditions often call functions (e.g., list.is_empty). Using 
/// `case condition { True -> ... False -> ... }` instead of 
/// guards allows arbitrary expressions in conditions while 
/// still producing exhaustive matches.
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

/// Else-if chains become nested case expressions where each
/// False branch contains the next condition's case. This
/// recursive structure mirrors the if/else-if/else chain
/// naturally, producing correctly indented and exhaustive Gleam 
/// code for any chain length.
///
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

/// Template authors write `l-if="slot"` or `l-if="slot.header"`
/// to conditionally render based on slot content, but slots are 
/// String parameters in generated code. Transforming these to 
/// emptiness checks lets authors use intuitive slot references 
/// without knowing the String representation.
///
fn transform_slot_condition(condition: String) -> String {
  case string.trim(condition) {
    "slot" -> "slot != \"\""
    "slot." <> name -> "slot_" <> to_field_name(name) <> " != \"\""
    _ -> condition
  }
}

/// Convenience entry point for node code generation when no 
/// loop variables are in scope (i.e., at the top level). Avoids 
/// passing an empty set through every call site that doesn't 
/// involve l-for loops.
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

/// Loop variables have properties like loop.index (Int) and
/// loop.first (Bool) that need type conversion when used in 
/// string concatenation. Threading the set of active loop 
/// variables through code generation lets the emitter insert 
/// the right conversion calls.
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

/// Each AST node type maps to a different code generation
/// strategy — text becomes string literals, variables become 
/// runtime.display calls, slots become parameter references, 
/// and components become function calls. Exhaustive pattern 
/// matching ensures every node type produces valid Gleam code.
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
      let converted_expr = convert_loop_var_expr(expr)
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

/// Component attributes serve dual roles: some are props
/// (passed as function arguments) and others are HTML
/// attributes (passed through to the root element). Separating 
/// them here ensures props reach the render function while HTML 
/// attributes flow to the DOM, with type-appropriate defaults 
/// for omitted props.
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
        lexer.LmOn(event, modifiers, handler, line) ->
          generate_handler_attr_code(
            event,
            modifiers,
            handler,
            line,
            handler_lookup,
          )
        lexer.LmModel(prop, line) ->
          generate_model_attr_code(prop, line, handler_lookup)
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

/// When a parent template omits a component prop, the generated 
/// code still needs a valid value to compile. Type-based 
/// defaults (False for Bool, "" for String, etc.) provide the 
/// safest zero-value semantics so partial component usage 
/// doesn't cause compile errors.
///
fn default_for_type(type_str: String) -> String {
  case type_str {
    "Bool" -> "False"
    "String" -> "\"\""
    "Int" -> "0"
    "Float" -> "0.0"
    "Option(" <> _ -> "None"
    "List(" <> _ -> "[]"
    _ -> "todo"
  }
}

/// HTML boolean attributes (disabled, checked, etc.) must be 
/// rendered differently — their presence alone means true, and 
/// they should be omitted entirely when false. Identifying them 
/// lets the generator emit BoolAttribute instead of Attribute 
/// for correct rendering behavior.
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

/// Named slots provided by the parent must become arguments in 
/// the component's render call, and any expected slots not 
/// provided must be filled with empty strings. Loop variable 
/// tracking is threaded through because slot content can 
/// reference variables from enclosing l-for loops.
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

/// Element attributes (unlike component attributes) are all
/// HTML attributes — there's no prop/attribute split. They
/// still need conversion to runtime constructors because
/// different attribute types (string, expression, boolean,
/// class, style, handler) have different rendering logic.
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
            lexer.LmOn(event, modifiers, handler, line) ->
              generate_handler_attr_code(
                event,
                modifiers,
                handler,
                line,
                handler_lookup,
              )
            lexer.LmModel(prop, line) ->
              generate_model_attr_code(prop, line, handler_lookup)
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

/// l-on:* handlers become data-l-{event} attributes that the
/// client-side JS runtime reads to wire up event listeners.
/// Looking up the handler ID from the pre-built map ensures the 
/// attribute value matches the server-side dispatch function. 
/// Shared by both component and element codegen.
///
fn generate_handler_attr_code(
  event: String,
  modifiers: List(String),
  handler: String,
  line: Int,
  handler_lookup: HandlerLookup,
) -> Result(String, Nil) {
  use handler_id <- result.try(
    dict.get(handler_lookup, #(event, handler, line))
    |> result.replace_error(Nil),
  )
  let handler_attr =
    "runtime.Attribute(\"data-l-" <> event <> "\", \"" <> handler_id <> "\")"

  let modifier_attrs =
    modifiers
    |> list.map(fn(m) {
      case string.starts_with(m, "debounce") {
        True -> {
          let value = case string.split(m, "-") {
            ["debounce", ms] -> ms
            _ -> "150"
          }
          "runtime.Attribute(\"data-l-debounce\", \"" <> value <> "\")"
        }
        False -> "runtime.Attribute(\"data-l-" <> m <> "\", \"true\")"
      }
    })
    |> string.join(", ")

  case modifier_attrs {
    "" -> Ok(handler_attr)
    _ -> Ok(handler_attr <> ", " <> modifier_attrs)
  }
}

/// l-model is syntactic sugar for l-on:input with $value
/// assignment. The generated attribute uses data-l-input with 
/// the same handler ID that the desugared handler received 
/// during collection, maintaining consistency between markup 
/// and dispatch.
///
fn generate_model_attr_code(
  prop: String,
  line: Int,
  handler_lookup: HandlerLookup,
) -> Result(String, Nil) {
  let handler_str = prop <> " = $value"
  case dict.get(handler_lookup, #("input", handler_str, line)) {
    Ok(handler_id) ->
      Ok("runtime.Attribute(\"data-l-input\", \"" <> handler_id <> "\")")
    Error(_) -> Error(Nil)
  }
}

/// Void elements must be rendered as self-closing tags (<img />)
/// rather than with a closing tag (</img>), which would be 
/// invalid HTML. Identifying them here lets the element code 
/// generator emit the correct syntax per HTML specification.
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

/// Base attributes from the component's root element need to be 
/// passed to runtime.merge_attributes alongside the 
/// parent-provided attributes. Converting them to runtime
/// constructors here lets the merge happen at render time so 
/// parent attributes can override component defaults.
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

/// Components without an explicit @attributes directive get
/// automatic injection into the root element. Checking for
/// existing directives first prevents double injection when the 
/// author has already placed @attributes where they want it.
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

/// Most component authors expect parent attributes to land on 
/// the root element without having to write @attributes
/// explicitly. Auto-injecting into the first element provides 
/// this convenient default behavior while still allowing 
/// explicit placement when needed.
///
fn inject_attributes_into_first_element(nodes: List(Node)) -> List(Node) {
  inject_attributes_helper(nodes, False).0
}

/// The first HTML element may be inside a conditional or loop, 
/// not at the top level. Recursing into control flow nodes with 
/// an "injected" flag ensures we find the first element 
/// regardless of nesting, and the flag prevents injecting into 
/// more than one branch.
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

/// When a component's root element is inside a conditional,
/// each branch may have its own first element. Injecting into 
/// all branches ensures attributes are applied regardless of 
/// which branch renders, while the injected flag propagates to 
/// avoid duplicate injection.
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

/// Text nodes from the lexer may contain raw HTML tags that
/// weren't parsed as elements (e.g., in static content). To 
/// inject @attributes, we need to find the first tag in the 
/// text, split it open, and insert the attributes node between 
/// the tag name and closing bracket.
///
fn find_first_tag_and_extract_attrs(
  text: String,
) -> Option(#(String, List(lexer.ComponentAttr), String)) {
  case string.split_once(text, "<") {
    Error(_) -> None
    Ok(#(before_tag, after_open)) ->
      parse_tag_content(after_open)
      |> option.map(fn(parsed) {
        let #(tag_name, attrs, closing) = parsed
        #(before_tag <> "<" <> tag_name <> " ", attrs, closing)
      })
  }
}

/// After splitting on '<', we need to distinguish real element 
/// tags from closing tags, comments, and doctype declarations. 
/// Parsing the tag name and any existing attributes lets 
/// inject_attributes_helper preserve the original attributes 
/// during injection.
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

/// Tag names and unquoted attribute values are terminated by
/// whitespace or tag delimiters (>, /). A shared extraction
/// function avoids duplicating this character-by-character
/// scanning logic across multiple parsing contexts.
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

/// Existing attributes on the root element must be preserved
/// as base attributes for runtime.merge_attributes. Parsing
/// them into structured ComponentAttr values lets the generator 
/// emit correct merge code that combines the element's own 
/// attributes with parent-provided ones.
///
fn parse_html_attributes(
  input: String,
  acc: List(lexer.ComponentAttr),
) -> #(List(lexer.ComponentAttr), String) {
  let input = string.trim_start(input)

  case string.pop_grapheme(input) {
    Error(_) -> #(list.reverse(acc), "")
    Ok(#(">", rest)) -> #(list.reverse(acc), ">" <> rest)
    Ok(#("/", rest)) -> #(list.reverse(acc), "/" <> rest)
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

/// HTML allows multiple attribute syntax forms (double-quoted,
/// single-quoted, unquoted, valueless boolean). Supporting all 
/// of them here means component authors can write natural HTML 
/// without being constrained to a single quoting style for 
/// their root element attributes.
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

/// Attribute names are terminated by =, whitespace, or tag
/// delimiters. Extracting the name as a separate step lets the 
/// caller decide how to handle the value based on what follows 
/// — an = means a valued attribute, anything else means a 
/// boolean attribute.
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

/// Quoted attribute values can contain any character except
/// their closing quote. A parameterized stop character lets 
/// this single function handle both single-quoted and 
/// double-quoted values without duplication.
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

/// HTML attribute names use dashes and colons (data-value,
/// aria-label) but Gleam identifiers require snake_case.
/// Normalizing here ensures generated field names are valid 
/// Gleam regardless of the naming conventions used in the 
/// template markup.
///
fn to_field_name(name: String) -> String {
  name
  |> string.replace("-", "_")
  |> string.replace(":", "_")
  |> string.replace(" ", "_")
  |> to_snake_case
}

/// Component names and slot names may use PascalCase or
/// camelCase in templates, but Gleam conventions and module
/// paths require snake_case. Converting here ensures generated 
/// identifiers follow Gleam naming rules and match expected 
/// module paths.
///
fn to_snake_case(input: String) -> String {
  input
  |> string.to_graphemes
  |> list.fold("", fn(acc, char) {
    case string_util.is_uppercase_letter(char) {
      True -> acc <> "_" <> string.lowercase(char)
      False -> acc <> char
    }
  })
  |> strip_leading_underscores
}

/// Snake_case conversion of names starting with uppercase (e.g., 
/// "MyComponent" -> "_my_component") produces a leading 
/// underscore. Stripping it prevents generating identifiers that 
/// Gleam would interpret as unused variables.
///
fn strip_leading_underscores(input: String) -> String {
  case input {
    "_" <> rest -> strip_leading_underscores(rest)
    _ -> input
  }
}

/// Template text nodes are emitted as Gleam string literals, so 
/// characters that have special meaning in Gleam strings
/// (backslashes, quotes, newlines) must be escaped. Without 
/// this, a template containing a literal backslash or quote
/// would produce syntactically invalid generated code.
///
fn escape_gleam_string(input: String) -> String {
  input
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\r", "\\r")
  |> string.replace("\t", "\\t")
}

/// Component names like "ui:button" or "nav-bar" use characters 
/// invalid in Gleam identifiers. The alias (e.g., 
/// components_ui_button) provides a safe identifier for the 
/// import statement that the generated render calls can 
/// reference.
///
fn component_module_alias(name: String) -> String {
  "components_"
  <> name
  |> string.replace(":", "_")
  |> string.replace("-", "_")
}

/// Bare strings in :class lists are always-on class names, but 
/// the runtime expects uniform #(String, Bool) tuples. Wrapping 
/// them with runtime.class() (which returns #(name, True)) 
/// normalizes the list so build_classes can process all 
/// entries uniformly.
///
fn transform_class_list(value: String) -> String {
  transform_list_strings(value, "runtime.class")
}

/// Same pattern as transform_class_list but for :style. Bare 
/// strings in :style lists are always-on style rules, and 
/// wrapping with runtime.style() normalizes them into
/// #(String, Bool) tuples for uniform processing by 
/// build_styles.
///
fn transform_style_list(value: String) -> String {
  transform_list_strings(value, "runtime.style")
}

/// Only top-level string literals need wrapping — strings
/// inside tuples like #("foo", cond) are already part of the 
/// expected format. Tracking nesting depth ensures only the 
/// right strings get wrapped while leaving tuple contents 
/// untouched.
///
fn transform_list_strings(value: String, wrapper: String) -> String {
  let chars = string.to_graphemes(value)
  transform_list_chars(chars, wrapper, 0, False, "", "")
}

/// Character-by-character processing is needed because the list 
/// expression can contain escaped quotes, nested parentheses, 
/// and commas inside tuples. A simple regex or split-based 
/// approach would misidentify boundaries in expressions like 
/// ["a\"b", #("c", True)].
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

/// Template variables can be any type (String, Int, Bool, etc.) 
/// but string concatenation requires strings. runtime.display() 
/// handles type conversion and HTML escaping in one call, 
/// preventing XSS while supporting non-string types without 
/// explicit conversion.
///
fn convert_loop_var_expr(expr: String) -> String {
  "runtime.display(" <> expr <> ")"
}

/// Raw variables ({!! expr !!}) skip HTML escaping for cases
/// like pre-rendered HTML. Loop properties (loop.index,
/// loop.first) still need type conversion since they're Int/Bool,
/// but regular raw variables are assumed to be strings already 
/// and are passed through directly.
///
fn convert_loop_var_expr_raw(expr: String, loop_vars: Set(String)) -> String {
  case get_loop_property(expr, loop_vars) {
    Some(_) -> "runtime.to_string(" <> expr <> ")"
    None -> expr
  }
}

/// Loop variable properties (loop.index, loop.first, etc.) have
/// non-String types that need conversion for string output. 
/// Identifying them by checking if the expression root is a 
/// known loop variable lets the generator insert the right 
/// conversion without a full type system.
///
fn get_loop_property(
  expr: String,
  loop_vars: Set(String),
) -> Option(#(String, String)) {
  case string.split_once(expr, ".") {
    Error(_) -> None
    Ok(#(root, rest)) -> {
      use <- bool.guard(!set.contains(loop_vars, root), None)
      let property = case string.split_once(rest, ".") {
        Ok(#(prop, _)) -> prop
        Error(_) -> rest
      }
      Some(#(root, property))
    }
  }
}

// ------------------------------------------------------------- Tree Code Generation

/// Generates the render_tree() function for a live template.
/// Extracts children from the root layout component and walks
/// them with the tree code generator.
///
fn generate_tree_function(
  template: Template,
  _module_name: String,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
  handler_lookup: HandlerLookup,
) -> String {
  // Extract the content nodes for the tree.
  // For templates with a root layout component, extract the default slot children.
  let content_nodes = extract_tree_content_nodes(template.nodes)

  // Build function parameters (same as render, minus slots/attributes)
  let prop_params =
    template.props
    |> list.map(fn(prop) {
      let #(name, type_str) = prop
      name <> " " <> name <> ": " <> type_str
    })
    |> string.join(", ")

  // Generate the tree body
  let body =
    generate_tree_body(
      content_nodes,
      1,
      component_data,
      component_slots,
      set.new(),
      handler_lookup,
    )

  "/// Returns a LiveTree with statics/dynamics split for efficient\n"
  <> "/// WebSocket patching. Only the content inside the layout is included.\n"
  <> "///\n"
  <> "pub fn render_tree("
  <> prop_params
  <> ") -> loom.LiveTree {\n"
  <> body
  <> "}\n"
}

/// Generates the render_tree_json() wrapper for dynamic dispatch.
///
fn generate_tree_json_function(template: Template) -> String {
  let props_decoder = generate_props_decoder(template.props)
  let render_tree_call = generate_render_tree_call(template.props)

  "/// JSON wrapper for render_tree() — used by live_socket for dynamic dispatch.\n"
  <> "/// Returns the tree as a JSON string.\n"
  <> "///\n"
  <> "pub fn render_tree_json(props_json: String) -> String {\n"
  <> "  let decoder = {\n"
  <> props_decoder
  <> "  }\n"
  <> "\n"
  <> "  case json.parse(props_json, decoder) {\n"
  <> "    Ok(props) -> "
  <> render_tree_call
  <> "\n"
  <> "    Error(_) -> \"{}\"  // Error fallback\n"
  <> "  }\n"
  <> "}\n"
}

/// Generate the call expression for render_tree, matching
/// the pattern used by generate_render_call.
///
fn generate_render_tree_call(props: List(#(String, String))) -> String {
  case props {
    [] -> "runtime.tree_to_json(render_tree())"
    [single] ->
      "runtime.tree_to_json(render_tree(" <> single.0 <> ": props))"
    _ -> {
      let pattern =
        "#(" <> string.join(list.map(props, fn(p) { p.0 }), ", ") <> ")"
      let args =
        string.join(list.map(props, fn(p) { p.0 <> ": " <> p.0 }), ", ")
      "{ let "
      <> pattern
      <> " = props\n      runtime.tree_to_json(render_tree("
      <> args
      <> ")) }"
    }
  }
}

/// Extract the content nodes for tree rendering. For live
/// templates with a root layout component, extract the default
/// slot children (skipping SlotDefNodes). Otherwise use all nodes.
///
fn extract_tree_content_nodes(nodes: List(Node)) -> List(Node) {
  case nodes {
    [parser.ComponentNode(name, _, children)]
    | [parser.TextNode(_), parser.ComponentNode(name, _, children)]
    | [parser.ComponentNode(name, _, children), parser.TextNode(_)]
    | [
        parser.TextNode(_),
        parser.ComponentNode(name, _, children),
        parser.TextNode(_),
      ]
    -> {
      case string.contains(name, "layouts") {
        True -> {
          // Extract default slot children (non-SlotDefNode)
          let #(default_children, _) = separate_slot_defs(children)
          default_children
        }
        False -> nodes
      }
    }
    _ -> nodes
  }
}

/// State accumulator for tree code generation. Tracks the
/// current static fragment being built and the list of dynamic
/// expressions collected so far.
///
type TreeAcc {
  TreeAcc(
    /// Current static fragment being accumulated
    current_static: String,
    /// Dynamic expressions collected (in reverse order)
    dynamics: List(String),
    /// Static fragments collected (in reverse order)
    statics: List(String),
  )
}

/// Generate the body of a render_tree function. Walks the nodes
/// and produces a LiveTree(...) expression.
///
fn generate_tree_body(
  nodes: List(Node),
  indent: Int,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
  loop_vars: Set(String),
  handler_lookup: HandlerLookup,
) -> String {
  let pad = string.repeat("  ", indent)

  let acc =
    list.fold(
      nodes,
      TreeAcc(current_static: "", dynamics: [], statics: []),
      fn(acc, node) {
        generate_node_tree(
          node,
          acc,
          indent,
          component_data,
          component_slots,
          loop_vars,
          handler_lookup,
        )
      },
    )

  // Close the final static fragment
  let final_statics = list.reverse([acc.current_static, ..acc.statics])
  let final_dynamics = list.reverse(acc.dynamics)

  let statics_code =
    final_statics
    |> list.map(fn(s) { pad <> "  \"" <> escape_gleam_string(s) <> "\"" })
    |> string.join(",\n")

  let dynamics_code =
    final_dynamics
    |> list.map(fn(d) { pad <> "  " <> d })
    |> string.join(",\n")

  let statics_inner = case statics_code {
    "" -> ""
    code -> code <> ",\n"
  }

  let dynamics_inner = case dynamics_code {
    "" -> ""
    code -> code <> ",\n"
  }

  pad
  <> "loom.LiveTree(\n"
  <> pad
  <> "  statics: [\n"
  <> statics_inner
  <> pad
  <> "  ],\n"
  <> pad
  <> "  dynamics: [\n"
  <> dynamics_inner
  <> pad
  <> "  ],\n"
  <> pad
  <> ")\n"
}

/// Process a single node for tree code generation.
/// Text becomes static fragments, variables become dynamics,
/// control flow nodes become DynTree/DynList dynamics.
///
fn generate_node_tree(
  node: Node,
  acc: TreeAcc,
  indent: Int,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
  loop_vars: Set(String),
  handler_lookup: HandlerLookup,
) -> TreeAcc {
  case node {
    parser.TextNode(text) -> {
      TreeAcc(..acc, current_static: acc.current_static <> text)
    }

    parser.VariableNode(expr, _line) -> {
      // Close current static, push DynString
      TreeAcc(
        current_static: "",
        dynamics: [
          "loom.DynString(runtime.display(" <> expr <> "))",
          ..acc.dynamics
        ],
        statics: [acc.current_static, ..acc.statics],
      )
    }

    parser.RawVariableNode(expr, _line) -> {
      let converted_expr = convert_loop_var_expr_raw(expr, loop_vars)
      TreeAcc(
        current_static: "",
        dynamics: [
          "loom.DynString(" <> converted_expr <> ")",
          ..acc.dynamics
        ],
        statics: [acc.current_static, ..acc.statics],
      )
    }

    parser.IfNode(branches) -> {
      let tree_code =
        generate_if_tree_code(
          branches,
          indent + 1,
          component_data,
          component_slots,
          loop_vars,
          handler_lookup,
        )
      TreeAcc(
        current_static: "",
        dynamics: [
          "loom.DynTree(" <> tree_code <> ")",
          ..acc.dynamics
        ],
        statics: [acc.current_static, ..acc.statics],
      )
    }

    parser.EachNode(collection, items, loop_var, body, _line) -> {
      let new_loop_vars = case loop_var {
        Some(lv) -> set.insert(loop_vars, lv)
        None -> loop_vars
      }
      let each_code =
        generate_each_tree_code(
          collection,
          items,
          loop_var,
          body,
          indent + 1,
          component_data,
          component_slots,
          new_loop_vars,
          handler_lookup,
        )
      TreeAcc(
        current_static: "",
        dynamics: [each_code, ..acc.dynamics],
        statics: [acc.current_static, ..acc.statics],
      )
    }

    parser.ComponentNode(name, attributes, children) -> {
      // Components are treated as opaque DynString
      let component_code =
        generate_component_render_expr(
          name,
          attributes,
          children,
          indent,
          component_data,
          component_slots,
          loop_vars,
          handler_lookup,
        )
      TreeAcc(
        current_static: "",
        dynamics: [
          "loom.DynString(" <> component_code <> ")",
          ..acc.dynamics
        ],
        statics: [acc.current_static, ..acc.statics],
      )
    }

    parser.ElementNode(tag, attributes, children) -> {
      // For elements, the tag and static attrs are static,
      // dynamic attrs (event handlers, expressions) split further
      generate_element_tree(
        tag,
        attributes,
        children,
        acc,
        indent,
        component_data,
        component_slots,
        loop_vars,
        handler_lookup,
      )
    }

    parser.SlotNode(None, []) -> {
      // Slot becomes dynamic (its content may change)
      TreeAcc(
        current_static: "",
        dynamics: ["loom.DynString(slot)", ..acc.dynamics],
        statics: [acc.current_static, ..acc.statics],
      )
    }

    parser.SlotNode(_, _)
    | parser.SlotDefNode(_, _)
    | parser.AttributesNode(_) -> acc
  }
}

/// Generate a DynTree case expression for if/else-if/else branches.
/// Each branch produces its own LiveTree.
///
fn generate_if_tree_code(
  branches: List(#(Option(String), Int, List(Node))),
  indent: Int,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
  loop_vars: Set(String),
  handler_lookup: HandlerLookup,
) -> String {
  let pad = string.repeat("  ", indent)
  generate_if_tree_branches(
    branches,
    indent,
    pad,
    component_data,
    component_slots,
    loop_vars,
    handler_lookup,
  )
}

fn generate_if_tree_branches(
  branches: List(#(Option(String), Int, List(Node))),
  indent: Int,
  pad: String,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
  loop_vars: Set(String),
  handler_lookup: HandlerLookup,
) -> String {
  case branches {
    [] -> "loom.LiveTree(statics: [\"\"], dynamics: [])"
    [#(None, _line, body), ..] -> {
      // Else branch
      let body_code =
        generate_tree_body(
          body,
          indent + 1,
          component_data,
          component_slots,
          loop_vars,
          handler_lookup,
        )
      "{\n" <> body_code <> pad <> "}"
    }
    [#(Some(cond), _line, body), ..rest] -> {
      let transformed_cond = transform_slot_condition(cond)
      let body_code =
        generate_tree_body(
          body,
          indent + 2,
          component_data,
          component_slots,
          loop_vars,
          handler_lookup,
        )
      let else_code = case rest {
        [] ->
          pad
          <> "  False -> loom.LiveTree(statics: [\"\"], dynamics: [])\n"
        [#(None, _, else_body)] -> {
          let else_body_code =
            generate_tree_body(
              else_body,
              indent + 2,
              component_data,
              component_slots,
              loop_vars,
              handler_lookup,
            )
          pad <> "  False -> {\n" <> else_body_code <> pad <> "  }\n"
        }
        _ -> {
          let nested =
            generate_if_tree_branches(
              rest,
              indent + 1,
              pad <> "  ",
              component_data,
              component_slots,
              loop_vars,
              handler_lookup,
            )
          pad <> "  False -> " <> nested <> "\n"
        }
      }
      "case "
      <> transformed_cond
      <> " {\n"
      <> pad
      <> "  True -> {\n"
      <> body_code
      <> pad
      <> "  }\n"
      <> else_code
      <> pad
      <> "}"
    }
  }
}

/// Generate tree code for an l-for loop. Returns a DynList
/// expression using runtime.map_each or runtime.map_each_with_loop.
///
fn generate_each_tree_code(
  collection: String,
  items: List(String),
  loop_var: Option(String),
  body: List(Node),
  indent: Int,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
  loop_vars: Set(String),
  handler_lookup: HandlerLookup,
) -> String {
  let pad = string.repeat("  ", indent)
  let body_code =
    generate_tree_body(
      body,
      indent + 1,
      component_data,
      component_slots,
      loop_vars,
      handler_lookup,
    )

  // Handle tuple destructuring
  let #(param_name, destructure_code) = case items {
    [single] -> #(single, "")
    multiple -> {
      let pattern = "#(" <> string.join(multiple, ", ") <> ")"
      #("item__", pad <> "  let " <> pattern <> " = item__\n")
    }
  }

  case loop_var {
    None ->
      "runtime.map_each("
      <> collection
      <> ", fn("
      <> param_name
      <> ") {\n"
      <> destructure_code
      <> body_code
      <> pad
      <> "})"
    Some(loop_name) ->
      "runtime.map_each_with_loop("
      <> collection
      <> ", fn("
      <> param_name
      <> ", "
      <> loop_name
      <> ") {\n"
      <> destructure_code
      <> body_code
      <> pad
      <> "})"
  }
}

/// Generate a component render expression as a string value
/// (for DynString wrapping). Produces the same component call
/// as the normal codegen path but as an expression rather than
/// a pipe append.
///
fn generate_component_render_expr(
  name: String,
  attributes: List(lexer.ComponentAttr),
  children: List(Node),
  indent: Int,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
  loop_vars: Set(String),
  handler_lookup: HandlerLookup,
) -> String {
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

  let pad = string.repeat("  ", indent)

  let slot_info = dict.get(component_slots, name)
  let component_has_slot = case slot_info {
    Ok(info) -> info.has_default_slot
    Error(_) -> True
  }

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

  module_alias
  <> ".render(\n"
  <> props_code
  <> named_slots_code
  <> slot_code
  <> pad
  <> "    attributes: "
  <> extra_attrs_code
  <> ",\n"
  <> pad
  <> "  )"
}

/// Generate tree code for an element node. Static parts of the
/// tag (name, static attributes) go into the static fragment.
/// Dynamic attributes split the static and create DynString slots.
///
fn generate_element_tree(
  tag: String,
  attributes: List(lexer.ComponentAttr),
  children: List(Node),
  acc: TreeAcc,
  indent: Int,
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
  loop_vars: Set(String),
  handler_lookup: HandlerLookup,
) -> TreeAcc {
  // <template> is a phantom wrapper — only render children
  case tag {
    "template" ->
      list.fold(children, acc, fn(acc, child) {
        generate_node_tree(
          child,
          acc,
          indent,
          component_data,
          component_slots,
          loop_vars,
          handler_lookup,
        )
      })
    _ -> {
      // Build the opening tag. Static attrs go into the current static
      // fragment; dynamic attrs (expressions, :class, :style) create
      // dynamic slots.
      let acc = TreeAcc(..acc, current_static: acc.current_static <> "<" <> tag)
      let acc =
        generate_element_attrs_tree(acc, attributes, handler_lookup)

      case is_void_element(tag) {
        True -> TreeAcc(..acc, current_static: acc.current_static <> " />")
        False -> {
          let acc = TreeAcc(..acc, current_static: acc.current_static <> ">")
          // Process children recursively
          let acc =
            list.fold(children, acc, fn(acc, child) {
              generate_node_tree(
                child,
                acc,
                indent,
                component_data,
                component_slots,
                loop_vars,
                handler_lookup,
              )
            })
          TreeAcc(..acc, current_static: acc.current_static <> "</" <> tag <> ">")
        }
      }
    }
  }
}

/// Process element attributes for tree mode. Static attributes
/// are appended to current_static; dynamic attributes (expressions,
/// :class, :style) close the static fragment, push a DynString,
/// and start a new static fragment.
///
fn generate_element_attrs_tree(
  acc: TreeAcc,
  attrs: List(lexer.ComponentAttr),
  handler_lookup: HandlerLookup,
) -> TreeAcc {
  list.fold(attrs, acc, fn(acc, attr) {
    case attr {
      lexer.StringAttr(name, value) ->
        TreeAcc(
          ..acc,
          current_static: acc.current_static
            <> " "
            <> name
            <> "=\""
            <> value
            <> "\"",
        )
      lexer.BoolAttr(name) ->
        TreeAcc(..acc, current_static: acc.current_static <> " " <> name)
      lexer.LmOn(event, modifiers, handler, line) -> {
        case dict.get(handler_lookup, #(event, handler, line)) {
          Ok(handler_id) -> {
            let base =
              " data-l-" <> event <> "=\"" <> handler_id <> "\""
            let mod_str =
              modifiers
              |> list.map(fn(m) {
                case string.starts_with(m, "debounce") {
                  True -> {
                    let value = case string.split(m, "-") {
                      ["debounce", ms] -> ms
                      _ -> "150"
                    }
                    " data-l-debounce=\"" <> value <> "\""
                  }
                  False -> " data-l-" <> m <> "=\"true\""
                }
              })
              |> string.join("")
            TreeAcc(
              ..acc,
              current_static: acc.current_static <> base <> mod_str,
            )
          }
          Error(_) -> acc
        }
      }
      lexer.LmModel(prop, line) -> {
        let handler_str = prop <> " = $value"
        case dict.get(handler_lookup, #("input", handler_str, line)) {
          Ok(handler_id) -> {
            // Add data-l-input attr as static, plus a dynamic value attr
            let acc =
              TreeAcc(
                ..acc,
                current_static: acc.current_static
                  <> " data-l-input=\""
                  <> handler_id
                  <> "\" value=\"",
              )
            // The value is dynamic (it's the prop expression)
            TreeAcc(
              current_static: "\"",
              dynamics: [
                "loom.DynString(runtime.escape(" <> prop <> "))",
                ..acc.dynamics
              ],
              statics: [acc.current_static, ..acc.statics],
            )
          }
          Error(_) -> acc
        }
      }
      lexer.ExprAttr(name, value) -> {
        case is_html_boolean_attribute(name) {
          True -> {
            // Boolean HTML attribute: rendered as name-only when true, omitted when false
            // This requires a dynamic slot
            TreeAcc(
              current_static: "",
              dynamics: [
                "loom.DynString(case "
                  <> value
                  <> " { True -> \""
                  <> name
                  <> "\" False -> \"\" })",
                ..acc.dynamics
              ],
              statics: [acc.current_static <> " ", ..acc.statics],
            )
          }
          False -> {
            // Regular expression attribute: name is static, value is dynamic
            let acc =
              TreeAcc(
                ..acc,
                current_static: acc.current_static
                  <> " "
                  <> name
                  <> "=\"",
              )
            TreeAcc(
              current_static: "\"",
              dynamics: [
                "loom.DynString(runtime.escape(" <> value <> "))",
                ..acc.dynamics
              ],
              statics: [acc.current_static, ..acc.statics],
            )
          }
        }
      }
      lexer.ClassAttr(value) -> {
        let acc =
          TreeAcc(
            ..acc,
            current_static: acc.current_static <> " class=\"",
          )
        TreeAcc(
          current_static: "\"",
          dynamics: [
            "loom.DynString(runtime.escape(runtime.build_classes("
              <> transform_class_list(value)
              <> ")))",
            ..acc.dynamics
          ],
          statics: [acc.current_static, ..acc.statics],
        )
      }
      lexer.StyleAttr(value) -> {
        let acc =
          TreeAcc(
            ..acc,
            current_static: acc.current_static <> " style=\"",
          )
        TreeAcc(
          current_static: "\"",
          dynamics: [
            "loom.DynString(runtime.escape(runtime.build_styles("
              <> transform_style_list(value)
              <> ")))",
            ..acc.dynamics
          ],
          statics: [acc.current_static, ..acc.statics],
        )
      }
      // Control flow attributes are not element attributes
      lexer.LmIf(_, _)
      | lexer.LmElseIf(_, _)
      | lexer.LmElse
      | lexer.LmFor(_, _, _, _) -> acc
    }
  })
}
