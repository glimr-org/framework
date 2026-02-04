import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import glimr/loom/generator
import glimr/loom/lexer.{ClassAttr, ExprAttr, StringAttr, StyleAttr}
import glimr/loom/parser.{
  type Node, type Template, AttributesNode, ComponentNode, EachNode, IfNode,
  RawVariableNode, SlotDefNode, SlotNode, Template, TextNode, VariableNode,
}

// Helper to create a template (simplified after layout removal)
fn template(nodes: List(Node)) -> Template {
  Template(nodes: nodes)
}

// Helper to create a simple if node (single branch, no else)
fn if_node(condition: String, body: List(Node)) -> Node {
  IfNode([#(Some(condition), body)])
}

// Helper to create an if/else node
fn if_else_node(
  condition: String,
  if_body: List(Node),
  else_body: List(Node),
) -> Node {
  IfNode([#(Some(condition), if_body), #(None, else_body)])
}

// Helper to create an if/elseif/else node
fn if_elseif_else_node(
  branches: List(#(String, List(Node))),
  else_body: List(Node),
) -> Node {
  let if_branches =
    branches
    |> list.map(fn(b) { #(Some(b.0), b.1) })
  IfNode(list.append(if_branches, [#(None, else_body)]))
}

// Helper to call generator with common defaults (6 args after layout removal)
fn generate(
  tmpl: Template,
  name: String,
  is_component: Bool,
) -> generator.GeneratedCode {
  generator.generate(tmpl, name, is_component, None, dict.new(), dict.new())
}

// ------------------------------------------------------------- Basic Generation Tests

pub fn generate_empty_template_test() {
  let result = generate(template([]), "empty", False)

  result.module_name
  |> should.equal("empty")

  // Should have html function with no params (no view file = no data params)
  result.code
  |> string.contains("pub fn html() -> String")
  |> should.be_true
}

pub fn generate_text_only_test() {
  let result = generate(template([TextNode("Hello!")]), "hello", False)

  result.code
  |> string.contains("<> \"Hello!\"")
  |> should.be_true

  // Should have html function with no params (no view file = no data params)
  result.code
  |> string.contains("pub fn html() -> String")
  |> should.be_true
}

pub fn generate_variable_test() {
  // With new architecture, template uses explicit data.name
  let result = generate(template([VariableNode("data.name")]), "greet", False)

  // Should pass through the expression directly
  result.code
  |> string.contains("runtime.escape(data.name)")
  |> should.be_true
}

pub fn generate_raw_variable_test() {
  let result = generate(template([RawVariableNode("data.html")]), "raw", False)

  // Should pass through directly without escape
  result.code
  |> string.contains("<> data.html")
  |> should.be_true

  // Should NOT escape raw variables
  result.code
  |> string.contains("runtime.escape(data.html)")
  |> should.be_false
}

// ------------------------------------------------------------- Slot Generation Tests

pub fn generate_template_with_slot_test() {
  // Templates with slots get slot parameters
  let result =
    generate(
      template([TextNode("<main>"), SlotNode(None, []), TextNode("</main>")]),
      "app",
      False,
    )

  // Should use slot argument directly
  result.code
  |> string.contains("<> slot")
  |> should.be_true

  // Should have slot parameter
  result.code
  |> string.contains("slot slot: String")
  |> should.be_true
}

// ------------------------------------------------------------- If Node Generation Tests

pub fn generate_if_node_test() {
  // With new arch, condition is passed through as a case guard
  let result =
    generate(
      template([if_node("data.show", [TextNode("visible")])]),
      "conditional",
      False,
    )

  // Should generate case expression with guard
  result.code
  |> string.contains("case Nil {")
  |> should.be_true

  result.code
  |> string.contains("_ if data.show ->")
  |> should.be_true
}

pub fn generate_nested_if_test() {
  let result =
    generate(
      template([if_node("data.a", [if_node("data.b", [TextNode("nested")])])]),
      "nested_if",
      False,
    )

  result.code
  |> string.contains("_ if data.a ->")
  |> should.be_true

  result.code
  |> string.contains("_ if data.b ->")
  |> should.be_true
}

pub fn generate_if_expression_test() {
  // @if with an expression should use the expression directly
  let result =
    generate(
      template([if_node("data.user == \"Miguel\"", [TextNode("Hello Miguel!")])]),
      "if_expr",
      False,
    )

  // Should use the expression directly in the guard
  result.code
  |> string.contains("_ if data.user == \"Miguel\" ->")
  |> should.be_true
}

pub fn generate_if_literal_true_test() {
  // @if(True) should use True directly
  let result =
    generate(
      template([if_node("True", [TextNode("always shown")])]),
      "if_true",
      False,
    )

  // Should use True directly
  result.code
  |> string.contains("_ if True ->")
  |> should.be_true
}

pub fn generate_if_literal_false_test() {
  // @if(False) should use False directly
  let result =
    generate(
      template([if_node("False", [TextNode("never shown")])]),
      "if_false",
      False,
    )

  // Should use False directly
  result.code
  |> string.contains("_ if False ->")
  |> should.be_true
}

pub fn generate_if_comparison_test() {
  // @if with comparison operators
  let result =
    generate(
      template([if_node("data.count > 0", [TextNode("has items")])]),
      "if_compare",
      False,
    )

  // Should use expression directly
  result.code
  |> string.contains("_ if data.count > 0 ->")
  |> should.be_true
}

pub fn generate_if_negation_test() {
  // @if with negation
  let result =
    generate(
      template([if_node("!data.is_hidden", [TextNode("visible")])]),
      "if_negate",
      False,
    )

  // Should use expression directly
  result.code
  |> string.contains("_ if !data.is_hidden ->")
  |> should.be_true
}

pub fn generate_if_else_test() {
  // @if with @else should generate case with two branches
  let result =
    generate(
      template([if_else_node("data.show", [TextNode("yes")], [TextNode("no")])]),
      "if_else",
      False,
    )

  // Should generate case expression
  result.code
  |> string.contains("case Nil {")
  |> should.be_true

  // Should have the if condition
  result.code
  |> string.contains("_ if data.show ->")
  |> should.be_true

  // Should have the else branch (no condition)
  result.code
  |> string.contains("_ -> {")
  |> should.be_true

  // Both branch contents should be present
  result.code
  |> string.contains("<> \"yes\"")
  |> should.be_true

  result.code
  |> string.contains("<> \"no\"")
  |> should.be_true
}

pub fn generate_if_elseif_else_test() {
  // @if with @elseif and @else should generate case with multiple branches
  let result =
    generate(
      template([
        if_elseif_else_node(
          [#("data.a", [TextNode("A")]), #("data.b", [TextNode("B")])],
          [TextNode("C")],
        ),
      ]),
      "if_elseif_else",
      False,
    )

  // Should generate case expression
  result.code
  |> string.contains("case Nil {")
  |> should.be_true

  // Should have all conditions
  result.code
  |> string.contains("_ if data.a ->")
  |> should.be_true

  result.code
  |> string.contains("_ if data.b ->")
  |> should.be_true

  // Should have the else branch (no condition)
  result.code
  |> string.contains("_ -> {")
  |> should.be_true
}

pub fn generate_nested_if_else_test() {
  // Nested if/else structures should work correctly
  let result =
    generate(
      template([
        if_else_node(
          "data.outer",
          [
            if_else_node("data.inner", [TextNode("both")], [
              TextNode("outer only"),
            ]),
          ],
          [TextNode("neither")],
        ),
      ]),
      "nested_if_else",
      False,
    )

  // Should have multiple case expressions (one for each if)
  result.code
  |> string.contains("_ if data.outer ->")
  |> should.be_true

  result.code
  |> string.contains("_ if data.inner ->")
  |> should.be_true
}

// ------------------------------------------------------------- Each Node Generation Tests

pub fn generate_each_node_test() {
  // @each(data.items as item) with item.name access
  let result =
    generate(
      template([
        EachNode("data.items", ["item"], None, [VariableNode("item.name")]),
      ]),
      "list",
      False,
    )

  // Should use append_each with collection passed through
  result.code
  |> string.contains("runtime.append_each(data.items")
  |> should.be_true

  // Should access item fields correctly
  result.code
  |> string.contains("item.name")
  |> should.be_true
}

pub fn generate_each_with_multiple_fields_test() {
  let result =
    generate(
      template([
        EachNode("data.users", ["user"], None, [
          VariableNode("user.name"),
          VariableNode("user.email"),
        ]),
      ]),
      "users",
      False,
    )

  // Should pass through field accesses
  result.code
  |> string.contains("user.name")
  |> should.be_true

  result.code
  |> string.contains("user.email")
  |> should.be_true
}

pub fn generate_each_tuple_destructuring_test() {
  let result =
    generate(
      template([
        EachNode("items", ["key", "value"], None, [
          VariableNode("key"),
          TextNode(": "),
          VariableNode("value"),
        ]),
      ]),
      "tuples",
      False,
    )

  // Should generate tuple pattern in callback
  result.code
  |> string.contains("fn(acc, #(key, value))")
  |> should.be_true

  // Should use destructured variables
  result.code
  |> string.contains("runtime.escape(key)")
  |> should.be_true

  result.code
  |> string.contains("runtime.escape(value)")
  |> should.be_true
}

pub fn generate_each_triple_tuple_test() {
  let result =
    generate(
      template([
        EachNode("triplets", ["a", "b", "c"], None, [
          VariableNode("a"),
          VariableNode("b"),
          VariableNode("c"),
        ]),
      ]),
      "triple",
      False,
    )

  // Should generate triple tuple pattern
  result.code
  |> string.contains("fn(acc, #(a, b, c))")
  |> should.be_true
}

pub fn generate_each_single_item_still_works_test() {
  let result =
    generate(
      template([EachNode("items", ["item"], None, [VariableNode("item")])]),
      "single",
      False,
    )

  // Single item should NOT be wrapped in tuple
  result.code
  |> string.contains("fn(acc, item)")
  |> should.be_true

  // Should NOT have tuple syntax
  result.code
  |> string.contains("fn(acc, #(item))")
  |> should.be_false
}

pub fn generate_each_with_loop_var_test() {
  let result =
    generate(
      template([
        EachNode("users", ["user"], Some("loop"), [VariableNode("user.name")]),
      ]),
      "with_loop",
      False,
    )

  // Should use append_each_with_loop
  result.code
  |> string.contains("runtime.append_each_with_loop(users, fn(acc, user, loop)")
  |> should.be_true
}

pub fn generate_each_tuple_with_loop_var_test() {
  let result =
    generate(
      template([
        EachNode("pairs", ["key", "value"], Some("idx"), [VariableNode("key")]),
      ]),
      "tuple_with_loop",
      False,
    )

  // Should use append_each_with_loop with tuple pattern
  result.code
  |> string.contains(
    "runtime.append_each_with_loop(pairs, fn(acc, #(key, value), idx)",
  )
  |> should.be_true
}

pub fn generate_each_without_loop_uses_simple_append_test() {
  let result =
    generate(
      template([EachNode("items", ["item"], None, [VariableNode("item")])]),
      "no_loop",
      False,
    )

  // Should use simple append_each (not append_each_with_loop)
  result.code
  |> string.contains("runtime.append_each(items, fn(acc, item)")
  |> should.be_true

  // Should NOT use append_each_with_loop
  result.code
  |> string.contains("append_each_with_loop")
  |> should.be_false
}

// ------------------------------------------------------------- Component Generation Tests

pub fn generate_component_test() {
  // Components use is_component: True with labeled arguments
  // Data comes from view_file parsing (passed as separate parameter)
  let result =
    generate(
      template([VariableNode("title"), SlotNode(None, [])]),
      "component",
      True,
    )

  // Should have html function with slot and attributes params
  result.code
  |> string.contains("slot slot: String")
  |> should.be_true

  result.code
  |> string.contains("attributes attributes: List(runtime.Attribute)")
  |> should.be_true

  // Should access title directly (variable from template)
  result.code
  |> string.contains("runtime.escape(title)")
  |> should.be_true

  // Should use slot argument directly
  result.code
  |> string.contains("<> slot")
  |> should.be_true
}

pub fn generate_slot_node_in_component_test() {
  let result =
    generate(
      template([TextNode("<div>"), SlotNode(None, []), TextNode("</div>")]),
      "wrapper",
      True,
    )

  // Should use slot argument directly
  result.code
  |> string.contains("<> slot")
  |> should.be_true
}

pub fn generate_component_usage_test() {
  let result =
    generate(
      template([
        ComponentNode("alert", [StringAttr("type", "success")], [
          TextNode("Hello"),
        ]),
      ]),
      "page",
      False,
    )

  // Should import component
  result.code
  |> string.contains(
    "import compiled/loom/components/alert as components_alert",
  )
  |> should.be_true

  // Should call component html with labeled arguments
  result.code
  |> string.contains("components_alert.html(")
  |> should.be_true

  // String attrs go to the attributes list
  result.code
  |> string.contains("runtime.Attribute(\"type\", \"success\")")
  |> should.be_true
}

pub fn generate_nested_component_path_test() {
  let result =
    generate(template([ComponentNode("forms:input", [], [])]), "form", False)

  // Should have correct import path
  result.code
  |> string.contains(
    "import compiled/loom/components/forms/input as components_forms_input",
  )
  |> should.be_true
}

pub fn generate_component_with_expr_attr_test() {
  let tmpl =
    template([ComponentNode("input", [ExprAttr("value", "data.name")], [])])
  // Create component props map with "value" as a known prop for "input" component
  let component_props = dict.from_list([#("input", [#("value", "String")])])
  let result =
    generator.generate(tmpl, "form", False, None, component_props, dict.new())

  // Should pass expression directly as prop (since "value" is a known prop)
  result.code
  |> string.contains("value: data.name")
  |> should.be_true
}

// ------------------------------------------------------------- String Escaping Tests

pub fn generate_escaped_text_test() {
  let result =
    generate(
      template([TextNode("Line 1\nLine 2\t\"quoted\"")]),
      "escaped",
      False,
    )

  // Newlines should be escaped
  result.code
  |> string.contains("\\n")
  |> should.be_true

  // Tabs should be escaped
  result.code
  |> string.contains("\\t")
  |> should.be_true

  // Quotes should be escaped
  result.code
  |> string.contains("\\\"")
  |> should.be_true
}

// ------------------------------------------------------------- Header Generation Test

pub fn generate_header_comment_test() {
  let result = generate(template([]), "test", False)

  result.code
  |> string.contains("This file was generated by Glimr ✨")
  |> should.be_true

  result.code
  |> string.contains("DO NOT EDIT THIS FILE DIRECTLY")
  |> should.be_true
}

// ------------------------------------------------------------- Import Generation Tests

pub fn generate_runtime_import_test() {
  let result = generate(template([]), "test", False)

  result.code
  |> string.contains("import glimr/loom/runtime")
  |> should.be_true
}

pub fn generate_multiple_component_imports_test() {
  let result =
    generate(
      template([
        ComponentNode("alert", [], []),
        ComponentNode("button", [], []),
        ComponentNode("alert", [], []),
      ]),
      "page",
      False,
    )

  // Should import both (only once each)
  result.code
  |> string.contains("as components_alert")
  |> should.be_true

  result.code
  |> string.contains("as components_button")
  |> should.be_true
}

// ------------------------------------------------------------- Named Slot Tests

pub fn generate_named_slot_in_component_test() {
  let result =
    generate(
      template([
        TextNode("<header>"),
        SlotNode(Some("header"), []),
        TextNode("</header>"),
        SlotNode(None, []),
      ]),
      "card",
      True,
    )

  // Should use named slot argument directly
  result.code
  |> string.contains("<> slot_header")
  |> should.be_true

  // Should use default slot argument directly
  result.code
  |> string.contains("<> slot")
  |> should.be_true
}

pub fn generate_component_with_named_slot_def_test() {
  let result =
    generate(
      template([
        ComponentNode("card", [], [
          SlotDefNode(Some("header"), [TextNode("Title")]),
          TextNode("Body"),
        ]),
      ]),
      "page",
      False,
    )

  // Should pass named slot
  result.code
  |> string.contains("slot_header:")
  |> should.be_true

  // Should pass default slot
  result.code
  |> string.contains("slot:")
  |> should.be_true
}

pub fn generate_multiple_named_slots_test() {
  // Templates with multiple named slots get all slot parameters
  let result =
    generate(
      template([
        SlotNode(Some("header"), []),
        SlotNode(Some("sidebar"), []),
        SlotNode(None, []),
        SlotNode(Some("footer"), []),
      ]),
      "layout",
      False,
    )

  result.code
  |> string.contains("<> slot_header")
  |> should.be_true

  result.code
  |> string.contains("<> slot_sidebar")
  |> should.be_true

  result.code
  |> string.contains("<> slot_footer")
  |> should.be_true

  result.code
  |> string.contains("<> slot")
  |> should.be_true
}

// ------------------------------------------------------------- Slot Fallback Tests

pub fn generate_slot_with_fallback_test() {
  // Slot with fallback content should check if empty
  let result =
    generate(
      template([SlotNode(None, [TextNode("Default content")])]),
      "card",
      True,
    )

  // Should check if slot is empty
  result.code
  |> string.contains("slot == \"\"")
  |> should.be_true

  // Should have fallback content
  result.code
  |> string.contains("<> \"Default content\"")
  |> should.be_true

  // Should also append slot when not empty
  result.code
  |> string.contains("slot != \"\"")
  |> should.be_true
}

pub fn generate_named_slot_with_fallback_test() {
  // Named slot with fallback content
  let result =
    generate(
      template([SlotNode(Some("header"), [TextNode("Default Header")])]),
      "card",
      True,
    )

  // Should check if named slot is empty
  result.code
  |> string.contains("slot_header == \"\"")
  |> should.be_true

  // Should have fallback content
  result.code
  |> string.contains("<> \"Default Header\"")
  |> should.be_true
}

// ------------------------------------------------------------- Slot Conditional Tests

pub fn generate_slot_conditional_test() {
  // l-if="slot" should transform to slot != ""
  let result =
    generate(
      template([
        IfNode([
          #(Some("slot"), [TextNode("<div class=\"has-content\">")]),
        ]),
      ]),
      "card",
      True,
    )

  // Should transform slot condition
  result.code
  |> string.contains("slot != \"\"")
  |> should.be_true
}

pub fn generate_named_slot_conditional_test() {
  // l-if="slot.header" should transform to slot_header != ""
  let result =
    generate(
      template([
        IfNode([
          #(Some("slot.header"), [TextNode("<div class=\"header-wrapper\">")]),
        ]),
      ]),
      "card",
      True,
    )

  // Should transform slot.header condition
  result.code
  |> string.contains("slot_header != \"\"")
  |> should.be_true
}

// ------------------------------------------------------------- Attribute Forwarding Tests

pub fn generate_component_call_with_attributes_test() {
  // When using a component, all attributes should be passed in attributes list
  let result =
    generate(
      template([
        ComponentNode(
          "button",
          [StringAttr("class", "btn"), StringAttr("type", "submit")],
          [TextNode("Click")],
        ),
      ]),
      "page",
      False,
    )

  // Should generate attributes list
  result.code
  |> string.contains("attributes:")
  |> should.be_true

  result.code
  |> string.contains("runtime.Attribute(\"class\", \"btn\")")
  |> should.be_true

  result.code
  |> string.contains("runtime.Attribute(\"type\", \"submit\")")
  |> should.be_true
}

pub fn generate_explicit_attributes_node_test() {
  // When @attributes is explicitly used, it should render attributes argument
  let result =
    generate(
      template([
        TextNode("<button "),
        AttributesNode([]),
        TextNode(">"),
        SlotNode(None, []),
        TextNode("</button>"),
      ]),
      "button",
      True,
    )

  // Should render attributes at the @attributes location (direct argument)
  result.code
  |> string.contains("runtime.render_attributes(attributes)")
  |> should.be_true
}

pub fn generate_auto_inject_attributes_test() {
  // When no @attributes is present in component, attributes should be auto-injected
  let result =
    generate(
      template([
        TextNode("<button class=\"default\">"),
        SlotNode(None, []),
        TextNode("</button>"),
      ]),
      "button",
      True,
    )

  // Should auto-inject attributes with merge_attributes (direct argument)
  result.code
  |> string.contains(
    "runtime.merge_attributes([runtime.Attribute(\"class\", \"default\")], attributes)",
  )
  |> should.be_true
}

pub fn generate_component_empty_attributes_test() {
  // Components with no attributes should pass empty list
  let result =
    generate(template([ComponentNode("icon", [], [])]), "page", False)

  // Should pass empty attributes list
  result.code
  |> string.contains("attributes: []")
  |> should.be_true
}

pub fn generate_component_with_expr_attr_in_attributes_test() {
  // Expression attributes should also be in the attributes list
  let result =
    generate(
      template([
        ComponentNode("card", [ExprAttr("class", "data.user_class")], []),
      ]),
      "page",
      False,
    )

  // Should generate runtime.Attribute with data reference
  result.code
  |> string.contains("runtime.Attribute(\"class\", data.user_class)")
  |> should.be_true
}

// ------------------------------------------------------------- Extract Named Slots Test

pub fn extract_named_slots_test() {
  let tmpl =
    template([
      SlotNode(Some("header"), []),
      SlotNode(None, []),
      SlotNode(Some("footer"), []),
    ])
  let slots = generator.extract_named_slots(tmpl)

  slots
  |> list_contains("header")
  |> should.be_true

  slots
  |> list_contains("footer")
  |> should.be_true
}

// ------------------------------------------------------------- Class and Style Attribute Tests

pub fn generate_component_with_class_attr_test() {
  let result =
    generate(
      template([
        ComponentNode("button", [ClassAttr("[#(\"active\", is_active)]")], []),
      ]),
      "page",
      False,
    )

  // Should generate runtime.build_classes call
  result.code
  |> string.contains("runtime.build_classes([#(\"active\", is_active)])")
  |> should.be_true
}

pub fn generate_component_with_style_attr_test() {
  let result =
    generate(
      template([
        ComponentNode("div", [StyleAttr("[#(\"color: red\", has_error)]")], []),
      ]),
      "page",
      False,
    )

  // Should generate runtime.build_styles call
  result.code
  |> string.contains("runtime.build_styles([#(\"color: red\", has_error)])")
  |> should.be_true
}

pub fn generate_component_with_static_and_dynamic_class_test() {
  let result =
    generate(
      template([
        ComponentNode(
          "button",
          [StringAttr("class", "btn"), ClassAttr("[#(\"active\", cond)]")],
          [],
        ),
      ]),
      "page",
      False,
    )

  // Both static class and dynamic :class are generated as separate attributes (no merging)
  // Use unified syntax :class="[runtime.class(\"btn\"), #(\"active\", cond)]" if you want merged
  result.code
  |> string.contains("runtime.Attribute(\"class\", \"btn\")")
  |> should.be_true

  result.code
  |> string.contains(
    "runtime.Attribute(\"class\", runtime.build_classes([#(\"active\", cond)]))",
  )
  |> should.be_true
}

pub fn generate_component_with_static_and_dynamic_style_test() {
  let result =
    generate(
      template([
        ComponentNode(
          "div",
          [
            StringAttr("style", "margin: 0"),
            StyleAttr("[#(\"color: red\", cond)]"),
          ],
          [],
        ),
      ]),
      "page",
      False,
    )

  // Both static style and dynamic :style are generated as separate attributes (no merging)
  // Use unified syntax :style="[runtime.style(\"margin: 0\"), #(\"color: red\", cond)]" if you want merged
  result.code
  |> string.contains("runtime.Attribute(\"style\", \"margin: 0\")")
  |> should.be_true

  result.code
  |> string.contains(
    "runtime.Attribute(\"style\", runtime.build_styles([#(\"color: red\", cond)]))",
  )
  |> should.be_true
}

pub fn generate_component_with_class_and_style_attrs_test() {
  let result =
    generate(
      template([
        ComponentNode(
          "div",
          [
            ClassAttr("[#(\"p-4\", True)]"),
            StyleAttr("[#(\"color: blue\", True)]"),
          ],
          [],
        ),
      ]),
      "page",
      False,
    )

  // Should generate both class and style
  result.code
  |> string.contains("runtime.build_classes([#(\"p-4\", True)])")
  |> should.be_true

  result.code
  |> string.contains("runtime.build_styles([#(\"color: blue\", True)])")
  |> should.be_true
}

pub fn generate_class_attr_transforms_static_strings_test() {
  let result =
    generate(
      template([
        ComponentNode(
          "div",
          [ClassAttr("[\"btn primary\", #(\"active\", is_active)]")],
          [],
        ),
      ]),
      "page",
      False,
    )

  // Static strings should be wrapped with runtime.class()
  result.code
  |> string.contains(
    "runtime.build_classes([runtime.class(\"btn primary\"), #(\"active\", is_active)])",
  )
  |> should.be_true
}

pub fn generate_style_attr_transforms_static_strings_test() {
  let result =
    generate(
      template([
        ComponentNode(
          "div",
          [StyleAttr("[\"color: red\", #(\"display: none\", is_hidden)]")],
          [],
        ),
      ]),
      "page",
      False,
    )

  // Static strings should be wrapped with runtime.style()
  result.code
  |> string.contains(
    "runtime.build_styles([runtime.style(\"color: red\"), #(\"display: none\", is_hidden)])",
  )
  |> should.be_true
}

pub fn generate_class_attr_only_conditionals_unchanged_test() {
  let result =
    generate(
      template([
        ComponentNode(
          "div",
          [ClassAttr("[#(\"active\", True), #(\"disabled\", False)]")],
          [],
        ),
      ]),
      "page",
      False,
    )

  // List with only conditionals should be unchanged
  result.code
  |> string.contains(
    "runtime.build_classes([#(\"active\", True), #(\"disabled\", False)])",
  )
  |> should.be_true
}

// ------------------------------------------------------------- Helpers

fn list_contains(list: List(String), target: String) -> Bool {
  case list {
    [] -> False
    [first, ..rest] ->
      case first == target {
        True -> True
        False -> list_contains(rest, target)
      }
  }
}
