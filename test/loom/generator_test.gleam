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
  IfNode([#(Some(condition), 1, body)])
}

// Helper to create an if/else node
fn if_else_node(
  condition: String,
  if_body: List(Node),
  else_body: List(Node),
) -> Node {
  IfNode([#(Some(condition), 1, if_body), #(None, 0, else_body)])
}

// Helper to create an if/elseif/else node
fn if_elseif_else_node(
  branches: List(#(String, List(Node))),
  else_body: List(Node),
) -> Node {
  let if_branches =
    branches
    |> list.map(fn(b) { #(Some(b.0), 1, b.1) })
  IfNode(list.append(if_branches, [#(None, 0, else_body)]))
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
  let result =
    generate(template([VariableNode("data.name", 1)]), "greet", False)

  // Should pass through the expression directly
  result.code
  |> string.contains("runtime.escape(data.name)")
  |> should.be_true
}

pub fn generate_raw_variable_test() {
  let result =
    generate(template([RawVariableNode("data.html", 1)]), "raw", False)

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
        EachNode(
          "data.items",
          ["item"],
          None,
          [VariableNode("item.name", 1)],
          1,
        ),
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
        EachNode(
          "data.users",
          ["user"],
          None,
          [
            VariableNode("user.name", 1),
            VariableNode("user.email", 1),
          ],
          1,
        ),
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
        EachNode(
          "items",
          ["key", "value"],
          None,
          [
            VariableNode("key", 1),
            TextNode(": "),
            VariableNode("value", 1),
          ],
          1,
        ),
      ]),
      "tuples",
      False,
    )

  // Should use temp variable and let binding for tuple destructuring
  // (Gleam doesn't allow pattern matching in fn parameters)
  result.code
  |> string.contains("fn(acc, item__)")
  |> should.be_true

  result.code
  |> string.contains("let #(key, value) = item__")
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
        EachNode(
          "triplets",
          ["a", "b", "c"],
          None,
          [
            VariableNode("a", 1),
            VariableNode("b", 1),
            VariableNode("c", 1),
          ],
          1,
        ),
      ]),
      "triple",
      False,
    )

  // Should use temp variable and let binding for tuple destructuring
  result.code
  |> string.contains("fn(acc, item__)")
  |> should.be_true

  result.code
  |> string.contains("let #(a, b, c) = item__")
  |> should.be_true
}

pub fn generate_each_single_item_still_works_test() {
  let result =
    generate(
      template([EachNode("items", ["item"], None, [VariableNode("item", 1)], 1)]),
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
        EachNode(
          "users",
          ["user"],
          Some("loop"),
          [VariableNode("user.name", 1)],
          1,
        ),
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
        EachNode(
          "pairs",
          ["key", "value"],
          Some("idx"),
          [VariableNode("key", 1)],
          1,
        ),
      ]),
      "tuple_with_loop",
      False,
    )

  // Should use append_each_with_loop with temp variable
  result.code
  |> string.contains(
    "runtime.append_each_with_loop(pairs, fn(acc, item__, idx)",
  )
  |> should.be_true

  // Should destructure the tuple inside the function
  result.code
  |> string.contains("let #(key, value) = item__")
  |> should.be_true
}

pub fn generate_each_without_loop_uses_simple_append_test() {
  let result =
    generate(
      template([EachNode("items", ["item"], None, [VariableNode("item", 1)], 1)]),
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
      template([VariableNode("title", 1), SlotNode(None, [])]),
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
          #(Some("slot"), 1, [TextNode("<div class=\"has-content\">")]),
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
          #(Some("slot.header"), 1, [TextNode("<div class=\"header-wrapper\">")]),
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

// ------------------------------------------------------------- Validation Tests

import glimr/loom/gleam_parser.{ParsedViewFile}

pub fn validate_template_with_defined_variable_test() {
  let tmpl = template([VariableNode("title", 1)])
  let view_file = ParsedViewFile(fields: [#("title", "String")], imports: [])

  generator.validate_template(tmpl, Some(view_file), "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_undefined_variable_test() {
  let tmpl = template([VariableNode("undefined_var", 5)])
  let view_file = ParsedViewFile(fields: [#("title", "String")], imports: [])

  let result =
    generator.validate_template(tmpl, Some(view_file), "test.loom.html")

  result
  |> should.be_error

  case result {
    Error(msg) -> {
      msg
      |> string.contains("Undefined variable 'undefined_var'")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_template_with_dotted_access_test() {
  // user.name should validate the root "user"
  let tmpl = template([VariableNode("user.name", 1)])
  let view_file = ParsedViewFile(fields: [#("user", "User")], imports: [])

  generator.validate_template(tmpl, Some(view_file), "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_undefined_dotted_access_test() {
  // user.name where "user" is not defined
  let tmpl = template([VariableNode("user.name", 1)])
  let view_file = ParsedViewFile(fields: [#("title", "String")], imports: [])

  generator.validate_template(tmpl, Some(view_file), "test.loom.html")
  |> should.be_error
}

pub fn validate_template_with_loop_variable_test() {
  // Loop variables should be valid inside the loop
  let tmpl =
    template([
      EachNode("items", ["item"], None, [VariableNode("item.name", 1)], 1),
    ])
  let view_file =
    ParsedViewFile(fields: [#("items", "List(Item)")], imports: [])

  generator.validate_template(tmpl, Some(view_file), "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_loop_var_outside_scope_test() {
  // Loop variable used outside the loop should fail
  let tmpl =
    template([
      EachNode("items", ["item"], None, [TextNode("in loop")], 1),
      VariableNode("item", 1),
    ])
  let view_file =
    ParsedViewFile(fields: [#("items", "List(Item)")], imports: [])

  generator.validate_template(tmpl, Some(view_file), "test.loom.html")
  |> should.be_error
}

pub fn validate_template_with_tuple_destructuring_test() {
  // Tuple destructuring in l-for should make both vars available
  let tmpl =
    template([
      EachNode(
        "pairs",
        ["key", "value"],
        None,
        [
          VariableNode("key", 1),
          VariableNode("value", 1),
        ],
        1,
      ),
    ])
  let view_file =
    ParsedViewFile(fields: [#("pairs", "List(#(String, String))")], imports: [])

  generator.validate_template(tmpl, Some(view_file), "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_loop_index_test() {
  // Loop index variable should be available
  let tmpl =
    template([
      EachNode(
        "items",
        ["item"],
        Some("loop"),
        [VariableNode("loop.index", 1)],
        1,
      ),
    ])
  let view_file =
    ParsedViewFile(fields: [#("items", "List(Item)")], imports: [])

  generator.validate_template(tmpl, Some(view_file), "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_slot_variable_test() {
  // Default slot variable should always be valid
  let tmpl = template([IfNode([#(Some("slot"), 1, [TextNode("has slot")])])])

  generator.validate_template(tmpl, None, "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_named_slot_variable_test() {
  // Named slot variable should be valid
  let tmpl =
    template([
      SlotNode(Some("header"), []),
      IfNode([#(Some("slot.header"), 1, [TextNode("has header")])]),
    ])

  generator.validate_template(tmpl, None, "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_if_condition_test() {
  // Variables in l-if conditions should be validated
  let tmpl =
    template([IfNode([#(Some("is_visible"), 1, [TextNode("visible")])])])
  let view_file = ParsedViewFile(fields: [#("is_visible", "Bool")], imports: [])

  generator.validate_template(tmpl, Some(view_file), "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_undefined_if_condition_test() {
  // Undefined variable in l-if should fail
  let tmpl = template([IfNode([#(Some("undefined"), 1, [TextNode("text")])])])
  let view_file = ParsedViewFile(fields: [#("title", "String")], imports: [])

  generator.validate_template(tmpl, Some(view_file), "test.loom.html")
  |> should.be_error
}

pub fn validate_template_with_raw_variable_test() {
  // Raw variables should also be validated
  let tmpl = template([RawVariableNode("html_content", 1)])
  let view_file =
    ParsedViewFile(fields: [#("html_content", "String")], imports: [])

  generator.validate_template(tmpl, Some(view_file), "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_undefined_raw_variable_test() {
  let tmpl = template([RawVariableNode("undefined", 1)])
  let view_file = ParsedViewFile(fields: [#("title", "String")], imports: [])

  generator.validate_template(tmpl, Some(view_file), "test.loom.html")
  |> should.be_error
}

pub fn validate_template_with_no_view_file_test() {
  // Without a view file, all variables should fail (except slot vars)
  let tmpl = template([VariableNode("title", 1)])

  generator.validate_template(tmpl, None, "test.loom.html")
  |> should.be_error
}

pub fn validate_template_with_nested_loop_test() {
  // Nested loop variables should have correct scope
  let tmpl =
    template([
      EachNode(
        "users",
        ["user"],
        None,
        [
          EachNode(
            "user.posts",
            ["post"],
            None,
            [
              VariableNode("user.name", 1),
              VariableNode("post.title", 1),
            ],
            1,
          ),
        ],
        1,
      ),
    ])
  let view_file =
    ParsedViewFile(fields: [#("users", "List(User)")], imports: [])

  generator.validate_template(tmpl, Some(view_file), "test.loom.html")
  |> should.be_ok
}

// ------------------------------------------------------------- Loop Variable Type Conversion Tests

pub fn generate_loop_index_uses_int_to_string_test() {
  // loop.index is an Int, so it should use int.to_string
  let tmpl =
    template([
      EachNode(
        "items",
        ["item"],
        Some("loop"),
        [
          VariableNode("loop.index", 1),
        ],
        1,
      ),
    ])

  let result = generate(tmpl, "loop_test", False)

  // Should import gleam/int
  result.code
  |> string.contains("import gleam/int")
  |> should.be_true

  // Should NOT import gleam/bool (not using bool properties)
  result.code
  |> string.contains("import gleam/bool")
  |> should.be_false

  // Should use int.to_string for loop.index
  result.code
  |> string.contains("int.to_string(loop.index)")
  |> should.be_true
}

pub fn generate_loop_iteration_uses_int_to_string_test() {
  // loop.iteration is an Int
  let tmpl =
    template([
      EachNode(
        "items",
        ["item"],
        Some("loop"),
        [
          VariableNode("loop.iteration", 1),
        ],
        1,
      ),
    ])

  let result = generate(tmpl, "loop_test", False)

  result.code
  |> string.contains("int.to_string(loop.iteration)")
  |> should.be_true
}

pub fn generate_loop_count_uses_int_to_string_test() {
  // loop.count is an Int
  let tmpl =
    template([
      EachNode(
        "items",
        ["item"],
        Some("loop"),
        [
          VariableNode("loop.count", 1),
        ],
        1,
      ),
    ])

  let result = generate(tmpl, "loop_test", False)

  result.code
  |> string.contains("int.to_string(loop.count)")
  |> should.be_true
}

pub fn generate_loop_remaining_uses_int_to_string_test() {
  // loop.remaining is an Int
  let tmpl =
    template([
      EachNode(
        "items",
        ["item"],
        Some("loop"),
        [
          VariableNode("loop.remaining", 1),
        ],
        1,
      ),
    ])

  let result = generate(tmpl, "loop_test", False)

  result.code
  |> string.contains("int.to_string(loop.remaining)")
  |> should.be_true
}

pub fn generate_loop_first_uses_bool_to_string_test() {
  // loop.first is a Bool, so it should use bool.to_string
  let tmpl =
    template([
      EachNode(
        "items",
        ["item"],
        Some("loop"),
        [
          VariableNode("loop.first", 1),
        ],
        1,
      ),
    ])

  let result = generate(tmpl, "loop_test", False)

  // Should import gleam/bool
  result.code
  |> string.contains("import gleam/bool")
  |> should.be_true

  // Should NOT import gleam/int (not using int properties)
  result.code
  |> string.contains("import gleam/int")
  |> should.be_false

  // Should use bool.to_string for loop.first
  result.code
  |> string.contains("bool.to_string(loop.first)")
  |> should.be_true
}

pub fn generate_loop_last_uses_bool_to_string_test() {
  // loop.last is a Bool
  let tmpl =
    template([
      EachNode(
        "items",
        ["item"],
        Some("loop"),
        [
          VariableNode("loop.last", 1),
        ],
        1,
      ),
    ])

  let result = generate(tmpl, "loop_test", False)

  result.code
  |> string.contains("bool.to_string(loop.last)")
  |> should.be_true
}

pub fn generate_loop_even_uses_bool_to_string_test() {
  // loop.even is a Bool
  let tmpl =
    template([
      EachNode(
        "items",
        ["item"],
        Some("loop"),
        [
          VariableNode("loop.even", 1),
        ],
        1,
      ),
    ])

  let result = generate(tmpl, "loop_test", False)

  result.code
  |> string.contains("bool.to_string(loop.even)")
  |> should.be_true
}

pub fn generate_loop_odd_uses_bool_to_string_test() {
  // loop.odd is a Bool
  let tmpl =
    template([
      EachNode(
        "items",
        ["item"],
        Some("loop"),
        [
          VariableNode("loop.odd", 1),
        ],
        1,
      ),
    ])

  let result = generate(tmpl, "loop_test", False)

  result.code
  |> string.contains("bool.to_string(loop.odd)")
  |> should.be_true
}

pub fn generate_loop_mixed_properties_imports_both_test() {
  // Using both Int and Bool loop properties should import both modules
  let tmpl =
    template([
      EachNode(
        "items",
        ["item"],
        Some("loop"),
        [
          VariableNode("loop.index", 1),
          VariableNode("loop.first", 1),
        ],
        1,
      ),
    ])

  let result = generate(tmpl, "loop_test", False)

  // Should import both
  result.code
  |> string.contains("import gleam/int")
  |> should.be_true

  result.code
  |> string.contains("import gleam/bool")
  |> should.be_true
}

pub fn generate_loop_no_properties_no_imports_test() {
  // Using loop variable but not accessing properties shouldn't import int/bool
  let tmpl =
    template([
      EachNode(
        "items",
        ["item"],
        Some("loop"),
        [
          VariableNode("item", 1),
        ],
        1,
      ),
    ])

  let result = generate(tmpl, "loop_test", False)

  // Should NOT import int or bool
  result.code
  |> string.contains("import gleam/int")
  |> should.be_false

  result.code
  |> string.contains("import gleam/bool")
  |> should.be_false
}

pub fn generate_named_loop_variable_test() {
  // Named loop variables like user_loop should work the same
  let tmpl =
    template([
      EachNode(
        "users",
        ["user"],
        Some("user_loop"),
        [
          VariableNode("user_loop.index", 1),
          VariableNode("user_loop.first", 1),
        ],
        1,
      ),
    ])

  let result = generate(tmpl, "loop_test", False)

  result.code
  |> string.contains("int.to_string(user_loop.index)")
  |> should.be_true

  result.code
  |> string.contains("bool.to_string(user_loop.first)")
  |> should.be_true
}

pub fn generate_nested_loops_with_different_loop_vars_test() {
  // Nested loops with different loop variable names
  let tmpl =
    template([
      EachNode(
        "users",
        ["user"],
        Some("user_loop"),
        [
          VariableNode("user_loop.index", 1),
          EachNode(
            "user.posts",
            ["post"],
            Some("post_loop"),
            [
              VariableNode("post_loop.iteration", 1),
            ],
            1,
          ),
        ],
        1,
      ),
    ])

  let result = generate(tmpl, "loop_test", False)

  result.code
  |> string.contains("int.to_string(user_loop.index)")
  |> should.be_true

  result.code
  |> string.contains("int.to_string(post_loop.iteration)")
  |> should.be_true
}

pub fn generate_raw_loop_variable_also_converts_test() {
  // Raw variables with loop properties should also convert types
  let tmpl =
    template([
      EachNode(
        "items",
        ["item"],
        Some("loop"),
        [
          RawVariableNode("loop.index", 1),
        ],
        1,
      ),
    ])

  let result = generate(tmpl, "loop_test", False)

  // Should still use int.to_string even for raw output
  result.code
  |> string.contains("int.to_string(loop.index)")
  |> should.be_true
}

// ------------------------------------------------------------- Tuple Destructuring Tests
// These tests verify that tuple destructuring generates valid Gleam syntax.
// Gleam does NOT allow pattern matching in anonymous function parameters,
// so we must use a temp variable and let binding inside the function body.

pub fn generate_tuple_destructuring_no_inline_pattern_test() {
  // CRITICAL: This test would have caught the original bug where we generated
  // fn(acc, #(key, value)) which is invalid Gleam syntax
  let result =
    generate(
      template([
        EachNode(
          "items",
          ["a", "b"],
          None,
          [
            VariableNode("a", 1),
          ],
          1,
        ),
      ]),
      "test",
      False,
    )

  // Should NOT generate inline pattern matching in fn parameter (invalid Gleam)
  result.code
  |> string.contains("fn(acc, #(")
  |> should.be_false

  // Should use temp variable instead
  result.code
  |> string.contains("fn(acc, item__)")
  |> should.be_true
}

pub fn generate_tuple_destructuring_has_let_binding_test() {
  // The destructuring must happen inside the function body with let binding
  let result =
    generate(
      template([
        EachNode(
          "items",
          ["x", "y", "z"],
          None,
          [
            VariableNode("x", 1),
          ],
          1,
        ),
      ]),
      "test",
      False,
    )

  // Must have let binding inside function
  result.code
  |> string.contains("let #(x, y, z) = item__")
  |> should.be_true
}

pub fn generate_tuple_destructuring_four_elements_test() {
  // Test with 4-element tuple to ensure it scales
  let result =
    generate(
      template([
        EachNode(
          "items",
          ["a", "b", "c", "d"],
          None,
          [
            VariableNode("a", 1),
            VariableNode("d", 1),
          ],
          1,
        ),
      ]),
      "test",
      False,
    )

  // Should NOT use inline pattern
  result.code
  |> string.contains("fn(acc, #(")
  |> should.be_false

  // Should use temp variable with let binding
  result.code
  |> string.contains("fn(acc, item__)")
  |> should.be_true

  result.code
  |> string.contains("let #(a, b, c, d) = item__")
  |> should.be_true
}

pub fn generate_tuple_destructuring_with_loop_no_inline_pattern_test() {
  // Tuple + loop variable should also not use inline pattern
  let result =
    generate(
      template([
        EachNode(
          "items",
          ["name", "value"],
          Some("idx"),
          [
            VariableNode("name", 1),
            VariableNode("idx.index", 1),
          ],
          1,
        ),
      ]),
      "test",
      False,
    )

  // Should NOT generate inline pattern matching
  result.code
  |> string.contains("fn(acc, #(")
  |> should.be_false

  // Should use temp variable with loop var
  result.code
  |> string.contains("fn(acc, item__, idx)")
  |> should.be_true

  // Should have let binding
  result.code
  |> string.contains("let #(name, value) = item__")
  |> should.be_true
}

pub fn generate_single_var_no_tuple_syntax_test() {
  // Single variable should NOT use tuple syntax at all
  let result =
    generate(
      template([
        EachNode(
          "items",
          ["item"],
          None,
          [
            VariableNode("item", 1),
          ],
          1,
        ),
      ]),
      "test",
      False,
    )

  // Should use direct parameter name
  result.code
  |> string.contains("fn(acc, item)")
  |> should.be_true

  // Should NOT have tuple pattern
  result.code
  |> string.contains("#(item)")
  |> should.be_false

  // Should NOT use temp variable
  result.code
  |> string.contains("fn(acc, item__)")
  |> should.be_false

  // Should NOT have let binding for single var
  result.code
  |> string.contains("let item = item__")
  |> should.be_false
}

pub fn generate_nested_loops_tuple_destructuring_test() {
  // Nested loops with tuple destructuring should both use temp variables
  let result =
    generate(
      template([
        EachNode(
          "outer",
          ["a", "b"],
          None,
          [
            EachNode(
              "inner",
              ["x", "y"],
              None,
              [
                VariableNode("a", 1),
                VariableNode("x", 1),
              ],
              1,
            ),
          ],
          1,
        ),
      ]),
      "test",
      False,
    )

  // Both loops should use temp variable approach
  result.code
  |> string.contains("let #(a, b) = item__")
  |> should.be_true

  result.code
  |> string.contains("let #(x, y) = item__")
  |> should.be_true

  // Neither should use inline pattern
  result.code
  |> string.contains("fn(acc, #(")
  |> should.be_false
}

// ------------------------------------------------------------- Tuple Arity Validation Tests

pub fn validate_tuple_arity_mismatch_test() {
  // Destructuring with wrong number of variables should fail validation
  let tmpl =
    template([
      EachNode("items", ["a", "b", "c", "d"], None, [VariableNode("a", 1)], 5),
    ])
  let view_file =
    ParsedViewFile(
      fields: [#("items", "List(#(String, String, String))")],
      imports: [],
    )

  let result =
    generator.validate_template(tmpl, Some(view_file), "test.loom.html")

  result
  |> should.be_error

  case result {
    Error(msg) -> {
      msg
      |> string.contains("Tuple destructuring mismatch")
      |> should.be_true

      msg
      |> string.contains("expected 3 variables but got 4")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_tuple_arity_correct_test() {
  // Correct number of variables should pass validation
  let tmpl =
    template([
      EachNode("items", ["a", "b", "c"], None, [VariableNode("a", 1)], 5),
    ])
  let view_file =
    ParsedViewFile(
      fields: [#("items", "List(#(String, String, String))")],
      imports: [],
    )

  generator.validate_template(tmpl, Some(view_file), "test.loom.html")
  |> should.be_ok
}

pub fn validate_tuple_arity_two_elements_test() {
  // Two-element tuple should work
  let tmpl =
    template([
      EachNode("pairs", ["key", "value"], None, [VariableNode("key", 1)], 5),
    ])
  let view_file =
    ParsedViewFile(fields: [#("pairs", "List(#(String, Int))")], imports: [])

  generator.validate_template(tmpl, Some(view_file), "test.loom.html")
  |> should.be_ok
}

pub fn validate_tuple_arity_two_elements_mismatch_test() {
  // Three variables for two-element tuple should fail
  let tmpl =
    template([
      EachNode("pairs", ["a", "b", "c"], None, [VariableNode("a", 1)], 5),
    ])
  let view_file =
    ParsedViewFile(fields: [#("pairs", "List(#(String, Int))")], imports: [])

  let result =
    generator.validate_template(tmpl, Some(view_file), "test.loom.html")

  result
  |> should.be_error

  case result {
    Error(msg) -> {
      msg
      |> string.contains("expected 2 variables but got 3")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_tuple_arity_single_var_skips_check_test() {
  // Single variable (no destructuring) should skip arity check
  let tmpl =
    template([
      EachNode("items", ["item"], None, [VariableNode("item", 1)], 5),
    ])
  let view_file =
    ParsedViewFile(
      fields: [#("items", "List(#(String, String, String))")],
      imports: [],
    )

  // Should pass - single var means no tuple destructuring
  generator.validate_template(tmpl, Some(view_file), "test.loom.html")
  |> should.be_ok
}

pub fn validate_tuple_arity_non_tuple_list_skips_check_test() {
  // List of non-tuples should skip arity check
  let tmpl =
    template([
      EachNode("names", ["a", "b"], None, [VariableNode("a", 1)], 5),
    ])
  let view_file =
    ParsedViewFile(fields: [#("names", "List(String)")], imports: [])

  // Should pass - can't validate non-tuple types
  generator.validate_template(tmpl, Some(view_file), "test.loom.html")
  |> should.be_ok
}

pub fn validate_tuple_arity_nested_tuple_types_test() {
  // Nested tuple types should count correctly
  let tmpl =
    template([
      EachNode("data", ["a", "b"], None, [VariableNode("a", 1)], 5),
    ])
  let view_file =
    ParsedViewFile(
      fields: [#("data", "List(#(String, List(#(Int, Int))))")],
      imports: [],
    )

  // Should pass - 2 elements in outer tuple
  generator.validate_template(tmpl, Some(view_file), "test.loom.html")
  |> should.be_ok
}

pub fn validate_tuple_arity_nested_tuple_mismatch_test() {
  // Nested tuple with wrong count should fail
  let tmpl =
    template([
      EachNode("data", ["a", "b", "c"], None, [VariableNode("a", 1)], 5),
    ])
  let view_file =
    ParsedViewFile(
      fields: [#("data", "List(#(String, List(#(Int, Int))))")],
      imports: [],
    )

  let result =
    generator.validate_template(tmpl, Some(view_file), "test.loom.html")

  result
  |> should.be_error

  case result {
    Error(msg) -> {
      msg
      |> string.contains("expected 2 variables but got 3")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_tuple_arity_unknown_collection_skips_test() {
  // Unknown collection should skip validation (no view file)
  let tmpl =
    template([
      EachNode("items", ["a", "b", "c", "d"], None, [VariableNode("a", 1)], 5),
    ])

  // No view file - can't validate
  generator.validate_template(tmpl, None, "test.loom.html")
  |> should.be_error
  // Will fail for undefined variable, not arity
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
