import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import glimr/loom/generator
import glimr/loom/lexer.{
  BoolAttr, ClassAttr, ExprAttr, LmModel, LmOn, StringAttr, StyleAttr,
}
import glimr/loom/parser.{
  type Node, type Template, AttributesNode, ComponentNode, EachNode, ElementNode,
  IfNode, RawVariableNode, SlotDefNode, SlotNode, Template, TextNode,
  VariableNode,
}

// Helper to create a template (simplified after layout removal)
fn template(nodes: List(Node)) -> Template {
  Template(imports: [], props: [], nodes: nodes, is_live: False)
}

// Helper to create a template with props
fn template_with_props(
  props: List(#(String, String)),
  nodes: List(Node),
) -> Template {
  Template(imports: [], props: props, nodes: nodes, is_live: False)
}

// Helper to create a live template (with is_live: True)
fn live_template(props: List(#(String, String)), nodes: List(Node)) -> Template {
  Template(imports: [], props: props, nodes: nodes, is_live: True)
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

// Helper to call generator with common defaults
fn generate(
  tmpl: Template,
  name: String,
  is_component: Bool,
) -> generator.GeneratedCode {
  generator.generate(tmpl, name, is_component, dict.new(), dict.new())
}

// ------------------------------------------------------------- Basic Generation Tests

pub fn generate_empty_template_test() {
  let result = generate(template([]), "empty", False)

  result.module_name
  |> should.equal("empty")

  // Should have html function with no params (no view file = no data params)
  result.code
  |> string.contains("pub fn render() -> String")
  |> should.be_true
}

pub fn generate_text_only_test() {
  let result = generate(template([TextNode("Hello!")]), "hello", False)

  result.code
  |> string.contains("<> \"Hello!\"")
  |> should.be_true

  // Should have html function with no params (no view file = no data params)
  result.code
  |> string.contains("pub fn render() -> String")
  |> should.be_true
}

pub fn generate_variable_test() {
  // With new architecture, template uses explicit data.name
  let result =
    generate(template([VariableNode("data.name", 1)]), "greet", False)

  // Should pass through the expression directly
  result.code
  |> string.contains("runtime.display(data.name)")
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
  |> string.contains("runtime.display(data.html)")
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

  // Should generate case expression with condition
  result.code
  |> string.contains("case data.show {")
  |> should.be_true

  result.code
  |> string.contains("True -> {")
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
  |> string.contains("case data.a {")
  |> should.be_true

  result.code
  |> string.contains("case data.b {")
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

  // Should use the expression directly in case
  result.code
  |> string.contains("case data.user == \"Miguel\" {")
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
  |> string.contains("case True {")
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
  |> string.contains("case False {")
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
  |> string.contains("case data.count > 0 {")
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
  |> string.contains("case !data.is_hidden {")
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

  // Should generate case expression with condition
  result.code
  |> string.contains("case data.show {")
  |> should.be_true

  // Should have True and False branches
  result.code
  |> string.contains("True -> {")
  |> should.be_true

  result.code
  |> string.contains("False -> {")
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
  // @if with @elseif and @else should generate nested case expressions
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

  // Should generate case expressions for each condition
  result.code
  |> string.contains("case data.a {")
  |> should.be_true

  result.code
  |> string.contains("case data.b {")
  |> should.be_true

  // Should have True branches
  result.code
  |> string.contains("True -> {")
  |> should.be_true
}

pub fn generate_if_with_function_call_test() {
  // @if with function call in condition - generates case with function call
  let result =
    generate(
      template([if_node("list.length(data.items) > 0", [TextNode("has items")])]),
      "if_func",
      False,
    )

  // Should generate case expression with function call
  result.code
  |> string.contains("case list.length(data.items) > 0 {")
  |> should.be_true

  result.code
  |> string.contains("True -> {")
  |> should.be_true
}

pub fn generate_if_elseif_with_function_calls_test() {
  // @if/@elseif with function calls in conditions
  let result =
    generate(
      template([
        if_elseif_else_node(
          [
            #("list.is_empty(data.items)", [TextNode("empty")]),
            #("list.length(data.items) == 1", [TextNode("one")]),
          ],
          [TextNode("many")],
        ),
      ]),
      "if_elseif_func",
      False,
    )

  // Should generate case expressions with function calls
  result.code
  |> string.contains("case list.is_empty(data.items) {")
  |> should.be_true

  result.code
  |> string.contains("case list.length(data.items) == 1 {")
  |> should.be_true
}

pub fn generate_if_with_nested_function_calls_test() {
  // @if with nested function calls
  let result =
    generate(
      template([
        if_node("string.length(string.uppercase(data.name)) > 5", [
          TextNode("long name"),
        ]),
      ]),
      "if_nested_func",
      False,
    )

  // Should generate case expression with nested function calls
  result.code
  |> string.contains("case string.length(string.uppercase(data.name)) > 5 {")
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

  // Should have case expressions for each condition
  result.code
  |> string.contains("case data.outer {")
  |> should.be_true

  result.code
  |> string.contains("case data.inner {")
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
  |> string.contains("runtime.display(key)")
  |> should.be_true

  result.code
  |> string.contains("runtime.display(value)")
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
  |> string.contains("runtime.display(title)")
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

  // Should call component render with labeled arguments
  result.code
  |> string.contains("components_alert.render(")
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
  let component_props =
    dict.from_list([
      #(
        "input",
        generator.ComponentData(props: [#("value", "String")], is_live: False),
      ),
    ])
  let result =
    generator.generate(tmpl, "form", False, component_props, dict.new())

  // Should pass expression directly as prop (since "value" is a known prop)
  result.code
  |> string.contains("value: data.name")
  |> should.be_true
}

pub fn generate_component_string_attr_as_prop_test() {
  // title="Welcome" (without : prefix) should pass "Welcome" as a string
  // prop when the component declares @props(title: String)
  let tmpl =
    template([ComponentNode("card", [StringAttr("title", "Welcome")], [])])
  let component_props =
    dict.from_list([
      #(
        "card",
        generator.ComponentData(props: [#("title", "String")], is_live: False),
      ),
    ])
  let result =
    generator.generate(tmpl, "page", False, component_props, dict.new())

  // Should pass as a Gleam string literal prop
  result.code
  |> string.contains("title: \"Welcome\"")
  |> should.be_true
}

pub fn generate_component_string_attr_not_prop_stays_as_attribute_test() {
  // class="card" should remain an HTML attribute when it doesn't match a prop name
  let tmpl =
    template([ComponentNode("card", [StringAttr("class", "card")], [])])
  let component_props =
    dict.from_list([
      #(
        "card",
        generator.ComponentData(props: [#("title", "String")], is_live: False),
      ),
    ])
  let result =
    generator.generate(tmpl, "page", False, component_props, dict.new())

  // "class" is not a prop, so should NOT appear as class: "card"
  result.code
  |> string.contains("class: \"card\"")
  |> should.be_false

  // Should be passed through as an HTML attribute
  result.code
  |> string.contains("Attribute(\"class\", \"card\")")
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

pub fn validate_template_with_defined_variable_test() {
  let tmpl =
    template_with_props([#("title", "String")], [VariableNode("title", 1)])

  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_expression_variable_test() {
  // Expressions are allowed - Gleam compiler validates
  let tmpl =
    template_with_props([#("title", "String")], [
      VariableNode("string.uppercase(title)", 5),
    ])

  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_dotted_access_test() {
  // user.name should validate the root "user"
  let tmpl =
    template_with_props([#("user", "User")], [VariableNode("user.name", 1)])

  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_any_dotted_access_test() {
  // Any expression is allowed - Gleam compiler validates
  let tmpl =
    template_with_props([#("title", "String")], [VariableNode("user.name", 1)])

  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_loop_variable_test() {
  // Loop variables should be valid inside the loop
  let tmpl =
    template_with_props([#("items", "List(Item)")], [
      EachNode("items", ["item"], None, [VariableNode("item.name", 1)], 1),
    ])

  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_loop_var_outside_scope_test() {
  // Expressions are allowed - Gleam compiler validates scope
  let tmpl =
    template_with_props([#("items", "List(Item)")], [
      EachNode("items", ["item"], None, [TextNode("in loop")], 1),
      VariableNode("item", 1),
    ])

  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_tuple_destructuring_test() {
  // Tuple destructuring in l-for should make both vars available
  let tmpl =
    template_with_props([#("pairs", "List(#(String, String))")], [
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

  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_loop_index_test() {
  // Loop index variable should be available
  let tmpl =
    template_with_props([#("items", "List(Item)")], [
      EachNode(
        "items",
        ["item"],
        Some("loop"),
        [VariableNode("loop.index", 1)],
        1,
      ),
    ])

  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_slot_variable_test() {
  // Default slot variable should always be valid
  let tmpl = template([IfNode([#(Some("slot"), 1, [TextNode("has slot")])])])

  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_named_slot_variable_test() {
  // Named slot variable should be valid
  let tmpl =
    template([
      SlotNode(Some("header"), []),
      IfNode([#(Some("slot.header"), 1, [TextNode("has header")])]),
    ])

  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_if_condition_test() {
  // Variables in l-if conditions should be validated
  let tmpl =
    template_with_props([#("is_visible", "Bool")], [
      IfNode([#(Some("is_visible"), 1, [TextNode("visible")])]),
    ])

  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_expression_in_if_condition_test() {
  // Expressions in l-if are allowed - Gleam compiler validates
  let tmpl =
    template_with_props([#("age", "Int")], [
      IfNode([#(Some("age > 18"), 1, [TextNode("adult")])]),
    ])

  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_raw_variable_test() {
  // Raw variables should also be validated
  let tmpl =
    template_with_props([#("html_content", "String")], [
      RawVariableNode("html_content", 1),
    ])

  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_raw_expression_test() {
  // Raw expressions are allowed - Gleam compiler validates
  let tmpl =
    template_with_props([#("title", "String")], [
      RawVariableNode("string.uppercase(title)", 1),
    ])

  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_no_props_test() {
  // Expressions are allowed - Gleam compiler validates
  let tmpl = template([VariableNode("title", 1)])

  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_template_with_nested_loop_test() {
  // Nested loop variables should have correct scope
  let tmpl =
    template_with_props([#("users", "List(User)")], [
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

  generator.validate_template(tmpl, "test.loom.html")
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

  result.code
  |> string.contains("runtime.display(loop.index)")
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
  |> string.contains("runtime.display(loop.iteration)")
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
  |> string.contains("runtime.display(loop.count)")
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
  |> string.contains("runtime.display(loop.remaining)")
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

  result.code
  |> string.contains("runtime.display(loop.first)")
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
  |> string.contains("runtime.display(loop.last)")
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
  |> string.contains("runtime.display(loop.even)")
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
  |> string.contains("runtime.display(loop.odd)")
  |> should.be_true
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
  |> string.contains("runtime.display(user_loop.index)")
  |> should.be_true

  result.code
  |> string.contains("runtime.display(user_loop.first)")
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
  |> string.contains("runtime.display(user_loop.index)")
  |> should.be_true

  result.code
  |> string.contains("runtime.display(post_loop.iteration)")
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

  // Raw output uses to_string (no escaping) instead of display
  result.code
  |> string.contains("runtime.to_string(loop.index)")
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
    template_with_props([#("items", "List(#(String, String, String))")], [
      EachNode("items", ["a", "b", "c", "d"], None, [VariableNode("a", 1)], 5),
    ])

  let result = generator.validate_template(tmpl, "test.loom.html")

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
    template_with_props([#("items", "List(#(String, String, String))")], [
      EachNode("items", ["a", "b", "c"], None, [VariableNode("a", 1)], 5),
    ])

  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_tuple_arity_two_elements_test() {
  // Two-element tuple should work
  let tmpl =
    template_with_props([#("pairs", "List(#(String, Int))")], [
      EachNode("pairs", ["key", "value"], None, [VariableNode("key", 1)], 5),
    ])

  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_tuple_arity_two_elements_mismatch_test() {
  // Three variables for two-element tuple should fail
  let tmpl =
    template_with_props([#("pairs", "List(#(String, Int))")], [
      EachNode("pairs", ["a", "b", "c"], None, [VariableNode("a", 1)], 5),
    ])

  let result = generator.validate_template(tmpl, "test.loom.html")

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
    template_with_props([#("items", "List(#(String, String, String))")], [
      EachNode("items", ["item"], None, [VariableNode("item", 1)], 5),
    ])

  // Should pass - single var means no tuple destructuring
  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_tuple_arity_non_tuple_list_skips_check_test() {
  // List of non-tuples should skip arity check
  let tmpl =
    template_with_props([#("names", "List(String)")], [
      EachNode("names", ["a", "b"], None, [VariableNode("a", 1)], 5),
    ])

  // Should pass - can't validate non-tuple types
  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_tuple_arity_nested_tuple_types_test() {
  // Nested tuple types should count correctly
  let tmpl =
    template_with_props([#("data", "List(#(String, List(#(Int, Int))))")], [
      EachNode("data", ["a", "b"], None, [VariableNode("a", 1)], 5),
    ])

  // Should pass - 2 elements in outer tuple
  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

pub fn validate_tuple_arity_nested_tuple_mismatch_test() {
  // Nested tuple with wrong count should fail
  let tmpl =
    template_with_props([#("data", "List(#(String, List(#(Int, Int))))")], [
      EachNode("data", ["a", "b", "c"], None, [VariableNode("a", 1)], 5),
    ])

  let result = generator.validate_template(tmpl, "test.loom.html")

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
  // Unknown collection should skip arity validation
  let tmpl =
    template([
      EachNode("items", ["a", "b", "c", "d"], None, [VariableNode("a", 1)], 5),
    ])

  // Expressions are allowed - Gleam compiler validates
  generator.validate_template(tmpl, "test.loom.html")
  |> should.be_ok
}

// ------------------------------------------------------------- Live Template Tests

pub fn generate_lm_on_data_attribute_test() {
  // Template with l-on:click handler
  let tmpl =
    live_template([#("count", "Int")], [
      ElementNode("button", [LmOn("click", [], "count = count + 1", 1)], [
        TextNode("+"),
      ]),
    ])

  let result = generate(tmpl, "counter", False)

  // Should generate data-l-click attribute with handler ID
  result.code
  |> string.contains("data-l-click")
  |> should.be_true

  result.code
  |> string.contains("handle_click_0")
  |> should.be_true
}

pub fn generate_multiple_lm_on_handlers_test() {
  // Template with multiple handlers
  let tmpl =
    live_template([#("count", "Int")], [
      ElementNode("button", [LmOn("click", [], "count = count + 1", 1)], [
        TextNode("+"),
      ]),
      ElementNode("button", [LmOn("click", [], "count = count - 1", 2)], [
        TextNode("-"),
      ]),
    ])

  let result = generate(tmpl, "counter", False)

  // Should have two different handler IDs
  result.code
  |> string.contains("handle_click_0")
  |> should.be_true

  result.code
  |> string.contains("handle_click_1")
  |> should.be_true
}

pub fn generate_lm_model_data_attribute_test() {
  // Template with l-model binding
  let tmpl =
    live_template([#("name", "String")], [
      ElementNode("input", [LmModel("name", 1)], []),
    ])

  let result = generate(tmpl, "form", False)

  // Should generate data-l-input attribute with handler ID
  result.code
  |> string.contains("data-l-input")
  |> should.be_true

  result.code
  |> string.contains("handle_input_0")
  |> should.be_true

  // Should also generate value attribute for the input
  result.code
  |> string.contains("\"value\", name")
  |> should.be_true
}

pub fn generate_lm_on_with_modifiers_test() {
  // Template with event modifiers
  let tmpl =
    live_template([#("errors", "List(String)")], [
      ElementNode(
        "form",
        [LmOn("submit", ["prevent"], "errors = form.submit()", 1)],
        [],
      ),
    ])

  let result = generate(tmpl, "registration", False)

  // Should generate data-l-submit attribute
  result.code
  |> string.contains("data-l-submit")
  |> should.be_true

  result.code
  |> string.contains("handle_submit_0")
  |> should.be_true

  // Should generate modifier data attribute
  result.code
  |> string.contains("data-l-prevent")
  |> should.be_true
}

pub fn generate_lm_on_with_multiple_modifiers_test() {
  // Template with multiple event modifiers
  let tmpl =
    live_template([#("count", "Int")], [
      ElementNode(
        "button",
        [LmOn("click", ["prevent", "stop"], "count = count + 1", 1)],
        [TextNode("+")],
      ),
    ])

  let result = generate(tmpl, "counter", False)

  // Should generate both modifier data attributes
  result.code
  |> string.contains("data-l-prevent")
  |> should.be_true

  result.code
  |> string.contains("data-l-stop")
  |> should.be_true
}

pub fn generate_lm_on_mixed_events_test() {
  // Template with different event types
  let tmpl =
    live_template([#("count", "Int"), #("name", "String")], [
      ElementNode("button", [LmOn("click", [], "count = count + 1", 1)], [
        TextNode("+"),
      ]),
      ElementNode("input", [LmOn("input", [], "name = $value", 2)], []),
    ])

  let result = generate(tmpl, "mixed", False)

  // Should have both data-l-click and data-l-input
  result.code
  |> string.contains("data-l-click")
  |> should.be_true

  result.code
  |> string.contains("data-l-input")
  |> should.be_true

  result.code
  |> string.contains("handle_click_0")
  |> should.be_true

  result.code
  |> string.contains("handle_input_1")
  |> should.be_true
}

pub fn generate_non_live_template_no_data_attrs_test() {
  // Non-live template with l-on should NOT generate data attributes
  // (because handler_lookup will be empty)
  let tmpl =
    Template(
      imports: [],
      props: [#("count", "Int")],
      nodes: [
        ElementNode("button", [LmOn("click", [], "count = count + 1", 1)], [
          TextNode("+"),
        ]),
      ],
      is_live: False,
    )

  let result = generate(tmpl, "notlive", False)

  // Should NOT have data-l-click (because is_live is False)
  result.code
  |> string.contains("data-l-click")
  |> should.be_false
}

pub fn generate_live_template_has_is_live_function_test() {
  let tmpl =
    live_template([#("count", "Int")], [
      ElementNode("button", [LmOn("click", [], "count = count + 1", 1)], [
        TextNode("+"),
      ]),
    ])

  let result = generate(tmpl, "counter", False)

  // Should have is_live() function that returns True
  result.code
  |> string.contains("pub fn is_live() -> Bool")
  |> should.be_true

  result.code
  |> string.contains("True")
  |> should.be_true
}

pub fn generate_live_template_has_handlers_function_test() {
  let tmpl =
    live_template([#("count", "Int")], [
      ElementNode(
        "button",
        [LmOn("click", ["prevent"], "count = count + 1", 1)],
        [TextNode("+")],
      ),
    ])

  let result = generate(tmpl, "counter", False)

  // Should have handlers() function
  result.code
  |> string.contains("pub fn handlers()")
  |> should.be_true

  // Handler info should include the handler ID
  result.code
  |> string.contains("handle_click_0")
  |> should.be_true

  // Handler info should include the event type
  result.code
  |> string.contains("\"click\"")
  |> should.be_true

  // Handler info should include modifiers
  result.code
  |> string.contains("\"prevent\"")
  |> should.be_true
}

pub fn generate_live_template_has_prop_names_function_test() {
  let tmpl =
    live_template([#("count", "Int"), #("name", "String")], [
      ElementNode("button", [LmOn("click", [], "count = count + 1", 1)], [
        TextNode("+"),
      ]),
    ])

  let result = generate(tmpl, "counter", False)

  // Should have prop_names() function
  result.code
  |> string.contains("pub fn prop_names()")
  |> should.be_true

  // Should list prop names
  result.code
  |> string.contains("\"count\"")
  |> should.be_true

  result.code
  |> string.contains("\"name\"")
  |> should.be_true
}

pub fn generate_non_live_template_no_live_functions_test() {
  // Non-live template should NOT have is_live or handlers functions
  let tmpl = template([TextNode("Hello")])

  let result = generate(tmpl, "static", False)

  result.code
  |> string.contains("pub fn is_live()")
  |> should.be_false

  result.code
  |> string.contains("pub fn handlers()")
  |> should.be_false
}

pub fn generate_live_template_has_wrapper_div_test() {
  // Live template (page, not component) should use runtime.inject_live_wrapper
  let tmpl =
    live_template([#("count", "Int")], [
      ElementNode("button", [LmOn("click", [], "count = count + 1", 1)], [
        TextNode("+"),
      ]),
    ])

  let result = generate(tmpl, "counter", False)

  // Should call runtime.inject_live_wrapper with module name
  result.code
  |> string.contains("runtime.inject_live_wrapper")
  |> should.be_true

  // Should pass the module name to inject_live_wrapper
  result.code
  |> string.contains("\"counter\"")
  |> should.be_true
}

pub fn generate_live_component_no_wrapper_test() {
  // Live component should NOT have wrapper div (only pages get wrapper)
  let tmpl =
    live_template([#("count", "Int")], [
      ElementNode("button", [LmOn("click", [], "count = count + 1", 1)], [
        TextNode("+"),
      ]),
    ])

  // Pass is_component=True
  let result = generator.generate(tmpl, "counter", True, dict.new(), dict.new())

  // Should NOT have wrapper div (components don't get wrapped)
  result.code
  |> string.contains("data-l-live=\\\"live\\\"")
  |> should.be_false

  // Should NOT have loom.js script (that's for pages)
  result.code
  |> string.contains("/loom.js")
  |> should.be_false
}

pub fn generate_non_live_template_no_wrapper_test() {
  // Non-live template should NOT have wrapper div
  let tmpl = template([TextNode("Hello")])

  let result = generate(tmpl, "static", False)

  result.code
  |> string.contains("data-l-live")
  |> should.be_false

  result.code
  |> string.contains("/loom.js")
  |> should.be_false
}

// ------------------------------------------------------------- Attribute Normalization Integration Tests
// These go through the full pipeline (tokenize → parse → generate)
// to verify that l-* attributes on non-dynamic elements get
// normalized to data-l-* in the final output.

pub fn normalize_l_no_nav_bool_attr_full_pipeline_test() {
  // l-no-nav on a plain <a> tag — element is NOT dynamic so the lexer
  // emits it as raw text. The attribute must still be normalized.
  let html = "<a href=\"/redirect\" l-no-nav>Redirect</a>"
  let assert Ok(tokens) = lexer.tokenize(html)
  let assert Ok(tmpl) = parser.parse(tokens)
  let result = generate(tmpl, "page", False)

  result.code
  |> string.contains("data-l-no-nav")
  |> should.be_true

  result.code
  |> string.contains("\" l-no-nav\"")
  |> should.be_false
}

pub fn normalize_l_loading_string_attr_full_pipeline_test() {
  // l-loading="save" on a plain <span> — not dynamic, emitted as text
  let html = "<span l-loading=\"save\">Saving</span>"
  let assert Ok(tokens) = lexer.tokenize(html)
  let assert Ok(tmpl) = parser.parse(tokens)
  let result = generate(tmpl, "page", False)

  result.code
  |> string.contains("data-l-loading")
  |> should.be_true
}

pub fn normalize_leaves_regular_attrs_alone_full_pipeline_test() {
  // Regular attributes like class and href must NOT be prefixed
  let html = "<a href=\"/about\" class=\"link\">About</a>"
  let assert Ok(tokens) = lexer.tokenize(html)
  let assert Ok(tmpl) = parser.parse(tokens)
  let result = generate(tmpl, "page", False)

  result.code
  |> string.contains("data-href")
  |> should.be_false

  result.code
  |> string.contains("data-class")
  |> should.be_false
}

// ------------------------------------------------------------- Attribute Normalization Tests

pub fn generate_l_prefix_string_attr_normalized_test() {
  // l-loading="save-btn" on a standard element should become data-l-loading
  let result =
    generate(
      template([
        ElementNode("span", [StringAttr("l-loading", "save-btn")], [
          TextNode("Saving..."),
        ]),
      ]),
      "norm",
      False,
    )

  result.code
  |> string.contains("data-l-loading")
  |> should.be_true

  // Should NOT have bare l-loading (without data- prefix)
  result.code
  |> string.contains("\"l-loading\"")
  |> should.be_false
}

pub fn generate_l_prefix_bool_attr_normalized_test() {
  // <span l-no-nav> should become data-l-no-nav
  let result =
    generate(
      template([
        ElementNode("span", [BoolAttr("l-no-nav")], [TextNode("Link")]),
      ]),
      "norm",
      False,
    )

  result.code
  |> string.contains("data-l-no-nav")
  |> should.be_true

  // Should NOT have bare l-no-nav
  result.code
  |> string.contains("\"l-no-nav\"")
  |> should.be_false
}

pub fn generate_regular_attr_not_normalized_test() {
  // class="btn" should NOT be prefixed with data-
  let result =
    generate(
      template([
        ElementNode("span", [StringAttr("class", "btn")], [TextNode("Click")]),
      ]),
      "norm",
      False,
    )

  result.code
  |> string.contains("\"class\"")
  |> should.be_true

  result.code
  |> string.contains("data-class")
  |> should.be_false
}

pub fn generate_data_l_prefix_not_double_normalized_test() {
  // data-l-loading should NOT become data-data-l-loading
  let result =
    generate(
      template([
        ElementNode("span", [StringAttr("data-l-loading", "save")], []),
      ]),
      "norm",
      False,
    )

  result.code
  |> string.contains("data-l-loading")
  |> should.be_true

  result.code
  |> string.contains("data-data-l-loading")
  |> should.be_false
}

pub fn generate_live_l_prefix_string_attr_normalized_test() {
  // l-loading on a live template element (tree path) should also be normalized
  let tmpl =
    live_template([#("count", "Int")], [
      ElementNode(
        "button",
        [
          LmOn("click", [], "count = count + 1", 1),
          StringAttr("l-loading", "counter-btn"),
        ],
        [TextNode("+")],
      ),
    ])

  let result = generate(tmpl, "counter", False)

  result.code
  |> string.contains("data-l-loading")
  |> should.be_true

  result.code
  |> string.contains("\"l-loading\"")
  |> should.be_false
}

pub fn generate_live_l_prefix_bool_attr_normalized_test() {
  // l-no-nav on a live template element (tree path) should also be normalized
  let tmpl =
    live_template([#("count", "Int")], [
      ElementNode(
        "a",
        [BoolAttr("l-no-nav"), LmOn("click", [], "count = count + 1", 1)],
        [TextNode("Link")],
      ),
    ])

  let result = generate(tmpl, "counter", False)

  result.code
  |> string.contains("data-l-no-nav")
  |> should.be_true

  result.code
  |> string.contains("\"l-no-nav\"")
  |> should.be_false
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
