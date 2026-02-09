import gleam/option.{None, Some}
import gleeunit/should
import glimr/loom/lexer.{
  Attributes, BoolAttr, ClassAttr, Component, ComponentEnd, Element, ElementEnd,
  ExprAttr, LmElse, LmElseIf, LmFor, LmIf, LmModel, LmOn, RawVariable, Slot,
  SlotDef, SlotDefEnd, StringAttr, StyleAttr, Text, Variable,
}

// ------------------------------------------------------------- Text and Variable Tests

pub fn tokenize_plain_text_test() {
  let assert Ok(tokens) = lexer.tokenize("Hello, World!")

  tokens
  |> should.equal([Text("Hello, World!")])
}

pub fn tokenize_variable_test() {
  let assert Ok(tokens) = lexer.tokenize("{{ name }}")

  tokens
  |> should.equal([Variable("name", 1)])
}

pub fn tokenize_variable_no_spaces_test() {
  let assert Ok(tokens) = lexer.tokenize("{{name}}")

  tokens
  |> should.equal([Variable("name", 1)])
}

pub fn tokenize_raw_variable_test() {
  let assert Ok(tokens) = lexer.tokenize("{{{ html }}}")

  tokens
  |> should.equal([RawVariable("html", 1)])
}

pub fn tokenize_text_with_variable_test() {
  let assert Ok(tokens) = lexer.tokenize("Hello, {{ name }}!")

  tokens
  |> should.equal([Text("Hello, "), Variable("name", 1), Text("!")])
}

pub fn tokenize_multiple_variables_test() {
  let assert Ok(tokens) = lexer.tokenize("{{ first }} and {{ second }}")

  tokens
  |> should.equal([Variable("first", 1), Text(" and "), Variable("second", 1)])
}

pub fn tokenize_dotted_variable_test() {
  let assert Ok(tokens) = lexer.tokenize("{{ user.name }}")

  tokens
  |> should.equal([Variable("user.name", 1)])
}

// ------------------------------------------------------------- l-if Tests

pub fn tokenize_lm_if_test() {
  let assert Ok(tokens) = lexer.tokenize("<p l-if=\"show\">visible</p>")

  tokens
  |> should.equal([
    Element("p", [LmIf("show", 1)], False),
    Text("visible"),
    ElementEnd("p"),
  ])
}

pub fn tokenize_lm_if_with_single_quotes_test() {
  let assert Ok(tokens) = lexer.tokenize("<p l-if='show'>visible</p>")

  tokens
  |> should.equal([
    Element("p", [LmIf("show", 1)], False),
    Text("visible"),
    ElementEnd("p"),
  ])
}

pub fn tokenize_lm_if_with_condition_test() {
  let assert Ok(tokens) = lexer.tokenize("<p l-if=\"count > 0\">has items</p>")

  tokens
  |> should.equal([
    Element("p", [LmIf("count > 0", 1)], False),
    Text("has items"),
    ElementEnd("p"),
  ])
}

pub fn tokenize_lm_if_self_closing_test() {
  let assert Ok(tokens) = lexer.tokenize("<br l-if=\"show\" />")

  tokens
  |> should.equal([Element("br", [LmIf("show", 1)], True)])
}

pub fn tokenize_lm_if_with_function_call_test() {
  // l-if with function call in condition - Gleam compiler validates
  let assert Ok(tokens) =
    lexer.tokenize("<p l-if=\"list.length(items) > 0\">has items</p>")

  tokens
  |> should.equal([
    Element("p", [LmIf("list.length(items) > 0", 1)], False),
    Text("has items"),
    ElementEnd("p"),
  ])
}

pub fn tokenize_lm_if_with_nested_function_calls_test() {
  // l-if with nested function calls
  let assert Ok(tokens) =
    lexer.tokenize(
      "<p l-if=\"string.length(string.uppercase(name)) > 5\">long</p>",
    )

  tokens
  |> should.equal([
    Element("p", [LmIf("string.length(string.uppercase(name)) > 5", 1)], False),
    Text("long"),
    ElementEnd("p"),
  ])
}

// ------------------------------------------------------------- l-else-if and l-else Tests

pub fn tokenize_lm_else_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<p l-if=\"show\">yes</p><p l-else>no</p>")

  tokens
  |> should.equal([
    Element("p", [LmIf("show", 1)], False),
    Text("yes"),
    ElementEnd("p"),
    Element("p", [LmElse], False),
    Text("no"),
    ElementEnd("p"),
  ])
}

pub fn tokenize_lm_else_if_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<p l-if=\"a\">A</p><p l-else-if=\"b\">B</p><p l-else>C</p>")

  tokens
  |> should.equal([
    Element("p", [LmIf("a", 1)], False),
    Text("A"),
    ElementEnd("p"),
    Element("p", [LmElseIf("b", 1)], False),
    Text("B"),
    ElementEnd("p"),
    Element("p", [LmElse], False),
    Text("C"),
    ElementEnd("p"),
  ])
}

pub fn tokenize_multiple_lm_else_if_test() {
  let assert Ok(tokens) =
    lexer.tokenize(
      "<p l-if=\"a\">A</p><p l-else-if=\"b\">B</p><p l-else-if=\"c\">C</p><p l-else>D</p>",
    )

  tokens
  |> should.equal([
    Element("p", [LmIf("a", 1)], False),
    Text("A"),
    ElementEnd("p"),
    Element("p", [LmElseIf("b", 1)], False),
    Text("B"),
    ElementEnd("p"),
    Element("p", [LmElseIf("c", 1)], False),
    Text("C"),
    ElementEnd("p"),
    Element("p", [LmElse], False),
    Text("D"),
    ElementEnd("p"),
  ])
}

pub fn tokenize_lm_else_if_with_function_call_test() {
  // l-else-if with function calls in conditions
  let assert Ok(tokens) =
    lexer.tokenize(
      "<p l-if=\"list.is_empty(items)\">empty</p><p l-else-if=\"list.length(items) == 1\">one</p><p l-else>many</p>",
    )

  tokens
  |> should.equal([
    Element("p", [LmIf("list.is_empty(items)", 1)], False),
    Text("empty"),
    ElementEnd("p"),
    Element("p", [LmElseIf("list.length(items) == 1", 1)], False),
    Text("one"),
    ElementEnd("p"),
    Element("p", [LmElse], False),
    Text("many"),
    ElementEnd("p"),
  ])
}

// ------------------------------------------------------------- l-for Tests

pub fn tokenize_lm_for_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<li l-for=\"item in items\">{{ item }}</li>")

  tokens
  |> should.equal([
    Element("li", [LmFor("items", ["item"], None, 1)], False),
    Variable("item", 1),
    ElementEnd("li"),
  ])
}

pub fn tokenize_lm_for_with_single_quotes_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<li l-for='item in items'>{{ item }}</li>")

  tokens
  |> should.equal([
    Element("li", [LmFor("items", ["item"], None, 1)], False),
    Variable("item", 1),
    ElementEnd("li"),
  ])
}

pub fn tokenize_lm_for_tuple_destructuring_test() {
  let assert Ok(tokens) =
    lexer.tokenize(
      "<li l-for=\"(key, value) in items\">{{ key }}: {{ value }}</li>",
    )

  tokens
  |> should.equal([
    Element("li", [LmFor("items", ["key", "value"], None, 1)], False),
    Variable("key", 1),
    Text(": "),
    Variable("value", 1),
    ElementEnd("li"),
  ])
}

pub fn tokenize_lm_for_with_loop_var_test() {
  let assert Ok(tokens) =
    lexer.tokenize(
      "<li l-for=\"item in items, loop\">{{ loop.index }}: {{ item }}</li>",
    )

  tokens
  |> should.equal([
    Element("li", [LmFor("items", ["item"], Some("loop"), 1)], False),
    Variable("loop.index", 1),
    Text(": "),
    Variable("item", 1),
    ElementEnd("li"),
  ])
}

pub fn tokenize_lm_for_tuple_with_loop_var_test() {
  let assert Ok(tokens) =
    lexer.tokenize(
      "<li l-for=\"(key, value) in pairs, idx\">{{ idx.iteration }}: {{ key }}</li>",
    )

  tokens
  |> should.equal([
    Element("li", [LmFor("pairs", ["key", "value"], Some("idx"), 1)], False),
    Variable("idx.iteration", 1),
    Text(": "),
    Variable("key", 1),
    ElementEnd("li"),
  ])
}

pub fn tokenize_lm_for_self_closing_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<img l-for=\"img in images\" :src=\"img.url\" />")

  tokens
  |> should.equal([
    Element(
      "img",
      [LmFor("images", ["img"], None, 1), ExprAttr("src", "img.url")],
      True,
    ),
  ])
}

pub fn tokenize_element_expr_attr_single_quotes_test() {
  // HTML element with single quotes for expression attribute
  let assert Ok(tokens) =
    lexer.tokenize("<input l-if=\"show\" :disabled='status == \"locked\"' />")

  tokens
  |> should.equal([
    Element(
      "input",
      [LmIf("show", 1), ExprAttr("disabled", "status == \"locked\"")],
      True,
    ),
  ])
}

// ------------------------------------------------------------- l-* with other attributes

pub fn tokenize_lm_if_with_class_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<p l-if=\"show\" class=\"message\">Hello</p>")

  tokens
  |> should.equal([
    Element("p", [LmIf("show", 1), StringAttr("class", "message")], False),
    Text("Hello"),
    ElementEnd("p"),
  ])
}

pub fn tokenize_lm_for_with_multiple_attrs_test() {
  let assert Ok(tokens) =
    lexer.tokenize(
      "<li l-for=\"item in items\" class=\"item\" :id=\"item.id\">{{ item.name }}</li>",
    )

  tokens
  |> should.equal([
    Element(
      "li",
      [
        LmFor("items", ["item"], None, 1),
        StringAttr("class", "item"),
        ExprAttr("id", "item.id"),
      ],
      False,
    ),
    Variable("item.name", 1),
    ElementEnd("li"),
  ])
}

// ------------------------------------------------------------- Component with l-* attributes

pub fn tokenize_component_with_lm_if_test() {
  let assert Ok(tokens) =
    lexer.tokenize(
      "<x-alert l-if=\"show_error\" type=\"error\">{{ message }}</x-alert>",
    )

  tokens
  |> should.equal([
    Component(
      "alert",
      [LmIf("show_error", 1), StringAttr("type", "error")],
      False,
    ),
    Variable("message", 1),
    ComponentEnd("alert"),
  ])
}

pub fn tokenize_component_with_lm_for_test() {
  let assert Ok(tokens) =
    lexer.tokenize(
      "<x-card l-for=\"card in cards\" :title=\"card.title\">{{ card.body }}</x-card>",
    )

  tokens
  |> should.equal([
    Component(
      "card",
      [LmFor("cards", ["card"], None, 1), ExprAttr("title", "card.title")],
      False,
    ),
    Variable("card.body", 1),
    ComponentEnd("card"),
  ])
}

pub fn tokenize_component_self_closing_with_lm_if_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<x-icon l-if=\"show\" name=\"check\" />")

  tokens
  |> should.equal([
    Component("icon", [LmIf("show", 1), StringAttr("name", "check")], True),
  ])
}

// ------------------------------------------------------------- Template element tests

pub fn tokenize_template_element_test() {
  let assert Ok(tokens) =
    lexer.tokenize(
      "<template l-if=\"show\"><p>First</p><p>Second</p></template>",
    )

  tokens
  |> should.equal([
    Element("template", [LmIf("show", 1)], False),
    Text("<p>First</p><p>Second</p>"),
    ElementEnd("template"),
  ])
}

// ------------------------------------------------------------- Slot Tests

pub fn tokenize_slot_self_closing_test() {
  let assert Ok(tokens) = lexer.tokenize("<div><slot /></div>")

  tokens
  |> should.equal([Text("<div>"), Slot(None), Text("</div>")])
}

pub fn tokenize_slot_self_closing_no_space_test() {
  let assert Ok(tokens) = lexer.tokenize("<slot/>")

  tokens
  |> should.equal([Slot(None)])
}

pub fn tokenize_named_slot_self_closing_test() {
  let assert Ok(tokens) = lexer.tokenize("<div><slot name=\"header\" /></div>")

  tokens
  |> should.equal([Text("<div>"), Slot(Some("header")), Text("</div>")])
}

pub fn tokenize_named_slot_with_underscores_test() {
  let assert Ok(tokens) = lexer.tokenize("<slot name=\"left_sidebar\" />")

  tokens
  |> should.equal([Slot(Some("left_sidebar"))])
}

pub fn tokenize_slot_with_fallback_test() {
  let assert Ok(tokens) = lexer.tokenize("<slot>Default content</slot>")

  tokens
  |> should.equal([SlotDef(None), Text("Default content"), SlotDefEnd])
}

pub fn tokenize_named_slot_with_fallback_test() {
  let assert Ok(tokens) = lexer.tokenize("<slot name=\"header\">Title</slot>")

  tokens
  |> should.equal([SlotDef(Some("header")), Text("Title"), SlotDefEnd])
}

pub fn tokenize_slot_def_single_quotes_test() {
  let assert Ok(tokens) = lexer.tokenize("<slot name='footer'>Copyright</slot>")

  tokens
  |> should.equal([SlotDef(Some("footer")), Text("Copyright"), SlotDefEnd])
}

// ------------------------------------------------------------- Component Tests

pub fn tokenize_self_closing_component_test() {
  let assert Ok(tokens) = lexer.tokenize("<x-alert />")

  tokens
  |> should.equal([Component("alert", [], True)])
}

pub fn tokenize_component_with_children_test() {
  let assert Ok(tokens) = lexer.tokenize("<x-card>Content</x-card>")

  tokens
  |> should.equal([
    Component("card", [], False),
    Text("Content"),
    ComponentEnd("card"),
  ])
}

pub fn tokenize_nested_component_name_test() {
  let assert Ok(tokens) = lexer.tokenize("<x-forms:input />")

  tokens
  |> should.equal([Component("forms:input", [], True)])
}

pub fn tokenize_component_with_string_attr_test() {
  let assert Ok(tokens) = lexer.tokenize("<x-alert type=\"success\" />")

  tokens
  |> should.equal([Component("alert", [StringAttr("type", "success")], True)])
}

pub fn tokenize_component_with_expr_attr_test() {
  let assert Ok(tokens) = lexer.tokenize("<x-alert :message=\"error_msg\" />")

  tokens
  |> should.equal([Component("alert", [ExprAttr("message", "error_msg")], True)])
}

pub fn tokenize_expr_attr_single_quotes_test() {
  // Single quotes outside, double quotes inside for string comparison
  let assert Ok(tokens) =
    lexer.tokenize("<x-input :disabled='status == \"inactive\"' />")

  tokens
  |> should.equal([
    Component("input", [ExprAttr("disabled", "status == \"inactive\"")], True),
  ])
}

pub fn tokenize_expr_attr_double_quotes_test() {
  // Double quotes outside, single quotes inside - single quotes get normalized to double
  let assert Ok(tokens) =
    lexer.tokenize("<x-input :disabled=\"status == 'inactive'\" />")

  // Single quotes are converted to double quotes for valid Gleam
  tokens
  |> should.equal([
    Component("input", [ExprAttr("disabled", "status == \"inactive\"")], True),
  ])
}

pub fn tokenize_component_with_bool_attr_test() {
  let assert Ok(tokens) = lexer.tokenize("<x-button disabled />")

  tokens
  |> should.equal([Component("button", [BoolAttr("disabled")], True)])
}

pub fn tokenize_component_with_multiple_attrs_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<x-input type=\"text\" :value=\"name\" required />")

  tokens
  |> should.equal([
    Component(
      "input",
      [
        StringAttr("type", "text"),
        ExprAttr("value", "name"),
        BoolAttr("required"),
      ],
      True,
    ),
  ])
}

pub fn tokenize_nested_components_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<x-card><x-button>Click</x-button></x-card>")

  tokens
  |> should.equal([
    Component("card", [], False),
    Component("button", [], False),
    Text("Click"),
    ComponentEnd("button"),
    ComponentEnd("card"),
  ])
}

// ------------------------------------------------------------- Attributes Tests

pub fn tokenize_attributes_standalone_test() {
  let assert Ok(tokens) = lexer.tokenize("@attributes")

  tokens
  |> should.equal([Attributes])
}

pub fn tokenize_attributes_in_tag_test() {
  let assert Ok(tokens) = lexer.tokenize("<div@attributes>content</div>")

  tokens
  |> should.equal([Text("<div"), Attributes, Text(">content</div>")])
}

pub fn tokenize_attributes_with_space_test() {
  let assert Ok(tokens) = lexer.tokenize("<button @attributes>Click</button>")

  tokens
  |> should.equal([Text("<button "), Attributes, Text(">Click</button>")])
}

pub fn tokenize_attributes_word_boundary_test() {
  // @attributesfoo should be treated as text, not @attributes + "foo"
  let assert Ok(tokens) = lexer.tokenize("@attributesfoo")

  // The lexer splits this but importantly does NOT produce an Attributes token
  tokens
  |> list_contains(Attributes)
  |> should.be_false
}

// ------------------------------------------------------------- Error Tests

pub fn error_unterminated_variable_test() {
  let result = lexer.tokenize("{{ name")

  result
  |> should.be_error
}

pub fn error_unterminated_raw_variable_test() {
  let result = lexer.tokenize("{{{ html")

  result
  |> should.be_error
}

pub fn error_unterminated_component_test() {
  let result = lexer.tokenize("<x-alert")

  result
  |> should.be_error
}

pub fn tokenize_expression_with_function_call_test() {
  // Expressions with function calls are now valid - Gleam compiler validates
  let assert Ok(tokens) = lexer.tokenize("{{ some_fn(arg) }}")
  tokens |> should.equal([Variable("some_fn(arg)", 1)])
}

pub fn tokenize_raw_expression_with_function_call_test() {
  // Raw expressions with function calls are now valid
  let assert Ok(tokens) = lexer.tokenize("{{{ string.uppercase(name) }}}")
  tokens |> should.equal([RawVariable("string.uppercase(name)", 1)])
}

pub fn tokenize_unterminated_expression_test() {
  let result = lexer.tokenize("{{ unclosed")

  result
  |> should.equal(Error(lexer.UnterminatedExpression(0)))
}

pub fn tokenize_empty_expression_test() {
  let result = lexer.tokenize("{{  }}")

  result
  |> should.equal(Error(lexer.EmptyExpression(0)))
}

pub fn tokenize_valid_variable_names_test() {
  // These should all be valid
  let assert Ok(_) = lexer.tokenize("{{ name }}")
  let assert Ok(_) = lexer.tokenize("{{ user.name }}")
  let assert Ok(_) = lexer.tokenize("{{ user_name }}")
  let assert Ok(_) = lexer.tokenize("{{ item.first_name }}")
  let assert Ok(_) = lexer.tokenize("{{ user123 }}")
}

// ------------------------------------------------------------- Class and Style Attribute Tests

pub fn tokenize_class_attr_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<x-button :class='[#(\"active\", is_active)]' />")

  tokens
  |> should.equal([
    Component("button", [ClassAttr("[#(\"active\", is_active)]")], True),
  ])
}

pub fn tokenize_style_attr_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<x-button :style='[#(\"color: red\", has_error)]' />")

  tokens
  |> should.equal([
    Component("button", [StyleAttr("[#(\"color: red\", has_error)]")], True),
  ])
}

pub fn tokenize_class_attr_double_quotes_test() {
  // Single quotes inside get normalized to double quotes for valid Gleam
  let assert Ok(tokens) =
    lexer.tokenize("<x-div :class=\"[#('p-4', True)]\" />")

  tokens
  |> should.equal([Component("div", [ClassAttr("[#(\"p-4\", True)]")], True)])
}

pub fn tokenize_class_with_static_class_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<x-button class=\"btn\" :class='[#(\"active\", cond)]' />")

  tokens
  |> should.equal([
    Component(
      "button",
      [StringAttr("class", "btn"), ClassAttr("[#(\"active\", cond)]")],
      True,
    ),
  ])
}

pub fn tokenize_style_with_static_style_test() {
  let assert Ok(tokens) =
    lexer.tokenize(
      "<x-div style=\"margin: 0\" :style='[#(\"color: red\", cond)]' />",
    )

  tokens
  |> should.equal([
    Component(
      "div",
      [StringAttr("style", "margin: 0"), StyleAttr("[#(\"color: red\", cond)]")],
      True,
    ),
  ])
}

pub fn tokenize_class_and_style_together_test() {
  let assert Ok(tokens) =
    lexer.tokenize(
      "<x-div :class='[#(\"p-4\", True)]' :style='[#(\"color: red\", True)]' />",
    )

  tokens
  |> should.equal([
    Component(
      "div",
      [ClassAttr("[#(\"p-4\", True)]"), StyleAttr("[#(\"color: red\", True)]")],
      True,
    ),
  ])
}

// ------------------------------------------------------------- Plain HTML Tests

pub fn tokenize_plain_html_as_text_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<div class=\"container\">Content</div>")

  tokens
  |> should.equal([Text("<div class=\"container\">Content</div>")])
}

pub fn tokenize_plain_self_closing_html_as_text_test() {
  let assert Ok(tokens) = lexer.tokenize("<img src=\"image.png\" />")

  tokens
  |> should.equal([Text("<img src=\"image.png\" />")])
}

// ------------------------------------------------------------- Elements with Dynamic Attributes

pub fn tokenize_element_with_dynamic_class_test() {
  // Element with :class but no l-* should be parsed as Element
  let assert Ok(tokens) =
    lexer.tokenize(
      "<div class=\"subheading\" :class='[#(\"testing\", True)]'>Content</div>",
    )

  tokens
  |> should.equal([
    Element(
      "div",
      [StringAttr("class", "subheading"), ClassAttr("[#(\"testing\", True)]")],
      False,
    ),
    Text("Content"),
    ElementEnd("div"),
  ])
}

pub fn tokenize_element_with_dynamic_style_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<span :style='[#(\"color: red\", active)]'>Text</span>")

  tokens
  |> should.equal([
    Element("span", [StyleAttr("[#(\"color: red\", active)]")], False),
    Text("Text"),
    ElementEnd("span"),
  ])
}

pub fn tokenize_element_with_expr_attr_test() {
  let assert Ok(tokens) = lexer.tokenize("<input :value=\"name\" />")

  tokens
  |> should.equal([
    Element("input", [ExprAttr("value", "name")], True),
  ])
}

pub fn tokenize_nested_divs_with_dynamic_inner_test() {
  // Outer div is text, inner div with :class is Element
  // The closing tags should be handled correctly
  let assert Ok(tokens) =
    lexer.tokenize(
      "<div class=\"outer\"><div :class='[#(\"x\", True)]'>inner</div></div>",
    )

  tokens
  |> should.equal([
    Text("<div class=\"outer\">"),
    Element("div", [ClassAttr("[#(\"x\", True)]")], False),
    Text("inner"),
    ElementEnd("div"),
    Text("</div>"),
  ])
}

// ------------------------------------------------------------- l-on Tests

pub fn tokenize_lm_on_click_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<button l-on:click=\"count = count + 1\">+</button>")

  tokens
  |> should.equal([
    Element("button", [LmOn("click", [], "count = count + 1", 1)], False),
    Text("+"),
    ElementEnd("button"),
  ])
}

pub fn tokenize_lm_on_click_with_single_quotes_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<button l-on:click='count = count + 1'>+</button>")

  tokens
  |> should.equal([
    Element("button", [LmOn("click", [], "count = count + 1", 1)], False),
    Text("+"),
    ElementEnd("button"),
  ])
}

pub fn tokenize_lm_on_click_with_modifier_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<button l-on:click.prevent=\"handler()\">Click</button>")

  tokens
  |> should.equal([
    Element("button", [LmOn("click", ["prevent"], "handler()", 1)], False),
    Text("Click"),
    ElementEnd("button"),
  ])
}

pub fn tokenize_lm_on_click_with_multiple_modifiers_test() {
  let assert Ok(tokens) =
    lexer.tokenize(
      "<button l-on:click.prevent.stop=\"handler()\">Click</button>",
    )

  tokens
  |> should.equal([
    Element(
      "button",
      [LmOn("click", ["prevent", "stop"], "handler()", 1)],
      False,
    ),
    Text("Click"),
    ElementEnd("button"),
  ])
}

pub fn tokenize_lm_on_input_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<input l-on:input=\"name = $value\" />")

  tokens
  |> should.equal([
    Element("input", [LmOn("input", [], "name = $value", 1)], True),
  ])
}

pub fn tokenize_lm_on_input_immediate_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<input l-on:input.immediate=\"name = $value\" />")

  tokens
  |> should.equal([
    Element("input", [LmOn("input", ["immediate"], "name = $value", 1)], True),
  ])
}

pub fn tokenize_lm_on_input_debounce_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<input l-on:input.debounce-300=\"name = $value\" />")

  tokens
  |> should.equal([
    Element(
      "input",
      [LmOn("input", ["debounce-300"], "name = $value", 1)],
      True,
    ),
  ])
}

pub fn tokenize_lm_on_submit_prevent_test() {
  let assert Ok(tokens) =
    lexer.tokenize(
      "<form l-on:submit.prevent=\"errors = form.submit(name)\"></form>",
    )

  tokens
  |> should.equal([
    Element(
      "form",
      [LmOn("submit", ["prevent"], "errors = form.submit(name)", 1)],
      False,
    ),
    ElementEnd("form"),
  ])
}

pub fn tokenize_lm_on_keydown_enter_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<input l-on:keydown.enter=\"submitted = True\" />")

  tokens
  |> should.equal([
    Element("input", [LmOn("keydown", ["enter"], "submitted = True", 1)], True),
  ])
}

pub fn tokenize_lm_on_with_function_call_test() {
  let assert Ok(tokens) =
    lexer.tokenize(
      "<button l-on:click=\"count = counter.increment(count)\">+</button>",
    )

  tokens
  |> should.equal([
    Element(
      "button",
      [LmOn("click", [], "count = counter.increment(count)", 1)],
      False,
    ),
    Text("+"),
    ElementEnd("button"),
  ])
}

pub fn tokenize_lm_on_with_tuple_destructuring_test() {
  let assert Ok(tokens) =
    lexer.tokenize(
      "<button l-on:click=\"#(count, total) = counter.increment_both(count, total)\">+</button>",
    )

  tokens
  |> should.equal([
    Element(
      "button",
      [
        LmOn(
          "click",
          [],
          "#(count, total) = counter.increment_both(count, total)",
          1,
        ),
      ],
      False,
    ),
    Text("+"),
    ElementEnd("button"),
  ])
}

// ------------------------------------------------------------- l-model Tests

pub fn tokenize_lm_model_test() {
  let assert Ok(tokens) = lexer.tokenize("<input l-model=\"name\" />")

  tokens
  |> should.equal([Element("input", [LmModel("name", 1)], True)])
}

pub fn tokenize_lm_model_with_single_quotes_test() {
  let assert Ok(tokens) = lexer.tokenize("<input l-model='name' />")

  tokens
  |> should.equal([Element("input", [LmModel("name", 1)], True)])
}

pub fn tokenize_lm_model_textarea_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<textarea l-model=\"bio\"></textarea>")

  tokens
  |> should.equal([
    Element("textarea", [LmModel("bio", 1)], False),
    ElementEnd("textarea"),
  ])
}

pub fn tokenize_lm_model_select_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<select l-model=\"country\"><option>US</option></select>")

  tokens
  |> should.equal([
    Element("select", [LmModel("country", 1)], False),
    Text("<option>US</option>"),
    ElementEnd("select"),
  ])
}

pub fn tokenize_lm_model_checkbox_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<input type=\"checkbox\" l-model=\"agreed\" />")

  tokens
  |> should.equal([
    Element(
      "input",
      [StringAttr("type", "checkbox"), LmModel("agreed", 1)],
      True,
    ),
  ])
}

// ------------------------------------------------------------- l-on and l-model with other attributes Tests

pub fn tokenize_lm_on_with_class_test() {
  let assert Ok(tokens) =
    lexer.tokenize(
      "<button class=\"btn\" l-on:click=\"count = count + 1\">+</button>",
    )

  tokens
  |> should.equal([
    Element(
      "button",
      [StringAttr("class", "btn"), LmOn("click", [], "count = count + 1", 1)],
      False,
    ),
    Text("+"),
    ElementEnd("button"),
  ])
}

pub fn tokenize_lm_model_with_type_and_placeholder_test() {
  let assert Ok(tokens) =
    lexer.tokenize(
      "<input type=\"email\" l-model=\"email\" placeholder=\"you@example.com\" />",
    )

  tokens
  |> should.equal([
    Element(
      "input",
      [
        StringAttr("type", "email"),
        LmModel("email", 1),
        StringAttr("placeholder", "you@example.com"),
      ],
      True,
    ),
  ])
}

pub fn tokenize_lm_on_on_component_test() {
  let assert Ok(tokens) =
    lexer.tokenize("<x-button l-on:click=\"count = count + 1\">+</x-button>")

  tokens
  |> should.equal([
    Component("button", [LmOn("click", [], "count = count + 1", 1)], False),
    Text("+"),
    ComponentEnd("button"),
  ])
}

pub fn tokenize_lm_model_on_component_test() {
  let assert Ok(tokens) = lexer.tokenize("<x-input l-model=\"name\" />")

  tokens
  |> should.equal([Component("input", [LmModel("name", 1)], True)])
}

// ------------------------------------------------------------- Helpers

fn list_contains(tokens: List(lexer.Token), target: lexer.Token) -> Bool {
  case tokens {
    [] -> False
    [first, ..rest] ->
      case first == target {
        True -> True
        False -> list_contains(rest, target)
      }
  }
}
