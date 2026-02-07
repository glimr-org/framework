import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import glimr/loom/generator
import glimr/loom/lexer
import glimr/loom/parser
import glimr/loom/runtime

// ------------------------------------------------------------- Runtime Function Tests

pub fn runtime_string_concat_test() {
  { "" <> "Hello" <> ", " <> "World!" }
  |> should.equal("Hello, World!")
}

pub fn runtime_append_if_true_test() {
  "Start"
  |> runtime.append_if(True, fn(acc) { acc <> " - shown" })
  |> should.equal("Start - shown")
}

pub fn runtime_append_if_false_test() {
  "Start"
  |> runtime.append_if(False, fn(acc) { acc <> " - hidden" })
  |> should.equal("Start")
}

pub fn runtime_append_each_test() {
  ""
  |> runtime.append_each(["a", "b", "c"], fn(acc, item) {
    acc <> "[" <> item <> "]"
  })
  |> should.equal("[a][b][c]")
}

pub fn runtime_append_each_empty_test() {
  "prefix"
  |> runtime.append_each([], fn(acc, _item) { acc <> "x" })
  |> should.equal("prefix")
}

pub fn runtime_escape_html_test() {
  runtime.escape("<script>alert('xss')</script>")
  |> should.equal("&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;")
}

pub fn runtime_escape_all_entities_test() {
  runtime.escape("< > \" ' &")
  |> should.equal("&lt; &gt; &quot; &#39; &amp;")
}

pub fn runtime_escape_preserves_normal_text_test() {
  runtime.escape("Hello, World!")
  |> should.equal("Hello, World!")
}

// ------------------------------------------------------------- Full Pipeline Tests

pub fn pipeline_simple_text_test() {
  let template = "Hello, World!"
  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  parsed.nodes
  |> should.equal([parser.TextNode("Hello, World!")])
}

pub fn pipeline_variable_interpolation_test() {
  let template = "Hello, {{ name }}!"
  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  parsed.nodes
  |> should.equal([
    parser.TextNode("Hello, "),
    parser.VariableNode("name", 1),
    parser.TextNode("!"),
  ])
}

pub fn pipeline_conditional_test() {
  let template = "<span l-if=\"show\">visible</span>"
  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  parsed.nodes
  |> should.equal([
    parser.IfNode([
      #(Some("show"), 1, [
        parser.ElementNode("span", [], [parser.TextNode("visible")]),
      ]),
    ]),
  ])
}

pub fn pipeline_loop_test() {
  let template = "<span l-for=\"item in items\">{{ item }}</span>"
  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  parsed.nodes
  |> should.equal([
    parser.EachNode(
      "items",
      ["item"],
      None,
      [
        parser.ElementNode("span", [], [parser.VariableNode("item", 1)]),
      ],
      1,
    ),
  ])
}

pub fn pipeline_component_test() {
  let template = "<x-alert type=\"success\">Message</x-alert>"
  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  case parsed.nodes {
    [parser.ComponentNode("alert", attrs, children)] -> {
      attrs
      |> should.equal([lexer.StringAttr("type", "success")])
      children
      |> should.equal([parser.TextNode("Message")])
    }
    _ -> panic as "Unexpected node structure"
  }
}

pub fn pipeline_self_closing_component_test() {
  let template = "<x-input type=\"text\" :value=\"name\" required />"
  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  case parsed.nodes {
    [parser.ComponentNode("input", attrs, children)] -> {
      attrs
      |> should.equal([
        lexer.StringAttr("type", "text"),
        lexer.ExprAttr("value", "name"),
        lexer.BoolAttr("required"),
      ])
      children
      |> should.equal([])
    }
    _ -> panic as "Unexpected node structure"
  }
}

// ------------------------------------------------------------- Full Template Compilation Tests

pub fn compile_page_template_test() {
  // With new architecture, templates use explicit data. prefixes
  let template =
    "<div class=\"container\">
  <h1>{{ data.title }}</h1>
  <p l-if=\"data.show_content\">{{ data.content }}</p>
</div>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)
  let generated =
    generator.generate(parsed, "home", False, dict.new(), dict.new())

  // Should pass through data.title, etc.
  generated.code
  |> string.contains("data.title")
  |> should.be_true

  generated.code
  |> string.contains("data.content")
  |> should.be_true

  generated.code
  |> string.contains("data.show_content")
  |> should.be_true
}

pub fn compile_component_template_test() {
  // Components use labeled arguments in new architecture
  // Data types are now defined in app/loom/*.gleam, not in templates
  let template =
    "<div class=\"alert alert-{{ type }}\">
  <slot />
  <button l-if=\"dismissible\" class=\"close\">&times;</button>
</div>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)
  let generated =
    generator.generate(parsed, "alert", True, dict.new(), dict.new())

  // Should have html function with slot and attributes params (no view file = no data params)
  generated.code
  |> string.contains("slot slot: String")
  |> should.be_true

  generated.code
  |> string.contains("attributes attributes: List(runtime.Attribute)")
  |> should.be_true

  // Should access variables directly
  generated.code
  |> string.contains("runtime.escape(type)")
  |> should.be_true

  generated.code
  |> string.contains("_ if dismissible ->")
  |> should.be_true

  // Should use slot argument directly
  generated.code
  |> string.contains("<> slot")
  |> should.be_true
}

pub fn compile_layout_template_test() {
  // Layouts use labeled arguments in new architecture
  // Template variables strip data. prefix
  let template =
    "<!DOCTYPE html>
<html>
<head>
  <title>{{ title }}</title>
</head>
<body>
  <slot />
</body>
</html>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)
  let generated =
    generator.generate(parsed, "app", False, dict.new(), dict.new())

  // Should have html function with slot param (no view file = no data params)
  generated.code
  |> string.contains("slot slot: String")
  |> should.be_true

  // Should use slot argument directly
  generated.code
  |> string.contains("<> slot")
  |> should.be_true
}

pub fn compile_template_with_nested_components_test() {
  let template =
    "<x-card>
  <x-card-header>{{ data.title }}</x-card-header>
  <x-card-body>
    {{ data.content }}
  </x-card-body>
</x-card>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)
  let generated =
    generator.generate(parsed, "page", False, dict.new(), dict.new())

  // Should import all components
  generated.code
  |> string.contains("as components_card")
  |> should.be_true

  generated.code
  |> string.contains("as components_card_header")
  |> should.be_true

  generated.code
  |> string.contains("as components_card_body")
  |> should.be_true
}

pub fn compile_template_with_each_and_component_test() {
  let template =
    "<x-alert l-for=\"alert in data.alerts\" :type=\"alert.type\">{{ alert.message }}</x-alert>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)
  let generated =
    generator.generate(
      parsed,
      "alerts_page",
      False,
      dict.new(),
      dict.new(),
    )

  // Should pass through data.alerts
  generated.code
  |> string.contains("data.alerts")
  |> should.be_true

  // Should import component
  generated.code
  |> string.contains("as components_alert")
  |> should.be_true
}

// ------------------------------------------------------------- Edge Case Tests

pub fn pipeline_multiple_at_signs_test() {
  // Make sure @ not followed by directive is treated as text
  let template = "email@example.com"
  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  parsed.nodes
  |> should.equal([parser.TextNode("email@example.com")])
}

pub fn pipeline_curly_braces_not_variable_test() {
  // Single curly braces should be treated as text
  let template = "JSON: {\"key\": \"value\"}"
  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  // Should be all text, no variables
  parsed.nodes
  |> should.equal([parser.TextNode("JSON: {\"key\": \"value\"}")])
}

pub fn pipeline_nested_structure_test() {
  let template =
    "<div l-if=\"a\">
  <div l-if=\"b\">
    <x-item l-for=\"item in items\" :value=\"item\">{{ item }}</x-item>
  </div>
</div>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  // Should parse without error - structure validation
  case parsed.nodes {
    [parser.IfNode([#(Some("a"), _, _), ..])] -> Nil
    _ -> panic as "Expected outer if node with condition 'a'"
  }
}

pub fn pipeline_raw_html_not_escaped_test() {
  let template = "{{{ data.html_content }}}"
  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)
  let generated =
    generator.generate(parsed, "raw", False, dict.new(), dict.new())

  // Raw variable should not call escape
  generated.code
  |> string.contains("<> data.html_content")
  |> should.be_true

  generated.code
  |> string.contains("runtime.escape(data.html_content)")
  |> should.be_false
}

// ------------------------------------------------------------- Error Propagation Tests

pub fn error_lexer_propagates_test() {
  let template = "{{ unclosed"
  let result = lexer.tokenize(template)

  result
  |> should.be_error
}

pub fn error_parser_propagates_test() {
  // l-else without l-if should error
  let template = "<p l-else>content</p>"
  let assert Ok(tokens) = lexer.tokenize(template)
  let result = parser.parse(tokens)

  result
  |> should.be_error
}

pub fn error_mismatched_tags_test() {
  // l-else-if without l-if should error
  let template = "<p l-else-if=\"cond\">content</p>"
  let assert Ok(tokens) = lexer.tokenize(template)
  let result = parser.parse(tokens)

  result
  |> should.be_error
}

pub fn error_mismatched_component_test() {
  let template = "<x-card></x-button>"
  let assert Ok(tokens) = lexer.tokenize(template)
  let result = parser.parse(tokens)

  result
  |> should.be_error
}

// ------------------------------------------------------------- Attribute Forwarding Tests

pub fn runtime_render_attributes_test() {
  let attrs = [
    runtime.Attribute("class", "btn btn-primary"),
    runtime.Attribute("type", "submit"),
  ]

  runtime.render_attributes(attrs)
  |> should.equal("class=\"btn btn-primary\" type=\"submit\"")
}

pub fn runtime_render_attributes_empty_test() {
  runtime.render_attributes([])
  |> should.equal("")
}

pub fn runtime_render_attributes_escapes_values_test() {
  let attrs = [runtime.Attribute("onclick", "alert(\"xss\")")]

  runtime.render_attributes(attrs)
  |> should.equal("onclick=\"alert(&quot;xss&quot;)\"")
}

pub fn runtime_merge_attributes_class_test() {
  let base = [runtime.Attribute("class", "btn")]
  let extra = [runtime.Attribute("class", "btn-primary")]

  let merged = runtime.merge_attributes(base, extra)

  // Classes should be combined
  merged
  |> should.equal([runtime.Attribute("class", "btn btn-primary")])
}

pub fn runtime_merge_attributes_style_test() {
  let base = [runtime.Attribute("style", "color: red")]
  let extra = [runtime.Attribute("style", "font-size: 14px")]

  let merged = runtime.merge_attributes(base, extra)

  // Styles should be combined with semicolon
  merged
  |> should.equal([runtime.Attribute("style", "color: red; font-size: 14px")])
}

pub fn runtime_merge_attributes_override_test() {
  let base = [runtime.Attribute("id", "old")]
  let extra = [runtime.Attribute("id", "new")]

  let merged = runtime.merge_attributes(base, extra)

  // Other attributes should be overridden
  merged
  |> should.equal([runtime.Attribute("id", "new")])
}

pub fn runtime_merge_attributes_mixed_test() {
  let base = [
    runtime.Attribute("class", "card"),
    runtime.Attribute("id", "card-1"),
  ]
  let extra = [
    runtime.Attribute("class", "shadow"),
    runtime.Attribute("data-id", "123"),
  ]

  let merged = runtime.merge_attributes(base, extra)

  // Class combined, id unchanged, data-id added
  case merged {
    [class_attr, id_attr, data_attr] -> {
      class_attr |> should.equal(runtime.Attribute("class", "card shadow"))
      id_attr |> should.equal(runtime.Attribute("id", "card-1"))
      data_attr |> should.equal(runtime.Attribute("data-id", "123"))
    }
    _ -> panic as "Expected 3 attributes"
  }
}

pub fn pipeline_attributes_directive_test() {
  // Test that @attributes is tokenized and parsed correctly
  let template = "<button @attributes>{{ label }}</button>"
  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  // Verify AttributesNode is in the parsed nodes
  let has_attributes =
    parsed.nodes
    |> contains_attributes_node
  has_attributes
  |> should.be_true
}

fn contains_attributes_node(nodes: List(parser.Node)) -> Bool {
  case nodes {
    [] -> False
    [parser.AttributesNode(_), ..] -> True
    [_, ..rest] -> contains_attributes_node(rest)
  }
}

pub fn compile_component_with_attributes_test() {
  // Data types are now defined in app/loom/*.gleam, not in templates
  let template = "<button class=\"btn\" @attributes>{{ label }}</button>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)
  let generated =
    generator.generate(parsed, "button", True, dict.new(), dict.new())

  // Should have html function with attributes param
  generated.code
  |> string.contains("attributes attributes: List(runtime.Attribute)")
  |> should.be_true

  // Should render attributes at the explicit @attributes location (direct argument)
  generated.code
  |> string.contains("runtime.render_attributes(attributes)")
  |> should.be_true
}

pub fn compile_component_auto_inject_attributes_test() {
  // Component without explicit @attributes should auto-inject into first element
  // with merge_attributes to combine base class with user-passed attributes
  let template = "<button class=\"btn\">{{ label }}</button>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)
  let generated =
    generator.generate(parsed, "button", True, dict.new(), dict.new())

  // Should auto-inject merge_attributes with base class
  generated.code
  |> string.contains(
    "runtime.merge_attributes([runtime.Attribute(\"class\", \"btn\")]",
  )
  |> should.be_true
}

pub fn compile_using_component_with_attributes_test() {
  // Test that using a component generates the attributes list
  let template =
    "<x-button class=\"primary\" type=\"submit\">Click me</x-button>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)
  let generated =
    generator.generate(parsed, "page", False, dict.new(), dict.new())

  // Should generate attributes list with all attributes
  generated.code
  |> string.contains("runtime.Attribute(\"class\", \"primary\")")
  |> should.be_true

  generated.code
  |> string.contains("runtime.Attribute(\"type\", \"submit\")")
  |> should.be_true
}

// ------------------------------------------------------------- Attribute Merging End-to-End Tests

pub fn merge_class_attributes_at_runtime_test() {
  // Test that class attributes get merged correctly at runtime
  // Component defines class="btn", user passes class="primary"
  let base = [runtime.Attribute("class", "btn")]
  let extra = [runtime.Attribute("class", "primary")]
  let merged = runtime.merge_attributes(base, extra)

  // Classes should be combined with space
  merged
  |> should.equal([runtime.Attribute("class", "btn primary")])
}

pub fn merge_style_attributes_at_runtime_test() {
  // Test that style attributes get merged correctly at runtime
  let base = [runtime.Attribute("style", "color: red")]
  let extra = [runtime.Attribute("style", "font-size: 14px")]
  let merged = runtime.merge_attributes(base, extra)

  // Styles should be combined with semicolon
  merged
  |> should.equal([runtime.Attribute("style", "color: red; font-size: 14px")])
}

pub fn merge_mixed_attributes_at_runtime_test() {
  // Test that mixed attributes work - class/style combine, others override
  let base = [
    runtime.Attribute("class", "btn"),
    runtime.Attribute("style", "color: red"),
    runtime.Attribute("id", "submit-btn"),
  ]
  let extra = [
    runtime.Attribute("class", "primary"),
    runtime.Attribute("id", "new-id"),
    runtime.Attribute("data-testid", "button"),
  ]
  let merged = runtime.merge_attributes(base, extra)

  // Verify the merged result
  case merged {
    [class_attr, style_attr, id_attr, data_attr] -> {
      class_attr |> should.equal(runtime.Attribute("class", "btn primary"))
      style_attr |> should.equal(runtime.Attribute("style", "color: red"))
      id_attr |> should.equal(runtime.Attribute("id", "new-id"))
      data_attr |> should.equal(runtime.Attribute("data-testid", "button"))
    }
    _ -> panic as "Expected 4 attributes"
  }
}

// ------------------------------------------------------------- Line Number Tracking Tests

pub fn pipeline_tracks_variable_line_numbers_test() {
  // Multi-line template with variables at different lines
  let template =
    "line 1
{{ first_var }}
line 3
line 4
{{ second_var }}"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  // Verify the parsed nodes have correct line numbers
  case parsed.nodes {
    [
      parser.TextNode("line 1\n"),
      parser.VariableNode("first_var", line1),
      parser.TextNode("\nline 3\nline 4\n"),
      parser.VariableNode("second_var", line2),
    ] -> {
      line1 |> should.equal(2)
      line2 |> should.equal(5)
    }
    _ -> panic as "Unexpected node structure"
  }
}

pub fn pipeline_tracks_lm_if_line_numbers_test() {
  // Template with l-if at a specific line (using span which gets parsed as Element)
  let template =
    "line 1
line 2
<span l-if=\"show\">conditional</span>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  // Find the IfNode and verify its line number
  case parsed.nodes {
    [parser.TextNode(_), parser.IfNode([#(Some("show"), line, _)])] -> {
      line |> should.equal(3)
    }
    _ -> panic as "Expected TextNode followed by IfNode"
  }
}

pub fn pipeline_tracks_lm_for_line_numbers_test() {
  // Template with l-for at a specific line
  let template =
    "line 1
line 2
<li l-for=\"item in items\">{{ item }}</li>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  // Find the EachNode and verify its line number
  case parsed.nodes {
    [parser.TextNode(_), parser.EachNode("items", ["item"], None, _, line)] -> {
      line |> should.equal(3)
    }
    _ -> panic as "Expected TextNode followed by EachNode"
  }
}

pub fn pipeline_tracks_mixed_line_numbers_test() {
  // Complex template with variables and directives at various lines
  let template =
    "{{ header }}
<div l-if=\"show_content\">
  {{ body }}
  <ul l-for=\"item in items\">
    {{ item }}
  </ul>
</div>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  // Verify header variable is on line 1
  case parsed.nodes {
    [parser.VariableNode("header", header_line), ..rest] -> {
      header_line |> should.equal(1)

      // Verify the IfNode condition is on line 2
      case rest {
        [
          parser.TextNode(_),
          parser.IfNode([#(Some("show_content"), if_line, if_body)]),
        ] -> {
          if_line |> should.equal(2)

          // Find the body variable and EachNode inside the if body
          case if_body {
            [parser.ElementNode("div", _, div_children)] -> {
              case div_children {
                [
                  parser.TextNode(_),
                  parser.VariableNode("body", body_line),
                  parser.TextNode(_),
                  parser.EachNode("items", ["item"], None, each_body, each_line),
                  ..
                ] -> {
                  body_line |> should.equal(3)
                  each_line |> should.equal(4)

                  // Verify the item variable inside the loop is on line 5
                  case each_body {
                    [parser.ElementNode("ul", _, ul_children)] -> {
                      case ul_children {
                        [
                          parser.TextNode(_),
                          parser.VariableNode("item", item_line),
                          ..
                        ] -> {
                          item_line |> should.equal(5)
                        }
                        _ -> panic as "Expected item variable in ul"
                      }
                    }
                    _ -> panic as "Expected ul element in each body"
                  }
                }
                _ -> panic as "Unexpected div children structure"
              }
            }
            _ -> panic as "Expected ElementNode div in if body"
          }
        }
        _ -> panic as "Expected IfNode after header"
      }
    }
    _ -> panic as "Expected header variable first"
  }
}
