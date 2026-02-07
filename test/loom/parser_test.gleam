import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import glimr/loom/lexer.{
  Attributes, BoolAttr, Component, ComponentEnd, Element, ElementEnd, ExprAttr,
  LmElse, LmElseIf, LmFor, LmIf, RawVariable, Slot, SlotDef, SlotDefEnd,
  StringAttr, Text, Variable,
}
import glimr/loom/parser.{
  type Node, type Template, AttributesNode, ComponentNode, EachNode, ElementNode,
  IfNode, LmElseAfterLmElse, LmElseIfAfterLmElse, RawVariableNode, SlotDefNode,
  SlotNode, Template, TextNode, UnclosedComponent, UnclosedElement, UnclosedSlot,
  UnexpectedComponentEnd, UnexpectedElementEnd, UnexpectedLmElse,
  UnexpectedLmElseIf, UnexpectedSlotDefEnd, VariableNode,
}

// Helper to make templates more concise
fn t(nodes: List(Node)) -> Template {
  Template(imports: [], props: [], nodes: nodes)
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

// ------------------------------------------------------------- Basic Parsing Tests

pub fn parse_empty_test() {
  let assert Ok(template) = parser.parse([])

  template
  |> should.equal(t([]))
}

pub fn parse_text_only_test() {
  let tokens = [Text("Hello, World!")]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(t([TextNode("Hello, World!")]))
}

pub fn parse_variable_test() {
  let tokens = [Variable("name", 1)]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(t([VariableNode("name", 1)]))
}

pub fn parse_raw_variable_test() {
  let tokens = [RawVariable("html", 1)]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(t([RawVariableNode("html", 1)]))
}

pub fn parse_multiple_nodes_test() {
  let tokens = [Text("Hello, "), Variable("name", 1), Text("!")]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      TextNode("Hello, "),
      VariableNode("name", 1),
      TextNode("!"),
    ]),
  )
}

// ------------------------------------------------------------- Slot Tests

pub fn parse_slot_test() {
  let tokens = [Text("<div>"), Slot(None), Text("</div>")]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      TextNode("<div>"),
      SlotNode(None, []),
      TextNode("</div>"),
    ]),
  )
}

pub fn parse_named_slot_test() {
  let tokens = [Text("<div>"), Slot(Some("header")), Text("</div>")]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      TextNode("<div>"),
      SlotNode(Some("header"), []),
      TextNode("</div>"),
    ]),
  )
}

// ------------------------------------------------------------- l-if Tests

pub fn parse_lm_if_test() {
  let tokens = [
    Element("p", [LmIf("show", 1)], False),
    Text("visible"),
    ElementEnd("p"),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([if_node("show", [ElementNode("p", [], [TextNode("visible")])])]),
  )
}

pub fn parse_lm_if_self_closing_test() {
  let tokens = [Element("br", [LmIf("show", 1)], True)]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(t([if_node("show", [ElementNode("br", [], [])])]))
}

pub fn parse_lm_if_with_attrs_test() {
  let tokens = [
    Element("p", [LmIf("show", 1), StringAttr("class", "message")], False),
    Text("Hello"),
    ElementEnd("p"),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      if_node("show", [
        ElementNode("p", [StringAttr("class", "message")], [TextNode("Hello")]),
      ]),
    ]),
  )
}

// ------------------------------------------------------------- l-else and l-else-if Tests

pub fn parse_lm_if_else_test() {
  let tokens = [
    Element("p", [LmIf("show", 1)], False),
    Text("yes"),
    ElementEnd("p"),
    Element("p", [LmElse], False),
    Text("no"),
    ElementEnd("p"),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      if_else_node("show", [ElementNode("p", [], [TextNode("yes")])], [
        ElementNode("p", [], [TextNode("no")]),
      ]),
    ]),
  )
}

pub fn parse_lm_if_elseif_else_test() {
  let tokens = [
    Element("p", [LmIf("a", 1)], False),
    Text("A"),
    ElementEnd("p"),
    Element("p", [LmElseIf("b", 1)], False),
    Text("B"),
    ElementEnd("p"),
    Element("p", [LmElse], False),
    Text("C"),
    ElementEnd("p"),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      if_elseif_else_node(
        [
          #("a", [ElementNode("p", [], [TextNode("A")])]),
          #("b", [ElementNode("p", [], [TextNode("B")])]),
        ],
        [ElementNode("p", [], [TextNode("C")])],
      ),
    ]),
  )
}

pub fn parse_multiple_lm_elseif_test() {
  let tokens = [
    Element("p", [LmIf("a", 1)], False),
    Text("A"),
    ElementEnd("p"),
    Element("p", [LmElseIf("b", 1)], False),
    Text("B"),
    ElementEnd("p"),
    Element("p", [LmElseIf("c", 1)], False),
    Text("C"),
    ElementEnd("p"),
    Element("p", [LmElseIf("d", 1)], False),
    Text("D"),
    ElementEnd("p"),
    Element("p", [LmElse], False),
    Text("E"),
    ElementEnd("p"),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      if_elseif_else_node(
        [
          #("a", [ElementNode("p", [], [TextNode("A")])]),
          #("b", [ElementNode("p", [], [TextNode("B")])]),
          #("c", [ElementNode("p", [], [TextNode("C")])]),
          #("d", [ElementNode("p", [], [TextNode("D")])]),
        ],
        [ElementNode("p", [], [TextNode("E")])],
      ),
    ]),
  )
}

pub fn parse_lm_if_without_else_test() {
  // l-if followed by non-conditional element should end the chain
  let tokens = [
    Element("p", [LmIf("show", 1)], False),
    Text("visible"),
    ElementEnd("p"),
    Text("always shown"),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      if_node("show", [ElementNode("p", [], [TextNode("visible")])]),
      TextNode("always shown"),
    ]),
  )
}

// ------------------------------------------------------------- l-for Tests

pub fn parse_lm_for_test() {
  let tokens = [
    Element("li", [LmFor("items", ["item"], None, 1)], False),
    Variable("item", 1),
    ElementEnd("li"),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      EachNode(
        "items",
        ["item"],
        None,
        [
          ElementNode("li", [], [VariableNode("item", 1)]),
        ],
        1,
      ),
    ]),
  )
}

pub fn parse_lm_for_with_loop_var_test() {
  let tokens = [
    Element("li", [LmFor("items", ["item"], Some("loop"), 1)], False),
    Variable("loop.index", 1),
    ElementEnd("li"),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      EachNode(
        "items",
        ["item"],
        Some("loop"),
        [
          ElementNode("li", [], [VariableNode("loop.index", 1)]),
        ],
        1,
      ),
    ]),
  )
}

pub fn parse_lm_for_tuple_test() {
  let tokens = [
    Element("li", [LmFor("pairs", ["key", "value"], None, 1)], False),
    Variable("key", 1),
    Text(": "),
    Variable("value", 1),
    ElementEnd("li"),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      EachNode(
        "pairs",
        ["key", "value"],
        None,
        [
          ElementNode("li", [], [
            VariableNode("key", 1),
            TextNode(": "),
            VariableNode("value", 1),
          ]),
        ],
        1,
      ),
    ]),
  )
}

pub fn parse_lm_for_self_closing_test() {
  let tokens = [
    Element(
      "img",
      [LmFor("images", ["img"], None, 1), ExprAttr("src", "img.url")],
      True,
    ),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      EachNode(
        "images",
        ["img"],
        None,
        [
          ElementNode("img", [ExprAttr("src", "img.url")], []),
        ],
        1,
      ),
    ]),
  )
}

// ------------------------------------------------------------- Combined l-if and l-for Tests

pub fn parse_lm_for_with_lm_if_inside_test() {
  let tokens = [
    Element("ul", [], False),
    Element("li", [LmFor("items", ["item"], None, 1)], False),
    Element("span", [LmIf("item.active", 1)], False),
    Variable("item.name", 1),
    ElementEnd("span"),
    ElementEnd("li"),
    ElementEnd("ul"),
  ]
  let assert Ok(template) = parser.parse(tokens)

  // The ElementNode for ul has no l-* attrs, so it's just an element
  case template.nodes {
    [ElementNode("ul", [], children)] -> {
      case children {
        [EachNode("items", ["item"], None, loop_children, _line)] -> {
          case loop_children {
            [ElementNode("li", [], li_children)] -> {
              case li_children {
                [IfNode(_)] -> Nil
                _ -> panic as "Expected IfNode inside li"
              }
            }
            _ -> panic as "Expected ElementNode li"
          }
        }
        _ -> panic as "Expected EachNode"
      }
    }
    _ -> panic as "Expected ElementNode ul"
  }
}

// ------------------------------------------------------------- Component with l-* Tests

pub fn parse_component_with_lm_if_test() {
  let tokens = [
    Component(
      "alert",
      [LmIf("show_error", 1), StringAttr("type", "error")],
      False,
    ),
    Variable("message", 1),
    ComponentEnd("alert"),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      if_node("show_error", [
        ComponentNode("alert", [StringAttr("type", "error")], [
          VariableNode("message", 1),
        ]),
      ]),
    ]),
  )
}

pub fn parse_component_with_lm_for_test() {
  let tokens = [
    Component(
      "card",
      [LmFor("cards", ["card"], None, 1), ExprAttr("title", "card.title")],
      False,
    ),
    Variable("card.body", 1),
    ComponentEnd("card"),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      EachNode(
        "cards",
        ["card"],
        None,
        [
          ComponentNode("card", [ExprAttr("title", "card.title")], [
            VariableNode("card.body", 1),
          ]),
        ],
        1,
      ),
    ]),
  )
}

pub fn parse_component_self_closing_with_lm_if_test() {
  let tokens = [
    Component("icon", [LmIf("show", 1), StringAttr("name", "check")], True),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      if_node("show", [
        ComponentNode("icon", [StringAttr("name", "check")], []),
      ]),
    ]),
  )
}

// ------------------------------------------------------------- Slot Definition Tests

pub fn parse_slot_with_fallback_test() {
  // At top level, <slot name="x">fallback</slot> creates SlotNode with fallback
  let tokens = [SlotDef(Some("header")), Text("Title"), SlotDefEnd]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(t([SlotNode(Some("header"), [TextNode("Title")])]))
}

pub fn parse_slot_with_variable_fallback_test() {
  // At top level, slot with variable fallback creates SlotNode with fallback
  let tokens = [SlotDef(Some("footer")), Variable("copyright", 1), SlotDefEnd]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      SlotNode(Some("footer"), [VariableNode("copyright", 1)]),
    ]),
  )
}

pub fn parse_component_with_named_slots_test() {
  let tokens = [
    Component("card", [], False),
    SlotDef(Some("header")),
    Text("Title"),
    SlotDefEnd,
    Text("Body"),
    ComponentEnd("card"),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      ComponentNode("card", [], [
        SlotDefNode(Some("header"), [TextNode("Title")]),
        TextNode("Body"),
      ]),
    ]),
  )
}

// ------------------------------------------------------------- Standard Component Tests

pub fn parse_self_closing_component_test() {
  let tokens = [Component("alert", [], True)]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(t([ComponentNode("alert", [], [])]))
}

pub fn parse_component_with_attrs_test() {
  let tokens = [
    Component(
      "alert",
      [StringAttr("type", "success"), BoolAttr("dismissible")],
      True,
    ),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      ComponentNode(
        "alert",
        [StringAttr("type", "success"), BoolAttr("dismissible")],
        [],
      ),
    ]),
  )
}

pub fn parse_component_with_children_test() {
  let tokens = [
    Component("card", [], False),
    Text("Card content"),
    ComponentEnd("card"),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      ComponentNode("card", [], [TextNode("Card content")]),
    ]),
  )
}

pub fn parse_nested_components_test() {
  let tokens = [
    Component("card", [], False),
    Component("button", [StringAttr("type", "primary")], False),
    Text("Click me"),
    ComponentEnd("button"),
    ComponentEnd("card"),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      ComponentNode("card", [], [
        ComponentNode("button", [StringAttr("type", "primary")], [
          TextNode("Click me"),
        ]),
      ]),
    ]),
  )
}

// ------------------------------------------------------------- Attributes Tests

pub fn parse_attributes_standalone_test() {
  let tokens = [Text("<div "), Attributes, Text(">content</div>")]
  let assert Ok(template) = parser.parse(tokens)

  template
  |> should.equal(
    t([
      TextNode("<div "),
      AttributesNode([]),
      TextNode(">content</div>"),
    ]),
  )
}

pub fn parse_attributes_in_component_template_test() {
  let tokens = [
    Text("<button "),
    Attributes,
    Text(">"),
    Slot(None),
    Text("</button>"),
  ]
  let assert Ok(template) = parser.parse(tokens)

  template.nodes
  |> should.equal([
    TextNode("<button "),
    AttributesNode([]),
    TextNode(">"),
    SlotNode(None, []),
    TextNode("</button>"),
  ])
}

// ------------------------------------------------------------- Error Tests

pub fn error_unexpected_lm_else_test() {
  let tokens = [
    Element("p", [LmElse], False),
    Text("no if before this"),
    ElementEnd("p"),
  ]
  let result = parser.parse(tokens)

  result
  |> should.equal(Error(UnexpectedLmElse))
}

pub fn error_unexpected_lm_elseif_test() {
  let tokens = [
    Element("p", [LmElseIf("condition", 1)], False),
    Text("no if before this"),
    ElementEnd("p"),
  ]
  let result = parser.parse(tokens)

  result
  |> should.equal(Error(UnexpectedLmElseIf))
}

pub fn error_lm_else_after_lm_else_test() {
  let tokens = [
    Element("p", [LmIf("a", 1)], False),
    Text("A"),
    ElementEnd("p"),
    Element("p", [LmElse], False),
    Text("B"),
    ElementEnd("p"),
    Element("p", [LmElse], False),
    Text("C"),
    ElementEnd("p"),
  ]
  let result = parser.parse(tokens)

  result
  |> should.equal(Error(LmElseAfterLmElse))
}

pub fn error_lm_elseif_after_lm_else_test() {
  let tokens = [
    Element("p", [LmIf("a", 1)], False),
    Text("A"),
    ElementEnd("p"),
    Element("p", [LmElse], False),
    Text("B"),
    ElementEnd("p"),
    Element("p", [LmElseIf("c", 1)], False),
    Text("C"),
    ElementEnd("p"),
  ]
  let result = parser.parse(tokens)

  result
  |> should.equal(Error(LmElseIfAfterLmElse))
}

pub fn error_unexpected_component_end_test() {
  let tokens = [Text("text"), ComponentEnd("alert")]
  let result = parser.parse(tokens)

  result
  |> should.equal(Error(UnexpectedComponentEnd("alert")))
}

pub fn error_unclosed_component_test() {
  let tokens = [Component("card", [], False), Text("content")]
  let result = parser.parse(tokens)

  result
  |> should.equal(Error(UnclosedComponent("card")))
}

pub fn error_unexpected_element_end_test() {
  let tokens = [Text("text"), ElementEnd("div")]
  let result = parser.parse(tokens)

  result
  |> should.equal(Error(UnexpectedElementEnd("div")))
}

pub fn error_unclosed_element_test() {
  let tokens = [
    Element("div", [LmIf("show", 1)], False),
    Text("content"),
  ]
  let result = parser.parse(tokens)

  result
  |> should.equal(Error(UnclosedElement("div")))
}

pub fn error_unexpected_slot_def_end_test() {
  let tokens = [Text("some text"), SlotDefEnd]
  let result = parser.parse(tokens)

  result
  |> should.equal(Error(UnexpectedSlotDefEnd))
}

pub fn error_unclosed_slot_def_test() {
  let tokens = [SlotDef(Some("header")), Text("content")]
  let result = parser.parse(tokens)

  result
  |> should.equal(Error(UnclosedSlot(Some("header"))))
}

pub fn error_mismatched_component_end_test() {
  let tokens = [
    Component("card", [], False),
    Text("content"),
    ComponentEnd("button"),
  ]
  let result = parser.parse(tokens)

  result
  |> should.equal(Error(UnexpectedComponentEnd("button")))
}
