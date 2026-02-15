import gleam/list
import gleeunit/should
import glimr/loom/handler_parser.{
  EmptyExpression, EmptyTarget, InvalidTupleSyntax,
}
import glimr/loom/lexer
import glimr/loom/parser.{type Template, ElementNode, Template, TextNode}

// ------------------------------------------------------------- Simple Assignment Tests

pub fn parse_simple_assignment_test() {
  let assert Ok(handler) =
    handler_parser.parse("click", [], "count = count + 1", 1)

  handler.event |> should.equal("click")
  handler.modifiers |> should.equal([])
  handler.targets |> should.equal(["count"])
  handler.expression |> should.equal("count + 1")
  handler.line |> should.equal(1)
}

pub fn parse_simple_assignment_with_spaces_test() {
  let assert Ok(handler) =
    handler_parser.parse("click", [], "  count  =  count + 1  ", 1)

  handler.targets |> should.equal(["count"])
  handler.expression |> should.equal("count + 1")
}

pub fn parse_function_call_test() {
  let assert Ok(handler) =
    handler_parser.parse("click", [], "count = counter.increment(count)", 1)

  handler.targets |> should.equal(["count"])
  handler.expression |> should.equal("counter.increment(count)")
}

pub fn parse_complex_expression_test() {
  let assert Ok(handler) =
    handler_parser.parse("click", [], "count = int.max(count - 1, 0)", 1)

  handler.targets |> should.equal(["count"])
  handler.expression |> should.equal("int.max(count - 1, 0)")
}

pub fn parse_with_modifiers_test() {
  let assert Ok(handler) =
    handler_parser.parse("click", ["prevent", "stop"], "count = count + 1", 1)

  handler.event |> should.equal("click")
  handler.modifiers |> should.equal(["prevent", "stop"])
}

// ------------------------------------------------------------- Tuple Destructuring Tests

pub fn parse_tuple_destructuring_test() {
  let assert Ok(handler) =
    handler_parser.parse(
      "click",
      [],
      "#(count, total) = counter.increment_both(count, total)",
      1,
    )

  handler.targets |> should.equal(["count", "total"])
  handler.expression |> should.equal("counter.increment_both(count, total)")
}

pub fn parse_tuple_three_elements_test() {
  let assert Ok(handler) =
    handler_parser.parse("click", [], "#(a, b, c) = fn(a, b, c)", 1)

  handler.targets |> should.equal(["a", "b", "c"])
  handler.expression |> should.equal("fn(a, b, c)")
}

pub fn parse_tuple_with_spaces_test() {
  let assert Ok(handler) =
    handler_parser.parse("click", [], "#( count , total ) = fn()", 1)

  handler.targets |> should.equal(["count", "total"])
}

// ------------------------------------------------------------- Special Variables Tests

pub fn get_special_vars_value_test() {
  let assert Ok(handler) = handler_parser.parse("input", [], "name = $value", 1)

  handler_parser.get_special_vars(handler)
  |> should.equal(["$value"])
}

pub fn get_special_vars_checked_test() {
  let assert Ok(handler) =
    handler_parser.parse("change", [], "agreed = $checked", 1)

  handler_parser.get_special_vars(handler)
  |> should.equal(["$checked"])
}

pub fn get_special_vars_key_test() {
  let assert Ok(handler) =
    handler_parser.parse("keydown", [], "pressed = $key", 1)

  handler_parser.get_special_vars(handler)
  |> should.equal(["$key"])
}

pub fn get_special_vars_multiple_test() {
  let assert Ok(handler) =
    handler_parser.parse("input", [], "result = process($value, $key)", 1)

  let vars = handler_parser.get_special_vars(handler)
  // Order may vary, just check both are present
  vars |> should.equal(["$key", "$value"])
}

pub fn get_special_vars_none_test() {
  let assert Ok(handler) =
    handler_parser.parse("click", [], "count = count + 1", 1)

  handler_parser.get_special_vars(handler)
  |> should.equal([])
}

// ------------------------------------------------------------- Handler ID Tests

pub fn handler_id_test() {
  handler_parser.handler_id("click", 0)
  |> should.equal("handle_click_0")

  handler_parser.handler_id("submit", 5)
  |> should.equal("handle_submit_5")

  handler_parser.handler_id("input", 12)
  |> should.equal("handle_input_12")
}

// ------------------------------------------------------------- Error Tests

pub fn side_effect_expression_test() {
  let assert Ok(handler) = handler_parser.parse("click", [], "count + 1", 1)

  handler.targets |> should.equal([])
  handler.expression |> should.equal("count + 1")
}

pub fn error_empty_target_test() {
  let result = handler_parser.parse("click", [], " = count + 1", 1)

  result |> should.equal(Error(EmptyTarget("= count + 1", 1)))
}

pub fn error_empty_expression_test() {
  let result = handler_parser.parse("click", [], "count = ", 1)

  result |> should.equal(Error(EmptyExpression("count =", 1)))
}

pub fn error_invalid_tuple_syntax_test() {
  // Missing closing paren on tuple - triggers InvalidTupleSyntax
  let result = handler_parser.parse("click", [], "#(count, 123) = fn()", 1)

  result |> should.equal(Error(InvalidTupleSyntax("#(count, 123) = fn()", 1)))
}

pub fn side_effect_unbalanced_parens_test() {
  // The = is inside unbalanced parens, so no valid assignment found
  // This is treated as a side-effect expression
  let assert Ok(handler) =
    handler_parser.parse("click", [], "#(count = fn()", 1)

  handler.targets |> should.equal([])
  handler.expression |> should.equal("#(count = fn()")
}

// ------------------------------------------------------------- Edge Cases

pub fn preserves_equality_operator_test() {
  // Make sure == doesn't get confused with =
  let assert Ok(handler) =
    handler_parser.parse("click", [], "result = a == b", 1)

  handler.targets |> should.equal(["result"])
  handler.expression |> should.equal("a == b")
}

pub fn handles_nested_parens_test() {
  let assert Ok(handler) =
    handler_parser.parse("click", [], "x = fn(a, fn(b, c))", 1)

  handler.targets |> should.equal(["x"])
  handler.expression |> should.equal("fn(a, fn(b, c))")
}

pub fn handles_list_syntax_test() {
  let assert Ok(handler) =
    handler_parser.parse("click", [], "items = [a, b, c]", 1)

  handler.targets |> should.equal(["items"])
  handler.expression |> should.equal("[a, b, c]")
}

pub fn original_preserved_test() {
  let assert Ok(handler) =
    handler_parser.parse("click", [], "count = count + 1", 1)

  handler.original |> should.equal("count = count + 1")
}

// ------------------------------------------------------------- Collect Handlers Tests

fn make_template(nodes: List(parser.Node)) -> Template {
  Template(imports: [], props: [], nodes: nodes, is_live: True)
}

pub fn collect_handlers_empty_template_test() {
  let template = make_template([TextNode("Hello")])
  let assert Ok(handlers) = handler_parser.collect_handlers(template)

  handlers |> should.equal([])
}

pub fn collect_handlers_single_lm_on_test() {
  let template =
    make_template([
      ElementNode("button", [lexer.LmOn("click", [], "count = count + 1", 1)], [
        TextNode("+"),
      ]),
    ])
  let assert Ok(handlers) = handler_parser.collect_handlers(template)

  list.length(handlers) |> should.equal(1)

  let assert [#(id, handler)] = handlers
  id |> should.equal("handle_click_0")
  handler.event |> should.equal("click")
  handler.targets |> should.equal(["count"])
}

pub fn collect_handlers_multiple_lm_on_test() {
  let template =
    make_template([
      ElementNode("button", [lexer.LmOn("click", [], "count = count + 1", 1)], [
        TextNode("+"),
      ]),
      ElementNode("button", [lexer.LmOn("click", [], "count = count - 1", 2)], [
        TextNode("-"),
      ]),
    ])
  let assert Ok(handlers) = handler_parser.collect_handlers(template)

  list.length(handlers) |> should.equal(2)

  let assert [#(id1, _), #(id2, _)] = handlers
  id1 |> should.equal("handle_click_0")
  id2 |> should.equal("handle_click_1")
}

pub fn collect_handlers_lm_model_test() {
  let template =
    make_template([ElementNode("input", [lexer.LmModel("name", 1)], [])])
  let assert Ok(handlers) = handler_parser.collect_handlers(template)

  list.length(handlers) |> should.equal(1)

  let assert [#(id, handler)] = handlers
  id |> should.equal("handle_input_0")
  handler.event |> should.equal("input")
  handler.targets |> should.equal(["name"])
  handler.expression |> should.equal("$value")
}

pub fn collect_handlers_nested_elements_test() {
  let template =
    make_template([
      ElementNode("div", [], [
        ElementNode(
          "button",
          [lexer.LmOn("click", [], "count = count + 1", 1)],
          [TextNode("+")],
        ),
      ]),
    ])
  let assert Ok(handlers) = handler_parser.collect_handlers(template)

  list.length(handlers) |> should.equal(1)
}

pub fn collect_handlers_with_modifiers_test() {
  let template =
    make_template([
      ElementNode(
        "form",
        [lexer.LmOn("submit", ["prevent"], "errors = form.submit()", 1)],
        [],
      ),
    ])
  let assert Ok(handlers) = handler_parser.collect_handlers(template)

  let assert [#(_, handler)] = handlers
  handler.modifiers |> should.equal(["prevent"])
}
