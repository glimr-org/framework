import gleeunit/should
import glimr/loom/runtime

// ------------------------------------------------------------- build_classes Tests

pub fn build_classes_all_true_test() {
  runtime.build_classes([
    #("p-4", True),
    #("font-bold", True),
    #("text-red", True),
  ])
  |> should.equal("p-4 font-bold text-red")
}

pub fn build_classes_some_false_test() {
  runtime.build_classes([
    #("p-4", True),
    #("font-bold", False),
    #("text-red", True),
  ])
  |> should.equal("p-4 text-red")
}

pub fn build_classes_all_false_test() {
  runtime.build_classes([#("p-4", False), #("font-bold", False)])
  |> should.equal("")
}

pub fn build_classes_empty_list_test() {
  runtime.build_classes([])
  |> should.equal("")
}

pub fn build_classes_single_true_test() {
  runtime.build_classes([#("active", True)])
  |> should.equal("active")
}

pub fn build_classes_single_false_test() {
  runtime.build_classes([#("active", False)])
  |> should.equal("")
}

pub fn build_classes_preserves_spaces_in_classes_test() {
  // Multiple classes in a single string are preserved
  runtime.build_classes([#("hover:bg-blue-500 focus:outline-none", True)])
  |> should.equal("hover:bg-blue-500 focus:outline-none")
}

// ------------------------------------------------------------- class() Helper Tests

pub fn class_helper_returns_always_true_tuple_test() {
  runtime.class("btn primary")
  |> should.equal(#("btn primary", True))
}

pub fn class_helper_works_with_build_classes_test() {
  runtime.build_classes([
    runtime.class("btn"),
    runtime.class("primary"),
    #("active", True),
    #("disabled", False),
  ])
  |> should.equal("btn primary active")
}

pub fn class_helper_with_empty_string_test() {
  runtime.class("")
  |> should.equal(#("", True))
}

pub fn class_helper_mixed_static_and_conditional_test() {
  // The new syntax: :class="[class('static classes'), #('conditional', cond)]"
  let is_active = True
  let is_disabled = False
  runtime.build_classes([
    runtime.class("btn lg rounded"),
    #("active", is_active),
    #("disabled", is_disabled),
  ])
  |> should.equal("btn lg rounded active")
}

// ------------------------------------------------------------- build_styles Tests

pub fn build_styles_all_true_test() {
  runtime.build_styles([
    #("color: red", True),
    #("font-size: 16px", True),
    #("margin: 0", True),
  ])
  |> should.equal("color: red font-size: 16px margin: 0")
}

pub fn build_styles_some_false_test() {
  runtime.build_styles([
    #("color: red", True),
    #("font-size: 16px", False),
    #("margin: 0", True),
  ])
  |> should.equal("color: red margin: 0")
}

pub fn build_styles_all_false_test() {
  runtime.build_styles([#("color: red", False), #("margin: 0", False)])
  |> should.equal("")
}

pub fn build_styles_empty_list_test() {
  runtime.build_styles([])
  |> should.equal("")
}

pub fn build_styles_single_true_test() {
  runtime.build_styles([#("display: none", True)])
  |> should.equal("display: none")
}

pub fn build_styles_single_false_test() {
  runtime.build_styles([#("display: none", False)])
  |> should.equal("")
}

pub fn build_styles_with_semicolons_test() {
  runtime.build_styles([#("color: red;", True), #("margin: 0;", True)])
  |> should.equal("color: red; margin: 0;")
}

// ------------------------------------------------------------- style() Helper Tests

pub fn style_helper_returns_always_true_tuple_test() {
  runtime.style("color: red; margin: 0")
  |> should.equal(#("color: red; margin: 0", True))
}

pub fn style_helper_works_with_build_styles_test() {
  runtime.build_styles([
    runtime.style("color: red"),
    runtime.style("font-size: 16px"),
    #("display: none", True),
    #("opacity: 0", False),
  ])
  |> should.equal("color: red font-size: 16px display: none")
}

pub fn style_helper_with_empty_string_test() {
  runtime.style("")
  |> should.equal(#("", True))
}

pub fn style_helper_mixed_static_and_conditional_test() {
  // The new syntax: :style="[style('color: red; margin: 0'), #('display: none', is_hidden)]"
  let is_hidden = True
  let is_faded = False
  runtime.build_styles([
    runtime.style("color: red; padding: 10px"),
    #("display: none", is_hidden),
    #("opacity: 0.5", is_faded),
  ])
  |> should.equal("color: red; padding: 10px display: none")
}
