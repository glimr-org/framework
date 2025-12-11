import gleam/dict
import gleeunit/should
import glimr/response/view
import lustre/element/html
import wisp

pub fn build_test() {
  let v = view.build()

  v.content
  |> should.equal("")

  v.data
  |> dict.to_list()
  |> should.equal([])

  v.layout |> should.equal("")
}

pub fn lustre_test() {
  let element = html.div([], [html.text("Hello, World!")])

  let v =
    view.build()
    |> view.lustre(element)

  v.content
  |> should.equal("<div>Hello, World!</div>")
}

pub fn lustre_with_nested_elements_test() {
  let element =
    html.div([], [
      html.h1([], [html.text("Title")]),
      html.p([], [html.text("Paragraph")]),
    ])

  let v =
    view.build()
    |> view.lustre(element)

  v.content
  |> should.equal("<div><h1>Title</h1><p>Paragraph</p></div>")
}

pub fn data_single_test() {
  let v =
    view.build()
    |> view.data([#("title", "My Page")])

  v.data
  |> dict.get("title")
  |> should.equal(Ok("My Page"))
}

pub fn data_multiple_test() {
  let v =
    view.build()
    |> view.data([#("title", "My Page"), #("author", "John Doe")])

  v.data
  |> dict.size()
  |> should.equal(2)

  v.data
  |> dict.get("title")
  |> should.equal(Ok("My Page"))

  v.data
  |> dict.get("author")
  |> should.equal(Ok("John Doe"))
}

pub fn data_merge_test() {
  let v =
    view.build()
    |> view.data([#("title", "Page")])
    |> view.data([#("author", "Jane")])

  v.data
  |> dict.size()
  |> should.equal(2)

  v.data
  |> dict.get("title")
  |> should.equal(Ok("Page"))

  v.data
  |> dict.get("author")
  |> should.equal(Ok("Jane"))
}

pub fn render_replaces_content_test() {
  let element = html.div([], [html.text("Content")])
  let layout = "<html><body>{{_content_}}</body></html>"

  let response =
    view.build()
    |> view.lustre(element)
    |> fn(v) { view.View(..v, layout: layout) }
    |> view.render()

  response.status
  |> should.equal(200)

  response.body
  |> should.equal(wisp.Text("<html><body><div>Content</div></body></html>"))
}

pub fn render_replaces_variables_test() {
  let layout = "<html><head><title>{{title}}</title></head></html>"

  let response =
    view.build()
    |> fn(v) { view.View(..v, content: "", layout: layout) }
    |> view.data([#("title", "My Page")])
    |> view.render()

  response.body
  |> should.equal(wisp.Text("<html><head><title>My Page</title></head></html>"))
}

pub fn render_strips_unused_variables_test() {
  let layout = "<html><head><title>{{title}}</title></head></html>"

  let response =
    view.build()
    |> fn(v) { view.View(..v, content: "", layout: layout) }
    |> view.render()

  response.body
  |> should.equal(wisp.Text("<html><head><title></title></head></html>"))
}

pub fn render_with_multiple_variables_test() {
  let layout =
    "<html><head><title>{{title}}</title></head><body><h1>{{heading}}</h1></body></html>"

  let response =
    view.build()
    |> fn(v) { view.View(..v, content: "", layout: layout) }
    |> view.data([#("title", "Page Title"), #("heading", "Welcome")])
    |> view.render()

  response.body
  |> should.equal(wisp.Text(
    "<html><head><title>Page Title</title></head><body><h1>Welcome</h1></body></html>",
  ))
}

pub fn render_strips_multiple_unused_variables_test() {
  let layout = "<p>{{foo}} and {{bar}} and {{baz}}</p>"

  let response =
    view.build()
    |> fn(v) { view.View(..v, content: "", layout: layout) }
    |> view.data([#("bar", "middle")])
    |> view.render()

  response.body
  |> should.equal(wisp.Text("<p> and middle and </p>"))
}

pub fn render_preserves_content_variable_test() {
  let layout = "<div>{{_content_}}</div>"
  let element = html.p([], [html.text("Test")])

  let response =
    view.build()
    |> view.lustre(element)
    |> fn(v) { view.View(..v, layout: layout) }
    |> view.render()

  response.body
  |> should.equal(wisp.Text("<div><p>Test</p></div>"))
}
