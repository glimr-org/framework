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

pub fn render_escapes_xss_in_variables_test() {
  let layout = "<html><body><div>{{content}}</div></body></html>"

  let response =
    view.build()
    |> fn(v) { view.View(..v, content: "", layout: layout) }
    |> view.data([#("content", "<script>alert('XSS')</script>")])
    |> view.render()

  response.body
  |> should.equal(wisp.Text(
    "<html><body><div>&lt;script&gt;alert(&#x27;XSS&#x27;)&lt;/script&gt;</div></body></html>",
  ))
}

pub fn render_escapes_html_tags_in_variables_test() {
  let layout = "<html><head><title>{{title}}</title></head></html>"

  let response =
    view.build()
    |> fn(v) { view.View(..v, content: "", layout: layout) }
    |> view.data([#("title", "<h1>My Title</h1>")])
    |> view.render()

  response.body
  |> should.equal(wisp.Text(
    "<html><head><title>&lt;h1&gt;My Title&lt;/h1&gt;</title></head></html>",
  ))
}

pub fn render_escapes_quotes_in_variables_test() {
  let layout = "<div title=\"{{title}}\">{{content}}</div>"

  let response =
    view.build()
    |> fn(v) { view.View(..v, content: "", layout: layout) }
    |> view.data([
      #("title", "Hello \"World\""),
      #("content", "It's a test"),
    ])
    |> view.render()

  response.body
  |> should.equal(wisp.Text(
    "<div title=\"Hello &quot;World&quot;\">It&#x27;s a test</div>",
  ))
}

pub fn render_prevents_template_injection_in_content_test() {
  // Content with template syntax should not be processed when inserted into layout
  let layout = "<html><body>{{_content_}}<div>{{title}}</div></body></html>"
  let malicious_content = "<p>{{title}}</p>"

  let response =
    view.build()
    |> view.html_raw(malicious_content)
    |> fn(v) { view.View(..v, layout: layout) }
    |> view.data([#("title", "Safe Title")])
    |> view.render()

  // The {{title}} in content should be escaped, not replaced
  // The {{title}} in layout should be replaced
  response.body
  |> should.equal(wisp.Text(
    "<html><body><p>&#123;&#123;title&#125;&#125;</p><div>Safe Title</div></body></html>",
  ))
}

pub fn render_prevents_template_injection_with_content_variable_test() {
  // Content trying to inject {{_content_}} should not cause issues
  let layout = "<div>{{_content_}}</div>"
  let malicious_content = "<p>{{_content_}}</p>"

  let response =
    view.build()
    |> view.html_raw(malicious_content)
    |> fn(v) { view.View(..v, layout: layout) }
    |> view.render()

  // The {{_content_}} in content should be escaped (note: _content_ is special
  // and gets replaced, but the escaped version in content won't match)
  response.body
  |> should.equal(wisp.Text("<div><p>&#123;&#123;_content_&#125;&#125;</p></div>"))
}
