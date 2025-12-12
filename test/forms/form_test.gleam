import gleeunit/should
import glimr/forms/form
import wisp

pub fn get_existing_field_test() {
  let form_data =
    wisp.FormData(
      values: [#("name", "John"), #("email", "john@example.com")],
      files: [],
    )

  form_data
  |> form.get("name")
  |> should.equal("John")
}

pub fn get_missing_field_test() {
  let form_data = wisp.FormData(values: [], files: [])

  form_data
  |> form.get("missing")
  |> should.equal("")
}

pub fn has_existing_field_test() {
  let form_data = wisp.FormData(values: [#("name", "John")], files: [])

  form_data
  |> form.has("name")
  |> should.be_true()
}

pub fn has_missing_field_test() {
  let form_data = wisp.FormData(values: [], files: [])

  form_data
  |> form.has("missing")
  |> should.be_false()
}

pub fn get_file_result_existing_test() {
  let uploaded_file =
    wisp.UploadedFile(file_name: "test.jpg", path: "/tmp/test")
  let form_data = wisp.FormData(values: [], files: [#("avatar", uploaded_file)])

  case form_data |> form.get_file_result("avatar") {
    Ok(file) -> {
      file.file_name
      |> should.equal("test.jpg")
    }
    Error(_) -> should.fail()
  }
}

pub fn get_file_result_missing_test() {
  let form_data = wisp.FormData(values: [], files: [])

  form_data
  |> form.get_file_result("avatar")
  |> should.be_error()
}

pub fn get_file_existing_test() {
  let uploaded_file =
    wisp.UploadedFile(file_name: "test.jpg", path: "/tmp/test")
  let form_data = wisp.FormData(values: [], files: [#("avatar", uploaded_file)])

  let file = form_data |> form.get_file("avatar")

  file.file_name
  |> should.equal("test.jpg")
}

pub fn has_file_existing_test() {
  let uploaded_file =
    wisp.UploadedFile(file_name: "test.jpg", path: "/tmp/test")
  let form_data = wisp.FormData(values: [], files: [#("avatar", uploaded_file)])

  form_data
  |> form.has_file("avatar")
  |> should.be_true()
}

pub fn has_file_missing_test() {
  let form_data = wisp.FormData(values: [], files: [])

  form_data
  |> form.has_file("avatar")
  |> should.be_false()
}
