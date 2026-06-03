//// Codec Directive Tests
////
//// These tests cover the fix for GitHub issue #21, where live
//// templates with custom prop types (e.g. `List(Image)`)
//// silently fell back to `decode.string` / `json.string(...)`
//// and produced generated code that would not compile. The fix
//// introduces a `codec(Type, encode: fn, decode: fn)`
//// frontmatter directive so users can supply JSON codecs for
//// their own types, and replaces the silent fallback with a
//// clear loom-level validation error.
////

import gleam/dict
import gleam/list
import gleam/string
import gleeunit/should
import glimr/loom/generator
import glimr/loom/lexer
import glimr/loom/parser

// ------------------------------------------------------------- Helpers

fn generate(parsed: parser.Template, name: String) -> generator.GeneratedCode {
  generator.generate(parsed, name, False, dict.new(), dict.new())
}

// ------------------------------------------------------------- Bug Fix: Unsupported Prop Types

/// Issue #21 reproducer. A live template with `List(Image)` and
/// no codec must surface a clear loom-level validation error
/// instead of silently generating broken decode.string /
/// json.string code.
///
pub fn live_template_with_undeclared_custom_type_errors_test() {
  let template =
    "---
import app/loom/cat.{type Image}
props(images: List(Image))
---
<div l-on:click=\"images = images\">click</div>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  case generator.validate_template(parsed, "gallery.loom.html") {
    Error(msg) -> {
      msg |> string.contains("Image") |> should.be_true
      msg |> string.contains("codec") |> should.be_true
    }
    Ok(_) ->
      panic as "Expected validation error for List(Image) in live template without codec"
  }
}

/// Bare custom prop type in a live template must also error
/// (not just List of custom). Same root cause as List(Image).
///
pub fn live_template_with_undeclared_bare_custom_type_errors_test() {
  let template =
    "---
import app/loom/cat.{type Image}
props(image: Image)
---
<div l-on:click=\"image = image\">click</div>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  case generator.validate_template(parsed, "gallery.loom.html") {
    Error(msg) -> msg |> string.contains("Image") |> should.be_true
    Ok(_) ->
      panic as "Expected validation error for bare Image in live template without codec"
  }
}

/// Non-live templates never serialize props through JSON, so an
/// unknown prop type there must continue to validate cleanly.
/// We don't want this bug fix to over-tighten static templates.
///
pub fn static_template_with_custom_prop_type_still_passes_test() {
  let template =
    "---
import app/loom/cat.{type Image}
props(image: Image)
---
<div>{{ image.url }}</div>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  generator.validate_template(parsed, "gallery.loom.html")
  |> should.be_ok
}

// ------------------------------------------------------------- Codec Directive: Lexer / Parser

/// The lexer must recognize `codec(Type, encode: fn, decode:
/// fn)` in the frontmatter and emit a CodecDirective token
/// alongside existing PropsDirective / ImportDirective tokens.
///
pub fn lexer_emits_codec_directive_token_test() {
  let template =
    "---
import app/loom/cat.{type Image, encode_image, image_decoder}
props(images: List(Image))
codec(Image, encode: encode_image, decode: image_decoder)
---
<div></div>"

  let assert Ok(tokens) = lexer.tokenize(template)

  let has_codec =
    list.any(tokens, fn(t) {
      case t {
        lexer.CodecDirective("Image", "encode_image", "image_decoder", _) ->
          True
        _ -> False
      }
    })
  should.be_true(has_codec)
}

/// The parser must propagate codec directives into the Template
/// value so the generator can look them up by type name when
/// emitting encoders / decoders.
///
pub fn parser_collects_codecs_into_template_test() {
  let template =
    "---
import app/loom/cat.{type Image, encode_image, image_decoder}
props(images: List(Image))
codec(Image, encode: encode_image, decode: image_decoder)
---
<div></div>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  parsed.codecs
  |> should.equal([#("Image", "encode_image", "image_decoder")])
}

/// Multiple codec lines should all be collected. Templates that
/// use several custom types need to declare codecs for each.
///
pub fn parser_collects_multiple_codecs_test() {
  let template =
    "---
import app/loom/cat.{type Image, encode_image, image_decoder}
import app/loom/user.{type User, encode_user, user_decoder}
props(images: List(Image), user: User)
codec(Image, encode: encode_image, decode: image_decoder)
codec(User, encode: encode_user, decode: user_decoder)
---
<div></div>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)

  parsed.codecs
  |> list.length
  |> should.equal(2)
}

// ------------------------------------------------------------- Generator: Codec Usage

/// With a codec declared, the generated decoder for
/// `List(Image)` must call `decode.list(image_decoder())` and
/// NOT fall back to `decode.string`.
///
pub fn generator_uses_codec_in_list_decoder_test() {
  let template =
    "---
import app/loom/cat.{type Image, encode_image, image_decoder}
props(images: List(Image))
codec(Image, encode: encode_image, decode: image_decoder)
---
<div l-on:click=\"images = images\">x</div>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)
  let generated = generate(parsed, "gallery")

  generated.code
  |> string.contains("decode.list(image_decoder())")
  |> should.be_true

  generated.code
  |> string.contains("decode.field(\"images\", decode.string)")
  |> should.be_false
}

/// With a codec declared, the generated encoder for
/// `List(Image)` must call `json.array(images, encode_image)`
/// and NOT fall back to `json.string(images)`.
///
pub fn generator_uses_codec_in_list_encoder_test() {
  let template =
    "---
import app/loom/cat.{type Image, encode_image, image_decoder}
props(images: List(Image))
codec(Image, encode: encode_image, decode: image_decoder)
---
<div l-on:click=\"images = images\">x</div>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)
  let generated = generate(parsed, "gallery")

  generated.code
  |> string.contains("json.array(images, encode_image)")
  |> should.be_true

  generated.code
  |> string.contains("json.string(images)")
  |> should.be_false
}

/// Bare custom type with codec: decoder uses `image_decoder()`
/// and encoder uses `encode_image(image)`.
///
pub fn generator_uses_codec_for_bare_custom_type_test() {
  let template =
    "---
import app/loom/cat.{type Image, encode_image, image_decoder}
props(image: Image)
codec(Image, encode: encode_image, decode: image_decoder)
---
<div l-on:click=\"image = image\">x</div>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)
  let generated = generate(parsed, "gallery")

  generated.code
  |> string.contains("decode.field(\"image\", image_decoder())")
  |> should.be_true

  generated.code
  |> string.contains("encode_image(image)")
  |> should.be_true
}

// ------------------------------------------------------------- Recursive Generic Support

/// `Option(T)` of a primitive should work without any codec
/// declaration, because Option and its inner primitive are both
/// natively supported.
///
pub fn generator_handles_option_of_primitive_test() {
  let template =
    "---
props(maybe_name: Option(String))
---
<input l-on:input=\"maybe_name = $value\" />"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)
  let generated = generate(parsed, "form")

  generated.code
  |> string.contains("decode.optional(decode.string)")
  |> should.be_true
}

/// `Option(T)` of a custom type uses the codec recursively.
///
pub fn generator_handles_option_of_custom_type_test() {
  let template =
    "---
import app/loom/cat.{type Image, encode_image, image_decoder}
props(maybe_image: Option(Image))
codec(Image, encode: encode_image, decode: image_decoder)
---
<div l-on:click=\"maybe_image = maybe_image\">x</div>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)
  let generated = generate(parsed, "gallery")

  generated.code
  |> string.contains("decode.optional(image_decoder())")
  |> should.be_true
}

/// Nested `List(List(Int))` should work because both layers are
/// natively supported and the generator recurses on the inner
/// type.
///
pub fn generator_handles_nested_list_of_primitives_test() {
  let template =
    "---
props(grid: List(List(Int)))
---
<div l-on:click=\"grid = grid\">x</div>"

  let assert Ok(tokens) = lexer.tokenize(template)
  let assert Ok(parsed) = parser.parse(tokens)
  let generated = generate(parsed, "grid")

  generated.code
  |> string.contains("decode.list(decode.list(decode.int))")
  |> should.be_true
}
