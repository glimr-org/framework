//// Vite Asset Integration
////
//// Glimr apps use Vite as their asset bundler, but templates
//// need the correct script and link tags for both development
//// (where Vite's dev server handles HMR) and production (where
//// hashed filenames come from a manifest). This module reads
//// the Vite manifest and hot-file to emit the right tags
//// automatically — like Laravel's @vite() directive.
////

import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/string
import glimr/config
import simplifile

// ------------------------------------------------------------- Public Types

/// A structured representation of a single HTML tag needed for
/// a Vite entry point. Useful when you need to render tags in a
/// typed UI library like Lustre instead of injecting raw HTML.
///
pub type Tag {
  Script(src: String)
  Stylesheet(href: String)
}

// ------------------------------------------------------------- Private Types

/// A single entry in Vite's manifest — the hashed output
/// filename and any CSS files it pulled in. Vite bundles CSS
/// separately from JS so the template needs both to avoid a
/// flash of unstyled content.
///
type ManifestChunk {
  ManifestChunk(file: String, css: List(String))
}

// ------------------------------------------------------------- Public Functions

/// Returns structured tag data for a Vite entry point. Each
/// item in the list is either a `Script` or `Stylesheet` with
/// the resolved URL. Use this when you need to map the tags
/// into a typed element tree (e.g. Lustre) rather than
/// injecting raw HTML.
///
/// ```gleam
/// vite.to_tags("src/resources/ts/app.ts")
/// |> list.map(fn(tag) {
///   case tag {
///     vite.Script(src) -> html.script([attribute.type_("module"), attribute.src(src)], [])
///     vite.Stylesheet(href) -> html.link([attribute.rel("stylesheet"), attribute.href(href)])
///   }
/// })
/// ```
///
pub fn to_tags(entry: String) -> List(Tag) {
  case simplifile.read("priv/static/hot") {
    Ok(dev_url) -> dev_tag_list(string.trim(dev_url), entry)
    Error(_) -> prod_tag_list(entry)
  }
}

/// Renders a list of tags to an HTML string. This is the
/// building block behind `tags()` and can be used standalone
/// when you have a `List(Tag)` from `to_tags()`.
///
pub fn render_tags(tags: List(Tag)) -> String {
  tags
  |> list.map(render_tag)
  |> string.join("\n")
}

/// Emits the `<script>` and `<link>` tags for a Vite entry
/// point as a ready-to-use HTML string.
///
/// In dev mode (when `priv/static/hot` exists), returns tags
/// pointing at Vite's dev server for HMR. In production, reads
/// the Vite manifest to resolve hashed filenames and returns
/// prebuilt asset tags with a `/static/` prefix.
///
/// ```loom
/// @import(glimr/vite)
/// {{{ vite.tags("resources/ts/app.ts") }}}
/// ```
///
pub fn tags(entry: String) -> String {
  to_tags(entry) |> render_tags
}

// ------------------------------------------------------------- Private Functions

/// Dev mode tags point directly at Vite's dev server. The
/// @vite/client script enables HMR and CSS injection, and the
/// entry module is loaded as a native ES module so changes
/// appear instantly without a full rebuild.
///
fn dev_tag_list(dev_url: String, entry: String) -> List(Tag) {
  let css_entry = resolve_css_entry(entry)

  list.flatten([
    [Script(src: dev_url <> "/@vite/client")],
    list.map(css_entry, fn(css_path) {
      Stylesheet(href: dev_url <> "/" <> css_path)
    }),
    [Script(src: dev_url <> "/" <> entry)],
  ])
}

/// Reads the JS entry file and extracts CSS import paths so
/// they can be emitted as `<link>` tags in dev mode. This
/// prevents FOUC by loading stylesheets before the page renders
/// instead of waiting for Vite's JS client to inject them.
///
fn resolve_css_entry(entry: String) -> List(String) {
  case simplifile.read(entry) {
    Ok(content) ->
      content
      |> string.split("\n")
      |> list.filter_map(fn(line) {
        let trimmed = string.trim(line)
        case string.starts_with(trimmed, "import \"") {
          True -> extract_import_path(trimmed, "import \"")
          False ->
            case string.starts_with(trimmed, "import '") {
              True -> extract_import_path(trimmed, "import '")
              False -> Error(Nil)
            }
        }
      })
      |> list.filter(fn(path) { string.ends_with(path, ".css") })
      |> list.map(fn(path) { resolve_relative_path(entry, path) })
    Error(_) -> []
  }
}

/// JS imports can use single or double quotes, and may have a
/// trailing semicolon. Rather than regex, splitting on the
/// matching quote char after the prefix cleanly isolates the
/// path regardless of quote style or trailing punctuation.
///
fn extract_import_path(line: String, prefix: String) -> Result(String, Nil) {
  let rest = string.drop_start(line, string.length(prefix))
  let quote = case prefix {
    "import \"" -> "\""
    _ -> "'"
  }
  case string.split_once(rest, quote) {
    Ok(#(path, _)) -> Ok(path)
    Error(_) -> Error(Nil)
  }
}

/// CSS imports in the entry file use relative paths like
/// `../css/app.css`, but the dev server needs a root-relative
/// path like `resources/css/app.css`. Walking `..` and `.`
/// segments against the entry file's directory produces the
/// path Vite's dev server expects.
///
fn resolve_relative_path(entry: String, relative: String) -> String {
  let parts = string.split(entry, "/")
  let dir = list.take(parts, list.length(parts) - 1)

  let relative_parts = string.split(relative, "/")
  let resolved =
    list.fold(relative_parts, dir, fn(acc, segment) {
      case segment {
        ".." -> list.take(acc, list.length(acc) - 1)
        "." -> acc
        _ -> list.append(acc, [segment])
      }
    })

  string.join(resolved, "/")
}

/// Production tags resolve hashed filenames from the Vite
/// manifest. CSS files are emitted as `Stylesheet` tags before
/// the script to avoid FOUC. The manifest is cached in
/// persistent_term after the first read so subsequent requests
/// don't hit the filesystem.
///
fn prod_tag_list(entry: String) -> List(Tag) {
  let manifest = case config.get_cached("vite_manifest") {
    Ok(m) -> m
    Error(_) -> {
      let m = read_manifest()
      config.cache("vite_manifest", m)
      m
    }
  }

  case dict.get(manifest, entry) {
    Ok(chunk) -> {
      let css_tags =
        list.map(chunk.css, fn(css_file) {
          Stylesheet(href: "/static/" <> css_file)
        })

      list.append(css_tags, [Script(src: "/static/" <> chunk.file)])
    }
    Error(_) -> []
  }
}

/// Reads and parses `priv/static/.vite/manifest.json`. Returns
/// an empty dict on failure so the app degrades to
/// missing-entry comments rather than crashing — useful during
/// first deploy before assets are built.
///
fn read_manifest() -> Dict(String, ManifestChunk) {
  case simplifile.read("priv/static/.vite/manifest.json") {
    Ok(content) -> parse_manifest(content)
    Error(_) -> dict.new()
  }
}

/// Vite's manifest maps entry points to their bundled output,
/// so `app.ts` might become `app-3f2a1b.js` with a separate
/// `app-9c4d.css`. Decoding into ManifestChunks gives us typed
/// access to both filenames so prod_tags can emit the right
/// HTML without string hacking.
///
fn parse_manifest(content: String) -> Dict(String, ManifestChunk) {
  let chunk_decoder = {
    use file <- decode.field("file", decode.string)
    use css <- decode.optional_field("css", [], decode.list(decode.string))
    decode.success(ManifestChunk(file: file, css: css))
  }

  let manifest_decoder = decode.dict(decode.string, chunk_decoder)

  case json.parse(content, manifest_decoder) {
    Ok(manifest) -> manifest
    Error(_) -> dict.new()
  }
}

fn render_tag(tag: Tag) -> String {
  case tag {
    Script(src:) -> "<script type=\"module\" src=\"" <> src <> "\"></script>"
    Stylesheet(href:) -> "<link rel=\"stylesheet\" href=\"" <> href <> "\">"
  }
}
