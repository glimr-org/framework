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
import glimr/config/config
import simplifile

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

/// Emits the `<script>` and `<link>` tags for a Vite entry
/// point.
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
  case simplifile.read("priv/static/hot") {
    Ok(dev_url) -> dev_tags(string.trim(dev_url), entry)
    Error(_) -> prod_tags(entry)
  }
}

// ------------------------------------------------------------- Private Functions

/// Dev mode tags point directly at Vite's dev server. The
/// @vite/client script enables HMR and CSS injection, and the
/// entry module is loaded as a native ES module so changes
/// appear instantly without a full rebuild.
///
fn dev_tags(dev_url: String, entry: String) -> String {
  string.concat([
    "<script type=\"module\" src=\"",
    dev_url,
    "/@vite/client\"></script>\n",
    "<script type=\"module\" src=\"",
    dev_url,
    "/",
    entry,
    "\"></script>",
  ])
}

/// Production tags resolve hashed filenames from the Vite
/// manifest. CSS files are emitted as `<link>` tags before the
/// script to avoid FOUC. The manifest is cached in
/// persistent_term after the first read so subsequent requests
/// don't hit the filesystem.
///
fn prod_tags(entry: String) -> String {
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
        chunk.css
        |> list.map(fn(css_file) {
          string.concat([
            "<link rel=\"stylesheet\" href=\"/static/",
            css_file,
            "\" />",
          ])
        })
        |> string.join("\n")

      let js_tag =
        string.concat([
          "<script type=\"module\" src=\"/static/",
          chunk.file,
          "\"></script>",
        ])

      case css_tags {
        "" -> js_tag
        _ -> css_tags <> "\n" <> js_tag
      }
    }
    Error(_) -> "<!-- vite entry not found: " <> entry <> " -->"
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
