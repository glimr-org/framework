//// Serve Static Middleware
////
//// Each middleware lives in its own module so the kernel
//// pipeline can compose them independently — apps that don't
//// serve static files can simply omit this step. Pulling the
//// directory prefix from app config rather than hardcoding it
//// means the URL path and filesystem path stay in sync without
//// duplicating the value across middleware and config.
////

import glimr/config/config
import glimr/http/kernel.{type Next}
import wisp.{type Request, type Response}

// ------------------------------------------------------------- Public Functions

/// The config is loaded via persistent_term so checking the
/// static prefix on every request adds almost no overhead.
/// Matching static files early means asset requests skip
/// session parsing, CSRF validation, and other downstream
/// middleware that only applies to dynamic routes.
///
pub fn run(req: Request, ctx: context, next: Next(context)) -> Response {
  let static_directory = config.get_string("app.static.directory")

  use <- wisp.serve_static(
    req,
    under: static_directory,
    from: "priv" <> static_directory,
  )

  next(req, ctx)
}
