//// Framework Context
////
//// The framework's core request context type. Embeds the HTTP
//// request, response format, session, and user-defined app state
//// into a single value that flows through the entire middleware
//// and controller pipeline. The `app` type parameter lets each
//// application define its own dependencies (database pools,
//// caches, etc.) without the framework needing to know about
//// them.

import glimr/http/request.{type Request}
import glimr/http/response.{type ResponseFormat}
import glimr/session/session.{type Session}

// ------------------------------------------------------------- Public Types

/// The unified context passed to every middleware and controller.
/// Framework state (request, response format, session) lives at
/// the top level, while app-specific state lives in the `app`
/// field. This means the framework can read and modify its own
/// fields (e.g., session.load updating the session) without
/// knowing the shape of the user's app type.
///
pub type Context(app) {
  Context(
    req: Request,
    response_format: ResponseFormat,
    session: Session,
    app: app,
  )
}

// ------------------------------------------------------------- Public Functions

/// Creates a new context with sensible defaults: HTML response
/// format and an empty session. The real session is hydrated
/// later by the session.load middleware, but having a default
/// here means the context can be constructed at boot time
/// before any request arrives.
///
pub fn new(req: Request, app: app) -> Context(app) {
  Context(
    req: req,
    response_format: response.HTML,
    session: session.empty(),
    app: app,
  )
}
