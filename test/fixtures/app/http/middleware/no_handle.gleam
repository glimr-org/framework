//// Test fixture for middleware validation tests - missing handle function

pub fn something_else(req, ctx, next) {
  next(req, ctx)
}
