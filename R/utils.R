# wrap expression in quosure
quo_expr <- function(expr, env = parent.frame(2L)) {
  rlang::as_quosure(expr, env)
}

vapply_c <- function(x, fn, ...) {
  vapply(x, fn, FUN.VALUE = character(1), ..., USE.NAMES = FALSE)
}

reduce <- function(x, fn, init, ...) {
  fn_wrap <- function(x, y) fn(x, y, ...)
  Reduce(fn_wrap, x, init = init)
}
