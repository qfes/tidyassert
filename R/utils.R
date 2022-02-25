quo_dots <- function(..., env = rlang::caller_env(2L), named = TRUE) {
  rlang::as_quosures(c(...), env, named)
}

vapply_c <- function(x, fn, ..., named = TRUE) {
  vapply(x, fn, FUN.VALUE = character(1), ..., USE.NAMES = named)
}

reduce <- function(x, fn, init, ...) {
  fn_wrap <- function(x, y) fn(x, y, ...)
  Reduce(fn_wrap, x, init = init)
}

all_true <- function(expr) all(expr) && length(expr) != 0
