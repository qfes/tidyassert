#' Assert
#'
#' Raises an assertion error when `any(expr)` is false.
#' @name assert
#' @param expr <`expression`> a logical expression to test.
#' @param error_message <`string`> a message to be displayed when assertion fails.
#' @param error_class <`character`> the class name/s for the error.
#'
#' @export
assert <- function(expr, error_message = NULL, error_class = NULL) {
  if (typeof(expr) != "logical") {
    rlang::abort("expr must be logical", "assert_error")
  }

  assert_(expr, quo_expr(substitute(expr)), error_message, error_class)
}

assert_ <- function(expr, qexpr, error_message = NULL, error_class = NULL) {
  if (!all(expr) || length(expr) == 0) {
    rlang::abort(
      message = fmt_bullets(error_message),
      class = c(error_class, "assert_error"),
      expr = qexpr,
      call = sys.call(-2L),
      # exclude this function from the trace
      trace = rlang::trace_back(bottom = sys.frame(-1L))
    )
  }
}

#' @importFrom rlang cnd_header
#' @export
cnd_header.assert_error <- function(cnd, ...) {
  paste("Assertion failed:", rlang::quo_text(cnd$expr))
}

#' @importFrom rlang cnd_body
#' @export
cnd_body.assert_error <- function(cnd, ...) {
  if (cnd$message != "") cnd$message
}
