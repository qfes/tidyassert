#' Assert
#'
#' Raises an assertion error when `any(expr)` is false.
#'
#' @name assert
#' @param expr <`expression`> a logical expression to test.
#' @param error_message <`string`> a message to be displayed when assertion fails.
#' @param error_class <`character`> the class name/s for the error.
#' @param env <`environment`> the environment for substituted `dots` and `print_expr`.
#' Has no effect if `dots` and `print_expr` are already quosures.
#' @param print_expr <`expression`> a diffused expression for altering the error message.
#' Defaults to `rlang::as_quosure(substitute(expr), rlang::caller_env())`
#' @param ... <`any`> values used in evaluating glue expressions, for the error message.
#'
#' @export
assert <- function(expr, error_message = NULL, error_class = NULL,
                   env = rlang::caller_env(2L), print_expr = NULL, ...) {
  if (!is.logical(expr)) rlang::abort("expr must be logical", "assert_error")

  if (!all_true(expr)) {
    quo_expr <- rlang::as_quosure(substitute(expr), rlang::caller_env())
    signal_error(
      quo_expr,
      error_message,
      error_class,
      env,
      print_expr = print_expr %||% quo_expr,
      rlang::as_quosures(c(...), env, named = TRUE)
    )
  }
}

signal_error <- function(expr, error_message = NULL, error_class = NULL,
                         env = rlang::caller_env(2L), print_expr = NULL, ...) {
  quo_dots <- rlang::as_quosures(c(...), env, named = TRUE)
  # diffused expr to be retrieved from the error object
  quo_expr <- rlang::as_quosure(expr, rlang::caller_env(2L))

  rlang::abort(
    message = format_message(error_message, quo_dots),
    class = c(error_class, "assert_error"),
    expr = quo_expr,
    # what is printed in the header?
    print_expr = rlang::as_quosure(print_expr %||% quo_expr, env),
    call = rlang::caller_call()
  )
}

#' @export
cnd_header.assert_error <- function(cnd, ...) {
  paste("Assertion failed:", rlang::quo_text(cnd$print_expr))
}

#' @export
cnd_body.assert_error <- function(cnd, ...) {
  if (cnd$message != "") cnd$message
}
