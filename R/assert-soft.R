#' Assert (soft)
#'
#' Raises an assertion warning when `any(expr)` is false.
#'
#' @name warn_if_not
#' @rdname assert_soft
#' @inherit assert
#' @param warn_message <`string`> a message to be displayed when assertion fails.
#' @param warn_class <`character`> the class name/s for the warning.
#'
#' @export
warn_if_not <- function(expr, warn_message = NULL, warn_class = NULL,
                        call = rlang::caller_call(),
                        env = rlang::caller_env(), print_expr = NULL, ...) {
  if (!is.logical(expr)) rlang::abort("expr must be logical", "assert_error")

  if (!all_true(expr)) {
    quo_expr <- rlang::as_quosure(substitute(expr), rlang::caller_env())
    signal_warning(
      quo_expr,
      warn_message,
      warn_class,
      call,
      env,
      print_expr = print_expr %||% quo_expr,
      ...
    )
  }
}

#' Assert (soft)
#'
#' Raises an assertion warning when `all(expr)` is true.
#'
#' @name warn_if
#' @rdname assert_soft
#' @inherit warn_if_not
#'
#' @export
warn_if <- function(expr, warn_message = NULL, warn_class = NULL,
                    call = rlang::caller_call(),
                    env = rlang::caller_env(2L), print_expr = NULL, ...) {
  if (!is.logical(expr)) rlang::abort("expr must be logical", "assert_error")

  if (all(expr)) {
    quo_expr <- rlang::as_quosure(substitute(expr), rlang::caller_env())
    signal_warning(
      quo_expr,
      warn_message,
      warn_class,
      call,
      env,
      print_expr = print_expr %||% quo_expr,
      ...
    )
  }
}


signal_warning <- function(expr, warn_message = NULL, warn_class = NULL,
                           call = rlang::caller_call(2L),
                           env = rlang::caller_env(2L), print_expr = NULL, ...) {
  quo_dots <- rlang::as_quosures(c(...), env, named = TRUE)
  # diffused expr to be retrieved from the error object
  quo_expr <- rlang::as_quosure(expr, rlang::caller_env(2L))

  rlang::warn(
    message = format_message(warn_message, quo_dots),
    class = c(warn_class, "assert_warning"),
    expr = quo_expr,
    # what is printed in the header?
    print_expr = rlang::as_quosure(print_expr %||% quo_expr, env),
    call = call
  )
}

#' @export
cnd_header.assert_warning <- function(cnd, ...) {
  header <- paste("Assertion failed:", rlang::quo_text(cnd$print_expr))
  rlang::format_error_bullets(rlang::set_names(header, "!"))
}

#' @export
cnd_body.assert_warning <- function(cnd, ...) {
  if (cnd$message != "") cnd$message
}
