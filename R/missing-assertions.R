#' Assert not null
#'
#' Raises an assertion error when `is.null(obj)`
#' @name assert_not_null
#' @param obj <`any`> any object
#' @param error_message <`string`> the error message.
#' Accepts placeholders `{obj}` which will be replaced with the unevaluated expression
#' for `obj`.
#'
#' @family assertions
#' @export
assert_not_null <- function(obj,
                            error_message = "{obj} must be not NULL",
                            error_class = NULL) {
  assert_(
    !is.null(obj),
    quo_expr(substitute(!is.null(obj))),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
}

#' Assert not na
#'
#' Raises an assertion error when `anyNA(obj)`
#' @name assert_not_na
#' @param obj <`any`> any object
#' @param error_message <`string`> the error message.
#' Accepts placeholders `{obj}` which will be replaced with the unevaluated expression
#' for `obj`.
#'
#' @family assertions
#' @export
assert_not_na <- function(obj,
                          error_message = "{obj} must be not contain NA",
                          error_class = NULL) {
  assert_(
    !anyNA(obj),
    quo_expr(substitute(!anyNA(obj))),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
}
