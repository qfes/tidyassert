#' Assert is string
#'
#' Raises an assertion error when `!rlang::is_string(obj)`.
#' @name assert_is_string
#' @param obj <`any`> any value
#' @param error_message <`string`> the error message.
#' Accepts placeholder `{obj}` which will be replaced with the unevaluated expression for `obj`.
#'
#' @family assertions
#' @export
assert_is_string <- function(obj,
                             error_message = "{obj} must be a <string>",
                             error_class = NULL) {
  assert_(
    rlang::is_string(obj),
    quo_expr(rlang::is_string(obj)),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
}
