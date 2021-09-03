#' Assert typeof
#'
#' Raises an assertion error when `typeof(obj) != type`.
#' @name assert_typeof
#' @param obj <`any`> any value
#' @param type <`string`> the expected type
#' @param error_message <`string`> the error message.
#' Accepts placeholders `{obj}` and `{type}`, which will be replaced with the
#' unevaluated expressions for `obj` and `type`.
#'
#' @family assertions
#' @export
assert_typeof <- function(obj, type,
                          error_message = c(x = "{obj} must be of type {!!type}"),
                          error_class = NULL) {
  fmt_type <- function() I(paste0("<", type, ">"))

  assert_(
    typeof(obj) == type,
    quo_expr(substitute(typeof(obj) == type)),
    fmt_message(error_message, obj = substitute(obj), type = fmt_type()),
    error_class
  )
}

#' Assert inherits
#'
#' Raises an assertion error when `!inherits(obj, class)`.
#' @name assert_inherits
#' @param obj <`any`> any value
#' @param class <`string` | `character`> the expected class(es)
#' @param error_message <`string`> the error message.
#' Accepts placeholders `{obj}` and `{class}`, which will be replaced with the
#' unevaluated expressions for `obj` and `class`.
#'
#' @family assertions
#' @export
assert_inherits <- function(obj, class,
                            error_message = c(x = "{obj} must inherit {!!class}"),
                            error_class = NULL) {
  fmt_class <- function() {
    class_str <- paste0("<", paste(class, collapse = " | "), ">")
    I(class_str)
  }

  assert_(
    inherits(obj, class),
    quo_expr(substitute(inherits(obj, class))),
    fmt_message(error_message, obj = quo_expr(substitute(obj)), class = fmt_class()),
    error_class
  )
}
