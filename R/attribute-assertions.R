#' Assert is named
#'
#' Raises an assertion error when `!rlang::is_named(obj)`.
#' @name assert_is_named
#' @inheritParams assert
#' @param obj <`any`> any value
#' @param error_message <`string`> the error message.
#' Accepts placeholder `{obj}` which will be replaced with the unevaluated expression for `obj`.
#'
#' @family attribute-assertions
#' @export
assert_is_named <- function(obj,
                            error_message = "{obj} must be named",
                            error_class = NULL) {
  assert_(
    rlang::is_named(obj),
    quo_expr(substitute(rlang::is_named(obj))),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
}

#' Assert has names
#'
#' Raises an assertion error when `!rlang::has_name(obj, names)`.
#' @name assert_has_names
#' @inheritParams assert_is_named
#' @param names <`character`> a vector of names.
#' @param error_message <`string`> the error message.
#' Accepts placeholders `{obj}` and `{names}`, which will be replaced with the
#' unevaluated expressions for `obj` and `names`.
#'
#' @family attribute-assertions
#' @export
assert_has_names <- function(obj,
                             names,
                             error_message = "{obj} must have names {!!names}",
                             error_class = NULL) {
  fmt_names <- function() {
    nms <- dQuote(names, "\"")
    names_str <- paste0("[", paste(nms, collapse = ", "), "]")
    I(names_str)
  }

  assert_(
    rlang::has_name(obj, names),
    quo_expr(substitute(rlang::has_name(obj, names))),
    fmt_message(error_message, obj = quo_expr(substitute(obj)), names = fmt_names()),
    error_class
  )
}
