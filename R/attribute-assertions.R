#' Assert is named
#'
#' Raises an assertion error when `!rlang::is_named(obj)`.
#' @name assert_is_named
#' @inheritParams assert
#' @param obj <`any`> any value
#'
#' @family attribute-assertions
#' @export
assert_is_named <- function(obj,
                            error_message = "{.arg obj} must be named",
                            error_class = NULL) {
  if (!all_true(rlang::is_named(obj))) {
    signal_error(
      substitute(rlang::is_named(obj)),
      error_message,
      error_class,
      obj = substitute(obj),
      error_class
    )
  }
}

#' Assert has names
#'
#' Raises an assertion error when `!rlang::has_name(obj, names)`.
#' @name assert_has_names
#' @inheritParams assert_is_named
#' @param names <`character`> a vector of names.
#'
#' @family attribute-assertions
#' @export
assert_has_names <- function(obj,
                             names,
                             error_message = "{.arg obj} must have names {.arg names}",
                             error_class = NULL) {
  if (!all_true(rlang::has_name(obj, names))) {
    signal_error(
      substitute(rlang::has_name(obj, names)),
      error_message,
      error_class,
      obj = substitute(obj),
      names = substitute(names)
    )
  }
}
