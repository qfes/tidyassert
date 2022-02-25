#' Assert typeof
#'
#' Raises an assertion error when `typeof(obj) != type`.
#' @name assert_is_typeof
#' @inheritParams assert
#' @param obj <`any`> any value
#' @param type <`string`> the expected type
#'
#' @family type-assertions
#' @export
assert_is_typeof <- function(obj, type,
                             error_message = "{.arg obj} must be of type {.cls {type}}",
                             error_class = NULL) {
  if (!all_true(typeof(obj) == type)) {
    signal_error(
      substitute(typeof(obj) == type),
      error_message,
      error_class,
      obj = substitute(obj),
      type = substitute(type)
    )
  }
}

#' @noRd
#' @export
assert_typeof <- function(...) {
  .Deprecated("assert_is_typeof")
  assert_is_typeof(...)
}

#' Assert inherits
#'
#' Raises an assertion error when `!inherits(obj, class)`.
#' @name assert_inherits
#' @inheritParams assert_is_typeof
#' @param class <`string` | `character`> the expected class(es)
#'
#' @family type-assertions
#' @export
assert_inherits <- function(obj, class,
                            error_message = "{.arg obj} must inherit {.cls {class}}",
                            error_class = NULL) {
  if (!all_true(inherits(obj, class))) {
    signal_error(
      substitute(inherits(obj, class)),
      error_message,
      error_class,
      obj = substitute(obj),
      class = substitute(class)
    )
  }
}

#' Assert is integer
#'
#' Raises an assertion error when `!rlang::is_integer(obj)`.
#' @name assert_is_integer
#' @inheritParams assert_is_typeof
#'
#' @family type-assertions
#' @export
assert_is_integer <- function(obj,
                              error_message = "{.arg obj} must be an {.cls integer} vector",
                              error_class = NULL) {
  if (!all_true(rlang::is_integer(obj))) {
    signal_error(
      substitute(rlang::is_integer(obj)),
      error_message,
      error_class,
      obj = substitute(obj)
    )
  }
}

#' Assert is integerish
#'
#' Raises an assertion error when `!rlang::is_integerish(obj)`.
#' @name assert_is_integerish
#' @inheritParams assert_is_integer
#'
#' @family type-assertions
#' @export
assert_is_integerish <- function(obj,
                                 error_message = "{.arg obj} must be an {.cls integerish} vector",
                                 error_class = NULL) {
  if (!all_true(rlang::is_integerish(obj))) {
    signal_error(
      substitute(rlang::is_integerish(obj)),
      error_message,
      error_class,
      obj = substitute(obj)
    )
  }
}

#' Assert is double
#'
#' Raises an assertion error when `!rlang::is_double(obj)`.
#' @name assert_is_double
#' @inheritParams assert_is_integer
#'
#' @family type-assertions
#' @export
assert_is_double <- function(obj,
                             error_message = "{.arg obj} must be a {.cls double} vector",
                             error_class = NULL) {
  if (!all_true(rlang::is_double(obj))) {
    signal_error(
      substitute(rlang::is_double(obj)),
      error_message,
      error_class,
      obj = substitute(obj),
    )
  }
}

#' Assert is character
#'
#' Raises an assertion error when `!rlang::is_character(obj)`.
#' @name assert_is_character
#' @inheritParams assert_is_integer
#'
#' @family type-assertions
#' @export
assert_is_character <- function(obj,
                                error_message = "{.arg obj} must be a {.cls character} vector",
                                error_class = NULL) {
  if (!all_true(rlang::is_character(obj))) {
    signal_error(
      substitute(rlang::is_character(obj)),
      error_message,
      error_class,
      obj = substitute(obj),
    )
  }
}

#' Assert is logical
#'
#' Raises an assertion error when `!rlang::is_logical(obj)`.
#' @name assert_is_logical
#' @inheritParams assert_is_integer
#'
#' @family type-assertions
#' @export
assert_is_logical <- function(obj,
                              error_message = "{.arg obj} must be a {.cls logical} vector",
                              error_class = NULL) {
  if (!all_true(rlang::is_logical(obj))) {
    signal_error(
      substitute(rlang::is_logical(obj)),
      error_message,
      error_class,
      obj = substitute(obj),
    )
  }
}

#' Assert is raw
#'
#' Raises an assertion error when `!rlang::is_raw(obj)`.
#' @name assert_is_raw
#' @inheritParams assert_is_integer
#'
#' @family type-assertions
#' @export
assert_is_raw <- function(obj,
                          error_message = "{.arg obj} must be a {.cls raw} vector",
                          error_class = NULL) {
  if (!all_true(rlang::is_raw(obj))) {
    signal_error(
      substitute(rlang::is_raw(obj)),
      error_message,
      error_class,
      obj = substitute(obj),
    )
  }
}

#' Assert is numeric
#'
#' Raises an assertion error when `!is.numeric(obj)`.
#' @name assert_is_numeric
#' @inheritParams assert_is_integer
#'
#' @family type-assertions
#' @export
assert_is_numeric <- function(obj,
                              error_message = "{.arg obj} must be a {.cls numeric} vector",
                              error_class = NULL) {
  if (!all_true(is.numeric(obj))) {
    signal_error(
      substitute(is.numeric(obj)),
      error_message,
      error_class,
      obj = substitute(obj)
    )
  }
}
