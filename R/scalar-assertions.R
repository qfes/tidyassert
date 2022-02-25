#' Assert is string
#'
#' Raises an assertion error when `!rlang::is_string(obj)`.
#' @name assert_is_string
#' @inheritParams assert
#' @param obj <`any`> any value
#'
#' @family scalar-assertions
#' @export
assert_is_string <- function(obj,
                             error_message = "{.arg obj} must be a {.cls string}",
                             error_class = NULL) {
  if (!all_true(rlang::is_string(obj))) {
    signal_error(
      substitute(rlang::is_string(obj)),
      error_message,
      error_class,
      obj = substitute(obj)
    )
  }
}

#' Assert is bool
#'
#' Raises an assertion error when `!rlang::is_bool(obj)`.
#' @name assert_is_bool
#' @inheritParams assert_is_string
#'
#' @family scalar-assertions
#' @export
assert_is_bool <- function(obj,
                           error_message = "{.arg obj} must be a {.cls bool}",
                           error_class = NULL) {
  if (!all_true(rlang::is_bool(obj))) {
    signal_error(
      substitute(rlang::is_bool(obj)),
      error_message,
      error_class,
      obj = substitute(obj)
    )
  }
}

#' Assert is scalar integer
#'
#' Raises an assertion error when `!rlang::is_scalar_integer(obj)`.
#' @name assert_is_scalar_integer
#' @inheritParams assert_is_string
#'
#' @family scalar-assertions
#' @export
assert_is_scalar_integer <- function(obj,
                                     error_message = "{.arg obj} must be a scalar {.cls integer}",
                                     error_class = NULL) {
  if (!all_true(rlang::is_scalar_integer(obj))) {
    signal_error(
      substitute(rlang::is_scalar_integer(obj)),
      error_message,
      error_class,
      obj = substitute(obj)
    )
  }
}

#' Assert is scalar integerish
#'
#' Raises an assertion error when `!rlang::is_scalar_integerish(obj)`.
#' @name assert_is_scalar_integerish
#' @inheritParams assert_is_scalar_integer
#'
#' @family scalar-assertions
#' @export
assert_is_scalar_integerish <- function(obj,
                                        error_message = "{.arg obj} must be a scalar {.cls integerish}",
                                        error_class = NULL) {
  if (!all_true(rlang::is_scalar_integerish(obj))) {
    signal_error(
      substitute(rlang::is_scalar_integerish(obj)),
      error_message,
      error_class,
      obj = substitute(obj)
    )
  }
}

#' Assert is scalar double
#'
#' Raises an assertion error when `!rlang::is_scalar_double(obj)`.
#' @name assert_is_scalar_double
#' @inheritParams assert_is_scalar_integer
#'
#' @family scalar-assertions
#' @export
assert_is_scalar_double <- function(obj,
                                    error_message = "{.arg obj} must be a scalar {.cls double}",
                                    error_class = NULL) {
  if (!all_true(rlang::is_scalar_double(obj))) {
    signal_error(
      substitute(rlang::is_scalar_double(obj)),
      error_message,
      error_class,
      obj = substitute(obj),
    )
  }
}

#' Assert is scalar character
#'
#' Raises an assertion error when `!rlang::is_scalar_character(obj)`.
#' @name assert_is_scalar_character
#' @inheritParams assert_is_scalar_integer
#'
#' @family scalar-assertions
#' @export
assert_is_scalar_character <- function(obj,
                                       error_message = "{.arg obj} must be a scalar {.cls character}",
                                       error_class = NULL) {
  if (!all_true(rlang::is_scalar_character(obj))) {
    signal_error(
      substitute(rlang::is_scalar_character(obj)),
      error_message,
      error_class,
      obj = substitute(obj)
    )
  }
}

#' Assert is scalar logical
#'
#' Raises an assertion error when `!rlang::is_scalar_logical(obj)`.
#' @name assert_is_scalar_logical
#' @inheritParams assert_is_scalar_integer
#'
#' @family scalar-assertions
#' @export
assert_is_scalar_logical <- function(obj,
                                     error_message = "{.arg obj} must be a scalar {.cls logical}",
                                     error_class = NULL) {
  if (!all_true(rlang::is_scalar_logical(obj))) {
    signal_error(
      substitute(rlang::is_scalar_logical(obj)),
      error_message,
      error_class,
      obj = substitute(obj),
    )
  }
}

#' Assert is scalar raw
#'
#' Raises an assertion error when `!rlang::is_scalar_raw(obj)`.
#' @name assert_is_scalar_raw
#' @inheritParams assert_is_scalar_integer
#'
#' @family scalar-assertions
#' @export
assert_is_scalar_raw <- function(obj,
                                 error_message = "{.arg obj} must be a scalar {.cls raw}",
                                 error_class = NULL) {
  if (!all_true(rlang::is_scalar_raw(obj))) {
    signal_error(
      substitute(rlang::is_scalar_raw(obj)),
      error_message,
      error_class,
      obj = substitute(obj)
    )
  }
}

#' Assert is scalar numeric
#'
#' Raises an assertion error when `!is.numeric(obj) || length(obj) != 1L`.
#' @name assert_is_scalar_numeric
#' @inheritParams assert_is_scalar_integer
#'
#' @family scalar-assertions
#' @export
assert_is_scalar_numeric <- function(obj,
                                     error_message = "{.arg obj} must be a scalar {.cls numeric}",
                                     error_class = NULL) {
  if (!all_true(is.numeric(obj) && length(obj) == 1L)) {
    signal_error(
      substitute(is.numeric(obj) && length(obj) == 1L),
      error_message,
      error_class,
      obj = substitute(obj)
    )
  }
}
