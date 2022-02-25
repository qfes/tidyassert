#' Assert equal
#'
#' Raises an assertion error when `!all(a == b)`.
#' @name assert_equal
#' @inheritParams assert
#' @param a <`any`> any value
#' @param b <`any`> any value
#'
#' @family logical-assertions
#' @export
assert_equal <- function(a, b,
                         error_message = "{.arg a} must equal {.arg b}",
                         error_class = NULL) {
  if (!all_true(a == b)) {
    signal_error(
      substitute(a == b),
      error_message,
      error_class,
      a = substitute(a),
      b = substitute(b)
    )
  }
}

#' Assert not equal
#'
#' Raises an assertion error when `!all(a != b)`
#' @name assert_not_equal
#' @inherit assert_equal
#'
#' @family logical-assertions
#' @export
assert_not_equal <- function(a, b,
                             error_message = "{.arg a} must not equal {.arg b}",
                             error_class = NULL) {
  if (!all_true(a != b)) {
    signal_error(
      substitute(a != b),
      error_message,
      error_class,
      a = substitute(a),
      b = substitute(b)
    )
  }
}

#' Assert less
#'
#' Raises an assertion error when `!all(a < b)`
#' @name assert_less
#' @inherit assert_equal
#'
#' @family logical-assertions
#' @export
assert_less <- function(a, b,
                        error_message = "{.arg a} must be less than {.arg b}",
                        error_class = NULL) {
  if (!all_true(a < b)) {
    signal_error(
      substitute(a < b),
      error_message,
      error_class,
      a = substitute(a),
      b = substitute(b)
    )
  }
}

#' Assert less equal
#'
#' Raises an assertion error when `!all(a <= b)`
#' @name assert_less_equal
#' @inherit assert_equal
#'
#' @family logical-assertions
#' @export
assert_less_equal <- function(a, b,
                              error_message = "{.arg a} must be less than or equal {.arg b}",
                              error_class = NULL) {
  if (!all_true(a <= b)) {
    signal_error(
      substitute(a <= b),
      error_message,
      error_class,
      a = substitute(a),
      b = substitute(b)
    )
  }
}

#' Assert greater
#'
#' Raises an assertion error when `!all(a > b)`
#' @name assert_greater
#' @inherit assert_equal
#'
#' @family logical-assertions
#' @export
assert_greater <- function(a, b,
                           error_message = "{.arg a} must be greater than {.arg b}",
                           error_class = NULL) {
  if (!all_true(a > b)) {
    signal_error(
      substitute(a > b),
      error_message,
      error_class,
      a = substitute(a),
      b = substitute(b)
    )
  }
}

#' Assert greater equal
#'
#' Raises an assertion error when `!all(a >= b)`
#' @name assert_greater_equal
#' @inherit assert_equal
#'
#' @family logical-assertions
#' @export
assert_greater_equal <- function(a, b,
                                 error_message = "{.arg a} must be greater than or equal {.arg b}",
                                 error_class = NULL) {
  if (!all_true(a >= b)) {
    signal_error(
      substitute(a >= b),
      error_message,
      error_class,
      a = substitute(a),
      b = substitute(b)
    )
  }
}

#' Assert range
#'
#' Raises an assertion error when `!all(a >= min & a <= max)`
#' @name assert_range
#' @inherit assert_equal
#' @param obj <`any`> any object
#' @param min <`any`> the minimum value
#' @param max <`any`> the maximum value
#'
#' @family logical-assertions
#' @export
assert_range <- function(obj, min, max,
                         error_message = "{.arg obj} must be within [{.arg min}, {.arg max}]",
                         error_class = NULL) {
  assert_less_equal(min, max)
  if (!all_true(obj >= min & obj <= max)) {
    signal_error(
      substitute(obj >= min & obj <= max),
      error_message,
      error_class,
      obj = substitute(obj),
      min = substitute(min),
      max = substitute(max)
    )
  }
}
