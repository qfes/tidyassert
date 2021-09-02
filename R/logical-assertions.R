#' Assert equal
#'
#' Raises an assertion error when `!all(a == b)`.
#' @name assert_equal
#' @param a <`any`> any value
#' @param b <`any`> any value
#' @param error_message <`string`> the error message.
#' Accepts placeholders `{a}` and `{b}`, which will be replaced with the unevaluated expressions
#' for `a` and `b`.
#'
#' @family assertions
#' @export
assert_equal <- function(a, b,
                         error_message = "{a} must equal {b}",
                         error_class = NULL) {
  assert_(
    a == b,
    get_qexpr(!!substitute(a == b)),
    fmt_message(error_message, a = substitute(a), b = substitute(b)),
    error_class
  )
}

#' Assert not equal
#'
#' Raises an assertion error when `!all(a != b)`
#' @name assert_not_equal
#' @inherit assert_equal
#'
#' @family assertions
#' @export
assert_not_equal <- function(a, b,
                             error_message = "{a} must not equal {b}",
                             error_class = NULL) {
  assert_(
    a != b,
    get_qexpr(!!substitute(a != b)),
    fmt_message(error_message, a = substitute(a), b = substitute(b)),
    error_class
  )
}

#' Assert less
#'
#' Raises an assertion error when `!all(a < b)`
#' @name assert_less
#' @inherit assert_equal
#'
#' @family assertions
#' @export
assert_less <- function(a, b,
                        error_message = "{a} must be less than {b}",
                        error_class = NULL) {
  assert_(
    a < b,
    get_qexpr(!!substitute(a < b)),
    fmt_message(error_message, a = substitute(a), b = substitute(b)),
    error_class
  )
}

#' Assert less equal
#'
#' Raises an assertion error when `!all(a <= b)`
#' @name assert_less_equal
#' @inherit assert_equal
#'
#' @family assertions
#' @export
assert_less_equal <- function(a, b,
                              error_message = "{a} must be less than or equal {b}",
                              error_class = NULL) {
  assert_(
    a <= b,
    get_qexpr(!!substitute(a <= b)),
    fmt_message(error_message, a = substitute(a), b = substitute(b)),
    error_class
  )
}

#' Assert greater
#'
#' Raises an assertion error when `!all(a > b)`
#' @name assert_greater
#' @inherit assert_equal
#'
#' @family assertions
#' @export
assert_greater <- function(a, b,
                           error_message = "{a} must be greater than {b}",
                           error_class = NULL) {
  assert_(
    a > b,
    get_qexpr(!!substitute(a > b)),
    fmt_message(error_message, a = substitute(a), b = substitute(b)),
    error_class
  )
}

#' Assert greater equal
#'
#' Raises an assertion error when `!all(a >= b)`
#' @name assert_greater_equal
#' @inherit assert_equal
#'
#' @family assertions
#' @export
assert_greater_equal <- function(a, b,
                                 error_message = "{a} must be greater than or equal {b}",
                                 error_class = NULL) {
  assert_(
    a >= b,
    get_qexpr(!!substitute(a >= b)),
    fmt_message(error_message, a = substitute(a), b = substitute(b)),
    error_class
  )
}

#' Assert range
#'
#' Raises an assertion error when `!all(a >= min & a <= max)`
#' @name assert_range
#' @inherit assert_equal
#' @param obj <`any`>
#' @param min <`any`> the minimum value
#' @param max <`any`> the maximum value
#' @param error_message <`string`> the error message.
#' Accepts placeholders `{a}` and `{b}`, which will be replaced with the unevaluated expressions
#' for `a` and `b`.
#'
#' @family assertions
#' @export
assert_range <- function(obj, min, max,
                         error_message = "{obj} must be within [{min}, {max}]",
                         error_class = NULL) {
  assert_less(min, max)
  assert_(
    obj >= min & obj <= max,
    get_qexpr(!!substitute(obj >= min & obj <= max)),
    fmt_message(
      error_message,
      obj = substitute(obj),
      min = substitute(min),
      max = substitute(max)
    ),
    error_class
  )
}
