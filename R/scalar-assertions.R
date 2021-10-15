#' Assert is string
#'
#' Raises an assertion error when `!rlang::is_string(obj)`.
#' @name assert_is_string
#' @inheritParams assert
#' @param obj <`any`> any value
#' @param error_message <`string`> the error message.
#' Accepts placeholder `{obj}` which will be replaced with the unevaluated expression for `obj`.
#'
#' @family scalar-assertions
#' @export
assert_is_string <- function(obj,
                             error_message = "{obj} must be a <string>",
                             error_class = NULL) {
  assert_(
    rlang::is_string(obj),
    quo_expr(substitute(rlang::is_string(obj))),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
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
                           error_message = "{obj} must be a <bool>",
                           error_class = NULL) {
  assert_(
    rlang::is_bool(obj),
    quo_expr(substitute(rlang::is_bool(obj))),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
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
                                     error_message = "{obj} must be a scalar <integer>",
                                     error_class = NULL) {
  assert_(
    rlang::is_scalar_integer(obj),
    quo_expr(substitute(rlang::is_scalar_integer(obj))),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
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
                                        error_message = "{obj} must be a scalar <integerish>",
                                        error_class = NULL) {
  assert_(
    rlang::is_scalar_integerish(obj),
    quo_expr(substitute(rlang::is_scalar_integerish(obj))),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
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
                                    error_message = "{obj} must be a scalar <double>",
                                    error_class = NULL) {
  assert_(
    rlang::is_scalar_double(obj),
    quo_expr(substitute(rlang::is_scalar_double(obj))),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
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
                                       error_message = "{obj} must be a scalar <character>",
                                       error_class = NULL) {
  assert_(
    rlang::is_scalar_character(obj),
    quo_expr(substitute(rlang::is_scalar_character(obj))),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
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
                                     error_message = "{obj} must be a scalar <logical>",
                                     error_class = NULL) {
  assert_(
    rlang::is_scalar_logical(obj),
    quo_expr(substitute(rlang::is_scalar_logical(obj))),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
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
                                 error_message = "{obj} must be a scalar <raw>",
                                 error_class = NULL) {
  assert_(
    rlang::is_scalar_raw(obj),
    quo_expr(substitute(rlang::is_scalar_raw(obj))),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
}
