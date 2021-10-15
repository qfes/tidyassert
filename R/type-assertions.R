#' Assert typeof
#'
#' Raises an assertion error when `typeof(obj) != type`.
#' @name assert_is_typeof
#' @inheritParams assert
#' @param obj <`any`> any value
#' @param type <`string`> the expected type
#' @param error_message <`string`> the error message.
#' Accepts placeholders `{obj}` and `{type}`, which will be replaced with the
#' unevaluated expressions for `obj` and `type`.
#'
#' @family type-assertions
#' @export
assert_is_typeof <- function(obj, type,
                             error_message = "{obj} must be of type {!!type}",
                             error_class = NULL) {
  fmt_type <- function() I(paste0("<", type, ">"))

  assert_(
    typeof(obj) == type,
    quo_expr(substitute(typeof(obj) == type)),
    fmt_message(error_message, obj = quo_expr(substitute(obj)), type = fmt_type()),
    error_class
  )
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
#' @param error_message <`string`> the error message.
#' Accepts placeholders `{obj}` and `{class}`, which will be replaced with the
#' unevaluated expressions for `obj` and `class`.
#'
#' @family type-assertions
#' @export
assert_inherits <- function(obj, class,
                            error_message = "{obj} must inherit {!!class}",
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

#' Assert is integer
#'
#' Raises an assertion error when `!rlang::is_integer(obj)`.
#' @name assert_is_integer
#' @inheritParams assert_is_typeof
#' @param error_message <`string`> the error message.
#' Accepts placeholder `{obj}` which will be replaced with the unevaluated expression for `obj`.
#'
#' @family type-assertions
#' @export
assert_is_integer <- function(obj,
                              error_message = "{obj} must be an <integer> vector",
                              error_class = NULL) {
  assert_(
    rlang::is_integer(obj),
    quo_expr(rlang::is_integer(obj)),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
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
                                 error_message = "{obj} must be an <integerish> vector",
                                 error_class = NULL) {
  assert_(
    rlang::is_integerish(obj),
    quo_expr(rlang::is_integerish(obj)),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
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
                             error_message = "{obj} must be a <double> vector",
                             error_class = NULL) {
  assert_(
    rlang::is_double(obj),
    quo_expr(rlang::is_double(obj)),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
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
                                error_message = "{obj} must be a <character> vector",
                                error_class = NULL) {
  assert_(
    rlang::is_character(obj),
    quo_expr(rlang::is_character(obj)),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
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
                              error_message = "{obj} must be a <logical> vector",
                              error_class = NULL) {
  assert_(
    rlang::is_logical(obj),
    quo_expr(rlang::is_logical(obj)),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
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
                          error_message = "{obj} must be a <raw> vector",
                          error_class = NULL) {
  assert_(
    rlang::is_raw(obj),
    quo_expr(rlang::is_raw(obj)),
    fmt_message(error_message, obj = quo_expr(substitute(obj))),
    error_class
  )
}
