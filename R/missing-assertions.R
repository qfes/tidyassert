#' Assert not null
#'
#' Raises an assertion error when `is.null(obj)`
#' @name assert_not_null
#' @inheritParams assert
#' @param obj <`any`> any object
#'
#' @family assertions
#' @export
assert_not_null <- function(obj,
                            error_message = "{.arg obj} must be not {.code NULL}",
                            error_class = NULL) {
  if (!all_true(!is.null(obj))) {
    signal_error(
      substitute(!is.null(obj)),
      error_message,
      error_class,
      obj = substitute(obj)
    )
  }
}

#' Assert not na
#'
#' Raises an assertion error when `anyNA(obj)`
#' @inheritParams assert_not_null
#' @name assert_not_na
#'
#' @family assertions
#' @export
assert_not_na <- function(obj,
                          error_message = "{.arg obj} must be not contain {.code NA}",
                          error_class = NULL) {
  if (!all_true(!anyNA(obj))) {
    signal_error(
      substitute(!anyNA(obj)),
      error_message,
      error_class,
      obj = substitute(obj),
    )
  }
}
