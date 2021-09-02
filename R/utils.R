# format bullets
fmt_bullets <- function(error_message) {
  if (is.null(error_message) || error_message == "") {
    return()
  }

  if (!rlang::is_named(error_message)) {
    names(error_message) <- rep("x", length(error_message))
  }

  rlang::format_error_bullets(error_message)
}

# wrap quoted expression in quosure
get_qexpr <- function(expr, env = parent.frame(2L)) {
  rlang::enquo(expr) |>
    rlang::quo_set_env(env)
}
