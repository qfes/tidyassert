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

# format a value
fmt_value <- function(value) {
  if (inherits(value, "AsIs")) {
    return(rlang::as_string(value))
  }

  if (rlang::is_quosure(value) && rlang::quo_is_symbol(value) || rlang::is_symbol(value)) {
    return(paste0("`", rlang::as_label(value), "`"))
  }

  rlang::as_label(value)
}

# replace placeholders with values from named dots
fmt_message <- function(templates, ...) {
  replace <- function(template, x) {
    placeholder <- paste0("{", x, "}")
    gsub(placeholder, fmt_value(dots[[x]]), template, fixed = TRUE)
  }

  fmt_template <- function(template) Reduce(replace, names(dots), template)

  dots <- rlang::dots_list(..., .named = TRUE)
  lapply(templates, fmt_template) |>
    unlist()
}
