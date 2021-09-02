# replace placeholders with values from named dots
fmt_message <- function(templates, ...) {
  sub_value <- function(template, name, value = args[[name]]) {
    placeholder <- paste0("{", name, "}")
    gsub(placeholder, fmt_value(value), template, fixed = TRUE)
  }

  sub_ivalue <- function(template, name, value = args[[name]]) {
    placeholder <- paste0("{!!", name, "}")
    gsub(placeholder, fmt_value(rlang::eval_tidy(value)), template, fixed = TRUE)
  }

  rpl_placeholders <- function(template, args) {
    arg_names <- names(args)
    template <- reduce(arg_names, sub_value, template)
    reduce(arg_names, sub_ivalue, template)
  }

  args <- rlang::dots_list(..., .named = TRUE)
  vapply_c(templates, rpl_placeholders, args = args)
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
