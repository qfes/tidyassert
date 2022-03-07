as_name <- function(quo) {
  label <- rlang::as_label(quo)
  if (rlang::quo_is_symbol(quo)) paste0("`", label, "`") else label
}

replace_names <- function(message, quos) {
  patterns <- paste0("\\{\\.(arg|var)\\s*", names(quos), "\\s*\\}")
  names <- vapply_c(quos, as_name)

  reduce(
    seq_along(patterns),
    function(msg, i) gsub(patterns[i], names[i], msg),
    message
  )
}

ensure_bullets <- function(error_message, default = "x") {
  nms <- names(error_message) %||% rep(default, length(error_message))
  rlang::set_names(error_message, replace(nms, is.na(nms) | lengths(nms) == 0, "x"))
}

format_message <- function(error_message, quos) {
  if (is.null(error_message) || error_message == "") {
    return()
  }

  patched_message <- replace_names(error_message, quos)
  env <- rlang::as_environment(lapply(quos, rlang::eval_tidy), rlang::current_env())

  vapply_c(
    ensure_bullets(patched_message),
    cli::format_inline,
    .envir = env
  )
}
