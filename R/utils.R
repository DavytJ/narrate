# Lightweight argument checks in the spirit of rlang's standalone-types-check.

check_bool <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (is_bool(x)) return(invisible(NULL))
  cli::cli_abort("{.arg {arg}} must be `TRUE` or `FALSE`, not {.obj_type_friendly {x}}.", call = call)
}

check_string <- function(x, allow_null = FALSE, arg = caller_arg(x), call = caller_env()) {
  if (is.null(x) && allow_null) return(invisible(NULL))
  if (is_string(x)) return(invisible(NULL))
  cli::cli_abort("{.arg {arg}} must be a single string, not {.obj_type_friendly {x}}.", call = call)
}

check_whole <- function(x, allow_null = FALSE, arg = caller_arg(x), call = caller_env()) {
  if (is.null(x) && allow_null) return(invisible(NULL))
  if (is_integerish(x, n = 1) && !is.na(x)) return(invisible(NULL))
  cli::cli_abort("{.arg {arg}} must be a whole number or `NULL`, not {.obj_type_friendly {x}}.", call = call)
}

check_tables <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (is.data.frame(x)) return(invisible(NULL))
  if (is.list(x) && length(x) > 0 && all(vapply(x, is.data.frame, logical(1)))) return(invisible(NULL))
  cli::cli_abort("{.arg {arg}} must be a data frame or a list of data frames, not {.obj_type_friendly {x}}.", call = call)
}

as_table_list <- function(tables, name = "table") {
  if (is.data.frame(tables)) tables <- set_names(list(tables), name)
  if (is.null(names(tables))) names(tables) <- paste0(name, "_", seq_along(tables))
  tables
}
