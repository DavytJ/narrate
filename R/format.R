#' Format numbers the way the report prints them
#'
#' Converts a numeric vector to character using the report's decimal mark,
#' big mark and precision. Whole numbers are never rounded. Precision is set
#' either as a fixed number of `decimals` or as `signif` significant digits;
#' when both are given, significant digits are applied first.
#'
#' Defaults come from the options `narrate.decimals`, `narrate.signif`,
#' `narrate.decimal_mark` and `narrate.big_mark`.
#'
#' @param x A numeric vector.
#' @param decimals Number of decimals for non-integers, or `NULL` to leave as is.
#' @param signif Significant digits for non-integers, or `NULL` to leave as is.
#' @param decimal_mark Decimal mark, e.g. `"."` or `","`.
#' @param big_mark Thousands separator, e.g. `","`, `"."` or `""`.
#' @returns A character vector the same length as `x`.
#' @examples
#' format_number(c(1512, 22.46, 0.0019841), signif = 2)
#' format_number(c(6732, 22.46), decimals = 1, decimal_mark = ",", big_mark = ".")
#' @export
format_number <- function(x,
                          decimals = getOption("narrate.decimals"),
                          signif = getOption("narrate.signif"),
                          decimal_mark = getOption("narrate.decimal_mark"),
                          big_mark = getOption("narrate.big_mark")) {
  check_whole(decimals, allow_null = TRUE)
  check_whole(signif, allow_null = TRUE)
  check_string(decimal_mark)
  check_string(big_mark)

  x <- as.numeric(x)
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  if (!any(ok)) {
    return(out)
  }

  v <- x[ok]
  is_whole <- v == round(v)
  if (!is.null(signif)) v[!is_whole] <- base::signif(v[!is_whole], signif)
  if (!is.null(decimals)) v[!is_whole] <- round(v[!is_whole], decimals)

  res <- character(length(v))
  if (any(is_whole)) {
    res[is_whole] <- formatC(v[is_whole], format = "d", big.mark = big_mark,
                             decimal.mark = decimal_mark)
  }
  if (any(!is_whole)) {
    d <- v[!is_whole]
    if (!is.null(decimals)) {
      s <- formatC(d, format = "f", digits = decimals, big.mark = big_mark,
                   decimal.mark = decimal_mark)
    } else if (!is.null(signif)) {
      s <- formatC(d, format = "fg", digits = signif, flag = "#", big.mark = big_mark,
                   decimal.mark = decimal_mark)
      s <- sub(paste0("\\", decimal_mark, "$"), "", s)
    } else {
      n_dec <- nchar(sub("^[^.]*\\.?", "", as.character(d)))
      s <- mapply(
        function(val, n) formatC(val, format = "f", digits = n, big.mark = big_mark,
                                 decimal.mark = decimal_mark),
        d, n_dec, USE.NAMES = FALSE
      )
    }
    res[!is_whole] <- s
  }
  out[ok] <- res
  out
}

# Inverse of format_number() for already formatted strings.
parse_number <- function(s, decimal_mark, big_mark) {
  s <- as.character(s)
  if (nzchar(big_mark)) s <- gsub(big_mark, "", s, fixed = TRUE)
  if (decimal_mark != ".") s <- gsub(decimal_mark, ".", s, fixed = TRUE)
  suppressWarnings(as.numeric(s))
}

#' Format the numeric columns of a table
#'
#' Returns the table with every numeric column converted to character with
#' [format_number()]. This is exactly what the model sees and what
#' [verify_figures()] checks against.
#'
#' @param table A data frame.
#' @inheritParams format_number
#' @returns A data frame with all columns of type character.
#' @export
format_table <- function(table,
                         decimals = getOption("narrate.decimals"),
                         signif = getOption("narrate.signif"),
                         decimal_mark = getOption("narrate.decimal_mark"),
                         big_mark = getOption("narrate.big_mark")) {
  check_tables(table)
  out <- table
  for (j in seq_along(out)) {
    col <- out[[j]]
    out[[j]] <- if (is.numeric(col)) {
      format_number(col, decimals, signif, decimal_mark, big_mark)
    } else {
      as.character(col)
    }
  }
  out
}

#' Render a table as Markdown
#'
#' Converts a data frame to a pipe table, formatting numbers with
#' [format_table()]. Row names other than `1..n` are added as a first column.
#'
#' @param table A data frame.
#' @param title Optional title, written in bold above the table.
#' @param ... Passed on to [format_table()].
#' @returns A character vector, one line per element.
#' @export
table_to_markdown <- function(table, title = NULL, ...) {
  check_string(title, allow_null = TRUE)
  ft <- format_table(table, ...)
  if (!identical(rownames(table), as.character(seq_len(nrow(table))))) {
    ft <- cbind(` ` = rownames(table), ft, stringsAsFactors = FALSE)
  }
  ft[] <- lapply(ft, function(col) {
    s <- as.character(col)
    s[is.na(s)] <- ""
    s
  })
  header <- paste0("| ", paste(names(ft), collapse = " | "), " |")
  rule <- paste0("|", paste(rep("---", ncol(ft)), collapse = "|"), "|")
  rows <- apply(as.matrix(ft), 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  c(if (!is.null(title)) c(paste0("**", title, "**"), ""), header, rule, unname(rows))
}
