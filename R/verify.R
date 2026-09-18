# Extract every number from a text, in the report's notation.
extract_figures <- function(text, decimal_mark) {
  text <- as.character(text)
  thousands <- if (decimal_mark == ",") "." else ","
  pattern <- if (decimal_mark == ",") {
    "\\d+(\\.\\d{3})*(,\\d+)?"
  } else {
    "\\d+(,\\d{3})*(\\.\\d+)?"
  }

  empty <- data.frame(
    figure = character(), value = numeric(), decimals = integer(),
    context = character(), stringsAsFactors = FALSE
  )
  out <- list()
  for (i in seq_along(text)) {
    m <- gregexpr(pattern, text[[i]], perl = TRUE)[[1]]
    if (m[[1]] == -1) next
    raw <- regmatches(text[[i]], list(m))[[1]]
    start <- as.integer(m)
    len <- attr(m, "match.length")
    decimals <- vapply(raw, function(s) {
      parts <- strsplit(s, decimal_mark, fixed = TRUE)[[1]]
      if (length(parts) > 1) nchar(parts[[2]]) else 0L
    }, integer(1), USE.NAMES = FALSE)
    context <- substr(text[[i]], pmax(1, start - 40), pmin(nchar(text[[i]]), start + len + 40))
    out[[length(out) + 1]] <- data.frame(
      figure = raw,
      value = parse_number(raw, decimal_mark, thousands),
      decimals = decimals,
      context = trimws(context),
      stringsAsFactors = FALSE
    )
  }
  if (length(out) == 0) empty else do.call(rbind, out)
}

# Every figure in the tables, as the model saw them (formatted), with location.
table_universe <- function(tables, decimals, signif, decimal_mark, big_mark) {
  tables <- as_table_list(tables)
  empty <- data.frame(
    value = numeric(), table_figure = character(), table = character(),
    row = integer(), column = character(), stringsAsFactors = FALSE
  )
  out <- list()
  for (name in names(tables)) {
    ft <- format_table(tables[[name]], decimals, signif, decimal_mark, big_mark)
    for (col in names(ft)) {
      v <- parse_number(ft[[col]], decimal_mark, big_mark)
      ok <- !is.na(v)
      if (!any(ok)) next
      out[[length(out) + 1]] <- data.frame(
        value = v[ok], table_figure = ft[[col]][ok], table = name,
        row = which(ok), column = col, stringsAsFactors = FALSE
      )
    }
  }
  if (length(out) == 0) empty else do.call(rbind, out)
}

#' Verify the figures in a text against one or more tables
#'
#' Extracts every number from `text` and checks that it exists in `tables`,
#' formatted exactly as the model saw them (see [format_table()]). Figures
#' that are not found are diagnosed as:
#'
#' * `"rounded"`: a table figure rounded to the decimals used in the text;
#' * `"proportion"`: a table proportion expressed as a percentage;
#' * `"unsourced"`: no relationship with any table figure was found.
#'
#' @param text A character vector (lines) or a single string.
#' @param tables A data frame, or a named list of data frames.
#' @param exclude Numeric values that are never flagged even if absent from
#'   the tables (years, table numbers, the 95 in "95% CI").
#' @inheritParams format_number
#' @returns A data frame of class `narrate_verification` with one row per
#'   figure in the text: `figure`, `value`, `found`, `table`, `row`, `column`,
#'   `status` (`"ok"`, `"rounded"`, `"proportion"`, `"unsourced"`,
#'   `"excluded"`), `table_figure` and `context`. The `summary` attribute holds
#'   the counts, including the number of `[MISSING DATA]` markers.
#' @examples
#' tab <- data.frame(site = c("A", "B"), n = c(1512, 1898), pct = c(22.46, 28.19))
#' verify_figures("A had 1,512 procedures (22.46%) and B 1898 (28.2%). Total: 3410.", tab)
#' @export
verify_figures <- function(text,
                           tables,
                           exclude = NULL,
                           decimals = getOption("narrate.decimals"),
                           signif = getOption("narrate.signif"),
                           decimal_mark = getOption("narrate.decimal_mark"),
                           big_mark = getOption("narrate.big_mark")) {
  check_tables(tables)
  text <- strsplit(paste(as.character(text), collapse = "\n"), "\n", fixed = TRUE)[[1]]

  figures <- extract_figures(text, decimal_mark)
  universe <- table_universe(tables, decimals, signif, decimal_mark, big_mark)

  n <- nrow(figures)
  figures$found <- logical(n)
  figures$table <- rep(NA_character_, n)
  figures$row <- rep(NA_integer_, n)
  figures$column <- rep(NA_character_, n)
  figures$table_figure <- rep(NA_character_, n)
  figures$status <- rep("unsourced", n)

  for (i in seq_len(n)) {
    v <- figures$value[[i]]
    d <- figures$decimals[[i]]
    hit <- NULL
    status <- "unsourced"

    exact <- which(abs(universe$value - v) < 1e-9)
    rounded <- which(abs(round(universe$value, d) - v) < 1e-9 & abs(universe$value - v) >= 1e-9)
    prop <- which(abs(round(universe$value * 100, d) - v) < 1e-9)

    if (length(exact) > 0) {
      hit <- exact[[1]]
      status <- "ok"
      figures$found[[i]] <- TRUE
    } else if (!is.null(exclude) && any(abs(exclude - v) < 1e-9)) {
      status <- "excluded"
    } else if (length(rounded) > 0) {
      hit <- rounded[[1]]
      status <- "rounded"
    } else if (length(prop) > 0) {
      hit <- prop[[1]]
      status <- "proportion"
    }

    figures$status[[i]] <- status
    if (!is.null(hit)) {
      figures$table[[i]] <- universe$table[[hit]]
      figures$row[[i]] <- universe$row[[hit]]
      figures$column[[i]] <- universe$column[[hit]]
      figures$table_figure[[i]] <- universe$table_figure[[hit]]
    }
  }

  figures <- figures[, c("figure", "value", "found", "table", "row", "column",
                         "status", "table_figure", "context")]
  missing <- sum(lengths(regmatches(text, gregexpr("[MISSING DATA]", text, fixed = TRUE))))
  flagged <- c("rounded", "proportion", "unsourced")
  summary <- list(
    figures = n,
    found = sum(figures$status == "ok"),
    excluded = sum(figures$status == "excluded"),
    flagged = sum(figures$status %in% flagged),
    rounded = sum(figures$status == "rounded"),
    proportion = sum(figures$status == "proportion"),
    unsourced = sum(figures$status == "unsourced"),
    missing = missing
  )
  structure(figures, class = c("narrate_verification", "data.frame"), summary = summary)
}

#' @export
print.narrate_verification <- function(x, ...) {
  s <- attr(x, "summary")
  cli::cli_text(
    "{s$figures} figure{?s}: {s$found} found, {s$excluded} excluded, ",
    "{s$flagged} flagged (rounded {s$rounded}, proportion {s$proportion}, unsourced {s$unsourced}); ",
    "{s$missing} [MISSING DATA] marker{?s}."
  )
  flagged <- x[x$status %in% c("rounded", "proportion", "unsourced"),
               c("figure", "status", "table_figure", "table", "column", "context")]
  if (nrow(flagged) > 0) {
    flagged$context <- substr(flagged$context, 1, 60)
    print.data.frame(flagged, row.names = FALSE)
  }
  invisible(x)
}
