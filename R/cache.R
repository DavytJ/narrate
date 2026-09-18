log_path <- function() file.path(getOption("narrate.cache_dir"), "log.csv")

log_call <- function(type, result) {
  verification <- attr(result, "verification")
  s <- attr(verification, "summary")
  tokens <- attr(result, "tokens")
  row <- data.frame(
    timestamp = attr(result, "timestamp"),
    type = type,
    model = attr(result, "model"),
    tokens_in = unname(tokens[["input"]]),
    tokens_out = unname(tokens[["output"]]),
    seconds = attr(result, "seconds"),
    figures = s$figures,
    flagged = s$flagged,
    rounded = s$rounded,
    proportion = s$proportion,
    unsourced = s$unsourced,
    missing = s$missing,
    key = attr(result, "key"),
    stringsAsFactors = FALSE
  )
  path <- log_path()
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  utils::write.table(row, path, sep = ",", row.names = FALSE,
                     col.names = !file.exists(path), append = file.exists(path))
  invisible(row)
}

#' Log of model calls
#'
#' One row per call: timestamp, type (`"narrate"` or `"report"`), model,
#' tokens, latency, and the number of verified and flagged figures. Use it to
#' compare models and estimate cost.
#'
#' @returns A data frame, or `NULL` if nothing has been logged yet.
#' @export
narrate_log <- function() {
  path <- log_path()
  if (!file.exists(path)) {
    return(NULL)
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}

#' Clear the cache of generated texts
#'
#' @param log If `TRUE`, also delete the call log.
#' @returns The number of cached texts removed, invisibly.
#' @export
clear_cache <- function(log = FALSE) {
  check_bool(log)
  dir <- getOption("narrate.cache_dir")
  if (!dir.exists(dir)) {
    return(invisible(0L))
  }
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  unlink(files)
  if (log) unlink(log_path())
  invisible(length(files))
}
