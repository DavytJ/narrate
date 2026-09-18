#' @keywords internal
#' @import rlang
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  op <- options()
  defaults <- list(
    narrate.chat          = NULL,
    narrate.language      = "English",
    narrate.decimals      = NULL,
    narrate.signif        = NULL,
    narrate.decimal_mark  = ".",
    narrate.big_mark      = ",",
    narrate.generate      = TRUE,
    narrate.cache_dir     = ".narrate_cache",
    narrate.system_prompt = NULL,
    narrate.prompt_dir    = NULL,
    narrate.highlight     = FALSE
  )
  toset <- !(names(defaults) %in% names(op))
  if (any(toset)) options(defaults[toset])
  invisible()
}
