#' Reproduce a periodic report from the previous edition and new tables
#'
#' Generates the full text of a report using the previous edition as the
#' reference for structure and style, and the new tables as the only source
#' of figures. The result is verified with [verify_figures()].
#'
#' @param previous The previous report as a character vector (lines) or a path
#'   to a Markdown or text file.
#' @param tables A named list of data frames; the names are used as table
#'   titles in the prompt.
#' @param period Label for the current period, e.g. `"January to June 2026"`.
#' @inheritParams narrate
#' @returns A `narrate_text` object (see [narrate()]).
#' @export
narrate_report <- function(previous,
                           tables,
                           period = NULL,
                           instructions = NULL,
                           exclude = NULL,
                           generate = getOption("narrate.generate"),
                           cache = TRUE,
                           chat = NULL) {
  check_tables(tables)
  check_string(period, allow_null = TRUE)
  check_string(instructions, allow_null = TRUE)
  if (is_string(previous) && file.exists(previous)) {
    previous <- readLines(previous, encoding = "UTF-8", warn = FALSE)
  }

  tables <- as_table_list(tables, name = "Table")
  markdown <- unlist(lapply(names(tables), function(name) {
    c(table_to_markdown(tables[[name]], title = name), "")
  }))
  prompt <- prompt_template(
    "report",
    previous = previous,
    tables = markdown,
    period = period %||% "the current period",
    instructions = instructions %||% "",
    language = getOption("narrate.language")
  )
  generate_text(prompt, tables, type = "report", generate = generate, cache = cache,
                chat = chat, exclude = exclude)
}
