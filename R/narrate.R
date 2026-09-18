#' Narrate a table: the paragraph that accompanies it in a report
#'
#' Uses a language model to write the interpretive paragraph for a table, in
#' the style of an `example` paragraph, and verifies that every figure in the
#' text exists in the table. Designed for inline use in R Markdown or Quarto,
#' right below the chunk that produces the table: `` `r narrate(tab1)` ``.
#'
#' Results are cached in `narrate.cache_dir` under a key derived from the
#' table, the example, the instructions and the model, so re-rendering a
#' document does not call the API. With `options(narrate.generate = FALSE)`
#' the API is never called: if there is no cached text, an error is raised.
#'
#' @param table A data frame, or a named list of data frames.
#' @param example A reference paragraph (e.g. last year's) that fixes style,
#'   length and structure. Optional.
#' @param instructions Free-text additional instructions. Optional.
#' @param title Table title, included in the prompt. Optional.
#' @param exclude Values that are never flagged even if absent from the table.
#' @param generate If `FALSE`, never call the API (cache only).
#' @param cache If `FALSE`, neither read nor write the cache.
#' @param chat An [ellmer::Chat] object, a function that creates one, or an
#'   ellmer model string such as `"google_gemini/gemini-3.6-flash"` (see
#'   [ellmer::chat()]). Defaults to the `narrate.chat` option.
#' @returns A `narrate_text` object: the text (character) with attributes
#'   `verification` (see [verify_figures()]), `tokens`, `model` and `seconds`.
#' @examples
#' \dontrun{
#' options(
#'   narrate.chat = "google_gemini/gemini-3.6-flash",
#'   narrate.language = "Spanish",
#'   narrate.decimals = 1,
#'   narrate.decimal_mark = ",",
#'   narrate.big_mark = "."
#' )
#' tab <- data.frame(site = c("A", "B"), n = c(1512, 1898), pct = c(22.46, 28.19))
#' narrate(tab, example = "En el periodo anterior se registraron 3.100 procedimientos...")
#' }
#' @export
narrate <- function(table,
                    example = NULL,
                    instructions = NULL,
                    title = NULL,
                    exclude = NULL,
                    generate = getOption("narrate.generate"),
                    cache = TRUE,
                    chat = NULL) {
  check_tables(table)
  check_string(example, allow_null = TRUE)
  check_string(instructions, allow_null = TRUE)
  check_string(title, allow_null = TRUE)

  tables <- as_table_list(table, name = title %||% "table")
  markdown <- unlist(lapply(names(tables), function(name) {
    c(table_to_markdown(tables[[name]], title = name), "")
  }))
  prompt <- prompt_template(
    "narrate",
    tables = markdown,
    example = example %||% "(no example: use a formal, technical register)",
    instructions = instructions %||% "",
    language = getOption("narrate.language")
  )
  generate_text(prompt, tables, type = "narrate", generate = generate, cache = cache,
                chat = chat, exclude = exclude)
}
