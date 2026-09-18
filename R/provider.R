# Resolve a Chat object from the argument or the `narrate.chat` option.
# Accepts a Chat, a function that creates one, or an ellmer model string such
# as "google_gemini/gemini-3.6-flash". Always returns a clone so history does
# not accumulate between calls.
resolve_chat <- function(chat = NULL, call = caller_env()) {
  chat <- chat %||% getOption("narrate.chat")
  if (is.null(chat)) {
    cli::cli_abort(c(
      "No chat model configured.",
      i = "Set {.code options(narrate.chat = \"google_gemini/gemini-3.6-flash\")} or a function that creates an {.cls ellmer::Chat}."
    ), call = call)
  }
  if (is_string(chat)) chat <- ellmer::chat(chat, echo = "none")
  if (is.function(chat)) chat <- chat()
  if (!inherits(chat, "Chat")) {
    cli::cli_abort(
      "{.arg chat} must be an {.cls ellmer::Chat} object or a function that creates one, not {.obj_type_friendly {chat}}.",
      call = call
    )
  }
  chat$clone()
}

chat_model <- function(chat) {
  tryCatch(chat$get_model(), error = function(e) NA_character_)
}

chat_tokens <- function(chat) {
  tokens <- tryCatch(chat$get_tokens(), error = function(e) NULL)
  if (!is.data.frame(tokens)) {
    return(c(input = NA_real_, output = NA_real_))
  }
  c(input = sum(tokens$input, na.rm = TRUE), output = sum(tokens$output, na.rm = TRUE))
}

# Read a prompt template and fill {{placeholders}}.
prompt_template <- function(name, ..., call = caller_env()) {
  dir <- getOption("narrate.prompt_dir")
  path <- if (!is.null(dir)) {
    file.path(dir, paste0(name, ".md"))
  } else {
    system.file("prompts", paste0(name, ".md"), package = "narrate")
  }
  if (!file.exists(path)) {
    cli::cli_abort("Prompt template not found: {.path {path}}.", call = call)
  }
  template <- paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  values <- list2(...)
  for (key in names(values)) {
    value <- values[[key]]
    value <- if (is.null(value)) "" else paste(as.character(value), collapse = "\n")
    template <- gsub(paste0("{{", key, "}}"), value, template, fixed = TRUE)
  }
  template
}

system_prompt <- function() {
  getOption("narrate.system_prompt") %||%
    prompt_template("system", language = getOption("narrate.language"))
}

# Call the model, retrying only on transient HTTP errors (429, 5xx).
call_model <- function(chat, prompt, attempts = 4, call = caller_env()) {
  for (i in seq_len(attempts)) {
    result <- tryCatch(chat$chat(prompt, echo = "none"), error = function(e) e)
    if (!inherits(result, "error")) {
      return(as.character(result))
    }
    msg <- conditionMessage(result)
    if (!grepl("HTTP (429|5[0-9]{2})", msg)) {
      cli::cli_abort(c("The model call failed.", x = msg), call = call)
    }
    cli::cli_inform("Attempt {i} failed: {msg}")
    if (i < attempts) Sys.sleep(30 * i)
  }
  cli::cli_abort("The model did not respond after {attempts} attempts.", call = call)
}

# Shared core of narrate() and narrate_report(): cache, call, verify, log.
generate_text <- function(prompt, tables, type, generate, cache, chat, exclude,
                          call = caller_env()) {
  check_bool(generate, call = call)
  check_bool(cache, call = call)

  system <- system_prompt()
  cache_dir <- getOption("narrate.cache_dir")
  model_name <- tryCatch(chat_model(resolve_chat(chat, call)), error = function(e) NA_character_)
  key <- hash(list(system, prompt, model_name))
  path <- file.path(cache_dir, paste0(type, "_", key, ".rds"))

  if (cache && file.exists(path)) {
    return(readRDS(path))
  }
  if (!generate) {
    cli::cli_abort(c(
      "No cached text for this input and {.code narrate.generate} is {.val FALSE}.",
      i = "Set {.code options(narrate.generate = TRUE)} to call the model."
    ), call = call)
  }

  chat <- resolve_chat(chat, call)
  chat$set_system_prompt(system)
  t0 <- Sys.time()
  text <- call_model(chat, prompt, call = call)
  seconds <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  tokens <- chat_tokens(chat)

  verification <- verify_figures(text, tables, exclude = exclude)
  summary <- attr(verification, "summary")
  if (isTRUE(getOption("narrate.highlight")) && summary$flagged > 0) {
    text <- highlight_flagged(text, verification)
  }

  result <- structure(
    text,
    class = c("narrate_text", "character"),
    verification = verification,
    tokens = tokens,
    model = chat_model(chat),
    seconds = seconds,
    key = key,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M")
  )

  if (cache) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    saveRDS(result, path)
  }
  log_call(type, result)

  if (summary$flagged > 0) {
    cli::cli_warn(c(
      "{summary$flagged} figure{?s} could not be verified: rounded {summary$rounded}, proportion {summary$proportion}, unsourced {summary$unsourced}.",
      i = "Inspect {.code attr(x, \"verification\")}."
    ))
  }
  result
}

highlight_flagged <- function(text, verification) {
  flagged <- unique(verification$figure[verification$status %in% c("rounded", "proportion", "unsourced")])
  for (s in flagged) {
    text <- gsub(s, paste0("**[", s, "?]**"), text, fixed = TRUE)
  }
  text
}

#' @export
print.narrate_text <- function(x, ...) {
  cat(as.vector(x), "\n")
  verification <- attr(x, "verification")
  if (!is.null(verification)) {
    s <- attr(verification, "summary")
    tokens <- attr(x, "tokens")
    cli::cli_text(
      "{.field {attr(x, 'model')}} | {attr(x, 'timestamp')} | tokens {tokens[['input']]}/{tokens[['output']]} | ",
      "{attr(x, 'seconds')}s | flagged {s$flagged} (unsourced {s$unsourced}) | missing {s$missing}"
    )
  }
  invisible(x)
}

#' @export
as.character.narrate_text <- function(x, ...) as.vector(x)
