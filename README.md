# narrate

<!-- badges: start -->
<!-- badges: end -->

Verified narration of tables for reproducible reports in R Markdown and Quarto.

`narrate()` uses a large language model to write the paragraph that
accompanies a table in a report, then **verifies that every figure in the text
exists in the table**. Figures that do not are diagnosed as rounded, converted
from a proportion, or unsourced. The model writes; the code owns the numbers.

It grew out of a pilot to reproduce an annual institutional report from the
previous edition and the new statistical tables. It uses
[ellmer](https://ellmer.tidyverse.org) to talk to any provider (Gemini, Claude,
OpenAI, Mistral, Groq, Ollama, ...).

## Installation

You can install the development version of narrate from GitHub with:

``` r
# install.packages("pak")
pak::pak("USER/narrate")
```

## Setup

Once per session, or in your project's `.Rprofile`:

``` r
library(narrate)

options(
  narrate.chat = "google_gemini/gemini-3.6-flash",   # or a function returning an ellmer Chat
  narrate.language = "Spanish",   # language of the generated text
  narrate.decimals = 1,           # precision of figures in the report
  narrate.decimal_mark = ",",
  narrate.big_mark = "."
)
```

API keys live in `.Renviron` (`GEMINI_API_KEY`, `ANTHROPIC_API_KEY`, ...), never
in code.

## Inline use in R Markdown / Quarto

Right below the chunk that produces the table:

```` markdown
```{r tab1}
tab1 <- data.frame(
  site = c("A", "B", "C"),
  n = c(1512, 1898, 187),
  pct = c(22.46, 28.19, 2.78)
)
knitr::kable(tab1)
```

`r narrate(tab1, example = "In the previous period, 3,100 procedures were recorded, ...")`
````

The paragraph is generated on the first render and cached in
`.narrate_cache/`; later renders do not call the API unless the table, the
example or the model change. With `options(narrate.generate = FALSE)` the
render never calls the API: it uses the cache only, and fails if there is none.

## Verify any text

``` r
tab <- data.frame(site = c("A", "B"), n = c(1512, 1898), pct = c(22.46, 28.19))
verify_figures("A had 1,512 procedures (22.5%) and B 1898. Total: 3410.", tab)
#> 4 figures: 2 found, 0 excluded, 2 flagged (rounded 1, proportion 0, unsourced 1); 0 [MISSING DATA] markers.
#>  figure  status table_figure table column ...
#>    22.5 rounded        22.46 table    pct
#>    3410 unsourced       <NA>  <NA>   <NA>
```

It works on hand-written text too. `exclude = 2018:2030` keeps years and other
non-figures from being flagged.

## Reproduce a full report

``` r
tables <- list(
  "Table 1. Procedures by site" = tab1,
  "Table 2. Mortality"          = tab2
)
report <- narrate_report("reports/report_2025.md", tables,
                         period = "January 2025 to June 2026")
writeLines(report, "output/report_2026.md")
attr(report, "verification")
```

## Log and model comparison

Every call appends a row to `.narrate_cache/log.csv`: model, tokens, latency,
verified and flagged figures. To compare models on the same table, change
`narrate.chat` and call again; `narrate_log()` returns the table. For
systematic evaluation, Posit's [vitals](https://vitals.tidyverse.org) is the
natural companion.

## What it does and does not do

- It verifies that each figure **exists** in the tables and, when found,
  reports table, row and column. It does not guarantee the model put the figure
  in the right sentence: that is still a human read.
- The model's rules (copy figures verbatim, write `[MISSING DATA]`, no
  evaluative adjectives) live in `inst/prompts/`. Override them with
  `narrate.system_prompt` or `narrate.prompt_dir`.
- It never trains anything and never sees individual-level data: only the
  aggregated tables you pass.

## Confidentiality

The package sends your tables to the configured provider. For confidential
material, use a commercial or institutional plan (paid API, Vertex AI,
Bedrock, Azure) whose terms exclude training on your data, or a local model
through `ellmer::chat_ollama()`. Free tiers are fine for prototypes with
synthetic data.
