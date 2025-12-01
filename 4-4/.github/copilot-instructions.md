<!-- Copilot instructions for the local econometrics workspace -->

Purpose
- Help an AI coding agent become productive quickly in this small R-based econometrics repo.

Big picture
- This repository contains short R scripts that fetch market data, compute simple returns, and perform small econometric checks. Key files:
  - `getStockData.r` — fetches monthly prices via `pdfetch::pdfetch_YAHOO`, converts prices to simple returns, and writes `returns.csv`.
  - `qq_jarque_bera.r` — (analysis script) performs distributional checks (QQ plots, Jarque–Bera tests) on return series.
  - `stock.csv` / `returns.csv` — data artifacts written by scripts.

Why things are structured this way
- Scripts are lightweight and intended to be runnable in an interactive R session or non-interactively with `Rscript`.
- Time-series results from `pdfetch` come back as `xts`/`zoo` objects; code converts them to data frames for CSV export.

Developer workflows / commands
- Install dependencies (one-off):
  - `R -e "install.packages(c('pdfetch','xts','zoo'))"`
- Run the data-fetch script:
  - `Rscript getStockData.r`  # produces `returns.csv` in the script working directory
- Run analysis script interactively or non-interactively:
  - `Rscript qq_jarque_bera.r`
  - or in R: `source('getStockData.r') ; source('qq_jarque_bera.r')`

Project-specific conventions and patterns
- Time-series handling:
  - `pdfetch_YAHOO()` returns xts-like objects. Use `library(xts)` / `library(zoo)` when working with `index()` or `coredata()`.
  - Subsetting caution: `obj["2023-01"]` is a time-based row selector (xts), whereas `obj[, "X.gspc"]` or `obj[["X.gspc"]]` selects columns.
  - When converting to a data.frame, use `df <- as.data.frame(x)` then `df$Date <- as.Date(rownames(df))` and `write.csv(..., row.names = FALSE)` to avoid an empty leading header column.

- Column naming and indexing:
  - R is 1-based. `colnames(df)[0]` returns `character(0)` and should not be used.
  - Rename safely with `colnames(df)[1] <- 'Date'` (after confirming the intended column index), or better add `df$Date <- ...` and reorder columns programmatically.

Integration points and external dependencies
- `pdfetch` (uses external data sources like Yahoo Finance) — network access required.
- Scripts depend on `xts`/`zoo` for time-index helpers; `as.data.frame()` is used to produce CSVs consumed by analysis scripts.

Agent guidance and constraints (do this when editing)
- Prefer minimal, focused edits. Do not change data-producing logic unless fixing a bug documented by failing runs.
- When you change how CSVs are written, preserve backward compatibility of column names and row formats where possible.
- Add `library(xts)` near the top of scripts that use `index()` rather than sprinkling `pkg::fn()` calls unless the repo intends explicit namespace calls.
- If adding or installing packages, update the repository README or leave an actionable note in the top-of-file comments.

Files to inspect for patterns
- `getStockData.r` — demonstrates xts -> data.frame conversion and the main data pipeline.
- `qq_jarque_bera.r` — shows downstream expectations for CSV layout and column names.

If unclear or incomplete
- Ask for the intended primary workflow (interactive R vs automated pipeline) and whether external dependencies may be added (e.g., `renv`, `DESCRIPTION`).

Examples (copy-paste safe)
- Add Date column and write CSV safely:
  ```r
  df <- as.data.frame(xts_obj)
  df$Date <- as.Date(rownames(df))
  df <- df[, c('Date', setdiff(names(df), 'Date'))]
  write.csv(df, file.path(getwd(), 'returns.csv'), row.names = FALSE)
  ```

End
