# Morgan Stanley | Fed 2025 Domestic | Local exhaustive search
# Source this file to run all one-to-eight-factor combinations, horizons 1:4.
# Ranking: 2018-2022 RMSE. Later checks: 2023-2024 and frozen-origin 2025 Q1-Q4.
# Previous PPNR is optional; every used factor counts toward the limit of eight.
# No commit or upload is performed. Reopen saved results with view_search_four_quarters.R.
source("ppnr_domestic/search_four_quarters.R")

ppnr_four_quarters_2025 <- local({
  output_dir <- file.path(ppnr_search_dir, "results_local_2026-10-25")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  log <- file(file.path(output_dir, "search_progress.log"), open = "wt")
  sink(log, split = TRUE)
  tryCatch(run_ppnr_four_quarters(include_own = TRUE, output_dir = output_dir),
    finally = { sink(); close(log) })
})

ppnr_top3_four_quarters <- ppnr_four_quarters_2025$top3
ppnr_winners_four_quarters <- ppnr_four_quarters_2025$winners
ppnr_2025_forecast_check <- ppnr_four_quarters_2025$final_winners
plot_ppnr_four_quarters(ppnr_four_quarters_2025)
if (interactive()) View(ppnr_top3_four_quarters, title = "Top 3 per factor count and horizon")
