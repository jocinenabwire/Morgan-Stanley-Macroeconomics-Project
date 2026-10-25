# Run the complete Domestic subset search at horizons 1:9, locally.
# Selection: 2018-2022. Check: common origins 2022 Q4-2024 Q1.
# Fully observed nine-quarter path: 2024 Q2-2026 Q2, from 2024 Q1.
# Latest path: 2025 Q1-2027 Q1, from 2024 Q4; only six actuals exist.
# No commit, push, macro-vintage update, or scenario import is performed.
source("ppnr_domestic/search_nine_quarters.R")

ppnr_nine_quarters_2025 <- local({
  output_dir <- file.path(ppnr_search_dir, "results_nine_quarters_local_2026-10-25")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  log <- file(file.path(output_dir, "search_progress.log"), open = "wt")
  sink(log, split = TRUE)
  tryCatch(run_ppnr_nine_quarters(output_dir = output_dir),
    finally = { sink(); close(log) })
})

ppnr_top3_nine_quarters <- ppnr_nine_quarters_2025$top3
ppnr_nine_quarter_comparison <- ppnr_nine_comparison(ppnr_nine_quarters_2025)
ppnr_horizon9_top3 <- subset(ppnr_top3_nine_quarters, horizon == 9L)
ppnr_horizon9_check_review <- ppnr_ninth_horizon_review(ppnr_nine_quarters_2025)
ppnr_nine_observed_path <- ppnr_nine_quarters_2025$final_winners
ppnr_nine_latest_path <- ppnr_nine_quarters_2025$latest_winners
plot_ppnr_nine_path(ppnr_nine_quarters_2025, latest = TRUE)
plot_ppnr_nine_path(ppnr_nine_quarters_2025)
plot_ppnr_nine_accuracy(ppnr_nine_quarters_2025)
plot_ppnr_ninth_model_size(ppnr_nine_quarters_2025)
if (interactive()) View(ppnr_horizon9_check_review,
  title = "Horizon 9: exploratory later-check comparison")
