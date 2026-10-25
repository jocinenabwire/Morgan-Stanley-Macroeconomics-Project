# Reopen the saved nine-quarter results without running the search again.
source("ppnr_domestic/search_nine_quarters.R")
ppnr_nine_quarters_2025 <- readRDS(file.path(ppnr_search_dir,
  "results_nine_quarters_local_2026-10-25", "ppnr_nine_quarters.rds"))
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
