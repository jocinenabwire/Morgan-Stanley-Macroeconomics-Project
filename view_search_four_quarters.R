# Reopen the locally saved exhaustive search without running it again.
source("ppnr_domestic/search_four_quarters.R")
ppnr_four_quarters_2025 <- readRDS(file.path(ppnr_search_dir,
  "results_local_2026-10-25", "ppnr_four_quarters.rds"))
ppnr_top3_four_quarters <- ppnr_four_quarters_2025$top3
ppnr_winners_four_quarters <- ppnr_four_quarters_2025$winners
ppnr_2025_forecast_check <- ppnr_four_quarters_2025$final_winners
plot_ppnr_four_quarters(ppnr_four_quarters_2025)
if (interactive()) View(ppnr_top3_four_quarters, title = "Top 3 per factor count and horizon")
