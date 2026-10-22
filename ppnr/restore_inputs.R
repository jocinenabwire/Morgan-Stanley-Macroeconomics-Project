# Restore inputs only, from this folder or the copied project root.
data_dir <- if (file.exists(file.path("joined_2025", "ms_macro_2025.rds"))) "." else
  file.path("output", "Makroadatok_2026-10-17")
stopifnot(dir.exists(data_dir))

fed_macro_2025 <- read.csv(
  file.path(data_dir, "ready_for_R", "fed_macro_historical_2025.csv"),
  stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
fed_macro_2025$quarter_end <- as.Date(fed_macro_2025$quarter_end)
ms_macro_2025 <- readRDS(file.path(data_dir, "joined_2025", "ms_macro_2025.rds"))

stopifnot(nrow(fed_macro_2025) == 196, ncol(fed_macro_2025) == 31,
          nrow(ms_macro_2025) == 66, ncol(ms_macro_2025) == 61,
          sum(ms_macro_2025$macro_matched) == 60)
