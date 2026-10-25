# Run from the repository root. Neither search runner is called by this test.
main <- function() {
  arguments <- commandArgs(trailingOnly = TRUE)
  repo <- normalizePath(if (length(arguments)) arguments[1] else ".", mustWork = TRUE)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(repo)
  analysis <- new.env(parent = globalenv())
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  source("run_ppnr.R", local = analysis)
  stopifnot(identical(getwd(), repo))

  stopifnot(nrow(analysis$company) == 70L,
    nrow(analysis$fed_macro_2025) == 196L,
    nrow(analysis$ms_ppnr_2025) == 64L,
    sum(!analysis$ms_macro_2025$macro_matched) == 6L,
    identical(range(analysis$ppnr_dates), as.Date(c("2009-03-31", "2024-12-31"))),
    all(analysis$ppnr_checks_2025[c("max_NII_difference", "max_source_difference")] == 0),
    all(analysis$ppnr_seasonal_pattern_2025$n == 16L),
    identical(analysis$macro_columns, analysis$dictionary$r_name),
    all(grepl("^us_", analysis$macro_columns)))
  company_quarters <- as.integer(format(analysis$company$Date, "%Y")) * 4L +
    as.integer(format(analysis$company$Date, "%m")) / 3L
  stopifnot(all(diff(company_quarters) == 1),
    all(is.na(as.matrix(analysis$ms_macro_2025[
      !analysis$ms_macro_2025$macro_matched, analysis$macro_columns]))))
  cat("PASS: 70 company rows, 64 matched quarters, 16 Domestic variables, six later quarters unfilled.\n")

  # The added history must not revise any existing 2010-2026 source values.
  previous <- read.csv("Morgan_Stanley_PPNR_Quarterly_Final.csv", stringsAsFactors = FALSE)
  previous$Date <- as.Date(previous$Date)
  overlap <- analysis$company[match(previous$Date, analysis$company$Date), ]
  rownames(overlap) <- rownames(previous) <- NULL
  # Scientific notation in the new CSV can change integer storage to double.
  stopifnot(isTRUE(all.equal(previous, overlap, tolerance = 0)),
    identical(analysis$company$Date[1:4],
      as.Date(c("2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31"))),
    all(analysis$company$Total.interest.income - analysis$company$Total.interest.expense ==
      analysis$company$Net.interest.income))
  # Independently verified public 2009 Q1 FR Y-9C aggregate values, in USD thousands.
  stopifnot(identical(as.numeric(analysis$company[1, analysis$financial_columns]),
    c(2502000, 2376000, 3026000, 3919000)))
  cat("PASS: all 66 overlapping rows unchanged; four 2009 rows added; 2009 Q1 source totals agree.\n")

  # Compare calendar targets with the old sample, rather than retaining old row numbers.
  old_sample <- analysis$ms_ppnr_2025[analysis$ppnr_dates >= "2010-01-01", ]
  for (period in c("selection", "check")) for (h in 1:3) {
    origins <- analysis$ppnr_forecast_origins(analysis$ms_ppnr_2025, h, period)
    old_origins <- analysis$ppnr_forecast_origins(old_sample, h, period)
    dates <- analysis$ms_ppnr_2025$Date[origins + h]
    stopifnot(identical(dates, old_sample$Date[old_origins + h]),
      length(origins) == if (period == "selection") 21L - h else 9L - h,
      all(format(dates, "%Y") %in% if (period == "selection") as.character(2018:2022) else
        as.character(2023:2024)))
  }
  for (runner in list(analysis$run_ppnr_search, analysis$check_ppnr_candidates))
    stopifnot("ppnr_forecast_origins" %in% all.names(body(runner)))
  predictions <- analysis$ppnr_trial_2025$predictions
  target_year <- as.integer(substr(predictions$target, 1, 4))
  stopifnot(all(target_year[predictions$period == "Development: 2018-2022"] %in% 2018:2022),
    all(target_year[predictions$period == "Check: 2023-2024"] %in% 2023:2024),
    all(is.finite(as.matrix(predictions[c("Trial", "Naive", "Difference_AR1")]))),
    nobs(analysis$ppnr_trial_2025$model) == 63L,
    all(is.finite(analysis$ppnr_trial_2025$coefficients$estimate)),
    nrow(analysis$ppnr_trial_2025$forecast) == 3L,
    all(is.finite(analysis$ppnr_trial_2025$forecast$PPNR_bn)))
  counts <- unique(analysis$ppnr_trial_2025$validation[c("period", "horizon", "n")])
  stopifnot(all(counts$n[counts$period == "Check: 2023-2024"] ==
    9L - counts$horizon[counts$period == "Check: 2023-2024"]))
  cat("PASS: ordinary diagnostics and trial model run; 2018-2022 and 2023-2024 targets stay fixed.\n")

  stopifnot(!exists("ppnr_search_2025", analysis, inherits = FALSE),
    !exists("ppnr_arx_search_2025", analysis, inherits = FALSE),
    !exists("ppnr_search_round", analysis, inherits = FALSE),
    !"Rcpp" %in% loadedNamespaces())
  cat("PASS: neither search ran; no search result or compiled search function was created.\n")

  # A changed source CSV must be rejected before any model is estimated.
  copy <- tempfile("ppnr-2009-integrity-")
  dir.create(copy)
  stopifnot(file.copy("ppnr_domestic", copy, recursive = TRUE),
    file.copy("Morgan_Stanley_PPNR_Quarterly_2009_2026.csv", copy))
  target <- file.path(copy, "Morgan_Stanley_PPNR_Quarterly_2009_2026.csv")
  cat("\n", file = target, append = TRUE)
  setwd(file.path(copy, "ppnr_domestic"))
  failure <- tryCatch({sys.source("restore_inputs.R", new.env()); "unexpected success"},
    error = conditionMessage)
  stopifnot(grepl("md5", failure, fixed = TRUE))
  setwd(repo)
  cat("PASS: modified company input rejected by the source-hash check.\n")

  qa_dir <- Sys.getenv("PPNR_INTEGRATION_QA_DIR", unset = "")
  if (nzchar(qa_dir)) {
    dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(list(checks = analysis$ppnr_checks_2025,
      sample_dates = range(analysis$ppnr_dates),
      trial = analysis$ppnr_trial_2025[c("coefficients", "diagnostics", "validation", "forecast")],
      search_started = FALSE), file.path(qa_dir, "integration_2009_validation.rds"))
  }
  cat("ALL INPUT-INTEGRATION CHECKS PASSED\n")
}
main()
