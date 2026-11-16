required_packages <- c("dplyr", "readr", "lmtest", "tidyr")
missing_packages <- required_packages[!vapply(
  required_packages,
  requireNamespace,
  logical(1),
  quietly = TRUE
)]
if (length(missing_packages)) install.packages(missing_packages)

library(lmtest)
library(dplyr)
library(readr)
library(tidyr)

getwd()
list.files(pattern = "\\.csv$")

percent_log_growth <- function(x, variable_name) {
  if (any(x <= 0, na.rm = TRUE)) {
    stop(
      paste0("Cannot calculate log growth for non-positive values in ", variable_name, "."),
      call. = FALSE
    )
  }
  100 * (log(x) - log(dplyr::lag(x)))
}

# 1. Import datasets
ppnr <- read_csv("Morgan_Stanley_PPNR_Quarterly_2009_2026.csv")
bs <- read_csv("Morgan_Stanley_Balance_Sheet.csv")
macro <- read_csv("2025-Table_1A_Historic_Domestic.csv")

# 2. Prepare interest expense
expense <- ppnr %>%
  transmute(
    Quarter = paste0(format(as.Date(Date), "%Y"), " Q",
                     (as.integer(format(as.Date(Date), "%m")) - 1) %/% 3 + 1),
    IE = `Total interest expense`
  ) %>%
  mutate(IE_lag = lag(IE))

# 3. Prepare balance sheet variables
balance <- bs %>%
  transmute(
    Quarter = Date,
    Deposits_US = `Interest-bearing domestic deposits`,
    Deposits_Foreign = `Interest-bearing foreign deposits`,
    Repos = Repos,
    Other_Borrowing = `Other borrowed money`,
    Subordinated_Debt = `Subordinated debt`,
    Trading_Liabilities = `Trading liabilities`,
    Total_Liabilities = `Total liabilities`
  )

# 4. Prepare macroeconomic variables
rates <- macro %>%
  transmute(
    Quarter = Date,
    Rate_3M = `3-month Treasury rate`,
    Rate_10Y = `10-year Treasury yield`,
    BBB_Yield = `BBB corporate yield`,
    VIX = `Market Volatility Index (Level)`
  ) %>%
  arrange(Quarter) %>%
  mutate(Rate_Change = Rate_3M - lag(Rate_3M))

# 5. Create selection dataset
selection_expense <- expense %>%
  left_join(balance, by = "Quarter") %>%
  left_join(rates, by = "Quarter") %>%
  arrange(Quarter) %>%
  mutate(
    # Existing level interactions remain available as candidates.
    Rate_Deposits = Rate_3M * Deposits_US,
    Rate_Repos = Rate_3M * Repos,
    Rate_Borrowing = Rate_3M * Other_Borrowing,
    # Stationary alternatives; original level columns are retained.
    Deposits_US_Growth = percent_log_growth(Deposits_US, "Deposits_US"),
    Repos_Growth = percent_log_growth(Repos, "Repos"),
    Other_Borrowing_Growth = percent_log_growth(Other_Borrowing, "Other_Borrowing"),
    Subordinated_Debt_Growth = percent_log_growth(Subordinated_Debt, "Subordinated_Debt"),
    Rate_Deposits_Change = Rate_Deposits - lag(Rate_Deposits),
    Rate_Repos_Change = Rate_Repos - lag(Rate_Repos)
  ) %>%
  drop_na()

# 6. Save and inspect the level and transformed candidate variables
saveRDS(selection_expense, "selection_expense.rds")
write_csv(selection_expense, "selection_expense.csv")

# 7. Settings
training_end <- "2024 Q2"
if (!training_end %in% selection_expense$Quarter) {
  stop(paste("Approved training cutoff is missing:", training_end))
}
# Keep the approved holdout completely out of model selection and rolling evaluation.
selection_expense <- selection_expense %>%
  filter(Quarter <= training_end)
if (interactive()) View(selection_expense)
dim(selection_expense)

window_grid <- c(32L, 28L, 24L, 20L, 16L)
window_sensitivity <- 12L
windows_to_evaluate <- c(window_grid, window_sensitivity)
horizon <- 9L
detected_cores <- parallel::detectCores(logical = FALSE)
default_workers <- if (is.na(detected_cores)) 8L else max(1L, min(8L, detected_cores - 2L))
requested_workers <- suppressWarnings(as.integer(Sys.getenv("PPNR_WORKERS", "")))
parallel_workers <- if (length(requested_workers) == 1L && is.finite(requested_workers) && requested_workers > 0L) {
  min(8L, requested_workers)
} else {
  default_workers
}
output_dir <- Sys.getenv("PPNR_OUTPUT_DIR", file.path("results", "rolling_backtest"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
max_window <- max(windows_to_evaluate)
if (nrow(selection_expense) <= max_window + horizon - 1L) {
  stop("Not enough training observations for the approved windows and horizon.")
}
# Every window uses all eligible origins. Common-origin metrics compare windows
# on the same dates, beginning at the largest window length.
common_starts <- seq.int(max_window + 1L, nrow(selection_expense) - horizon + 1L)
common_origin_quarters <- selection_expense$Quarter[common_starts - 1L]

variables <- setdiff(names(selection_expense), c("Quarter", "IE"))

combinations <- unlist(
  lapply(4:6, function(k) combn(variables, k, simplify = FALSE)),
  recursive = FALSE
)

# A level series and its stationary alternative are separate candidates.
stationarity_alternatives <- list(
  c("Deposits_US", "Deposits_US_Growth"),
  c("Repos", "Repos_Growth"),
  c("Other_Borrowing", "Other_Borrowing_Growth"),
  c("Subordinated_Debt", "Subordinated_Debt_Growth"),
  c("Rate_Deposits", "Rate_Deposits_Change"),
  c("Rate_Repos", "Rate_Repos_Change")
)

combinations <- Filter(function(x) {
  all(vapply(
    stationarity_alternatives,
    function(group) sum(group %in% x) <= 1,
    logical(1)
  ))
}, combinations)

parallel_map_rows <- function(items, FUN) {
  if (!length(items)) return(NULL)
  number_of_chunks <- min(parallel_workers, length(items))
  chunk_ids <- ceiling(seq_along(items) * number_of_chunks / length(items))
  chunks <- split(items, chunk_ids)
  run_chunk <- function(chunk) do.call(rbind, lapply(chunk, FUN))
  if (.Platform$OS.type == "unix" && number_of_chunks > 1L) {
    chunk_results <- parallel::mclapply(
      chunks, run_chunk, mc.cores = number_of_chunks,
      mc.preschedule = TRUE, mc.set.seed = FALSE
    )
  } else {
    chunk_results <- lapply(chunks, run_chunk)
  }
  failed_chunks <- vapply(chunk_results, inherits, logical(1), "try-error")
  if (any(failed_chunks)) stop(as.character(chunk_results[[which(failed_chunks)[1L]]]))
  do.call(rbind, chunk_results)
}

# Score every eligible origin; use the shared 32-quarter-origin dates when
# comparing window lengths so the window ranking has identical validation dates.
if (!length(common_starts)) {
  stop("No common rolling origins are available within the training sample.")
}
results_expense_by_window <- list()
results_mae_by_window <- list()
rate_correlations_by_window <- list()

for (window in windows_to_evaluate) {
window_started <- Sys.time()
starts <- seq.int(window + 1L, nrow(selection_expense) - horizon + 1L)
origin_quarters <- selection_expense$Quarter[starts - 1L]
cat("\n[IE] Window", window, "quarters:", length(combinations),
    "models across", length(starts), "origins;", parallel_workers, "worker(s).\n")

# 8. Collinearity diagnostics on each training window
# VIF > 5 is flagged for review. It does not automatically exclude a model,
# because VIF alone does not determine forecast quality.
vif_limit <- 5
exclude_high_vif <- FALSE

calculate_vif <- function(train, selected) {
  x <- train[, selected, drop = FALSE]
  x <- x[complete.cases(x), , drop = FALSE]

  if (ncol(x) < 2) {
    return(setNames(rep(1, ncol(x)), names(x)))
  }

  predictor_correlations <- cor(x)
  if (any(!is.finite(predictor_correlations))) {
    return(setNames(rep(Inf, ncol(x)), names(x)))
  }

  vif_values <- tryCatch(
    diag(solve(predictor_correlations)),
    error = function(e) rep(Inf, ncol(x))
  )
  invalid_vif <- !is.finite(vif_values) | vif_values < 1 - 1e-8
  vif_values[invalid_vif] <- Inf
  vif_values[!invalid_vif] <- pmax(vif_values[!invalid_vif], 1)
  setNames(vif_values, names(x))
}

get_max_vif_by_origin <- function(selected) {
  vapply(starts, function(start) {
    train <- selection_expense[(start - window):(start - 1), ]
    max(calculate_vif(train, selected), na.rm = TRUE)
  }, numeric(1))
}

vif_by_model <- parallel_map_rows(
  seq_along(combinations),
  function(i) get_max_vif_by_origin(combinations[[i]])
)
colnames(vif_by_model) <- paste0("Max_VIF_Origin_", seq_along(starts))

vif_summary <- data.frame(
  Variables = sapply(combinations, paste, collapse = " + "),
  vif_by_model,
  stringsAsFactors = FALSE
)
vif_summary$Max_VIF <- apply(vif_by_model, 1, max, na.rm = TRUE)
vif_summary$VIF_Flag <- ifelse(
  vif_summary$Max_VIF > vif_limit,
  paste0("Review: VIF > ", vif_limit),
  "No flag"
)

rate_correlations <- data.frame(
  Forecast_Origin = origin_quarters,
  First_Quarter = vapply(starts, function(start) {
    selection_expense$Quarter[start - window]
  }, character(1)),
  Last_Quarter = vapply(starts, function(start) {
    selection_expense$Quarter[start - 1]
  }, character(1)),
  Correlation_3M_10Y = vapply(starts, function(start) {
    train <- selection_expense[(start - window):(start - 1), ]
    cor(train$Rate_3M, train$Rate_10Y, use = "complete.obs")
  }, numeric(1))
)
rate_correlations$Pair_VIF_3M_10Y <- 1 / (1 - rate_correlations$Correlation_3M_10Y^2)

cat("\n3-month / 10-year rate correlation in training windows:\n")
print(rate_correlations, row.names = FALSE)
cat(
  "Candidate models flagged for VIF > ", vif_limit, ": ",
  sum(vif_summary$Max_VIF > vif_limit), " of ", nrow(vif_summary), "\n",
  sep = ""
)

both_rate_models <- vapply(combinations, function(x) {
  all(c("Rate_3M", "Rate_10Y") %in% x)
}, logical(1))
if (any(both_rate_models)) {
  rate_model_vif <- vif_summary[both_rate_models, , drop = FALSE]
  cat(
    "Models containing both rates flagged for VIF > ", vif_limit, ": ",
    sum(rate_model_vif$Max_VIF > vif_limit), " of ", nrow(rate_model_vif), "\n",
    sep = ""
  )
  print(
    head(rate_model_vif[order(-rate_model_vif$Max_VIF), ], 5),
    row.names = FALSE
  )
}

if (exclude_high_vif) {
  keep_models <- vif_summary$Max_VIF <= vif_limit
  model_combinations <- combinations[keep_models]
  vif_summary <- vif_summary[keep_models, , drop = FALSE]
} else {
  model_combinations <- combinations
}
rate_correlations$Window <- window
rate_correlations_by_window[[as.character(window)]] <- rate_correlations

# 9. Rolling-origin evaluation; record failed and warned fits instead of losing them.
# Validation outcomes stay inside the approved training sample. Regressors in
# those validation quarters are realized values, so these scores are conditional
# on the ex-post predictor path rather than a real-time macro forecast.
evaluate_model <- function(selected) {
  ape <- matrix(NA_real_, nrow = length(starts), ncol = horizon)
  absolute_error <- matrix(NA_real_, nrow = length(starts), ncol = horizon)
  failed_origins <- logical(length(starts))
  warned_origins <- logical(length(starts))
  for (j in seq_along(starts)) {
    start <- starts[j]
    tryCatch({
      train <- selection_expense[(start - window):(start - 1L), ]
      validation <- selection_expense[start:(start + horizon - 1L), ]
      model <- withCallingHandlers(
        lm(reformulate(selected, response = "IE"), data = train),
        warning = function(w) {
          warned_origins[j] <<- TRUE
          invokeRestart("muffleWarning")
        }
      )
      last_IE <- tail(train$IE, 1L)
      for (h in seq_len(horizon)) {
        new_row <- validation[h, , drop = FALSE]
        if ("IE_lag" %in% selected) new_row$IE_lag <- last_IE
        prediction <- withCallingHandlers(
          as.numeric(predict(model, newdata = new_row)),
          warning = function(w) {
            warned_origins[j] <<- TRUE
            invokeRestart("muffleWarning")
          }
        )
        if (length(prediction) != 1L || !is.finite(prediction)) {
          stop("Non-finite forecast.", call. = FALSE)
        }
        absolute_error[j, h] <- abs(validation$IE[h] - prediction)
        ape[j, h] <- absolute_error[j, h] / abs(validation$IE[h]) * 100
        last_IE <- prediction
      }
    }, error = function(e) {
      failed_origins[j] <<- TRUE
    })
  }
  list(APE = ape, Absolute_Error = absolute_error,
       Failed_Origins = failed_origins, Warned_Origins = warned_origins)
}

# Every origin-horizon error has equal weight. Common metrics compare windows
# on identical dates; All metrics additionally use every eligible origin.
summarize_model <- function(selected) {
  errors <- evaluate_model(selected)
  common_rows <- match(common_starts, starts)
  common_failed <- any(errors$Failed_Origins[common_rows])
  all_failed <- any(errors$Failed_Origins)
  common_ape <- errors$APE[common_rows, , drop = FALSE]
  common_ae <- errors$Absolute_Error[common_rows, , drop = FALSE]
  c(
    Average_MAPE_Common = if (common_failed) NA_real_ else mean(common_ape),
    Average_MAE_Billions_Common = if (common_failed) NA_real_ else mean(common_ae) / 1e6,
    Average_MAPE_All = if (all_failed) NA_real_ else mean(errors$APE),
    Average_MAE_Billions_All = if (all_failed) NA_real_ else mean(errors$Absolute_Error) / 1e6,
    Failed_Origins_Common = sum(errors$Failed_Origins[common_rows]),
    Failed_Origins_All = sum(errors$Failed_Origins),
    Warned_Origins_All = sum(errors$Warned_Origins),
    setNames(if (common_failed) rep(NA_real_, horizon) else colMeans(common_ape),
             paste0("MAPE_H", seq_len(horizon), "_Common")),
    setNames(if (common_failed) rep(NA_real_, horizon) else colMeans(common_ae) / 1e6,
             paste0("MAE_Billions_H", seq_len(horizon), "_Common"))
  )
}

# 10. Fit every candidate at each approved window length.
forecast_one_origin <- function(selected, start, window) {
  train <- selection_expense[(start - window):(start - 1L), ]
  validation <- selection_expense[start:(start + horizon - 1L), ]
  model <- lm(reformulate(selected, response = "IE"), data = train)
  last_IE <- tail(train$IE, 1L)
  predicted <- numeric(horizon)
  for (h in seq_len(horizon)) {
    new_row <- validation[h, , drop = FALSE]
    if ("IE_lag" %in% selected) new_row$IE_lag <- last_IE
    predicted[h] <- as.numeric(predict(model, newdata = new_row))
    last_IE <- predicted[h]
  }
  data.frame(Quarter = validation$Quarter, Actual = validation$IE,
             Forecast = predicted, Horizon = seq_len(horizon))
}

  results_expense <- parallel_map_rows(seq_along(model_combinations), function(i) {
    metrics <- summarize_model(model_combinations[[i]])
    data.frame(Window = window,
               Variables = paste(model_combinations[[i]], collapse = " + "),
               as.list(metrics),
               Max_VIF = vif_summary$Max_VIF[i],
               VIF_Flag = vif_summary$VIF_Flag[i],
               stringsAsFactors = FALSE, check.names = FALSE)
  })
  results_expense$Rank_MAPE_Common <- rank(results_expense$Average_MAPE_Common,
                                           ties.method = "first", na.last = "keep")
  results_expense$Rank_MAE_Common <- rank(results_expense$Average_MAE_Billions_Common,
                                          ties.method = "first", na.last = "keep")
  results_expense <- results_expense[order(results_expense$Rank_MAPE_Common, na.last = TRUE), , drop = FALSE]
  results_expense_by_window[[as.character(window)]] <- results_expense
  saveRDS(results_expense,
          file.path(output_dir, paste0("interest_expense_window_", window, "_checkpoint.rds")))

  # Rank by MAE from the same nine-quarter recursive forecasts.
  results_mae <- results_expense[order(results_expense$Rank_MAE_Common, na.last = TRUE), , drop = FALSE]
  results_mae_by_window[[as.character(window)]] <- results_mae

  best <- results_expense[is.finite(results_expense$Average_MAPE_Common), , drop = FALSE]
  if (interactive() && nrow(best)) {
    vars <- trimws(strsplit(best$Variables[1L], " + ", fixed = TRUE)[[1]])
    latest_forecast <- forecast_one_origin(vars, tail(starts, 1L), window)
    plot(latest_forecast$Horizon, latest_forecast$Actual / 1e6,
         type = "l", col = "black", lwd = 2,
         ylim = range(c(latest_forecast$Actual, latest_forecast$Forecast)) / 1e6,
         xaxt = "n", xlab = "Forecast horizon (quarters)",
         ylab = "Interest Expense (USD billions)",
         main = paste0("Window ", window, "; origin ", tail(origin_quarters, 1L),
                       "; common-origin MAPE ", round(best$Average_MAPE_Common[1L], 2), "%"))
    lines(latest_forecast$Horizon, latest_forecast$Forecast / 1e6,
          col = "blue", lwd = 2, lty = 2)
    axis(1, at = seq_len(horizon), labels = latest_forecast$Quarter)
    legend("topleft", legend = c("Actual", "Forecast"),
           col = c("black", "blue"), lty = c(1, 2), lwd = 2, bty = "n")
  }
cat("[IE] Window", window, "finished in",
    round(as.numeric(difftime(Sys.time(), window_started, units = "mins")), 1), "minute(s).\n")
}

results_expense <- do.call(rbind, results_expense_by_window)
results_mae <- do.call(rbind, results_mae_by_window)
rate_correlations <- do.call(rbind, rate_correlations_by_window)
rownames(results_expense) <- NULL
rownames(results_mae) <- NULL
rownames(rate_correlations) <- NULL
top50_mape_by_window <- do.call(rbind, lapply(windows_to_evaluate, function(window) {
  current <- results_expense[results_expense$Window == window, , drop = FALSE]
  head(current[order(current$Average_MAPE_Common, na.last = TRUE), , drop = FALSE], 50L)
}))
top50_mae_by_window <- do.call(rbind, lapply(windows_to_evaluate, function(window) {
  current <- results_mae[results_mae$Window == window, , drop = FALSE]
  head(current[order(current$Average_MAE_Billions_Common, na.last = TRUE), , drop = FALSE], 50L)
}))
top50_mape_all <- head(results_expense[order(results_expense$Average_MAPE_Common, na.last = TRUE), , drop = FALSE], 50L)
top50_mae_all <- head(results_mae[order(results_mae$Average_MAE_Billions_Common, na.last = TRUE), , drop = FALSE], 50L)
write_csv(results_expense, file.path(output_dir, "interest_expense_all_model_results.csv"))
saveRDS(results_expense, file.path(output_dir, "interest_expense_all_model_results.rds"))
write_csv(top50_mape_by_window, file.path(output_dir, "interest_expense_top50_mape_by_window.csv"))
write_csv(top50_mae_by_window, file.path(output_dir, "interest_expense_top50_mae_by_window.csv"))
write_csv(top50_mape_all, file.path(output_dir, "interest_expense_top50_mape_all_windows.csv"))
write_csv(top50_mae_all, file.path(output_dir, "interest_expense_top50_mae_all_windows.csv"))
write_csv(rate_correlations, file.path(output_dir, "interest_expense_rate_correlations_by_origin.csv"))
saveRDS(rate_correlations, file.path(output_dir, "interest_expense_rate_correlations_by_origin.rds"))
if (interactive()) {
  View(selection_expense)
  View(rate_correlations)
  View(top50_mape_by_window)
  View(top50_mae_by_window)
}

run_metadata <- data.frame(
  Component = "Interest Expense",
  Training_First_Quarter = selection_expense$Quarter[1L],
  Training_Last_Quarter = tail(selection_expense$Quarter, 1L),
  Training_Observations = nrow(selection_expense),
  Horizon_Quarters = horizon,
  Windows = paste(windows_to_evaluate, collapse = ", "),
  Common_Origins = length(common_starts),
  Candidate_Models = length(combinations),
  Model_Origin_Fits = length(combinations) * sum(vapply(windows_to_evaluate, function(w) {
    length(seq.int(w + 1L, nrow(selection_expense) - horizon + 1L))
  }, integer(1))),
  Workers = parallel_workers,
  Validation_Regressors = "Realized values within training period; conditional forecast evaluation",
  Holdout_Used = FALSE,
  stringsAsFactors = FALSE
)
write_csv(run_metadata, file.path(output_dir, "interest_expense_run_metadata.csv"))
