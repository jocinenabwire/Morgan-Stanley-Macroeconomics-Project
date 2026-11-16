required_packages <- c("dplyr", "readr", "lmtest")
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

getwd()

percent_log_growth <- function(x, variable_name) {
  if (any(x <= 0, na.rm = TRUE)) {
    stop(
      paste0("Cannot calculate log growth for non-positive values in ", variable_name, "."),
      call. = FALSE
    )
  }
  100 * (log(x) - log(dplyr::lag(x)))
}

selection_data <- read_csv("selection_data.csv") %>%
  arrange(Quarter)

# Create interest-rate interaction variables
selection_data <- selection_data %>%
  mutate(
    Rate_Trading = Rate_3M * Trading,   
    Rate_Repos = Rate_3M * Reverse_Repos,
    Rate_Loans = Rate_3M * Loans
  )

# Create quarterly interest-rate change
selection_data <- selection_data %>%
  mutate(Rate_Change = Rate_3M - lag(Rate_3M))

selection_data <- selection_data %>%
  mutate(
    # Stationary candidate alternatives; original level columns are retained.
    Loans_Change = Loans - lag(Loans),
    Reverse_Repos_Growth = percent_log_growth(Reverse_Repos, "Reverse_Repos"),
    Trading_Growth = percent_log_growth(Trading, "Trading"),
    Balances_Growth = percent_log_growth(Balances, "Balances"),
    Rate_Loans_Change = Rate_Loans - lag(Rate_Loans),
    Rate_Repos_Change = Rate_Repos - lag(Rate_Repos)
  )

selection_data <- selection_data %>%
  filter(!is.na(Rate_Change))

# 1. Settings
training_end <- "2024 Q2"
if (!training_end %in% selection_data$Quarter) {
  stop(paste("Approved training cutoff is missing:", training_end))
}
# The final holdout starts in 2024 Q3 and is excluded before model evaluation.
selection_data <- selection_data %>%
  filter(Quarter <= training_end)

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
if (nrow(selection_data) <= max_window + horizon - 1L) {
  stop("Not enough training observations for the approved windows and horizon.")
}
# Each window uses every eligible origin. The common-origin metrics compare
# every window on the same dates, beginning at the largest window length.
common_starts <- seq.int(max_window + 1L, nrow(selection_data) - horizon + 1L)
common_origin_quarters <- selection_data$Quarter[common_starts - 1L]
variables <- setdiff(names(selection_data), c("Quarter", "II"))
combinations <- unlist(
  lapply(4:6, function(k) combn(variables, k, simplify = FALSE)),
  recursive = FALSE
)

# A level series and its stationary alternative are separate candidates.
stationarity_alternatives <- list(
  c("Loans", "Loans_Change"),
  c("Reverse_Repos", "Reverse_Repos_Growth"),
  c("Trading", "Trading_Growth"),
  c("Balances", "Balances_Growth"),
  c("Rate_Loans", "Rate_Loans_Change"),
  c("Rate_Repos", "Rate_Repos_Change")
)
combinations <- Filter(function(x) {
  all(vapply(
    stationarity_alternatives,
    function(group) sum(group %in% x) <= 1,
    logical(1)
  )) &&
    (!"Rate_Trading" %in% x || all(c("Rate_3M", "Trading") %in% x)) &&
    (!"Rate_Repos" %in% x || all(c("Rate_3M", "Reverse_Repos") %in% x)) &&
    (!"Rate_Loans" %in% x || all(c("Rate_3M", "Loans") %in% x))
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

# 2. Collinearity diagnostics on each rolling training window
vif_limit <- 5
exclude_high_vif <- FALSE

calculate_vif <- function(train, selected) {
  x <- train[, selected, drop = FALSE]
  x <- x[complete.cases(x), , drop = FALSE]
  if (ncol(x) < 2) return(setNames(rep(1, ncol(x)), names(x)))
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

get_max_vif_by_origin <- function(selected, starts, window) {
  vapply(starts, function(start) {
    train <- selection_data[(start - window):(start - 1L), ]
    max(calculate_vif(train, selected), na.rm = TRUE)
  }, numeric(1))
}

# 3. Rolling-origin evaluation, with explicit tracking of failed/warned fits.
# Validation outcomes stay inside the approved training sample. Regressors in
# those validation quarters are realized values, so these scores are conditional
# on the ex-post predictor path rather than a real-time macro forecast.
evaluate_model <- function(selected, starts, window) {
  ape <- matrix(NA_real_, nrow = length(starts), ncol = horizon)
  absolute_error <- matrix(NA_real_, nrow = length(starts), ncol = horizon)
  failed_origins <- logical(length(starts))
  warned_origins <- logical(length(starts))

  for (j in seq_along(starts)) {
    start <- starts[j]
    tryCatch({
      train <- selection_data[(start - window):(start - 1L), ]
      validation <- selection_data[start:(start + horizon - 1L), ]
      model <- withCallingHandlers(
        lm(reformulate(selected, response = "II"), data = train),
        warning = function(w) {
          warned_origins[j] <<- TRUE
          invokeRestart("muffleWarning")
        }
      )
      last_II <- tail(train$II, 1L)
      for (h in seq_len(horizon)) {
        new_row <- validation[h, , drop = FALSE]
        if ("II_lag" %in% selected) new_row$II_lag <- last_II
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
        absolute_error[j, h] <- abs(validation$II[h] - prediction)
        ape[j, h] <- absolute_error[j, h] / abs(validation$II[h]) * 100
        last_II <- prediction
      }
    }, error = function(e) {
      failed_origins[j] <<- TRUE
    })
  }
  list(
    APE = ape,
    Absolute_Error = absolute_error,
    Failed_Origins = failed_origins,
    Warned_Origins = warned_origins
  )
}

summarize_errors <- function(errors, starts) {
  common_rows <- match(common_starts, starts)
  common_failed <- any(errors$Failed_Origins[common_rows])
  all_failed <- any(errors$Failed_Origins)
  common_ape <- errors$APE[common_rows, , drop = FALSE]
  common_ae <- errors$Absolute_Error[common_rows, , drop = FALSE]
  all_ape <- errors$APE
  all_ae <- errors$Absolute_Error
  c(
    Average_MAPE_Common = if (common_failed) NA_real_ else mean(common_ape),
    Average_MAE_Billions_Common = if (common_failed) NA_real_ else mean(common_ae) / 1e6,
    Average_MAPE_All = if (all_failed) NA_real_ else mean(all_ape),
    Average_MAE_Billions_All = if (all_failed) NA_real_ else mean(all_ae) / 1e6,
    Failed_Origins_Common = sum(errors$Failed_Origins[common_rows]),
    Failed_Origins_All = sum(errors$Failed_Origins),
    Warned_Origins_All = sum(errors$Warned_Origins),
    setNames(
      if (common_failed) rep(NA_real_, horizon) else colMeans(common_ape),
      paste0("MAPE_H", seq_len(horizon), "_Common")
    ),
    setNames(
      if (common_failed) rep(NA_real_, horizon) else colMeans(common_ae) / 1e6,
      paste0("MAE_Billions_H", seq_len(horizon), "_Common")
    )
  )
}

summarize_model <- function(selected, starts, window) {
  summarize_errors(evaluate_model(selected, starts, window), starts)
}

# 4. Search the same candidate models at each approved window length
run_window <- function(window) {
  window_started <- Sys.time()
  cat("\n[II] Window", window, "quarters:", length(combinations),
      "models across", length(seq.int(window + 1L, nrow(selection_data) - horizon + 1L)),
      "origins;", parallel_workers, "worker(s).\n")
  starts <- seq.int(window + 1L, nrow(selection_data) - horizon + 1L)
  vif_by_model <- parallel_map_rows(
    seq_along(combinations),
    function(i) get_max_vif_by_origin(combinations[[i]], starts, window)
  )
  colnames(vif_by_model) <- paste0("Max_VIF_Origin_", seq_along(starts))
  vif_summary <- data.frame(
    Variables = vapply(combinations, function(x) paste(x, collapse = " + "), character(1)),
    vif_by_model,
    stringsAsFactors = FALSE
  )
  vif_summary$Max_VIF <- apply(vif_by_model, 1L, max, na.rm = TRUE)
  vif_summary$VIF_Flag <- ifelse(
    vif_summary$Max_VIF > vif_limit,
    paste0("Review: VIF > ", vif_limit),
    "No flag"
  )
  keep <- if (exclude_high_vif) vif_summary$Max_VIF <= vif_limit else rep(TRUE, nrow(vif_summary))
  candidates <- combinations[keep]
  vif_summary <- vif_summary[keep, , drop = FALSE]

  results <- parallel_map_rows(seq_along(candidates), function(i) {
    metrics <- summarize_model(candidates[[i]], starts, window)
    data.frame(
      Window = window,
      Variables = paste(candidates[[i]], collapse = " + "),
      as.list(metrics),
      Max_VIF = vif_summary$Max_VIF[i],
      VIF_Flag = vif_summary$VIF_Flag[i],
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  results$Rank_MAPE_Common <- rank(results$Average_MAPE_Common, ties.method = "first", na.last = "keep")
  results$Rank_MAE_Common <- rank(results$Average_MAE_Billions_Common,
                                  ties.method = "first", na.last = "keep")
  results <- results[order(results$Rank_MAPE_Common, na.last = TRUE), , drop = FALSE]
  rownames(results) <- NULL
  saveRDS(results, file.path(output_dir, paste0("interest_income_window_", window, "_checkpoint.rds")))
  cat("[II] Window", window, "finished in",
      round(as.numeric(difftime(Sys.time(), window_started, units = "mins")), 1), "minute(s).\n")
  results
}

results_rolling <- do.call(rbind, lapply(windows_to_evaluate, run_window))
rownames(results_rolling) <- NULL
top50_mape_by_window <- do.call(rbind, lapply(windows_to_evaluate, function(window) {
  current <- results_rolling[results_rolling$Window == window, , drop = FALSE]
  head(current[order(current$Average_MAPE_Common, na.last = TRUE), , drop = FALSE], 50L)
}))
top50_mae_by_window <- do.call(rbind, lapply(windows_to_evaluate, function(window) {
  current <- results_rolling[results_rolling$Window == window, , drop = FALSE]
  head(current[order(current$Average_MAE_Billions_Common, na.last = TRUE), , drop = FALSE], 50L)
}))
top50_mape_all <- head(results_rolling[order(results_rolling$Average_MAPE_Common, na.last = TRUE), , drop = FALSE], 50L)
top50_mae_all <- head(results_rolling[order(results_rolling$Average_MAE_Billions_Common, na.last = TRUE), , drop = FALSE], 50L)
write_csv(results_rolling, file.path(output_dir, "interest_income_all_model_results.csv"))
saveRDS(results_rolling, file.path(output_dir, "interest_income_all_model_results.rds"))
write_csv(top50_mape_by_window, file.path(output_dir, "interest_income_top50_mape_by_window.csv"))
write_csv(top50_mae_by_window, file.path(output_dir, "interest_income_top50_mae_by_window.csv"))
write_csv(top50_mape_all, file.path(output_dir, "interest_income_top50_mape_all_windows.csv"))
write_csv(top50_mae_all, file.path(output_dir, "interest_income_top50_mae_all_windows.csv"))
if (interactive()) View(top50_mape_by_window)

# 5. Plot the best model at the latest training-only origin for each window
forecast_one_origin <- function(selected, start, window) {
  train <- selection_data[(start - window):(start - 1L), ]
  validation <- selection_data[start:(start + horizon - 1L), ]
  model <- lm(reformulate(selected, response = "II"), data = train)
  last_II <- tail(train$II, 1L)
  predicted <- numeric(horizon)
  for (h in seq_len(horizon)) {
    new_row <- validation[h, , drop = FALSE]
    if ("II_lag" %in% selected) new_row$II_lag <- last_II
    predicted[h] <- as.numeric(predict(model, newdata = new_row))
    last_II <- predicted[h]
  }
  data.frame(Quarter = validation$Quarter, Actual = validation$II,
             Forecast = predicted, Horizon = seq_len(horizon))
}

latest_start <- tail(common_starts, 1L)
if (interactive()) {
  for (window in windows_to_evaluate) {
    current <- results_rolling[results_rolling$Window == window, , drop = FALSE]
    current <- current[order(current$Rank_MAPE_Common), , drop = FALSE]
    if (!nrow(current) || !is.finite(current$Average_MAPE_Common[1L])) next
    selected <- trimws(strsplit(current$Variables[1L], " + ", fixed = TRUE)[[1]])
    latest_forecast <- forecast_one_origin(selected, latest_start, window)
    plot(latest_forecast$Horizon, latest_forecast$Actual / 1e6,
         type = "l", col = "black", lwd = 2,
         ylim = range(c(latest_forecast$Actual, latest_forecast$Forecast)) / 1e6,
         xaxt = "n", xlab = "Forecast horizon (quarters)",
         ylab = "Interest Income (USD billions)",
         main = paste0("Window ", window, "; origin ", selection_data$Quarter[latest_start - 1L],
                       "; common-origin MAPE ", round(current$Average_MAPE_Common[1L], 2), "%"))
    lines(latest_forecast$Horizon, latest_forecast$Forecast / 1e6,
          col = "blue", lwd = 2, lty = 2)
    axis(1, at = seq_len(horizon), labels = latest_forecast$Quarter)
    legend("topleft", legend = c("Actual", "Forecast"),
           col = c("black", "blue"), lty = c(1, 2), lwd = 2, bty = "n")
  }
}

# 6. Compare the top three models with and without Rate_Change per window
comparison_change <- do.call(rbind, lapply(windows_to_evaluate, function(window) {
  current <- results_rolling[results_rolling$Window == window, , drop = FALSE]
  current <- current[order(current$Rank_MAPE_Common), , drop = FALSE]
  current <- head(current[is.finite(current$Average_MAPE_Common), , drop = FALSE], 3L)
  if (!nrow(current)) return(NULL)
  do.call(rbind, lapply(seq_len(nrow(current)), function(i) {
    original <- trimws(strsplit(current$Variables[i], " + ", fixed = TRUE)[[1]])
    if ("Rate_Change" %in% original) {
      return(data.frame(Window = window, Model = i, Status = "Already contains Rate_Change",
                        Original_MAPE_Common = NA_real_, With_Rate_Change_MAPE_Common = NA_real_,
                        Original_MAE_Billions_Common = NA_real_, With_Rate_Change_MAE_Billions_Common = NA_real_))
    }
    if (length(original) >= 6L) {
      return(data.frame(Window = window, Model = i, Status = "Skipped: six-variable cap",
                        Original_MAPE_Common = NA_real_, With_Rate_Change_MAPE_Common = NA_real_,
                        Original_MAE_Billions_Common = NA_real_, With_Rate_Change_MAE_Billions_Common = NA_real_))
    }
    extended <- unique(c(original, "Rate_Change"))
    starts <- seq.int(window + 1L, nrow(selection_data) - horizon + 1L)
    old <- summarize_errors(evaluate_model(original, starts, window), starts)
    new <- summarize_errors(evaluate_model(extended, starts, window), starts)
    data.frame(
      Window = window, Model = i, Status = "Compared",
      Original_MAPE_Common = old["Average_MAPE_Common"],
      With_Rate_Change_MAPE_Common = new["Average_MAPE_Common"],
      Original_MAE_Billions_Common = old["Average_MAE_Billions_Common"],
      With_Rate_Change_MAE_Billions_Common = new["Average_MAE_Billions_Common"]
    )
  }))
}))
write_csv(comparison_change, file.path(output_dir, "interest_income_rate_change_comparison.csv"))
if (interactive()) View(comparison_change)

# 7. Compare the fixed benchmark with and without Rate_Change across windows
original <- c("II_lag", "Rate_3M", "Rate_10Y", "Loans", "Rate_Loans")
extended <- c(original, "Rate_Change")
rate_change_origin_comparison <- do.call(rbind, lapply(windows_to_evaluate, function(window) {
  starts <- seq.int(window + 1L, nrow(selection_data) - horizon + 1L)
  common_rows <- match(common_starts, starts)
  original_result <- evaluate_model(original, starts, window)
  extended_result <- evaluate_model(extended, starts, window)
  data.frame(
    Window = window,
    Origin = common_origin_quarters,
    Original_MAPE = rowMeans(original_result$APE[common_rows, , drop = FALSE]),
    With_Rate_Change_MAPE = rowMeans(extended_result$APE[common_rows, , drop = FALSE]),
    Original_MAE_Billions = rowMeans(original_result$Absolute_Error[common_rows, , drop = FALSE]) / 1e6,
    With_Rate_Change_MAE_Billions = rowMeans(extended_result$Absolute_Error[common_rows, , drop = FALSE]) / 1e6
  )
}))
write_csv(rate_change_origin_comparison,
          file.path(output_dir, "interest_income_rate_change_origin_comparison.csv"))
if (interactive()) View(rate_change_origin_comparison)

run_metadata <- data.frame(
  Component = "Interest Income",
  Training_First_Quarter = selection_data$Quarter[1L],
  Training_Last_Quarter = tail(selection_data$Quarter, 1L),
  Training_Observations = nrow(selection_data),
  Horizon_Quarters = horizon,
  Windows = paste(windows_to_evaluate, collapse = ", "),
  Common_Origins = length(common_starts),
  Candidate_Models = length(combinations),
  Model_Origin_Fits = length(combinations) * sum(vapply(windows_to_evaluate, function(w) {
    length(seq.int(w + 1L, nrow(selection_data) - horizon + 1L))
  }, integer(1))),
  Workers = parallel_workers,
  Validation_Regressors = "Realized values within training period; conditional forecast evaluation",
  Holdout_Used = FALSE,
  stringsAsFactors = FALSE
)
write_csv(run_metadata, file.path(output_dir, "interest_income_run_metadata.csv"))
