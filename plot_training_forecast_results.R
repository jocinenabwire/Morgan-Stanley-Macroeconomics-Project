# Create shareable accuracy figures from the approved training-only backtests.
# Run from the repository root. This script never reads the holdout split.

training_end <- "2024 Q2"
horizon <- 9L
target_error_pct <- 25
output_dir <- file.path("results", "forecast_accuracy_figures_20261116")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

required_files <- c(
  "data_splits/train_y9c_outcomes_2009Q1_2024Q2.csv",
  "selection_data.csv",
  "Morgan_Stanley_PPNR_Quarterly_2009_2026.csv",
  "Morgan_Stanley_Balance_Sheet.csv",
  "2025-Table_1A_Historic_Domestic.csv",
  "results/var_window_sensitivity_training_20261116/ppnr_component_composite_backtest_forecasts.csv",
  "results/var_window_sensitivity_training_20261116/ppnr_performance_by_window.csv"
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop("Required file(s) missing: ", paste(missing_files, collapse = ", "), call. = FALSE)
}

model_result_paths <- function(path) {
  if (file.exists(path)) return(path)
  window_parts <- Sys.glob(sub(
    "_all_model_results\\.csv$", "_all_model_results_window_*.csv.gz", path
  ))
  if (length(window_parts)) return(window_parts)
  compressed_path <- paste0(path, ".gz")
  if (file.exists(compressed_path)) return(compressed_path)
  character()
}
read_model_results <- function(path) {
  paths <- model_result_paths(path)
  if (!length(paths)) stop("No saved model result file found for ", path, call. = FALSE)
  pieces <- lapply(paths, function(file) utils::read.csv(
    file, stringsAsFactors = FALSE, check.names = FALSE
  ))
  do.call(rbind, pieces)
}
model_result_inputs <- c(
  "results/rolling_backtest/interest_income_all_model_results.csv",
  "results/rolling_backtest/interest_expense_all_model_results.csv"
)
missing_model_results <- model_result_inputs[!vapply(
  model_result_inputs, function(path) length(model_result_paths(path)) > 0L, logical(1)
)]
if (length(missing_model_results)) {
  stop("Required saved model results missing: ", paste(missing_model_results, collapse = ", "), call. = FALSE)
}

quarter_from_date <- function(x) {
  d <- as.Date(x)
  paste0(format(d, "%Y"), " Q", ((as.integer(format(d, "%m")) - 1L) %/% 3L) + 1L)
}
lag_one <- function(x) c(NA_real_, head(as.numeric(x), -1L))
log_growth <- function(x, variable_name) {
  x <- as.numeric(x)
  if (any(x <= 0, na.rm = TRUE)) {
    stop("Cannot calculate log growth for non-positive values in ", variable_name, call. = FALSE)
  }
  100 * (log(x) - log(lag_one(x)))
}
assert_training_end <- function(x, label) {
  if (!length(x) || tail(x, 1L) != training_end || any(x > training_end)) {
    stop(label, " did not end exactly at the approved training cutoff ", training_end, call. = FALSE)
  }
}
assert_close <- function(x, y, tolerance, label) {
  if (length(x) != length(y) || any(!is.finite(x)) || any(!is.finite(y)) ||
      max(abs(x - y)) > tolerance) {
    stop(label, " did not reconcile within tolerance ", tolerance, call. = FALSE)
  }
}

# Use only the explicitly approved Y-9C training split for actual outcomes.
train_y9c <- read.csv(
  "data_splits/train_y9c_outcomes_2009Q1_2024Q2.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
train_y9c$Quarter <- quarter_from_date(train_y9c$Date)
assert_training_end(train_y9c$Quarter, "Y-9C training outcomes")
if (any(abs(train_y9c$PPNR) < .Machine$double.eps)) {
  stop("MAPE is undefined for a zero actual PPNR observation.", call. = FALSE)
}
ppnr_identity <- with(train_y9c,
  `Total interest income` - `Total interest expense` +
    `Total noninterest income` - `Total noninterest expense`
)
assert_close(ppnr_identity, train_y9c$PPNR, 1e-8, "Y-9C PPNR accounting identity")

# Recover the exact W32-selected VAR composite forecasts and independently
# calculate MAPE and the fraction of individual forecasts within 25%.
var_ppnr <- read.csv(
  "results/var_window_sensitivity_training_20261116/ppnr_component_composite_backtest_forecasts.csv",
  stringsAsFactors = FALSE
)
if (any(!var_ppnr$Quarter %in% train_y9c$Quarter) || any(var_ppnr$Window %in% c(20, 16, 12))) {
  stop("VAR forecast file contains dates or windows outside the approved training sensitivity.", call. = FALSE)
}
if (any(!is.finite(var_ppnr$Actual_PPNR)) || any(abs(var_ppnr$Actual_PPNR) < .Machine$double.eps)) {
  stop("The PPNR forecast rows contain non-finite or zero actuals; MAPE cannot be computed safely.", call. = FALSE)
}

forecast_rows <- list()
add_forecasts <- function(model, series, origin, horizon_index, quarter, actual, forecast) {
  z <- data.frame(
    Model = model, Series = series, Origin = as.character(origin),
    Horizon = as.integer(horizon_index), Quarter = as.character(quarter),
    Actual_Billions = as.numeric(actual), Forecast_Billions = as.numeric(forecast),
    stringsAsFactors = FALSE
  )
  z$Absolute_Percent_Error <- abs(z$Actual_Billions - z$Forecast_Billions) /
    abs(z$Actual_Billions) * 100
  forecast_rows[[length(forecast_rows) + 1L]] <<- z
  invisible(z)
}

for (window in c(24L, 28L, 32L)) {
  d <- var_ppnr[var_ppnr$Window == window, , drop = FALSE]
  if (nrow(d) != 20L * horizon) {
    stop("Expected 20 complete rolling origins for VAR window ", window, call. = FALSE)
  }
  add_forecasts(
    paste0("VAR W", window), "PPNR", d$Origin_Index, d$Horizon, d$Quarter,
    d$Actual_PPNR, d$Forecast_PPNR
  )
}

# Add the common PPNR random-walk benchmark once (not once per VAR window).
rw_ref <- var_ppnr[var_ppnr$Window == 32L, , drop = FALSE]
origin_row <- as.integer(rw_ref$Origin_Index)
if (anyNA(origin_row) || any(origin_row < 1L | origin_row > nrow(train_y9c))) {
  stop("A VAR origin index does not map to the Y-9C training file.", call. = FALSE)
}
# A component-wise level random walk equals a PPNR level random walk because
# the PPNR accounting identity is additive.
random_walk_ppnr <- train_y9c$PPNR[origin_row] / 1e6
add_forecasts(
  "Random walk", "PPNR", rw_ref$Origin_Index, rw_ref$Horizon, rw_ref$Quarter,
  rw_ref$Actual_PPNR, random_walk_ppnr
)

# Recompute the earlier PPNR MASE, MAE, and RMSE outputs from the forecast rows.
# This checks that the MAPE denominator/units are attached to the same forecasts.
saved_ppnr_summary <- read.csv(
  "results/var_window_sensitivity_training_20261116/ppnr_performance_by_window.csv",
  stringsAsFactors = FALSE
)
for (window in c(24L, 28L, 32L)) {
  d <- var_ppnr[var_ppnr$Window == window, , drop = FALSE]
  saved_var <- saved_ppnr_summary[
    saved_ppnr_summary$Window == window &
      saved_ppnr_summary$Forecast_Type == "Selected component-pair VAR composite", , drop = FALSE
  ]
  saved_rw <- saved_ppnr_summary[
    saved_ppnr_summary$Window == window &
      saved_ppnr_summary$Forecast_Type == "Random walk PPNR benchmark", , drop = FALSE
  ]
  origin_row <- as.integer(d$Origin_Index)
  rw_forecast <- train_y9c$PPNR[origin_row] / 1e6
  var_mae <- mean(abs(d$Error_PPNR))
  var_rmse <- sqrt(mean(d$Error_PPNR^2))
  var_mase <- mean(abs(d$Error_PPNR) / d$MASE_Scale_PPNR)
  rw_error <- d$Actual_PPNR - rw_forecast
  rw_mae <- mean(abs(rw_error))
  rw_rmse <- sqrt(mean(rw_error^2))
  rw_mase <- mean(abs(rw_error) / d$MASE_Scale_PPNR)
  assert_close(c(var_mae, var_rmse, var_mase),
    c(saved_var$MAE_Billions, saved_var$RMSE_Billions, saved_var$MASE),
    1e-8, paste("VAR PPNR metrics W", window)
  )
  assert_close(c(rw_mae, rw_rmse, rw_mase),
    c(saved_rw$MAE_Billions, saved_rw$RMSE_Billions, saved_rw$MASE),
    1e-8, paste("Random walk PPNR metrics W", window)
  )
}

# Add component-level W32 forecasts from the same four Y-9C components.
var_w32 <- var_ppnr[var_ppnr$Window == 32L, , drop = FALSE]
for (component in c("II", "IE", "NIR", "NIE")) {
  add_forecasts(
    paste0("VAR W32 selected ", component), component,
    var_w32$Origin_Index, var_w32$Horizon, var_w32$Quarter,
    var_w32[[paste0("Actual_", component)]],
    var_w32[[paste0("Forecast_", component)]]
  )
}

# Recreate the regression input panels strictly through 2024 Q2. The income
# panel is the same prepared source used by Modeling_2.R. For expense, rebuild
# predictors from raw source rows truncated before the holdout begins.
income_data <- read.csv("selection_data.csv", nrows = 61L, stringsAsFactors = FALSE)
income_data <- income_data[order(income_data$Quarter), , drop = FALSE]
assert_training_end(income_data$Quarter, "Interest-income predictor panel")
income_data$Rate_Trading <- income_data$Rate_3M * income_data$Trading
income_data$Rate_Repos <- income_data$Rate_3M * income_data$Reverse_Repos
income_data$Rate_Loans <- income_data$Rate_3M * income_data$Loans
income_data$Rate_Change <- income_data$Rate_3M - lag_one(income_data$Rate_3M)
income_data$Loans_Change <- income_data$Loans - lag_one(income_data$Loans)
income_data$Reverse_Repos_Growth <- log_growth(income_data$Reverse_Repos, "Reverse_Repos")
income_data$Trading_Growth <- log_growth(income_data$Trading, "Trading")
income_data$Balances_Growth <- log_growth(income_data$Balances, "Balances")
income_data$Rate_Loans_Change <- income_data$Rate_Loans - lag_one(income_data$Rate_Loans)
income_data$Rate_Repos_Change <- income_data$Rate_Repos - lag_one(income_data$Rate_Repos)
income_data <- income_data[!is.na(income_data$Rate_Change) & income_data$Quarter <= training_end, , drop = FALSE]
income_match <- match(income_data$Quarter, train_y9c$Quarter)
if (anyNA(income_match)) stop("Interest-income panel dates do not match Y-9C training.", call. = FALSE)
assert_close(income_data$II, train_y9c$`Total interest income`[income_match], 0,
  "Interest-income response versus Y-9C training outcomes")

quarterly_ppnr <- read.csv(
  "Morgan_Stanley_PPNR_Quarterly_2009_2026.csv", nrows = 62L,
  stringsAsFactors = FALSE, check.names = FALSE
)
balance_sheet <- read.csv(
  "Morgan_Stanley_Balance_Sheet.csv", nrows = 62L,
  stringsAsFactors = FALSE, check.names = FALSE
)
macro_actual <- read.csv(
  "2025-Table_1A_Historic_Domestic.csv", nrows = 194L,
  stringsAsFactors = FALSE, check.names = FALSE
)
quarterly_ppnr$Quarter <- quarter_from_date(quarterly_ppnr$Date)
assert_training_end(quarterly_ppnr$Quarter, "Quarterly Y-9C source rows")
assert_training_end(balance_sheet$Date, "Balance-sheet source rows")
assert_training_end(macro_actual$Date, "Domestic macro source rows")
if (tail(macro_actual[["Scenario Name"]], 1L) != "Actual") {
  stop("The macro rows through the training cutoff are not all historical Actual rows.", call. = FALSE)
}

expense <- data.frame(
  Quarter = quarterly_ppnr$Quarter,
  IE = as.numeric(quarterly_ppnr[["Total interest expense"]]),
  stringsAsFactors = FALSE
)
expense$IE_lag <- lag_one(expense$IE)
balance <- data.frame(
  Quarter = balance_sheet$Date,
  Deposits_US = as.numeric(balance_sheet[["Interest-bearing domestic deposits"]]),
  Deposits_Foreign = as.numeric(balance_sheet[["Interest-bearing foreign deposits"]]),
  Repos = as.numeric(balance_sheet$Repos),
  Other_Borrowing = as.numeric(balance_sheet[["Other borrowed money"]]),
  Subordinated_Debt = as.numeric(balance_sheet[["Subordinated debt"]]),
  Trading_Liabilities = as.numeric(balance_sheet[["Trading liabilities"]]),
  Total_Liabilities = as.numeric(balance_sheet[["Total liabilities"]]),
  stringsAsFactors = FALSE
)
rates <- data.frame(
  Quarter = macro_actual$Date,
  Rate_3M = as.numeric(macro_actual[["3-month Treasury rate"]]),
  Rate_10Y = as.numeric(macro_actual[["10-year Treasury yield"]]),
  BBB_Yield = as.numeric(macro_actual[["BBB corporate yield"]]),
  VIX = as.numeric(macro_actual[["Market Volatility Index (Level)"]]),
  stringsAsFactors = FALSE
)
rates <- rates[order(rates$Quarter), , drop = FALSE]
rates$Rate_Change <- rates$Rate_3M - lag_one(rates$Rate_3M)
expense_data <- merge(expense, balance, by = "Quarter", all.x = TRUE, sort = TRUE)
expense_data <- merge(expense_data, rates, by = "Quarter", all.x = TRUE, sort = TRUE)
expense_data <- expense_data[order(expense_data$Quarter), , drop = FALSE]
expense_data$Rate_Deposits <- expense_data$Rate_3M * expense_data$Deposits_US
expense_data$Rate_Repos <- expense_data$Rate_3M * expense_data$Repos
expense_data$Rate_Borrowing <- expense_data$Rate_3M * expense_data$Other_Borrowing
expense_data$Deposits_US_Growth <- log_growth(expense_data$Deposits_US, "Deposits_US")
expense_data$Repos_Growth <- log_growth(expense_data$Repos, "Repos")
expense_data$Other_Borrowing_Growth <- log_growth(expense_data$Other_Borrowing, "Other_Borrowing")
expense_data$Subordinated_Debt_Growth <- log_growth(expense_data$Subordinated_Debt, "Subordinated_Debt")
expense_data$Rate_Deposits_Change <- expense_data$Rate_Deposits - lag_one(expense_data$Rate_Deposits)
expense_data$Rate_Repos_Change <- expense_data$Rate_Repos - lag_one(expense_data$Rate_Repos)
expense_data <- expense_data[complete.cases(expense_data), , drop = FALSE]
expense_data <- expense_data[expense_data$Quarter <= training_end, , drop = FALSE]
assert_training_end(expense_data$Quarter, "Interest-expense prepared panel")
expense_match <- match(expense_data$Quarter, train_y9c$Quarter)
if (anyNA(expense_match)) stop("Interest-expense panel dates do not match Y-9C training.", call. = FALSE)
assert_close(expense_data$IE, train_y9c$`Total interest expense`[expense_match], 0,
  "Interest-expense response versus Y-9C training outcomes")

select_regression_specs <- function(component, path) {
  result <- read_model_results(path)
  result <- result[result$Window == 32L & is.finite(result$Average_MAPE_Common), , drop = FALSE]
  if (!nrow(result)) stop("No W32 regression results found for ", component, call. = FALSE)
  best <- result[which.min(result$Average_MAPE_Common), , drop = FALSE]
  low_vif <- result[is.finite(result$Max_VIF) & result$Max_VIF <= 5, , drop = FALSE]
  if (!nrow(low_vif)) stop("No W32 model with VIF <= 5 found for ", component, call. = FALSE)
  low_vif <- low_vif[which.min(low_vif$Average_MAPE_Common), , drop = FALSE]
  specs <- rbind(best, low_vif)
  specs$Selection_Label <- c("Best MAPE", "Best MAPE among VIF <= 5")
  specs <- specs[!duplicated(specs$Variables), , drop = FALSE]
  specs$Component <- component
  specs
}

regression_specs <- rbind(
  select_regression_specs(
    "II", "results/rolling_backtest/interest_income_all_model_results.csv"
  ),
  select_regression_specs(
    "IE", "results/rolling_backtest/interest_expense_all_model_results.csv"
  )
)
regression_data <- list(II = income_data, IE = expense_data)
regression_forecasts <- list()
for (i in seq_len(nrow(regression_specs))) {
  spec <- regression_specs[i, , drop = FALSE]
  component <- spec$Component[1L]
  target <- component
  data <- regression_data[[component]]
  predictors <- trimws(strsplit(spec$Variables[1L], " + ", fixed = TRUE)[[1L]])
  starts <- seq.int(32L + 1L, nrow(data) - horizon + 1L)
  model_rows <- vector("list", length(starts))
  for (j in seq_along(starts)) {
    start <- starts[j]
    training <- data[(start - 32L):(start - 1L), , drop = FALSE]
    validation <- data[start:(start + horizon - 1L), , drop = FALSE]
    fit <- stats::lm(stats::reformulate(predictors, response = target), data = training)
    last_actual <- tail(training[[target]], 1L)
    prediction <- numeric(horizon)
    lag_name <- paste0(target, "_lag")
    for (h in seq_len(horizon)) {
      new_row <- validation[h, , drop = FALSE]
      if (lag_name %in% predictors) new_row[[lag_name]] <- last_actual
      prediction[h] <- as.numeric(stats::predict(fit, newdata = new_row))
      if (!is.finite(prediction[h])) {
        stop("Non-finite regression replay for ", component, ".", call. = FALSE)
      }
      last_actual <- prediction[h]
    }
    model_rows[[j]] <- data.frame(
      Component = component, Model = spec$Selection_Label[1L],
      Variables = spec$Variables[1L], Max_VIF = spec$Max_VIF[1L],
      Origin = data$Quarter[start - 1L],
      Horizon = seq_len(horizon), Quarter = validation$Quarter,
      Actual = validation[[target]], Forecast = prediction,
      stringsAsFactors = FALSE
    )
  }
  replay <- do.call(rbind, model_rows)
  replay$APE_pct <- abs(replay$Actual - replay$Forecast) / abs(replay$Actual) * 100
  replay$Actual_Billions <- replay$Actual / 1e6
  replay$Forecast_Billions <- replay$Forecast / 1e6
  stored_mape <- spec$Average_MAPE_Common[1L]
  replay_mape <- mean(replay$APE_pct)
  if (abs(stored_mape - replay_mape) > 1e-6) {
    stop(
      "Regression replay does not reproduce the stored common-origin MAPE for ",
      component, " / ", spec$Selection_Label[1L],
      ": saved=", stored_mape, ", replay=", replay_mape,
      call. = FALSE
    )
  }
  regression_forecasts[[length(regression_forecasts) + 1L]] <- replay
}
regression_forecasts <- do.call(rbind, regression_forecasts)
rownames(regression_forecasts) <- NULL

for (i in seq_len(nrow(regression_forecasts))) {
  row <- regression_forecasts[i, , drop = FALSE]
  add_forecasts(
    paste(row$Component, row$Model), row$Component, row$Origin,
    row$Horizon, row$Quarter, row$Actual_Billions, row$Forecast_Billions
  )
}

all_forecasts <- do.call(rbind, forecast_rows)
rownames(all_forecasts) <- NULL
summarize_accuracy <- function(d) {
  ape <- d$Absolute_Percent_Error
  by_origin <- split(ape <= target_error_pct, d$Origin)
  all_horizon_paths <- vapply(by_origin, function(x) length(x) == horizon && all(x), logical(1))
  row <- data.frame(
    Model = d$Model[1L], Series = d$Series[1L],
    Forecasts = nrow(d), Origins = length(unique(d$Origin)),
    MAPE_pct = mean(ape),
    Forecasts_within_25_pct = 100 * mean(ape <= target_error_pct),
    Origins_all_9_within_25_pct = 100 * mean(all_horizon_paths),
    Maximum_absolute_percentage_error_pct = max(ape),
    stringsAsFactors = FALSE
  )
  h <- data.frame(
    Model = d$Model[1L], Series = d$Series[1L], Horizon = seq_len(horizon),
    MAPE_pct = vapply(seq_len(horizon), function(k) mean(ape[d$Horizon == k]), numeric(1)),
    Forecasts_within_25_pct = vapply(seq_len(horizon), function(k) {
      100 * mean(ape[d$Horizon == k] <= target_error_pct)
    }, numeric(1)),
    stringsAsFactors = FALSE
  )
  list(summary = row, by_horizon = h)
}
groups <- split(all_forecasts, paste(all_forecasts$Series, all_forecasts$Model, sep = " || "))
accuracy <- lapply(groups, summarize_accuracy)
accuracy_summary <- do.call(rbind, lapply(accuracy, `[[`, "summary"))
accuracy_horizon <- do.call(rbind, lapply(accuracy, `[[`, "by_horizon"))
rownames(accuracy_summary) <- NULL
rownames(accuracy_horizon) <- NULL
utils::write.csv(accuracy_summary, file.path(output_dir, "forecast_accuracy_summary.csv"), row.names = FALSE)
utils::write.csv(accuracy_horizon, file.path(output_dir, "forecast_accuracy_by_horizon.csv"), row.names = FALSE)
utils::write.csv(all_forecasts, file.path(output_dir, "training_forecast_rows.csv"), row.names = FALSE)
utils::write.csv(regression_specs, file.path(output_dir, "selected_regression_specs_w32.csv"), row.names = FALSE)
utils::write.csv(regression_forecasts, file.path(output_dir, "regression_selected_forecasts_w32.csv"), row.names = FALSE)

render_png <- function(path, draw, width = 1800L, height = 1050L, res = 160L) {
  if (interactive()) draw()
  grDevices::png(path, width = width, height = height, res = res)
  draw()
  grDevices::dev.off()
}
get_group <- function(series, model) {
  all_forecasts[all_forecasts$Series == series & all_forecasts$Model == model, , drop = FALSE]
}
mape_at_horizon <- function(d) {
  vapply(seq_len(horizon), function(h) {
    mean(d$Absolute_Percent_Error[d$Horizon == h])
  }, numeric(1))
}

# PPNR MAPE across all 20 origins (180 forecast rows per window).
ppnr_models <- c("VAR W24", "VAR W28", "VAR W32", "Random walk")
ppnr_colors <- c("#3B6FB6", "#D9822B", "#238443", "#656565")
draw_ppnr_mape <- function() {
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(mar = c(5, 5, 4, 1), mgp = c(2.8, 0.8, 0))
  curves <- lapply(ppnr_models, function(model) mape_at_horizon(get_group("PPNR", model)))
  y_max <- max(unlist(curves), target_error_pct) * 1.12
  graphics::plot(seq_len(horizon), curves[[1L]], type = "b", pch = 16,
    lwd = 2, col = ppnr_colors[1L], ylim = c(0, y_max), xaxt = "n",
    xlab = "Forecast horizon (quarters)", ylab = "Mean absolute percentage error (%)",
    main = "PPNR rolling forecasts: error by horizon")
  graphics::axis(1, at = seq_len(horizon))
  for (i in 2:length(curves)) graphics::lines(seq_len(horizon), curves[[i]],
    type = "b", pch = 15 + i, lwd = 2, col = ppnr_colors[i])
  graphics::abline(h = target_error_pct, col = "#B22222", lty = 2, lwd = 2)
  legend_text <- vapply(ppnr_models, function(model) {
    d <- get_group("PPNR", model)
    paste0(model, " (overall MAPE ", sprintf("%.1f", mean(d$Absolute_Percent_Error)), "%)")
  }, character(1))
  graphics::legend("topleft", legend = c(legend_text, "25% target"),
    col = c(ppnr_colors, "#B22222"), lty = c(rep(1, length(ppnr_models)), 2),
    pch = c(rep(16, length(ppnr_models)), NA), lwd = 2, bty = "n", cex = 0.85)
  graphics::mtext("Training-only: 20 rolling origins x 9 quarters; overlapping forecasts counted separately",
    side = 3, line = 0.35, cex = 0.8)
}
render_png(file.path(output_dir, "ppnr_mape_by_horizon.png"), draw_ppnr_mape)

# One readable nine-quarter path from the final internal training origin.
draw_ppnr_latest <- function() {
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(mar = c(11, 5, 4, 1), mgp = c(2.8, 0.8, 0))
  d32 <- var_ppnr[var_ppnr$Window == 32L, , drop = FALSE]
  latest_origin <- max(d32$Origin_Index)
  actual <- d32[d32$Origin_Index == latest_origin, , drop = FALSE]
  actual <- actual[order(actual$Horizon), , drop = FALSE]
  if (nrow(actual) != horizon) stop("Latest-origin PPNR path is incomplete.", call. = FALSE)
  y <- c(actual$Actual_PPNR)
  for (window in c(24L, 28L, 32L)) {
    d <- var_ppnr[var_ppnr$Window == window & var_ppnr$Origin_Index == latest_origin, , drop = FALSE]
    y <- c(y, d$Forecast_PPNR)
  }
  tr_idx <- as.integer(latest_origin)
  rw <- rep(train_y9c$PPNR[tr_idx] / 1e6, horizon)
  y <- c(y, rw)
  graphics::plot(seq_len(horizon), actual$Actual_PPNR, type = "b", pch = 16,
    lwd = 2.5, col = "black", ylim = range(y, finite = TRUE), xaxt = "n",
    xlab = "", ylab = "PPNR (USD billions)",
    main = paste0("PPNR: origin ", train_y9c$Quarter[tr_idx], "; forecast ",
      actual$Quarter[1L], " to ", tail(actual$Quarter, 1L)))
  graphics::axis(1, at = seq_len(horizon), labels = actual$Quarter, las = 2, cex.axis = 0.8)
  graphics::mtext("Forecast quarter", side = 1, line = 9, cex = 1)
  for (i in seq_along(c(24L, 28L, 32L))) {
    window <- c(24L, 28L, 32L)[i]
    d <- var_ppnr[var_ppnr$Window == window & var_ppnr$Origin_Index == latest_origin, , drop = FALSE]
    d <- d[order(d$Horizon), , drop = FALSE]
    graphics::lines(seq_len(horizon), d$Forecast_PPNR, type = "b", pch = 15 + i,
      lwd = 2, col = ppnr_colors[i])
  }
  graphics::lines(seq_len(horizon), rw, type = "l", lty = 3, lwd = 2, col = ppnr_colors[4L])
  graphics::legend("topleft", legend = c("Actual", "VAR W24", "VAR W28", "VAR W32", "Random walk"),
    col = c("black", ppnr_colors), lty = c(1, 1, 1, 1, 3),
    pch = c(16, 16, 17, 18, NA), lwd = 2, bty = "n")
  graphics::mtext("The actual path ends at the training cutoff; the final holdout is excluded.",
    side = 3, line = 0.35, cex = 0.8)
}
render_png(file.path(output_dir, "ppnr_latest_training_origin.png"), draw_ppnr_latest)

# Component-level VAR MAPE explains why component scores need not imply a good
# net PPNR score. Each panel uses its own scale, with the same 25% reference.
draw_var_components <- function() {
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(mfrow = c(2, 2), mar = c(4, 4.5, 3, 1),
    oma = c(0, 0, 3, 0), mgp = c(2.5, 0.7, 0))
  component_colors <- c(II = "#3B6FB6", IE = "#D9822B", NIR = "#238443", NIE = "#756BB1")
  for (component in c("II", "IE", "NIR", "NIE")) {
    model <- paste0("VAR W32 selected ", component)
    d <- get_group(component, model)
    curve <- mape_at_horizon(d)
    overall <- mean(d$Absolute_Percent_Error)
    graphics::plot(seq_len(horizon), curve, type = "b", pch = 16, lwd = 2,
      col = component_colors[component], ylim = c(0, max(curve, target_error_pct) * 1.12),
      xlab = "Horizon (quarters)", ylab = "MAPE (%)", xaxt = "n",
      main = paste0(component, ": overall MAPE ", sprintf("%.1f", overall), "%"))
    graphics::axis(1, at = seq_len(horizon))
    graphics::abline(h = target_error_pct, col = "#B22222", lty = 2, lwd = 2)
  }
  graphics::mtext("Selected W32 VAR component forecasts; training-only rolling origins",
    outer = TRUE, line = 1, cex = 0.9)
}
render_png(file.path(output_dir, "var_w32_component_mape_by_horizon.png"),
  draw_var_components, width = 1800L, height = 1200L)

# Regression MAPE at each horizon: unrestricted best MAPE and best MAPE among
# candidates whose rolling-window VIF summary is <= 5.
draw_regression_mape <- function() {
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(mfrow = c(1, 2), mar = c(5, 4.7, 4, 1), mgp = c(2.7, 0.75, 0))
  for (component in c("II", "IE")) {
    d0 <- regression_forecasts[regression_forecasts$Component == component, , drop = FALSE]
    models <- unique(d0$Model)
    colors <- c("#3B6FB6", "#D9822B")
    curves <- lapply(models, function(m) {
      d <- d0[d0$Model == m, , drop = FALSE]
      vapply(seq_len(horizon), function(h) mean(d$APE_pct[d$Horizon == h]), numeric(1))
    })
    y_max <- max(unlist(curves), target_error_pct) * 1.12
    graphics::plot(seq_len(horizon), curves[[1L]], type = "b", pch = 16,
      lwd = 2, col = colors[1L], ylim = c(0, y_max), xaxt = "n",
      xlab = "Forecast horizon (quarters)", ylab = "Mean absolute percentage error (%)",
      main = if (component == "II") "Interest income" else "Interest expense")
    graphics::axis(1, at = seq_len(horizon))
    if (length(curves) > 1L) graphics::lines(seq_len(horizon), curves[[2L]],
      type = "b", pch = 17, lwd = 2, col = colors[2L])
    graphics::abline(h = target_error_pct, col = "#B22222", lty = 2, lwd = 2)
    legend_text <- vapply(models, function(m) {
      row <- regression_specs[regression_specs$Component == component &
        regression_specs$Selection_Label == m, , drop = FALSE]
      paste0(m, "; VIF=", sprintf("%.1f", row$Max_VIF[1L]),
        "; overall MAPE=", sprintf("%.1f", row$Average_MAPE_Common[1L]), "%")
    }, character(1))
    graphics::legend("topleft", legend = c(legend_text, "25% target"),
      col = c(colors[seq_along(models)], "#B22222"),
      lty = c(rep(1, length(models)), 2), pch = c(rep(16, length(models)), NA),
      lwd = 2, bty = "n", cex = 0.68)
  }
  graphics::mtext("W32 regression models; rolling-origin evaluation within the training sample",
    outer = TRUE, line = -1.3, cex = 0.9)
}
render_png(file.path(output_dir, "regression_w32_mape_by_horizon.png"), draw_regression_mape,
  width = 1900L, height = 950L)

# Actual and fitted forecasts for the final common training origin, with the
# selected formula and VIF status shown in each panel's legend.
draw_regression_latest <- function() {
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(mfrow = c(1, 2), mar = c(11, 4.8, 4, 1), mgp = c(2.7, 0.75, 0))
  colors <- c("#3B6FB6", "#D9822B")
  for (component in c("II", "IE")) {
    d0 <- regression_forecasts[regression_forecasts$Component == component, , drop = FALSE]
    latest_origin <- sort(unique(d0$Origin))[length(unique(d0$Origin))]
    d0 <- d0[d0$Origin == latest_origin, , drop = FALSE]
    models <- unique(regression_forecasts$Model[regression_forecasts$Component == component])
    actual <- d0[d0$Model == models[1L], , drop = FALSE]
    actual <- actual[order(actual$Horizon), , drop = FALSE]
    y <- actual$Actual_Billions
    for (m in models) y <- c(y, d0$Forecast_Billions[d0$Model == m][order(d0$Horizon[d0$Model == m])])
    graphics::plot(seq_len(horizon), actual$Actual_Billions, type = "b", pch = 16,
      lwd = 2.5, col = "black", ylim = range(y, finite = TRUE), xaxt = "n",
      xlab = "", ylab = "USD billions",
      main = paste0(if (component == "II") "Interest income" else "Interest expense",
        "; origin ", latest_origin))
    graphics::axis(1, at = seq_len(horizon), labels = actual$Quarter, las = 2, cex.axis = 0.75)
    graphics::mtext("Forecast quarter", side = 1, line = 9, cex = 1)
    legend_text <- c("Actual")
    legend_colors <- c("black")
    legend_lty <- c(1)
    for (i in seq_along(models)) {
      d <- d0[d0$Model == models[i], , drop = FALSE]
      d <- d[order(d$Horizon), , drop = FALSE]
      graphics::lines(seq_len(horizon), d$Forecast_Billions, type = "b",
        pch = 15 + i, lwd = 2, col = colors[i])
      spec <- regression_specs[regression_specs$Component == component &
        regression_specs$Selection_Label == models[i], , drop = FALSE]
      legend_text <- c(legend_text, paste0(models[i], " (VIF ", sprintf("%.1f", spec$Max_VIF[1L]), ")"))
      legend_colors <- c(legend_colors, colors[i])
      legend_lty <- c(legend_lty, 1)
    }
    graphics::legend("topleft", legend = legend_text, col = legend_colors,
      lty = legend_lty, pch = c(16, 16, 17)[seq_along(legend_text)],
      lwd = 2, bty = "n", cex = 0.72)
  }
  graphics::mtext("Latest common rolling origin within the training period; future regressors are realized historical values",
    outer = TRUE, line = -1.3, cex = 0.85)
}
render_png(file.path(output_dir, "regression_w32_latest_training_origin.png"),
  draw_regression_latest, width = 1900L, height = 950L)

cat("\nTraining-only forecast figures saved to:", normalizePath(output_dir), "\n")
cat("Holdout file read: no\n")
cat("Regression W32 replay fits:", nrow(regression_specs) * (length(seq.int(33L, nrow(income_data) - horizon + 1L)) +
  length(seq.int(33L, nrow(expense_data) - horizon + 1L))), "\n")
print(accuracy_summary[order(accuracy_summary$Series, accuracy_summary$Model), ], row.names = FALSE)
