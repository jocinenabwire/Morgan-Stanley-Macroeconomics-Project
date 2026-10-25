# Morgan Stanley PPNR | 2026-10-25 | Domestic-only workflow
# Fed 2025 history: 16 US series; build inputs from the repository CSV files.
library(urca)
library(sandwich)

# Load the saved inputs relative to this script; do not use a restored workspace.
ppnr_data_dir <- local({
  paths <- vapply(sys.frames(), function(frame)
    if (is.null(frame$ofile)) "" else as.character(frame$ofile)[1], character(1))
  argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  folders <- c(dirname(rev(paths[nzchar(paths)])),
    if (length(argument)) dirname(gsub("~+~", " ", sub("^--file=", "", argument[1]), fixed = TRUE)),
    ".", "ppnr_domestic")
  folders <- folders[file.exists(file.path(folders, "restore_inputs.R"))]
  if (!length(folders)) stop("Open the repository project or source this script by its path.")
  normalizePath(folders[1], winslash = "/", mustWork = TRUE)
})
source(file.path(ppnr_data_dir, "restore_inputs.R"), local = TRUE, chdir = TRUE)

# 1. Keep the 64 matched quarters -----------------------------------------
ms_ppnr_2025 <- ms_macro_2025[ms_macro_2025$macro_matched, ]
ms_ppnr_2025 <- ms_ppnr_2025[order(ms_ppnr_2025$Date), ]
ms_ppnr_2025$Date <- as.Date(ms_ppnr_2025$Date)
rownames(ms_ppnr_2025) <- NULL
ppnr_dates <- ms_ppnr_2025$Date
quarter_index <- as.integer(format(ppnr_dates, "%Y")) * 4 +
  as.integer(format(ppnr_dates, "%m")) / 3
financial_columns <- c("Total.interest.income", "Total.interest.expense",
                       "Total.noninterest.income", "Total.noninterest.expense")
macro_columns <- names(fed_macro_2025)[4:ncol(fed_macro_2025)]
stopifnot(nrow(ms_macro_2025) == 70, nrow(ms_ppnr_2025) == 64,
          !anyNA(ppnr_dates), !anyDuplicated(ppnr_dates), all(diff(quarter_index) == 1),
          identical(range(ppnr_dates), as.Date(c("2009-03-31", "2024-12-31"))),
          all(is.finite(as.matrix(ms_ppnr_2025[c(financial_columns, macro_columns)]))))

# Fix evaluation periods by calendar date when earlier training history is added.
ppnr_forecast_origins <- function(data, h, period = c("selection", "check")) {
  period <- match.arg(period)
  stopifnot(length(h) == 1L, h %in% 1:3)
  dates <- as.Date(data$Date)
  bounds <- if (period == "selection") c("2017-12-31", "2022-12-31") else
    c("2022-12-31", "2024-12-31")
  positions <- match(as.Date(bounds), dates)
  stopifnot(!anyNA(positions), positions[1] >= 2L,
            positions[1] <= positions[2] - h)
  seq.int(positions[1], positions[2] - h)
}

# 2. Calculate PPNR (source values are already quarterly, in USD thousands)
ms_ppnr_2025$net_interest_income_calc <- with(ms_ppnr_2025,
  Total.interest.income - Total.interest.expense)
ms_ppnr_2025$net_noninterest_income_calc <- with(ms_ppnr_2025,
  Total.noninterest.income - Total.noninterest.expense)
ms_ppnr_2025$PPNR <- with(ms_ppnr_2025,
  net_interest_income_calc + net_noninterest_income_calc)
ms_ppnr_2025$nii_check_difference <- with(ms_ppnr_2025,
  net_interest_income_calc - Net.interest.income)
stopifnot(all(ms_ppnr_2025$nii_check_difference == 0))

# Reconcile the 2024 quarterly totals with the reported FR Y-9C YTD totals.

reported_2024 <- rbind(Jan_Sep = c(43267000, 34585000, 36838000, 32711000),
                      Full_year = c(57308000, 45524000, 49939000, 43895000))
actual_2024 <- rbind(
  colSums(ms_ppnr_2025[ppnr_dates >= "2024-01-01" & ppnr_dates <= "2024-09-30", financial_columns]),
  colSums(ms_ppnr_2025[format(ppnr_dates, "%Y") == "2024", financial_columns]))
stopifnot(all(actual_2024 == reported_2024))
ppnr_checks_2025 <- c(input_rows = nrow(ms_macro_2025), sample_rows = nrow(ms_ppnr_2025),
                     max_NII_difference = max(abs(ms_ppnr_2025$nii_check_difference)),
                     max_source_difference = max(abs(actual_2024 - reported_2024)))

# All analysis and charts below use USD billions per quarter.
ppnr_values_2025 <- with(ms_ppnr_2025, data.frame(
  PPNR = PPNR / 1e6,
  net_interest_income = net_interest_income_calc / 1e6,
  net_noninterest_income = net_noninterest_income_calc / 1e6,
  interest_income = Total.interest.income / 1e6,
  interest_expense = Total.interest.expense / 1e6,
  noninterest_income = Total.noninterest.income / 1e6,
  noninterest_expense = Total.noninterest.expense / 1e6))
ppnr_summary_2025 <- summary(ppnr_values_2025$PPNR)
ppnr_log_eligibility_2025 <- data.frame(series = names(ppnr_values_2025),
  negative_n = sapply(ppnr_values_2025, function(x) sum(x < 0)),
  zero_n = sapply(ppnr_values_2025, function(x) sum(x == 0)),
  log_defined = sapply(ppnr_values_2025, function(x) all(x > 0)))

# First-difference working data; retain levels for reconstruction.
ppnr_difference_2025 <- data.frame(
  Date = ppnr_dates[-1],
  PPNR_bn = ppnr_values_2025$PPNR[-1],
  d_PPNR_bn = diff(ppnr_values_2025$PPNR))

# 3. Stationarity: ADF H0 = unit root; KPSS H0 = stationarity --------------
# Compare ADF lags 0:4 on the SAME observations, then refit the selected lag.
adf_lag_grid <- function(values, deterministic) {
  changes <- diff(values)
  index <- 5:length(changes)
  data <- data.frame(change = changes[index], level = values[index], time = index + 1)
  for (lag in 1:4) data[[paste0("change_lag", lag)]] <- changes[index - lag]
  rows <- list()
  for (lag in 0:4) {
    predictors <- c("level", if (deterministic == "trend") "time",
                    if (lag > 0) paste0("change_lag", 1:lag))
    fit <- lm(reformulate(predictors, response = "change"), data = data)
    rows[[lag + 1]] <- data.frame(p = lag, n_common = nobs(fit), AIC = AIC(fit), BIC = BIC(fit))
  }
  do.call(rbind, rows)
}

test_row <- function(object, series, form, test, setting) {
  is_adf <- test == "ADF"
  statistic <- as.numeric(object@teststat)[1]
  critical <- as.numeric(object@cval[1, "5pct"])
  lag <- if (is_adf) object@lags else object@lag
  deterministic <- if (is_adf) object@model else object@type
  data.frame(series, form, test, deterministic, setting,
    input_n = length(object@y),
    effective_n = if (is_adf) length(object@res) else length(object@y),
    lag, statistic, critical_5pct = critical,
    reject_H0_5pct = if (is_adf) statistic < critical else statistic > critical,
    auxiliary_LB8_p = if (is_adf) Box.test(object@res, lag = 8,
      type = "Ljung-Box", fitdf = lag + 1)$p.value else NA_real_,
    H0 = if (is_adf) "unit root" else if (deterministic == "mu")
      "level stationary" else "trend stationary")
}

stationarity_rows <- lag_tables <- ppnr_stationarity_objects_2025 <- list()
for (series in names(ppnr_values_2025)) {
  forms <- list(level = ppnr_values_2025[[series]],
                first_difference = diff(ppnr_values_2025[[series]]))
  for (form in names(forms)) {
    values <- forms[[form]]
    for (deterministic in c("drift", "trend")) {
      grid <- adf_lag_grid(values, deterministic)
      lag_tables[[paste(series, form, deterministic)]] <-
        cbind(series, form, deterministic, grid)
      for (setting in c("BIC", "AIC")) {
        lag <- grid$p[which.min(grid[[setting]])]
        object <- ur.df(values, type = deterministic, lags = lag, selectlags = "Fixed")
        key <- paste(series, form, "ADF", deterministic, setting, sep = ":")
        ppnr_stationarity_objects_2025[[key]] <- object
        stationarity_rows[[key]] <- test_row(object, series, form, "ADF", setting)
      }
    }
    for (deterministic in c("mu", "tau")) for (setting in c("short", "long")) {
      object <- ur.kpss(values, type = deterministic, lags = setting)
      key <- paste(series, form, "KPSS", deterministic, setting, sep = ":")
      ppnr_stationarity_objects_2025[[key]] <- object
      stationarity_rows[[key]] <- test_row(object, series, form, "KPSS", setting)
    }
  }
}
ppnr_stationarity_tests_2025 <- do.call(rbind, stationarity_rows)
ppnr_adf_lag_selection_2025 <- do.call(rbind, lag_tables)
ppnr_stationarity_main_2025 <- subset(ppnr_stationarity_tests_2025,
  series == "PPNR" & ((test == "ADF" & setting == "BIC") | (test == "KPSS" & setting == "short")))

ppnr_difference_tests_2025 <- subset(ppnr_stationarity_main_2025,
  form == "first_difference")

# 4. Historical trend and quarterly effects (not forecast assumptions) ----
ppnr <- ppnr_values_2025$PPNR
ppnr_trend_data <- data.frame(ppnr, time = seq_along(ppnr),
  quarter = factor(as.integer(format(ppnr_dates, "%m")) / 3, labels = paste0("Q", 1:4)))
ppnr_trend_fit <- lm(ppnr ~ time, data = ppnr_trend_data)
ppnr_seasonal_fit <- lm(ppnr ~ time + quarter, data = ppnr_trend_data)
ppnr_difference_data <- data.frame(ppnr = diff(ppnr), quarter = ppnr_trend_data$quarter[-1])
ppnr_difference_fit <- lm(ppnr ~ quarter, data = ppnr_difference_data)

quarterly_test <- function(fit, hac_lag = 4) {
  covariance <- NeweyWest(fit, lag = hac_lag, prewhite = FALSE, adjust = TRUE)
  terms <- grep("^quarter", names(coef(fit)))
  effects <- coef(fit)[terms]
  statistic <- as.numeric(crossprod(effects, solve(covariance[terms, terms], effects))) / 3
  data.frame(statistic, df1 = 3, df2 = df.residual(fit),
             p_value = pf(statistic, 3, df.residual(fit), lower.tail = FALSE))
}

season_rows <- list()
for (hac_lag in c(4, 8)) {
  covariance <- NeweyWest(ppnr_seasonal_fit, lag = hac_lag, prewhite = FALSE, adjust = TRUE)
  estimate <- unname(coef(ppnr_seasonal_fit)["time"])
  se <- sqrt(covariance["time", "time"])
  margin <- qt(0.975, df.residual(ppnr_seasonal_fit)) * se
  trend_test <- data.frame(estimate, ci95_low = estimate - margin,
    ci95_high = estimate + margin, statistic = estimate / se, df1 = 1,
    df2 = df.residual(ppnr_seasonal_fit),
    p_value = 2 * pt(abs(estimate / se), df.residual(ppnr_seasonal_fit), lower.tail = FALSE))
  for (form in c("level", "first_difference")) {
    quarter_fit <- if (form == "level") ppnr_seasonal_fit else ppnr_difference_fit
    result <- cbind(estimate = NA_real_, ci95_low = NA_real_, ci95_high = NA_real_,
                    quarterly_test(quarter_fit, hac_lag))
    season_rows[[paste(hac_lag, form)]] <- cbind(question = "joint quarterly effects", form, hac_lag, result)
  }
  season_rows[[paste(hac_lag, "trend")]] <- cbind(
    question = "linear trend per quarter", form = "level", hac_lag, trend_test)
}
ppnr_trend_seasonality_2025 <- do.call(rbind, season_rows)

# Center quarterly effects on the annual average; use pointwise HAC intervals.
contrasts <- cbind(0, 0, rbind(c(0, 0, 0), diag(3)))
contrasts <- sweep(contrasts, 2, colMeans(contrasts), "-")
covariance <- NeweyWest(ppnr_seasonal_fit, lag = 4, prewhite = FALSE, adjust = TRUE)
effects <- as.numeric(contrasts %*% coef(ppnr_seasonal_fit))
margin <- qt(0.975, df.residual(ppnr_seasonal_fit)) * sqrt(diag(contrasts %*% covariance %*% t(contrasts)))
ppnr_seasonal_pattern_2025 <- data.frame(quarter = levels(ppnr_trend_data$quarter),
  n = as.integer(table(ppnr_trend_data$quarter)),
  raw_mean_bn = as.numeric(tapply(ppnr, ppnr_trend_data$quarter, mean)),
  detrended_mean_bn = as.numeric(tapply(residuals(ppnr_trend_fit), ppnr_trend_data$quarter, mean)),
  trend_adjusted_effect_bn = effects, ci95_low = effects - margin, ci95_high = effects + margin)

# Sensitivity checks only: no observations are removed from the working data.
ppnr_seasonality_influence_2025 <- do.call(rbind, lapply(seq_along(ppnr), function(i) {
  fit <- lm(ppnr ~ time + quarter, data = ppnr_trend_data[-i, ])
  result <- quarterly_test(fit)
  data.frame(omitted_quarter = ms_ppnr_2025$quarter[i], seasonal_F = result$statistic,
             seasonal_p = result$p_value, trend_per_quarter_bn = unname(coef(fit)["time"]))
}))
ppnr_adf_fixed_lags_2025 <- do.call(rbind, lapply(0:4, function(lag) {
  test <- ur.df(ppnr, type = "trend", lags = lag, selectlags = "Fixed")
  data.frame(lag, statistic = test@teststat[1, 1], critical_5pct = test@cval[1, "5pct"],
             reject_unit_root_5pct = test@teststat[1, 1] < test@cval[1, "5pct"])
}))
ppnr_diagnostic_candidates_2025 <- data.frame(Date = ppnr_dates, quarter = ms_ppnr_2025$quarter,
  PPNR_bn = ppnr, first_difference_bn = c(NA, diff(ppnr)), linear_trend_only_bn = fitted(ppnr_trend_fit),
  trend_and_quarter_fit_bn = fitted(ppnr_seasonal_fit), trend_and_quarter_residual_bn = residuals(ppnr_seasonal_fit),
  status = "diagnostic_only")

# 5. Charts: RStudio Plots only ------------------------------------------
plot_series <- function(values, title, unit = "USD billion per quarter") {
  plot(ppnr_dates, values, type = "l", main = title, xlab = "Year", ylab = unit,
       ylim = range(c(0, values)), las = 1)
  abline(h = 0, lty = 3)
}

plot_ms_ppnr_2025 <- function(which = c("ppnr", "net", "gross")) {
  which <- match.arg(which)
  old <- par(no.readonly = TRUE)
  on.exit(par(old))
  if (which == "gross") {
    par(mfrow = c(2, 2), mar = c(4, 4.5, 3, 1))
    columns <- c("interest_income", "interest_expense", "noninterest_income", "noninterest_expense")
    titles <- c("Interest income", "Interest expense", "Noninterest income", "Noninterest expense")
    limits <- range(c(0, as.matrix(ppnr_values_2025[columns])))
    for (i in 1:4) plot(ppnr_dates, ppnr_values_2025[[columns[i]]], type = "l",
      main = titles[i], xlab = "Year", ylab = "USD billion per quarter", ylim = limits, las = 1)
  } else if (which == "net") {
    par(mfrow = c(1, 1), mar = c(4, 4.5, 3, 1))
    values <- ppnr_values_2025[c("net_interest_income", "net_noninterest_income")]
    plot(ppnr_dates, values$net_interest_income, type = "l", xlab = "Year",
         ylab = "USD billion per quarter", main = "PPNR components",
         ylim = range(c(0, as.matrix(values))))
    lines(ppnr_dates, values$net_noninterest_income, lty = 2)
    abline(h = 0, lty = 3)
    legend("bottomright", c("Net interest income", "Net noninterest income"), lty = 1:2, bty = "n")
  } else {
    par(mfrow = c(1, 1), mar = c(4, 4.5, 3, 1))
    plot_series(ppnr, sprintf("Morgan Stanley PPNR: %s - %s",
      ms_ppnr_2025$quarter[1], tail(ms_ppnr_2025$quarter, 1)))
  }
}

plot_ppnr_diagnostics_2025 <- function(which = c("stationarity", "seasonality")) {
  which <- match.arg(which)
  old <- par(no.readonly = TRUE)
  on.exit(par(old))
  if (which == "stationarity") {
    par(mfrow = c(2, 2), mar = c(4, 4.5, 3, 1))
    plot_series(ppnr, "PPNR and historical linear trend")
    lines(ppnr_dates, fitted(ppnr_trend_fit), lty = 2)
    plot(ppnr_dates[-1], diff(ppnr), type = "l", main = "PPNR: first difference",
         xlab = "Year", ylab = "Change in USD billion", las = 1)
    abline(h = 0, lty = 3)
    acf(ppnr, lag.max = 12, main = "PPNR ACF", xlab = "Lag (quarters)")
    acf(diff(ppnr), lag.max = 12, main = "First-difference ACF", xlab = "Lag (quarters)")
  } else {
    par(mfrow = c(1, 2), mar = c(4, 4.5, 4, 1))
    boxplot(residuals(ppnr_trend_fit) ~ ppnr_trend_data$quarter, main = "Detrended PPNR by quarter",
            xlab = sprintf("Quarter (%d observations each)", ppnr_seasonal_pattern_2025$n[1]),
            ylab = "Deviation in USD billion", las = 1)
    abline(h = 0, lty = 3)
    limits <- range(c(0, ppnr_seasonal_pattern_2025$ci95_low, ppnr_seasonal_pattern_2025$ci95_high))
    plot(1:4, effects, xaxt = "n", ylim = limits, pch = 16, main = "Quarterly effects: 95% HAC intervals",
         xlab = "Quarter", ylab = "USD billion relative to annual average", las = 1)
    axis(1, at = 1:4, labels = paste0("Q", 1:4))
    arrows(1:4, effects - margin, 1:4, effects + margin, angle = 90, code = 3, length = 0.05)
    abline(h = 0, lty = 3)
  }
}

plot_ms_ppnr_2025("gross")
plot_ms_ppnr_2025("net")
plot_ms_ppnr_2025("ppnr")
plot_ppnr_diagnostics_2025("stationarity")
plot_ppnr_diagnostics_2025("seasonality")

# 6. Trial model: exploratory, not a final specification ------------------
# Market levels give a simpler alternative to the differenced MA(1) fit near -1.
# Equity prices proxy fee-generating assets; VIX and credit spreads proxy market conditions.
# Transformations are local. This predictive regression does not establish causality or cointegration.
# Horizons 2-3 hold current market levels constant; no realized future macros are used.
ppnr_trial_2025 <- local({
  fit_model <- function(data) {
    n <- nrow(data)
    lm(PPNR ~ equity + VIX + credit_spread, data = data.frame(
      PPNR = data$PPNR[-1] / 1e6,
      equity = 100 * log(data$us_dow_jones_total_market[-n]),
      VIX = data$us_vix_max[-n],
      credit_spread = data$us_bbb_corporate_yield[-n] - data$us_treasury_10y[-n]))
  }
  forecast <- function(fit, data) {
    predict(fit, newdata = with(tail(data, 1), data.frame(
      equity = 100 * log(us_dow_jones_total_market), VIX = us_vix_max,
      credit_spread = us_bbb_corporate_yield - us_treasury_10y)))
  }

  # Expanding windows: develop through 2022, then check 2023-2024 separately.
  # Revised 2025 history: this is not a real-time vintage backtest.
  first_origin <- ppnr_forecast_origins(ms_ppnr_2025, 1, "selection")[1]
  check_origin <- ppnr_forecast_origins(ms_ppnr_2025, 1, "check")[1]
  rows <- list()
  for (origin in seq.int(first_origin, nrow(ms_ppnr_2025) - 1L)) {
    history <- ms_ppnr_2025[seq_len(origin), ]
    prediction <- as.numeric(forecast(fit_model(history), history))
    values <- history$PPNR / 1e6
    changes <- diff(values)
    benchmark <- lm(changes[-1] ~ head(changes, -1))
    step <- tail(changes, 1)
    baseline <- tail(values, 1)
    for (h in seq_len(min(3, nrow(ms_ppnr_2025) - origin))) {
      step <- sum(coef(benchmark) * c(1, step))
      baseline <- baseline + step
      if (origin < check_origin && origin + h > check_origin) next
      rows[[length(rows) + 1]] <- data.frame(
        period = if (origin < check_origin) "Development: 2018-2022" else "Check: 2023-2024",
        origin = history$quarter[origin], target = ms_ppnr_2025$quarter[origin + h], horizon = h,
        actual = ms_ppnr_2025$PPNR[origin + h] / 1e6,
        Trial = prediction, Naive = tail(values, 1), Difference_AR1 = baseline)
    }
  }
  predictions <- do.call(rbind, rows)
  scores <- do.call(rbind, lapply(split(predictions,
    list(predictions$period, predictions$horizon), drop = TRUE), function(z) {
      errors <- sweep(as.matrix(z[c("Trial", "Naive", "Difference_AR1")]), 1, z$actual, "-")
      data.frame(period = z$period[1], horizon = z$horizon[1], n = nrow(z),
        model = colnames(errors), RMSE = sqrt(colMeans(errors^2)), MAE = colMeans(abs(errors)))
    }))
  rownames(scores) <- NULL

  model <- fit_model(ms_ppnr_2025)
  covariance <- NeweyWest(model, lag = 4, prewhite = FALSE, adjust = TRUE)
  future_quarters <- tail(quarter_index, 1) + 1:3
  list(model = model,
    forecast = data.frame(horizon = 1:3,
      quarter = paste((future_quarters - 1) %/% 4, paste0("Q", (future_quarters - 1) %% 4 + 1)),
      PPNR_bn = rep(as.numeric(forecast(model, ms_ppnr_2025)), 3)),
    coefficients = data.frame(term = names(coef(model)), estimate = unname(coef(model)),
      HAC_SE = sqrt(diag(covariance))),
    diagnostics = c(n = nobs(model), adjusted_R2 = summary(model)$adj.r.squared,
      residual_LB8_p = Box.test(residuals(model), lag = 8, type = "Ljung-Box")$p.value),
    validation = scores, predictions = predictions)
})

# Methods: search.r-project.org/CRAN/refmans/urca/html/ur.df.html (also ur.kpss.html)
# HAC intervals: search.r-project.org/CRAN/refmans/sandwich/html/NeweyWest.html
# Source checks: FFIEC FR Y-9C, Morgan Stanley RSSD 2162966, 2024-09-30 and 2024-12-31.

# 7. Exhaustive forecast search: one to eight factors ---------------------
# Keep Fed 2025 and the 64 matched quarters; source columns stay unchanged.
# US price indices: log levels and 100*log changes. Yields: levels and percentage-point changes.
# US growth, inflation, unemployment and VIX stay in their published rate/level form.
# Search all OLS subsets of 1:8 local candidates, including two spreads and own PPNR.
# Rank by expanding-window RMSE in 2018-2022, separately for horizons 1:3 and level/change targets.
# Freeze the winners, then check 2023-2024. This revised-history exercise is exploratory, not causal.
# Searches run only when called explicitly. Rcpp compiles the C++ below and reports progress.
# One shared R process: 3 GiB vector heap, 4 GiB RSS stop, six native CPU threads, two-hour budget.
run_ppnr_search <- function(data, max_factors = 8L, threads = 6L, minutes = 120,
                            required_ppnr = FALSE) {
  if (!requireNamespace("Rcpp", quietly = TRUE)) stop("Install Rcpp before running this section")
  started <- proc.time()[3]
  previous_limit <- mem.maxVSize()
  invisible(mem.maxVSize(min(previous_limit, 3072)))
  on.exit(invisible(mem.maxVSize(previous_limit)), add = TRUE)
  Rcpp::sourceCpp(code = r"---(
// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <algorithm>
#include <atomic>
#include <chrono>
#include <cmath>
#include <memory>
#include <queue>
#include <thread>
#include <vector>
#include <sys/resource.h>
#ifdef __APPLE__
#include <mach/mach.h>
#endif
using namespace Rcpp;

static double ppnr_rss() {
#ifdef __APPLE__
  mach_task_basic_info_data_t info;
  mach_msg_type_number_t count = MACH_TASK_BASIC_INFO_COUNT;
  if (task_info(mach_task_self(), MACH_TASK_BASIC_INFO,
                (task_info_t)&info, &count) == KERN_SUCCESS)
    return (double)info.resident_size;
#endif
  return 0;
}
static double ppnr_peak() {
  struct rusage usage; getrusage(RUSAGE_SELF, &usage);
#ifdef __APPLE__
  return (double)usage.ru_maxrss;
#else
  return (double)usage.ru_maxrss * 1024;
#endif
}
static double ppnr_choose(int n, int k) {
  if (k < 0 || k > n) return 0;
  double value = 1;
  for (int j = 1; j <= k; ++j) value *= double(n-j+1)/j;
  return std::round(value);
}
struct PPNRFold {
  std::vector<double> gram, cy, future;
  double base, actual;
  bool selection;
};
struct PPNRCandidate {
  double mse;
  std::vector<int> variables;
  std::vector<double> predictions;
  bool operator<(const PPNRCandidate& other) const {
    if (mse != other.mse) return mse < other.mse;
    return variables < other.variables;
  }
};
struct PPNRTask {
  int p, k, nf, top_n, selection_n = 0, check_n = 0;
  double checked = 0, check_sum = 0, check_min = INFINITY, check_max = 0;
  double total, covered = 0, excluded = 0, nodes = 0, max_nodes;
  std::vector<PPNRFold> folds;
  std::vector<int> chosen;
  std::vector<double> L, z, w, predictions;
  std::priority_queue<PPNRCandidate> best;
  std::atomic<double> progress{0};
  std::atomic<bool>* cancel;
  std::string error;

  PPNRTask(List input, int size, int keep, double limit, std::atomic<bool>* stop_flag)
    : k(size), top_n(keep), max_nodes(limit), cancel(stop_flag) {
    nf = input.size(); p = 0;
    for (int f = 0; f < nf; ++f) {
      List entry = input[f]; NumericMatrix G = entry["gram"];
      NumericVector c = entry["cy"], x = entry["future"];
      if (!f) p = G.nrow();
      if (G.nrow() != p || G.ncol() != p || c.size() != p || x.size() != p)
        stop("Inconsistent search matrices");
      PPNRFold fold;
      fold.gram.assign(G.begin(), G.end()); fold.cy.assign(c.begin(), c.end());
      fold.future.assign(x.begin(), x.end());
      fold.base = as<double>(entry["base_prediction"]);
      fold.actual = as<double>(entry["actual"]);
      fold.selection = !entry.containsElementNamed("selection") || as<bool>(entry["selection"]);
      if (fold.selection) ++selection_n; else ++check_n;
      folds.push_back(fold);
    }
    if (k < 1 || k > 8 || k > p || selection_n < 1) stop("Invalid search dimensions");
    total = ppnr_choose(p, k);
    L.assign(nf*k*k, 0); z.assign(nf*k, 0); w.assign(nf*k, 0);
    predictions.assign(nf*(k+1), 0);
    for (int f = 0; f < nf; ++f) predictions[f*(k+1)] = folds[f].base;
  }
  void record(double mse, double check_mse) {
    if (!std::isfinite(mse)) return;
    if (check_n) {
      double rmse = std::sqrt(check_mse / check_n);
      ++checked; check_sum += rmse;
      check_min = std::min(check_min, rmse); check_max = std::max(check_max, rmse);
    }
    if (best.size() == (size_t)top_n && mse >= best.top().mse) return;
    PPNRCandidate item; item.mse = mse; item.variables = chosen;
    for (int f = 0; f < nf; ++f) item.predictions.push_back(predictions[f*(k+1)+k]);
    best.push(item); if (best.size() > (size_t)top_n) best.pop();
  }
  void search(int next, int depth) {
    for (int j = next; j <= p-(k-depth); ++j) {
      if (((unsigned long long)nodes & 4095) == 0) {
        progress.store(covered, std::memory_order_relaxed);
        if (cancel->load() || nodes >= max_nodes) return;
      }
      ++nodes; chosen.push_back(j); bool valid = true; double mse = 0, check_mse = 0;
      for (int f = 0; f < nf; ++f) {
        const PPNRFold& fold = folds[f];
        int li = f*k*k, vi = f*k, pi = f*(k+1);
        double diagonal = fold.gram[j+p*j];
        for (int r = 0; r < depth; ++r) {
          double value = fold.gram[j+p*chosen[r]];
          for (int s = 0; s < r; ++s) value -= L[li+depth*k+s]*L[li+r*k+s];
          value /= L[li+r*k+r]; L[li+depth*k+r] = value;
          diagonal -= value*value;
        }
        if (!std::isfinite(diagonal) || diagonal < 1e-10) { valid = false; break; }
        double yy = fold.cy[j], xx = fold.future[j];
        for (int r = 0; r < depth; ++r) {
          yy -= L[li+depth*k+r]*z[vi+r]; xx -= L[li+depth*k+r]*w[vi+r];
        }
        double root = std::sqrt(diagonal); L[li+depth*k+depth] = root;
        z[vi+depth] = yy/root; w[vi+depth] = xx/root;
        double prediction = predictions[pi+depth] + z[vi+depth]*w[vi+depth];
        predictions[pi+depth+1] = prediction;
        double residual = prediction-fold.actual;
        if (fold.selection) mse += residual*residual; else check_mse += residual*residual;
      }
      if (!valid) {
        double count = ppnr_choose(p-j-1, k-depth-1);
        covered += count; excluded += count;
      } else if (depth+1 == k) {
        ++covered; record(mse/selection_n, check_mse);
      } else search(j+1, depth+1);
      chosen.pop_back();
      if (cancel->load() || nodes >= max_nodes) return;
    }
  }
  void run() { search(0, 0); progress.store(covered); }
};
// All R API calls and progress output stay on the main thread.
// [[Rcpp::export]]
List ppnr_search_round(List inputs, int size, int threads = 6,
                       double seconds = 7200, double memory_bytes = 4294967296.0,
                       double max_nodes = 1e100, bool verbose = true) {
  std::atomic<bool> cancel{false}; std::atomic<int> next{0}, finished{0};
  int ntasks = inputs.size();
  if (!ntasks || threads < 1 || seconds <= 0) stop("Invalid execution limits");
  std::vector<std::unique_ptr<PPNRTask>> tasks;
  for (int i = 0; i < ntasks; ++i)
    tasks.emplace_back(new PPNRTask(as<List>(inputs[i]), size, 3, max_nodes, &cancel));
  std::vector<std::thread> workers;
  auto start = std::chrono::steady_clock::now();
  auto elapsed = [&]() { return std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count(); };
  double peak = ppnr_rss(), last_report = 0;
  std::string status = "complete";
  if (peak > memory_bytes) { status = "memory_limit"; cancel.store(true); }
  try {
    for (int t = 0; t < std::min(threads, ntasks); ++t) workers.emplace_back([&]() {
      while (!cancel.load()) {
        int id = next.fetch_add(1); if (id >= ntasks) break;
        try { tasks[id]->run(); }
        catch (const std::exception& e) { tasks[id]->error = e.what(); cancel.store(true); }
        catch (...) { tasks[id]->error = "Unknown native search error"; cancel.store(true); }
        finished.fetch_add(1);
      }
    });
    while (finished.load() < ntasks && !cancel.load()) {
      std::this_thread::sleep_for(std::chrono::milliseconds(100));
      Rcpp::checkUserInterrupt();
      double rss = ppnr_rss(); peak = std::max(peak, rss);
      if (rss > memory_bytes) { status = "memory_limit"; cancel.store(true); }
      if (elapsed() > seconds) { status = "time_limit"; cancel.store(true); }
      if (verbose && elapsed()-last_report >= 10) {
        last_report = elapsed(); double covered = 0, total = 0;
        for (auto& task : tasks) { covered += task->progress.load(); total += task->total; }
        Rcpp::Rcout << "  Factors " << size << " | " << std::round(1000*covered/total)/10
          << "% | " << std::round(elapsed()) << " s | R " << std::round(rss/1048576)
          << " MiB" << std::endl;
        R_FlushConsole();
      }
    }
  } catch (...) {
    cancel.store(true); for (auto& worker : workers) if (worker.joinable()) worker.join();
    throw;
  }
  for (auto& worker : workers) if (worker.joinable()) worker.join();
  List results(ntasks);
  for (int i = 0; i < ntasks; ++i) {
    auto& task = *tasks[i];
    if (!task.error.empty()) stop(task.error);
    std::vector<PPNRCandidate> ranked;
    while (!task.best.empty()) { ranked.push_back(task.best.top()); task.best.pop(); }
    std::sort(ranked.begin(), ranked.end()); List best(ranked.size());
    for (size_t j = 0; j < ranked.size(); ++j) {
      IntegerVector variables(ranked[j].variables.size());
      for (size_t q = 0; q < ranked[j].variables.size(); ++q) variables[q] = ranked[j].variables[q]+1;
      best[j] = List::create(_["MSE"] = ranked[j].mse, _["variables"] = variables,
                            _["predictions"] = wrap(ranked[j].predictions));
    }
    if (task.covered < task.total && status == "complete") status = "pilot_limit";
    results[i] = List::create(_["covered"] = task.covered, _["excluded"] = task.excluded,
                              _["total"] = task.total, _["top"] = best,
      _["check_models"] = task.checked,
      _["mean_check_RMSE"] = task.checked ? task.check_sum/task.checked : NA_REAL,
      _["min_check_RMSE"] = task.checked ? task.check_min : NA_REAL,
      _["max_check_RMSE"] = task.checked ? task.check_max : NA_REAL);
  }
  return List::create(_["status"] = status, _["seconds"] = elapsed(),
    _["peak_bytes"] = std::max(peak, ppnr_peak()), _["tasks"] = results);
}
)---", env = environment(), showOutput = FALSE)
  # Local candidates: keep rates/growth/VIX; add changes only where meaningful.
  sample_n <- nrow(data)
  stopifnot(sample_n == 64, identical(range(as.Date(data$Date)),
    as.Date(c("2009-03-31", "2024-12-31"))))
  macro <- c("us_real_gdp_growth", "us_nominal_gdp_growth",
    "us_real_disposable_income_growth", "us_nominal_disposable_income_growth",
    "us_unemployment_rate", "us_cpi_inflation", "us_treasury_3m",
    "us_treasury_5y", "us_treasury_10y", "us_bbb_corporate_yield",
    "us_mortgage_rate", "us_prime_rate", "us_dow_jones_total_market",
    "us_house_price_index", "us_cre_price_index", "us_vix_max")
  prices <- c("us_dow_jones_total_market", "us_house_price_index", "us_cre_price_index")
  rates <- c("us_treasury_3m", "us_treasury_5y", "us_treasury_10y",
    "us_bbb_corporate_yield", "us_mortgage_rate", "us_prime_rate")
  change <- function(z) c(NA_real_, diff(z))
  y <- data$PPNR / 1e6
  x <- as.data.frame(data[macro])
  x[prices] <- lapply(x[prices], function(z) 100 * log(z))
  names(x)[match(prices, macro)] <- paste0("log_", prices)
  for (nm in prices) x[[paste0("return_", nm)]] <- change(100 * log(data[[nm]]))
  for (nm in rates) x[[paste0("d_", nm)]] <- change(data[[nm]])
  x$credit_spread <- data$us_bbb_corporate_yield - data$us_treasury_10y
  x$term_spread <- data$us_treasury_10y - data$us_treasury_3m
  x$d_credit_spread <- change(x$credit_spread)
  x$d_term_spread <- change(x$term_spread)
  x$PPNR <- y
  if (!required_ppnr) x$d_PPNR <- change(y)
  x <- as.matrix(x)
  search_columns <- if (required_ppnr) which(colnames(x) != "PPNR") else seq_len(ncol(x))
  stopifnot(length(search_columns) == if (required_ppnr) 29L else 31L,
    all(is.finite(x[2:sample_n, ])), max_factors %in% 1:8)
  tasks <- expand.grid(horizon = 1:3, target = c("level", "change"),
                       stringsAsFactors = FALSE)
  folds <- function(h, target, period = "selection") {
    origins <- ppnr_forecast_origins(data, h, period)
    lapply(origins, function(o) {
      i <- 2:(o - h)
      response <- y[i + h] - if (target == "change") y[i] else 0
      centre <- colMeans(x[i, search_columns, drop = FALSE])
      z <- sweep(x[i, search_columns, drop = FALSE], 2, centre)
      scale <- sqrt(colSums(z^2)); scale[scale < 1e-12] <- 1
      z <- sweep(z, 2, scale, "/")
      future <- (x[o, search_columns] - centre) / scale
      residual <- response - mean(response); base <- mean(response)
      if (required_ppnr) {
        # Partial out the intercept and mandatory PPNR: exactly the same OLS fit.
        fixed <- lm.fit(cbind(1, y[i]), response)
        future <- future - drop(c(1, y[o]) %*% qr.coef(fixed$qr, z))
        z <- qr.resid(fixed$qr, z)
        residual <- fixed$residuals
        base <- sum(c(1, y[o]) * fixed$coefficients)
      }
      list(gram = crossprod(z), cy = as.numeric(crossprod(z, residual)),
        future = future, base_prediction = base + if (target == "change") y[o] else 0,
        actual = y[o + h], origin = o, selection = period == "selection")
    })
  }
  inputs <- lapply(seq_len(nrow(tasks)), function(j)
    c(folds(tasks$horizon[j], tasks$target[j]),
      if (required_ppnr) folds(tasks$horizon[j], tasks$target[j], "check")))
  forecast_subset <- function(v, o, h, target) {
    if (required_ppnr) v <- unique(c(v, match("PPNR", colnames(x))))
    i <- 2:(o - h)
    response <- y[i + h] - if (target == "change") y[i] else 0
    z <- x[i, v, drop = FALSE]
    centre <- colMeans(z); z <- sweep(z, 2, centre)
    scale <- sqrt(colSums(z^2)); scale[scale < 1e-12] <- 1
    z <- sweep(z, 2, scale, "/")
    fit <- lm.fit(cbind(1, z), response)
    if (fit$rank != length(v) + 1) stop("Shortlisted model is rank deficient")
    sum(c(1, (x[o, v] - centre) / scale) * fit$coefficients) +
      if (target == "change") y[o] else 0
  }
  candidates <- rounds <- all_model_checks <- list()
  cat(sprintf("Search: %d candidates | 1-%d factors%s | %d CPU threads | 1-3 quarters\n",
    length(search_columns), max_factors, if (required_ppnr) " + mandatory PPNR" else "",
    min(threads, nrow(tasks))))
  for (k in seq_len(max_factors)) {
    remaining <- 60 * minutes - (proc.time()[3] - started)
    if (remaining <= 0) break
    cat(sprintf("Round %d/%d: %s combinations per forecast task\n", k, max_factors,
      format(choose(length(search_columns), k), big.mark = ",", scientific = FALSE)))
    flush.console()
    # A short six-task pilot estimates the remaining cost before a large round.
    if (k >= 6) {
      pilot <- ppnr_search_round(inputs, k, threads, min(20, remaining),
        max_nodes = 250000, verbose = FALSE)
      done <- sum(vapply(pilot$tasks, `[[`, numeric(1), "covered"))
      estimate <- pilot$seconds * (nrow(tasks) * choose(length(search_columns), k)) / max(done, 1)
      cat(sprintf("  Pilot estimate: %.1f min; budget remaining: %.1f min\n",
        estimate / 60, remaining / 60))
      remaining <- 60 * minutes - (proc.time()[3] - started)
      if (1.5 * estimate > remaining) {
        cat("  Next round exceeds the remaining time budget; completed rounds retained.\n")
        break
      }
    }
    result <- ppnr_search_round(inputs, k, threads, remaining)
    rounds[[length(rounds) + 1]] <- data.frame(factors = k, status = result$status,
      combinations = sum(vapply(result$tasks, `[[`, numeric(1), "covered")),
      excluded = sum(vapply(result$tasks, `[[`, numeric(1), "excluded")),
      seconds = result$seconds, peak_R_MiB = result$peak_bytes / 2^20)
    if (result$status != "complete") {
      cat(sprintf("  Stopped safely: %s; only earlier complete rounds count.\n", result$status))
      break
    }
    stopifnot(all(vapply(result$tasks, `[[`, numeric(1), "covered") == choose(length(search_columns), k)))
    for (j in seq_len(nrow(tasks))) {
      top <- result$tasks[[j]]$top[[1]]
      h <- tasks$horizon[j]; target <- tasks$target[j]
      predicted <- vapply(inputs[[j]], function(f)
        forecast_subset(top$variables, f$origin, h, target), numeric(1))
      stopifnot(max(abs(predicted - top$predictions)) < 1e-6)
      selection_index <- vapply(inputs[[j]], `[[`, logical(1), "selection")
      if (required_ppnr) {
        statistics <- result$tasks[[j]]
        all_model_checks[[length(all_model_checks) + 1]] <- data.frame(
          macro_factors = k, horizon = h, target,
          valid_models = statistics$check_models,
          mean_check_RMSE = statistics$mean_check_RMSE,
          min_check_RMSE = statistics$min_check_RMSE,
          max_check_RMSE = statistics$max_check_RMSE)
      }
      selected <- c(top$variables, if (required_ppnr) match("PPNR", colnames(x)))
      candidate <- data.frame(factors = length(selected), horizon = h,
        target, selection_RMSE = sqrt(mean((predicted[selection_index] -
          vapply(inputs[[j]], `[[`, numeric(1), "actual")[selection_index])^2)),
        variables = paste(colnames(x)[selected], collapse = ";"))
      if (required_ppnr) candidate$macro_factors <- k
      candidates[[length(candidates) + 1]] <- candidate
    }
    cat(sprintf("  Round %d/%d: 100%% | %.1f s | peak R %.0f MiB\n",
      k, max_factors, result$seconds, result$peak_bytes / 2^20))
    flush.console()
  }
  if (!length(candidates)) stop("No complete search round")
  best_by_size <- do.call(rbind, candidates)
  winners <- do.call(rbind, lapply(split(best_by_size, best_by_size$horizon),
    function(z) z[order(z$selection_RMSE, z$factors, z$variables)[1], ]))
  rownames(winners) <- NULL
  # Freeze selection before the later historical check; never select on check errors.
  predictions <- models <- forecasts <- list()
  core <- c("log_us_dow_jones_total_market", "us_vix_max", "credit_spread")
  for (h in 1:3) {
    winner <- winners[winners$horizon == h, ]
    v <- match(strsplit(winner$variables, ";", fixed = TRUE)[[1]], colnames(x))
    for (period in c("selection", "check")) {
      origins <- ppnr_forecast_origins(data, h, period)
      for (o in origins) {
        i <- 2:(o - 1)
        trial <- lm.fit(cbind(1, x[i, core, drop = FALSE]), y[i + 1])
        predictions[[length(predictions) + 1]] <- data.frame(period, horizon = h,
          origin = data$quarter[o], quarter = data$quarter[o + h], actual = y[o + h],
          Search = forecast_subset(v, o, h, winner$target),
          Core3 = sum(c(1, x[o, core]) * trial$coefficients),
          Naive = y[o])
      }
    }
    i <- 2:(sample_n - h)
    response <- y[i + h] - if (winner$target == "change") y[i] else 0
    model_data <- data.frame(PPNR_target = response, x[i, v, drop = FALSE])
    models[[paste0("H", h)]] <- lm(reformulate(colnames(x)[v], response = "PPNR_target",
      env = baseenv()), data = model_data)
    forecasts[[h]] <- data.frame(horizon = h,
      PPNR_bn = forecast_subset(v, sample_n, h, winner$target))
  }
  predictions <- do.call(rbind, predictions)
  scores <- do.call(rbind, lapply(split(predictions,
    list(predictions$period, predictions$horizon), drop = TRUE), function(z) {
      errors <- sweep(as.matrix(z[c("Search", "Core3", "Naive")]), 1, z$actual)
      data.frame(period = z$period[1], horizon = z$horizon[1], n = nrow(z),
        model = colnames(errors), RMSE = sqrt(colMeans(errors^2)), MAE = colMeans(abs(errors)))
    }))
  rownames(scores) <- NULL
  elapsed <- proc.time()[3] - started
  cat(sprintf("Complete through %d %sfactors | %.1f min\n",
    max(best_by_size$factors) - as.integer(required_ppnr),
    if (required_ppnr) "macro " else "", elapsed / 60))
  print(winners[c("horizon", "factors", "target", "selection_RMSE")], row.names = FALSE)
  list(settings = list(vintage = 2025, scope = "Domestic only", max_requested = max_factors,
      max_completed = max(best_by_size$factors) - as.integer(required_ppnr),
      required_ppnr = required_ppnr, candidates = length(search_columns),
      threads = threads, budget_minutes = minutes, elapsed_seconds = elapsed),
    candidate_values = x, best_by_size = best_by_size, winners = winners,
    validation = scores, predictions = predictions, models = models,
    forecast = do.call(rbind, forecasts), rounds = do.call(rbind, rounds),
    all_model_checks = if (length(all_model_checks)) do.call(rbind, all_model_checks) else NULL)
}

# 8. Historical out-of-sample checks for every shortlisted model ---------
# Check all 48 size/horizon/target winners, not only the three overall winners.
# Specifications use 2018-2022 selection errors; coefficients expand using past data only.
# Horizons 1:3 use direct forecasts; "change" means the cumulative h-quarter change.
# The 2023-2024 period has already been inspected, so this remains exploratory.
# Rerun only this section to reuse a completed search.
# Model structure and estimation are separate: AR/ARX models also use OLS.
# Own PPNR predictors are observed before the future target; horizons are direct.
# For change targets, the label describes the cumulative-change equation.
label_ppnr_models <- function(models) {
  variables <- strsplit(models$variables, ";", fixed = TRUE)
  own <- vapply(variables, function(v) any(v %in% c("PPNR", "d_PPNR")), logical(1))
  macro <- vapply(variables, function(v) any(!v %in% c("PPNR", "d_PPNR")), logical(1))
  models$model_type <- ifelse(own, ifelse(macro, "ARX", "AR"), "Linear regression")
  models$estimation <- "OLS"
  models[c("model_type", "estimation", setdiff(names(models), c("model_type", "estimation")))]
}

check_ppnr_candidates <- function(result, data) {
  x <- result$candidate_values
  y <- data$PPNR / 1e6
  bbs <- result$best_by_size
  rows <- scores <- list()
  for (r in seq_len(nrow(bbs))) {
    h <- bbs$horizon[r]; target <- bbs$target[r]
    v <- match(strsplit(bbs$variables[r], ";", fixed = TRUE)[[1]], colnames(x))
    for (period in c("selection", "check")) {
      origins <- ppnr_forecast_origins(data, h, period)
      predicted <- vapply(origins, function(o) {
        i <- 2:(o - h)
        response <- y[i + h] - if (target == "change") y[i] else 0
        z <- x[i, v, drop = FALSE]
        centre <- colMeans(z); z <- sweep(z, 2, centre)
        sc <- sqrt(colSums(z^2)); sc[sc < 1e-12] <- 1
        fit <- lm.fit(cbind(1, sweep(z, 2, sc, "/")), response)
        if (fit$rank != length(v) + 1) stop("Shortlisted model is rank deficient")
        sum(c(1, (x[o, v] - centre) / sc) * fit$coefficients) +
          if (target == "change") y[o] else 0
      }, numeric(1))
      errors <- predicted - y[origins + h]
      metrics <- c(n = length(errors), RMSE = sqrt(mean(errors^2)), MAE = mean(abs(errors)))
      if (period == "selection") stopifnot(abs(metrics["RMSE"] - bbs$selection_RMSE[r]) < 1e-6)
      bbs[r, paste0(period, "_", names(metrics))] <- as.list(unname(metrics))
      scores[[length(scores) + 1]] <- data.frame(period, horizon = h, n = length(errors),
        model = "Search", factors = bbs$factors[r], target,
        RMSE = unname(metrics["RMSE"]), MAE = unname(metrics["MAE"]))
      rows[[length(rows) + 1]] <- data.frame(factors = bbs$factors[r], horizon = h,
        target, period, origin = data$quarter[origins], quarter = data$quarter[origins + h],
        actual = y[origins + h], predicted)
    }
  }
  if (is.null(result$winner_validation)) result$winner_validation <- result$validation
  # Retain the original flat-path Core3 and Naive baselines for comparison.
  benchmarks <- result$winner_validation[result$winner_validation$model != "Search", ]
  benchmarks$factors <- unname(c(Core3 = 3L, Naive = 0L)[benchmarks$model])
  benchmarks$target <- "level"
  columns <- c("period", "horizon", "n", "model", "factors", "target", "RMSE", "MAE")
  result$validation <- rbind(do.call(rbind, scores), benchmarks[columns])
  rownames(result$validation) <- NULL
  result$best_by_size <- label_ppnr_models(bbs[c(intersect("macro_factors", names(bbs)),
    "factors", "horizon", "target", "selection_n",
    "selection_RMSE", "selection_MAE", "check_n", "check_RMSE", "check_MAE", "variables")])
  result$winners <- label_ppnr_models(result$winners)
  result$candidate_predictions <- do.call(rbind, rows)
  result
}

# Run separately: ppnr_search_2025 <- run_ppnr_mixed()
run_ppnr_mixed <- function(data = ms_ppnr_2025, max_factors = 8L,
                           threads = 6L, minutes = 120) {
  result <- run_ppnr_search(data, max_factors, threads, minutes)
  check_ppnr_candidates(result, data)
}


# 9. ARX-only search: mandatory past PPNR plus one to eight macro factors ---
# Same 29 Domestic macro candidates, 2018-2022 selection and 2023-2024 historical check.
# Every model uses PPNR at origin t to predict t+h; OLS estimates all coefficients.
# "macro_factors" excludes PPNR; "factors" includes it (2:9, excluding the intercept).
# Level/change targets are equivalent when PPNR[t] has a free coefficient.
# Retain both forms for comparison; they are not independent model families.
# Score every valid subset on the later check period; select only on selection RMSE.
# Store aggregate check statistics and 48 selection winners, not billions of rows.
# Run separately: ppnr_arx_search_2025 <- run_ppnr_arx()
run_ppnr_arx <- function(data = ms_ppnr_2025, max_factors = 8L,
                         threads = 6L, minutes = 120) {
  result <- run_ppnr_search(data, max_factors, threads, minutes, required_ppnr = TRUE)
  result <- check_ppnr_candidates(result, data)
  stopifnot(all(result$best_by_size$model_type == "ARX"))
  result
}
