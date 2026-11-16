# Training-only stationary VAR search for Morgan Stanley II and IE.
# This is a separate, reproducible follow-up to Modeling_VAR_training.R.
# It does not read the Y-9C holdout/test outcome file.

required_packages <- c("readr", "vars", "urca", "tseries")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing R package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

train_file <- file.path("data_splits", "train_y9c_outcomes_2009Q1_2024Q2.csv")
macro_file <- "2025-Table_1A_Historic_Domestic.csv"
output_dir <- file.path("results", "var_stationary_training_20261116")
window_size <- 32L
forecast_horizon <- 9L
maximum_lag <- 4L
minimum_residual_df <- 8L
maximum_macro_variables <- 2L
progress_step_pct <- 5L

if (!file.exists(train_file)) stop("Missing approved training file: ", train_file, call. = FALSE)
if (!file.exists(macro_file)) stop("Missing macro source file: ", macro_file, call. = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

quarter_from_date <- function(x) {
  d <- as.Date(x)
  paste0(format(d, "%Y"), " Q", ((as.integer(format(d, "%m")) - 1L) %/% 3L) + 1L)
}
quarter_index <- function(x) {
  parts <- regmatches(x, regexec("^(\\d{4}) Q([1-4])$", x))
  if (any(lengths(parts) != 3L)) stop("Unexpected quarter label.", call. = FALSE)
  vapply(parts, function(z) as.integer(z[2L]) * 4L + as.integer(z[3L]), integer(1))
}

train_raw <- readr::read_csv(train_file, show_col_types = FALSE)
train <- data.frame(
  Quarter = quarter_from_date(train_raw$Date),
  II = as.numeric(train_raw[["Total interest income"]]) / 1e6,
  IE = as.numeric(train_raw[["Total interest expense"]]) / 1e6,
  NII_reported = as.numeric(train_raw[["Net interest income"]]) / 1e6,
  stringsAsFactors = FALSE
)
train$NII <- train$II - train$IE
if (nrow(train) != 62L || train$Quarter[1L] != "2009 Q1" ||
    tail(train$Quarter, 1L) != "2024 Q2") {
  stop("Training outcomes do not match the approved 62-quarter span.", call. = FALSE)
}
if (anyDuplicated(train$Quarter) || any(diff(quarter_index(train$Quarter)) != 1L)) {
  stop("Training outcomes contain duplicate or missing quarters.", call. = FALSE)
}
if (any(!is.finite(as.matrix(train[, c("II", "IE", "NII", "NII_reported")]))) ||
    any(train$II <= 0) || any(train$IE <= 0)) {
  stop("Training outcomes contain invalid or non-positive values.", call. = FALSE)
}
if (max(abs(train$NII - train$NII_reported)) > 1e-8) {
  stop("Training NII does not reconcile to II minus IE.", call. = FALSE)
}

macro_raw <- readr::read_csv(macro_file, show_col_types = FALSE)
macro <- data.frame(
  Quarter = trimws(as.character(macro_raw$Date)),
  Rate_3M = as.numeric(macro_raw[["3-month Treasury rate"]]),
  Rate_10Y = as.numeric(macro_raw[["10-year Treasury yield"]]),
  BBB_Yield = as.numeric(macro_raw[["BBB corporate yield"]]),
  VIX = as.numeric(macro_raw[["Market Volatility Index (Level)"]]),
  stringsAsFactors = FALSE
)
macro <- macro[match(train$Quarter, macro$Quarter), , drop = FALSE]
if (anyNA(macro$Quarter) || any(macro$Quarter != train$Quarter)) {
  stop("Macro data do not cover all approved training quarters.", call. = FALSE)
}
if (any(!is.finite(as.matrix(macro[, c("Rate_3M", "Rate_10Y", "BBB_Yield", "VIX")])))) {
  stop("Macro inputs contain missing/non-finite values.", call. = FALSE)
}

analysis_data <- cbind(train, macro[, c("Rate_3M", "Rate_10Y", "BBB_Yield", "VIX"), drop = FALSE])
analysis_data$Term_Spread <- analysis_data$Rate_10Y - analysis_data$Rate_3M
analysis_data$BBB_Spread <- analysis_data$BBB_Yield - analysis_data$Rate_10Y
analysis_data$d_II <- c(NA_real_, diff(analysis_data$II))
analysis_data$g_II <- c(NA_real_, diff(log(analysis_data$II)))
analysis_data$dd_IE <- c(NA_real_, NA_real_, diff(analysis_data$IE, differences = 2L))
analysis_data$ddlog_IE <- c(NA_real_, NA_real_, diff(log(analysis_data$IE), differences = 2L))
analysis_data$d_Rate_3M <- c(NA_real_, diff(analysis_data$Rate_3M))
analysis_data$d_Rate_10Y <- c(NA_real_, diff(analysis_data$Rate_10Y))
analysis_data$d_Term_Spread <- c(NA_real_, diff(analysis_data$Term_Spread))
analysis_data$d_BBB_Spread <- c(NA_real_, diff(analysis_data$BBB_Spread))
analysis_data$d_BBB_Yield <- c(NA_real_, diff(analysis_data$BBB_Yield))

unit_root_row <- function(name, series, adf_type = "drift", kpss_null = "Level") {
  x <- as.numeric(series)
  x <- x[is.finite(x)]
  adf <- tryCatch(
    suppressWarnings(urca::ur.df(x, type = adf_type, lags = 4L, selectlags = "AIC")),
    error = function(e) NULL
  )
  kpss_p <- tryCatch(
    suppressWarnings(tseries::kpss.test(x, null = kpss_null)$p.value),
    error = function(e) NA_real_
  )
  adf_stat <- if (is.null(adf)) NA_real_ else as.numeric(adf@teststat[1L])
  adf_crit <- if (is.null(adf)) NA_real_ else as.numeric(adf@cval[1L, "5pct"])
  data.frame(
    Variable = name, N = length(x), ADF_Deterministic = adf_type,
    ADF_SelectedLags = if (is.null(adf)) NA_integer_ else adf@lags,
    ADF_Statistic = adf_stat, ADF_5pct_Critical = adf_crit,
    ADF_Reject_UnitRoot_5pct = is.finite(adf_stat) && is.finite(adf_crit) && adf_stat < adf_crit,
    KPSS_Null = kpss_null, KPSS_p_value = kpss_p,
    KPSS_Reject_Stationarity_5pct = is.finite(kpss_p) && kpss_p < 0.05,
    stringsAsFactors = FALSE
  )
}

stationarity_candidates <- list(
  II_first_difference = analysis_data$d_II,
  II_log_difference = analysis_data$g_II,
  IE_second_difference = analysis_data$dd_IE,
  IE_log_second_difference = analysis_data$ddlog_IE,
  d_Rate_3M = analysis_data$d_Rate_3M,
  d_Rate_10Y = analysis_data$d_Rate_10Y,
  d_Term_Spread = analysis_data$d_Term_Spread,
  d_BBB_Spread = analysis_data$d_BBB_Spread,
  d_BBB_Yield_sensitivity_only = analysis_data$d_BBB_Yield
)
stationarity_results <- do.call(rbind, lapply(names(stationarity_candidates), function(nm) {
  unit_root_row(nm, stationarity_candidates[[nm]])
}))
readr::write_csv(stationarity_results, file.path(output_dir, "stationarity_diagnostics_training_only.csv"))

passes_stationarity <- with(
  stationarity_results,
  ADF_Reject_UnitRoot_5pct & !KPSS_Reject_Stationarity_5pct
)
names(passes_stationarity) <- stationarity_results$Variable
if (!passes_stationarity[["IE_second_difference"]] &&
    !passes_stationarity[["IE_log_second_difference"]]) {
  writeLines(
    c(
      "Stationary VAR search stopped before model fitting.",
      "Neither approved second-difference IE transform passed both the ADF and KPSS 5% screen."
    ),
    file.path(output_dir, "STOP_stationarity_gate.txt")
  )
  stop("Stationarity gate failed for both IE transforms; no VAR search was run.", call. = FALSE)
}

ii_candidates <- list()
if (passes_stationarity[["II_first_difference"]]) {
  ii_candidates[["II_D"]] <- list(column = "d_II", reconstruction = "Difference")
}
if (passes_stationarity[["II_log_difference"]]) {
  ii_candidates[["II_G"]] <- list(column = "g_II", reconstruction = "LogDifference")
}
ie_candidates <- list()
if (passes_stationarity[["IE_second_difference"]]) {
  ie_candidates[["IE_D2"]] <- list(column = "dd_IE", reconstruction = "SecondDifference")
}
if (passes_stationarity[["IE_log_second_difference"]]) {
  ie_candidates[["IE_G2"]] <- list(column = "ddlog_IE", reconstruction = "SecondLogDifference")
}

macro_pool <- c("d_Rate_3M", "d_Rate_10Y", "d_Term_Spread", "d_BBB_Spread")
macro_keep <- macro_pool[passes_stationarity[macro_pool]]
if (!length(macro_keep)) stop("No macro variables passed the strict stationarity screen.", call. = FALSE)

macro_systems <- list(Base = character(0))
for (m in macro_keep) macro_systems[[m]] <- m
if (length(macro_keep) >= 2L) {
  pairs <- combn(macro_keep, 2L, simplify = FALSE)
  for (pair in pairs) {
    nm <- paste(pair, collapse = "_and_")
    macro_systems[[nm]] <- pair
  }
}

target_grid <- expand.grid(
  II_ID = names(ii_candidates),
  IE_ID = names(ie_candidates),
  stringsAsFactors = FALSE
)
specs <- list()
for (i in seq_len(nrow(target_grid))) {
  ii_id <- target_grid$II_ID[i]
  ie_id <- target_grid$IE_ID[i]
  for (macro_name in names(macro_systems)) {
    endog <- c(ii_candidates[[ii_id]]$column, ie_candidates[[ie_id]]$column, macro_systems[[macro_name]])
    if (length(macro_systems[[macro_name]]) > maximum_macro_variables) next
    for (p in seq_len(maximum_lag)) {
      residual_df <- window_size - (length(endog) * p + 1L)
      if (residual_df < minimum_residual_df) next
      id <- paste(ii_id, ie_id, macro_name, paste0("VAR", p), sep = "__")
      specs[[id]] <- list(
        Model_ID = id, II_ID = ii_id, IE_ID = ie_id,
        II_Column = ii_candidates[[ii_id]]$column,
        IE_Column = ie_candidates[[ie_id]]$column,
        II_Reconstruction = ii_candidates[[ii_id]]$reconstruction,
        IE_Reconstruction = ie_candidates[[ie_id]]$reconstruction,
        Macro_System = macro_name, Macro_Vars = macro_systems[[macro_name]],
        Endog_Vars = endog, Number_Endogenous = length(endog),
        Number_Macro = length(macro_systems[[macro_name]]), P = as.integer(p),
        Residual_DF = as.integer(residual_df)
      )
    }
  }
}
spec_table <- do.call(rbind, lapply(specs, function(s) {
  data.frame(
    Model_ID = s$Model_ID, II_Transform = s$II_ID, IE_Transform = s$IE_ID,
    Macro_System = s$Macro_System,
    Macro_Variables = if (length(s$Macro_Vars)) paste(s$Macro_Vars, collapse = " + ") else "None",
    Endogenous_Variables = paste(s$Endog_Vars, collapse = " + "),
    Number_Endogenous = s$Number_Endogenous, Number_Macro = s$Number_Macro,
    P = s$P, Residual_DF = s$Residual_DF, stringsAsFactors = FALSE
  )
}))
readr::write_csv(spec_table, file.path(output_dir, "candidate_specifications.csv"))

max_burnin <- 2L
common_origins <- seq.int(window_size + max_burnin, nrow(analysis_data) - forecast_horizon)
if (length(common_origins) < 10L) stop("Too few common rolling origins.", call. = FALSE)
forecast_rows <- list()
fit_rows <- list()
total_fits <- length(specs) * length(common_origins)
completed_fits <- 0L
last_pct <- 0L
progress_file <- file.path(output_dir, "progress.log")
writeLines(
  sprintf("Expected VAR origin fits: %d; window=%d; horizon=%d; origins=%d",
          total_fits, window_size, forecast_horizon, length(common_origins)),
  progress_file
)
cat(sprintf(
  "SEARCH_START specs=%d origins=%d expected_VAR_fits=%d window=%d horizon=%d\n",
  length(specs), length(common_origins), total_fits, window_size, forecast_horizon
))
flush.console()

reconstruct_ii <- function(z, origin, transform) {
  if (transform == "Difference") return(analysis_data$II[origin] + cumsum(z))
  if (transform == "LogDifference") return(exp(log(analysis_data$II[origin]) + cumsum(z)))
  stop("Unknown II reconstruction.", call. = FALSE)
}
reconstruct_ie <- function(z, origin, transform) {
  level <- analysis_data$IE[origin]
  prior_level <- analysis_data$IE[origin - 1L]
  if (transform == "SecondDifference") {
    prior_change <- level - prior_level
    forecast_changes <- prior_change + cumsum(z)
    return(level + cumsum(forecast_changes))
  }
  if (transform == "SecondLogDifference") {
    prior_growth <- log(level) - log(prior_level)
    forecast_growth <- prior_growth + cumsum(z)
    return(exp(log(level) + cumsum(forecast_growth)))
  }
  stop("Unknown IE reconstruction.", call. = FALSE)
}

progress_every <- max(1L, ceiling(total_fits * progress_step_pct / 100))
for (s in specs) {
  for (origin in common_origins) {
    training_idx <- seq.int(origin - window_size + 1L, origin)
    y <- as.data.frame(analysis_data[training_idx, s$Endog_Vars, drop = FALSE])
    fit <- NULL
    pred <- NULL
    warning_text <- character(0)
    error_text <- NA_character_
    if (any(!is.finite(as.matrix(y)))) {
      error_text <- "Non-finite input inside the 32-observation window."
    } else {
      attempt <- withCallingHandlers(
        tryCatch({
          model <- vars::VAR(y, p = s$P, type = "const")
          prediction <- stats::predict(model, n.ahead = forecast_horizon)
          list(fit = model, pred = prediction)
        }, error = function(e) e),
        warning = function(w) {
          warning_text <<- c(warning_text, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      if (inherits(attempt, "error")) {
        error_text <- conditionMessage(attempt)
      } else {
        fit <- attempt$fit
        pred <- attempt$pred
      }
    }
    max_root <- if (is.null(fit)) NA_real_ else tryCatch(max(Mod(vars::roots(fit))), error = function(e) NA_real_)
    fit_rows[[length(fit_rows) + 1L]] <- data.frame(
      Model_ID = s$Model_ID, Origin_Index = origin, Origin_Quarter = analysis_data$Quarter[origin],
      Fit_Failed = !is.null(error_text) && !is.na(error_text),
      Warning_Count = length(unique(warning_text)),
      Warning_Text = paste(unique(warning_text), collapse = " | "),
      Error_Text = if (is.na(error_text)) "" else error_text,
      Max_Root_Modulus = max_root,
      stringsAsFactors = FALSE
    )
    if (!is.null(fit) && !is.null(pred)) {
      transformed_forecasts <- vapply(s$Endog_Vars, function(v) {
        as.numeric(pred$fcst[[v]][, 1L])
      }, numeric(forecast_horizon))
      if (is.null(dim(transformed_forecasts))) {
        transformed_forecasts <- matrix(transformed_forecasts, ncol = length(s$Endog_Vars))
      }
      colnames(transformed_forecasts) <- s$Endog_Vars
      forecast_ii <- reconstruct_ii(
        transformed_forecasts[, s$II_Column], origin, s$II_Reconstruction
      )
      forecast_ie <- reconstruct_ie(
        transformed_forecasts[, s$IE_Column], origin, s$IE_Reconstruction
      )
      forecast_nii <- forecast_ii - forecast_ie
      target_forecasts <- list(II = forecast_ii, IE = forecast_ie, NII = forecast_nii)
      future_idx <- seq.int(origin + 1L, origin + forecast_horizon)
      actuals <- list(
        II = analysis_data$II[future_idx],
        IE = analysis_data$IE[future_idx],
        NII = analysis_data$NII[future_idx]
      )
      for (target in names(target_forecasts)) {
        scale <- mean(abs(diff(analysis_data[[target]][seq_len(origin)])))
        for (h in seq_len(forecast_horizon)) {
          value <- target_forecasts[[target]][h]
          forecast_rows[[length(forecast_rows) + 1L]] <- data.frame(
            Model_ID = s$Model_ID, Origin_Index = origin,
            Origin_Quarter = analysis_data$Quarter[origin], Horizon = h,
            Target = target, Actual_Billions = actuals[[target]][h],
            Forecast_Billions = value, Error_Billions = value - actuals[[target]][h],
            MASE_Scale_Billions = scale,
            Forecast_Is_Negative = is.finite(value) && value < 0,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    completed_fits <- completed_fits + 1L
    if (completed_fits %% progress_every == 0L || completed_fits == total_fits) {
      pct <- min(100L, floor(100 * completed_fits / total_fits))
      if (pct >= last_pct + progress_step_pct || completed_fits == total_fits) {
        line <- sprintf(
          "PROGRESS %d%% (%d/%d VAR fits), elapsed %.1f sec",
          pct, completed_fits, total_fits, proc.time()[["elapsed"]]
        )
        cat(line, "\n")
        flush.console()
        cat(line, "\n", file = progress_file, append = TRUE)
        last_pct <- pct
      }
    }
  }
}

all_forecasts <- if (length(forecast_rows)) do.call(rbind, forecast_rows) else data.frame()
fit_results <- do.call(rbind, fit_rows)
readr::write_csv(all_forecasts, file.path(output_dir, "all_origin_forecasts.csv"))
readr::write_csv(fit_results, file.path(output_dir, "fit_status_by_model_origin.csv"))

summarize_one <- function(model_id, rows) {
  d <- rows[rows$Model_ID == model_id, , drop = FALSE]
  target_stats <- lapply(c("II", "IE", "NII"), function(target) {
    z <- d[d$Target == target, , drop = FALSE]
    data.frame(
      Target = target,
      MASE = mean(abs(z$Error_Billions) / z$MASE_Scale_Billions),
      MAE_Billions = mean(abs(z$Error_Billions)),
      RMSE_Billions = sqrt(mean(z$Error_Billions^2)),
      MAPE_pct = mean(abs(z$Error_Billions / z$Actual_Billions)) * 100,
      stringsAsFactors = FALSE
    )
  })
  ts <- do.call(rbind, target_stats)
  f <- fit_results[fit_results$Model_ID == model_id, , drop = FALSE]
  fs <- spec_table[spec_table$Model_ID == model_id, , drop = FALSE]
  data.frame(
    Model_ID = model_id, II_Transform = fs$II_Transform, IE_Transform = fs$IE_Transform,
    Macro_System = fs$Macro_System, Macro_Variables = fs$Macro_Variables,
    Number_Macro = fs$Number_Macro, P = fs$P, Residual_DF = fs$Residual_DF,
    Common_Origins = length(unique(d$Origin_Index)),
    Forecast_Rows = nrow(d), Failed_Origins = sum(f$Fit_Failed),
    Warned_Origins = sum(f$Warning_Count > 0L),
    Stable_Root_Rate = mean(f$Max_Root_Modulus < 1, na.rm = TRUE),
    Max_Root_Modulus = if (all(is.na(f$Max_Root_Modulus))) NA_real_ else max(f$Max_Root_Modulus, na.rm = TRUE),
    II_MASE = ts$MASE[ts$Target == "II"], IE_MASE = ts$MASE[ts$Target == "IE"],
    NII_MASE = ts$MASE[ts$Target == "NII"],
    Joint_MASE = mean(ts$MASE),
    II_MAE_Billions = ts$MAE_Billions[ts$Target == "II"],
    IE_MAE_Billions = ts$MAE_Billions[ts$Target == "IE"],
    NII_MAE_Billions = ts$MAE_Billions[ts$Target == "NII"],
    II_RMSE_Billions = ts$RMSE_Billions[ts$Target == "II"],
    IE_RMSE_Billions = ts$RMSE_Billions[ts$Target == "IE"],
    NII_RMSE_Billions = ts$RMSE_Billions[ts$Target == "NII"],
    II_MAPE_pct = ts$MAPE_pct[ts$Target == "II"],
    IE_MAPE_pct = ts$MAPE_pct[ts$Target == "IE"],
    Negative_II_Forecasts = sum(d$Target == "II" & d$Forecast_Is_Negative, na.rm = TRUE),
    Negative_IE_Forecasts = sum(d$Target == "IE" & d$Forecast_Is_Negative, na.rm = TRUE),
    Finite_Forecasts = all(is.finite(d$Forecast_Billions)),
    Complete_Common_Backtest = length(unique(d$Origin_Index)) == length(common_origins) &&
      nrow(d) == length(common_origins) * forecast_horizon * 3L,
    No_Negative_Components = sum(d$Target == "II" & d$Forecast_Is_Negative, na.rm = TRUE) == 0L &&
      sum(d$Target == "IE" & d$Forecast_Is_Negative, na.rm = TRUE) == 0L,
    stringsAsFactors = FALSE
  )
}

model_results <- do.call(rbind, lapply(names(specs), summarize_one, rows = all_forecasts))
model_results$Rank_Eligible <- with(
  model_results,
  Complete_Common_Backtest & Finite_Forecasts & Failed_Origins == 0L & No_Negative_Components
)
model_results <- model_results[order(model_results$Joint_MASE), , drop = FALSE]
readr::write_csv(model_results, file.path(output_dir, "all_model_results.csv"))
top50 <- head(model_results[model_results$Rank_Eligible, , drop = FALSE], 50L)
readr::write_csv(top50, file.path(output_dir, "top50_feasible_joint_mase.csv"))

# Compare stationary macro VARs with target-only VARs and level benchmarks.
benchmark_rows <- list()
for (origin in common_origins) {
  history_idx <- seq.int(origin - window_size + 1L, origin)
  for (method in c("Random_Walk", "AR1", "AR2")) {
    component_fc <- list()
    for (target in c("II", "IE")) {
      y <- analysis_data[[target]][history_idx]
      if (method == "Random_Walk") {
        component_fc[[target]] <- rep(tail(y, 1L), forecast_horizon)
      } else {
        p <- as.integer(sub("AR", "", method))
        response <- y[seq.int(p + 1L, length(y))]
        frame <- data.frame(y = response)
        for (lag in seq_len(p)) {
          frame[[paste0("lag", lag)]] <- y[seq.int(p + 1L - lag, length(y) - lag)]
        }
        fit <- stats::lm(y ~ ., data = frame)
        history <- y
        pred <- numeric(forecast_horizon)
        for (h in seq_len(forecast_horizon)) {
          new_row <- as.data.frame(as.list(vapply(
            seq_len(p), function(lag) history[length(history) - lag + 1L], numeric(1)
          )))
          names(new_row) <- paste0("lag", seq_len(p))
          pred[h] <- as.numeric(stats::predict(fit, newdata = new_row))
          history <- c(history, pred[h])
        }
        component_fc[[target]] <- pred
      }
    }
    target_fc <- list(
      II = component_fc$II, IE = component_fc$IE,
      NII = component_fc$II - component_fc$IE
    )
    future_idx <- seq.int(origin + 1L, origin + forecast_horizon)
    actuals <- list(
      II = analysis_data$II[future_idx],
      IE = analysis_data$IE[future_idx],
      NII = analysis_data$NII[future_idx]
    )
    for (target in names(target_fc)) {
      scale <- mean(abs(diff(analysis_data[[target]][seq_len(origin)])))
      for (h in seq_len(forecast_horizon)) {
        value <- target_fc[[target]][h]
        benchmark_rows[[length(benchmark_rows) + 1L]] <- data.frame(
          Model_ID = paste0("BENCH__", method), Origin_Index = origin,
          Origin_Quarter = analysis_data$Quarter[origin], Horizon = h, Target = target,
          Actual_Billions = actuals[[target]][h], Forecast_Billions = value,
          Error_Billions = value - actuals[[target]][h], MASE_Scale_Billions = scale,
          Forecast_Is_Negative = is.finite(value) && value < 0,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}
benchmark_forecasts <- do.call(rbind, benchmark_rows)
benchmark_results <- do.call(rbind, lapply(unique(benchmark_forecasts$Model_ID), function(id) {
  d <- benchmark_forecasts[benchmark_forecasts$Model_ID == id, , drop = FALSE]
  ts <- lapply(c("II", "IE", "NII"), function(target) {
    z <- d[d$Target == target, , drop = FALSE]
    mean(abs(z$Error_Billions) / z$MASE_Scale_Billions)
  })
  data.frame(
    Model_ID = id, Common_Origins = length(unique(d$Origin_Index)),
    II_MASE = ts[[1L]], IE_MASE = ts[[2L]], NII_MASE = ts[[3L]],
    Joint_MASE = mean(unlist(ts)), stringsAsFactors = FALSE
  )
}))
readr::write_csv(benchmark_forecasts, file.path(output_dir, "benchmark_origin_forecasts.csv"))
readr::write_csv(benchmark_results, file.path(output_dir, "benchmark_results.csv"))

# Measure each macro block against its matching target-only VAR.
base_rows <- model_results[model_results$Macro_System == "Base", , drop = FALSE]
incremental_rows <- list()
for (i in seq_len(nrow(model_results))) {
  m <- model_results[i, , drop = FALSE]
  if (m$Macro_System == "Base") next
  b <- base_rows[
    base_rows$II_Transform == m$II_Transform &
      base_rows$IE_Transform == m$IE_Transform &
      base_rows$P == m$P, , drop = FALSE
  ]
  if (nrow(b) != 1L) next
  incremental_rows[[length(incremental_rows) + 1L]] <- data.frame(
    Model_ID = m$Model_ID, Macro_System = m$Macro_System, P = m$P,
    Base_Joint_MASE = b$Joint_MASE, Macro_Joint_MASE = m$Joint_MASE,
    Joint_MASE_Improvement_pct = 100 * (b$Joint_MASE - m$Joint_MASE) / b$Joint_MASE,
    stringsAsFactors = FALSE
  )
}
incremental_results <- if (length(incremental_rows)) do.call(rbind, incremental_rows) else data.frame()
readr::write_csv(incremental_results, file.path(output_dir, "macro_incremental_value.csv"))

saveRDS(
  list(
    stationarity = stationarity_results, specifications = spec_table,
    model_results = model_results, forecasts = all_forecasts,
    fit_status = fit_results, benchmarks = benchmark_results,
    benchmark_forecasts = benchmark_forecasts,
    macro_incremental_value = incremental_results
  ),
  file.path(output_dir, "var_stationary_results.rds"),
  compress = "gzip"
)

metadata <- data.frame(
  Run_Date = as.character(Sys.Date()),
  Training_Start = train$Quarter[1L], Training_End = tail(train$Quarter, 1L),
  Training_Rows = nrow(train), Holdout_Loaded = FALSE,
  Window = window_size, Horizon = forecast_horizon,
  Common_Origins = length(common_origins),
  First_Common_Origin = analysis_data$Quarter[common_origins[1L]],
  Last_Common_Origin = analysis_data$Quarter[tail(common_origins, 1L)],
  Maximum_Macro_Variables = maximum_macro_variables,
  Candidate_Specifications = length(specs),
  Expected_VAR_Fits = total_fits,
  Successful_VAR_Fits = sum(!fit_results$Fit_Failed),
  Failed_VAR_Fits = sum(fit_results$Fit_Failed),
  Warned_VAR_Fits = sum(fit_results$Warning_Count > 0L),
  Elapsed_Seconds = proc.time()[["elapsed"]],
  stringsAsFactors = FALSE
)
readr::write_csv(metadata, file.path(output_dir, "run_metadata.csv"))

readme <- c(
  "# Stationary training-only VAR search",
  "",
  paste0("Training outcomes: ", train$Quarter[1L], "–", tail(train$Quarter, 1L), " (", nrow(train), " quarters)."),
  paste0("Window: ", window_size, " observations; forecast horizon: ", forecast_horizon, " quarters."),
  paste0("Common origins: ", length(common_origins), " (", analysis_data$Quarter[common_origins[1L]],
         "–", analysis_data$Quarter[tail(common_origins, 1L)], ")."),
  "The holdout/test outcome file was not loaded.",
  "",
  paste0("Candidate specifications: ", length(specs), "; expected model-origin fits: ", total_fits, "."),
  paste0("Macro-variable cap: ", maximum_macro_variables, " per VAR; VAR lags: 1–", maximum_lag, "."),
  paste0("Stationary macro variables retained: ", paste(macro_keep, collapse = ", "), "."),
  paste0("II transformations retained: ", paste(names(ii_candidates), collapse = ", "), "."),
  paste0("IE transformations retained: ", paste(names(ie_candidates), collapse = ", "), "."),
  "",
  "The top table ranks complete, finite, nonnegative II/IE forecasts by the equal-weight mean MASE for II, IE and derived NII.",
  "Forecast origins overlap, so rolling errors are dependent and the best score is a training-only selection result.",
  "The current search does not apply a Fed stress scenario and does not model noninterest income/expense.",
  "",
  "Files: candidate_specifications.csv, stationarity_diagnostics_training_only.csv, all_model_results.csv,",
  "top50_feasible_joint_mase.csv, all_origin_forecasts.csv, fit_status_by_model_origin.csv,",
  "benchmark_results.csv, benchmark_origin_forecasts.csv, macro_incremental_value.csv, run_metadata.csv,",
  "var_stationary_results.rds, progress.log."
)
writeLines(readme, file.path(output_dir, "README.md"))

cat("\nTOP_10_FEASIBLE\n")
print(utils::head(top50, 10L), row.names = FALSE)
cat("\nBENCHMARKS\n")
print(benchmark_results, row.names = FALSE)
cat("\nRUN_METADATA\n")
print(metadata, row.names = FALSE)
cat("\nSEARCH_COMPLETE\n")
