# Training-only VAR model comparison for Morgan Stanley interest income/expense.
# This script reads only the approved training outcome file and the macro source.
# It never loads the Y-9C holdout/test outcome file.

required_packages <- c("readr", "vars", "urca", "tseries")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Missing R package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

train_file <- file.path("data_splits", "train_y9c_outcomes_2009Q1_2024Q2.csv")
macro_file <- "2025-Table_1A_Historic_Domestic.csv"
output_dir <- file.path("results", "var_training_20261116")
window_grid <- c(12L, 16L, 20L, 24L, 28L, 32L)
forecast_horizon <- 9L
minimum_residual_df <- 8L
maximum_lag <- 4L

if (!file.exists(train_file)) stop("Missing approved training file: ", train_file, call. = FALSE)
if (!file.exists(macro_file)) stop("Missing macro source file: ", macro_file, call. = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

quarter_from_date <- function(x) {
  d <- as.Date(x)
  q <- (as.integer(format(d, "%m")) - 1L) %/% 3L + 1L
  paste(format(d, "%Y"), paste0("Q", q))
}
quarter_index <- function(x) {
  parts <- regmatches(x, regexec("^(\\d{4}) Q([1-4])$", x))
  if (any(lengths(parts) != 3L)) stop("Unexpected quarter label.", call. = FALSE)
  vapply(parts, function(z) as.integer(z[2L]) * 4L + as.integer(z[3L]), integer(1))
}

# Use the approved target-only split; do not read any holdout/test outcomes.
train_raw <- readr::read_csv(train_file, show_col_types = FALSE)
train <- data.frame(
  Quarter = quarter_from_date(train_raw$Date),
  II = as.numeric(train_raw[["Total interest income"]]) / 1e6,
  IE = as.numeric(train_raw[["Total interest expense"]]) / 1e6,
  NII_reported = as.numeric(train_raw[["Net interest income"]]) / 1e6,
  stringsAsFactors = FALSE
)
train$NII <- train$II - train$IE
if (nrow(train) != 62L || train$Quarter[1L] != "2009 Q1" || tail(train$Quarter, 1L) != "2024 Q2") {
  stop("Training outcomes do not match the approved 62-quarter span.", call. = FALSE)
}
if (anyDuplicated(train$Quarter) || any(diff(quarter_index(train$Quarter)) != 1L)) {
  stop("Training outcomes contain duplicate or missing quarters.", call. = FALSE)
}
if (any(!is.finite(as.matrix(train[, c("II", "IE", "NII", "NII_reported")]))) || any(train$II <= 0) || any(train$IE <= 0)) {
  stop("Training outcomes contain invalid or non-positive component values.", call. = FALSE)
}
if (max(abs(train$NII - train$NII_reported)) > 1e-8) {
  stop("Training NII does not reconcile to interest income minus expense.", call. = FALSE)
}

# Read only macro series, then retain rows matching the approved Y-9C training quarters.
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
  stop("Macro data contain missing/non-finite values in the training span.", call. = FALSE)
}

analysis_data <- cbind(train, macro[, c("Rate_3M", "Rate_10Y", "BBB_Yield", "VIX"), drop = FALSE])
analysis_data$Term_Spread <- analysis_data$Rate_10Y - analysis_data$Rate_3M
analysis_data$BBB_Spread <- analysis_data$BBB_Yield - analysis_data$Rate_10Y
analysis_data$d_II <- c(NA_real_, diff(analysis_data$II))
analysis_data$d_IE <- c(NA_real_, diff(analysis_data$IE))
analysis_data$g_II <- c(NA_real_, diff(log(analysis_data$II)))
analysis_data$g_IE <- c(NA_real_, diff(log(analysis_data$IE)))
analysis_data$d_Rate_3M <- c(NA_real_, diff(analysis_data$Rate_3M))
analysis_data$d_Rate_10Y <- c(NA_real_, diff(analysis_data$Rate_10Y))
analysis_data$d_BBB_Yield <- c(NA_real_, diff(analysis_data$BBB_Yield))
analysis_data$d_Term_Spread <- c(NA_real_, diff(analysis_data$Term_Spread))
analysis_data$d_BBB_Spread <- c(NA_real_, diff(analysis_data$BBB_Spread))

# Unit-root diagnostics are descriptive and use only the 62-quarter training span.
unit_root_row <- function(name, series, adf_type, kpss_null) {
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
    ADF_Reject_UnitRoot_5pct = if (is.finite(adf_stat) && is.finite(adf_crit)) adf_stat < adf_crit else NA,
    KPSS_Null = kpss_null, KPSS_p_value = kpss_p,
    KPSS_Reject_Stationarity_5pct = if (is.finite(kpss_p)) kpss_p < 0.05 else NA,
    stringsAsFactors = FALSE
  )
}
stationarity_rows <- list()
stationarity_rows[[length(stationarity_rows) + 1L]] <- unit_root_row("II_level", analysis_data$II, "trend", "Trend")
stationarity_rows[[length(stationarity_rows) + 1L]] <- unit_root_row("IE_level", analysis_data$IE, "trend", "Trend")
stationarity_rows[[length(stationarity_rows) + 1L]] <- unit_root_row("II_first_difference", analysis_data$d_II, "drift", "Level")
stationarity_rows[[length(stationarity_rows) + 1L]] <- unit_root_row("IE_first_difference", analysis_data$d_IE, "drift", "Level")
stationarity_rows[[length(stationarity_rows) + 1L]] <- unit_root_row("II_log_difference", analysis_data$g_II, "drift", "Level")
stationarity_rows[[length(stationarity_rows) + 1L]] <- unit_root_row("IE_log_difference", analysis_data$g_IE, "drift", "Level")
for (nm in c("Rate_3M", "Rate_10Y", "BBB_Yield", "Term_Spread", "BBB_Spread", "VIX")) {
  adf_type <- if (nm %in% c("Rate_3M", "Rate_10Y", "BBB_Yield", "VIX")) "trend" else "drift"
  kpss_null <- if (adf_type == "trend") "Trend" else "Level"
  stationarity_rows[[length(stationarity_rows) + 1L]] <- unit_root_row(paste0(nm, "_level"), analysis_data[[nm]], adf_type, kpss_null)
}
for (nm in c("Rate_3M", "Rate_10Y", "BBB_Yield", "Term_Spread", "BBB_Spread")) {
  diff_nm <- paste0("d_", nm)
  stationarity_rows[[length(stationarity_rows) + 1L]] <- unit_root_row(diff_nm, analysis_data[[diff_nm]], "drift", "Level")
}
stationarity_results <- do.call(rbind, stationarity_rows)
readr::write_csv(stationarity_results, file.path(output_dir, "stationarity_diagnostics_training_only.csv"))

# Seasonal mean-shift checks on the two component levels and candidate stationary transforms.
quarter_factor <- factor(sub(".*Q", "", analysis_data$Quarter), levels = as.character(1:4))
seasonality_variables <- c("II", "IE", "d_II", "d_IE", "g_II", "g_IE")
seasonality_rows <- lapply(seasonality_variables, function(nm) {
  x <- analysis_data[[nm]]
  keep <- is.finite(x)
  fit <- stats::lm(x[keep] ~ quarter_factor[keep])
  av <- stats::anova(fit)
  data.frame(Variable = nm, N = sum(keep), Seasonality_F = unname(summary(fit)$fstatistic[1L]),
             Seasonality_p = av[["Pr(>F)"]][1L], stringsAsFactors = FALSE)
})
seasonality_results <- do.call(rbind, seasonality_rows)
readr::write_csv(seasonality_results, file.path(output_dir, "seasonality_diagnostics_training_only.csv"))

# Johansen trace tests for the two target levels; used to assess, not assume, VECM support.
cointegration_rows <- list()
for (k in c(2L, 3L)) {
  jo <- tryCatch(
    urca::ca.jo(as.matrix(analysis_data[, c("II", "IE")]), type = "trace", ecdet = "const", K = k, spec = "transitory"),
    error = function(e) NULL
  )
  if (is.null(jo)) next
  for (i in seq_along(jo@teststat)) {
    null_name <- rownames(jo@cval)[i]
    cointegration_rows[[length(cointegration_rows) + 1L]] <- data.frame(
      K_Level_VAR_Lags = k, Null_Hypothesis = null_name,
      Trace_Statistic = as.numeric(jo@teststat[i]),
      Critical_5pct = as.numeric(jo@cval[i, "5pct"]),
      Reject_Null_5pct = as.numeric(jo@teststat[i]) > as.numeric(jo@cval[i, "5pct"]),
      stringsAsFactors = FALSE
    )
  }
}
cointegration_results <- if (length(cointegration_rows)) do.call(rbind, cointegration_rows) else data.frame()
readr::write_csv(cointegration_results, file.path(output_dir, "johansen_trace_II_IE_training_only.csv"))

# Lag criteria on target-only systems, using the same training-only sample.
target_transform_grid <- list(
  list(id = "LEVEL_LEVEL", ii_col = "II", ie_col = "IE", ii_transform = "Level", ie_transform = "Level", family = "Level_VAR"),
  list(id = "D_D", ii_col = "d_II", ie_col = "d_IE", ii_transform = "Difference", ie_transform = "Difference", family = "Differenced_Target_VAR"),
  list(id = "D_G", ii_col = "d_II", ie_col = "g_IE", ii_transform = "Difference", ie_transform = "LogDifference", family = "Differenced_Target_VAR"),
  list(id = "G_D", ii_col = "g_II", ie_col = "d_IE", ii_transform = "LogDifference", ie_transform = "Difference", family = "Differenced_Target_VAR"),
  list(id = "G_G", ii_col = "g_II", ie_col = "g_IE", ii_transform = "LogDifference", ie_transform = "LogDifference", family = "Differenced_Target_VAR")
)
lag_selection_rows <- lapply(target_transform_grid, function(tg) {
  y <- as.data.frame(analysis_data[, c(tg$ii_col, tg$ie_col), drop = FALSE])
  y <- y[stats::complete.cases(y), , drop = FALSE]
  names(y) <- c("II_series", "IE_series")
  sel <- vars::VARselect(y, lag.max = maximum_lag, type = "const")$selection
  data.frame(Transform_Combo = tg$id, N = nrow(y), AIC = unname(sel["AIC(n)"]), HQ = unname(sel["HQ(n)"]),
             SC = unname(sel["SC(n)"]), FPE = unname(sel["FPE(n)"]), stringsAsFactors = FALSE)
})
lag_selection_results <- do.call(rbind, lag_selection_rows)
readr::write_csv(lag_selection_results, file.path(output_dir, "lag_selection_training_only.csv"))

# These candidate sets avoid placing raw highly-related Treasury yields together by default.
# The raw 3M+10Y system is retained explicitly as a multicollinearity sensitivity candidate.
level_macro_systems <- list(
  Base = character(0),
  Rate_3M_level = "Rate_3M",
  Rate_10Y_level = "Rate_10Y",
  BBB_Yield_level = "BBB_Yield",
  Term_Spread_level = "Term_Spread",
  BBB_Spread_level = "BBB_Spread",
  VIX_level = "VIX",
  Rate_3M_plus_Rate_10Y = c("Rate_3M", "Rate_10Y"),
  Rate_3M_plus_Term_Spread = c("Rate_3M", "Term_Spread")
)
stationary_macro_systems <- list(
  Base = character(0),
  Rate_3M_change = "d_Rate_3M",
  Rate_10Y_change = "d_Rate_10Y",
  BBB_Yield_change = "d_BBB_Yield",
  Term_Spread_change = "d_Term_Spread",
  BBB_Spread_change = "d_BBB_Spread",
  VIX_level = "VIX",
  Rate_3M_and_10Y_changes = c("d_Rate_3M", "d_Rate_10Y"),
  Rate_3M_change_and_Term_Spread_change = c("d_Rate_3M", "d_Term_Spread")
)

specs <- list()
for (tg in target_transform_grid) {
  systems <- if (tg$family == "Level_VAR") level_macro_systems else stationary_macro_systems
  for (system_name in names(systems)) {
    macro_vars <- systems[[system_name]]
    endog_vars <- c(tg$ii_col, tg$ie_col, macro_vars)
    if (length(endog_vars) > 4L) next
    for (p in seq_len(maximum_lag)) {
      id <- paste(tg$id, system_name, paste0("VAR", p), sep = "__")
      specs[[id]] <- list(
        Model_ID = id, Family = tg$family, Target_Transform = tg$id,
        II_Transform = tg$ii_transform, IE_Transform = tg$ie_transform,
        II_Column = tg$ii_col, IE_Column = tg$ie_col,
        Macro_System = system_name, Macro_Vars = macro_vars,
        Endog_Vars = endog_vars, P = as.integer(p)
      )
    }
  }
}

spec_table <- do.call(rbind, lapply(specs, function(s) data.frame(
  Model_ID = s$Model_ID, Family = s$Family, Target_Transform = s$Target_Transform,
  II_Transform = s$II_Transform, IE_Transform = s$IE_Transform,
  Macro_System = s$Macro_System,
  Macro_Variables = if (length(s$Macro_Vars)) paste(s$Macro_Vars, collapse = " + ") else "None",
  Endogenous_Variables = paste(s$Endog_Vars, collapse = " + "),
  Number_Endogenous = length(s$Endog_Vars), P = s$P,
  stringsAsFactors = FALSE
)))
readr::write_csv(spec_table, file.path(output_dir, "candidate_specifications.csv"))

# Rank by equal-weight rolling forecast errors, not by in-sample fit.
common_origins <- seq.int(max(window_grid) + 1L, nrow(analysis_data) - forecast_horizon)
common_origin_labels <- analysis_data$Quarter[common_origins]
tasks <- list()
excluded <- list()
for (s in specs) {
  k <- length(s$Endog_Vars)
  for (w in window_grid) {
    residual_df <- w - (k * s$P + 1L)
    if (residual_df < minimum_residual_df) {
      excluded[[length(excluded) + 1L]] <- data.frame(
        Model_ID = s$Model_ID, Window = w, Number_Endogenous = k, P = s$P,
        Residual_DF = residual_df, Exclusion_Reason = paste0("Residual df below ", minimum_residual_df),
        stringsAsFactors = FALSE
      )
    } else {
      tasks[[length(tasks) + 1L]] <- list(spec = s, window = as.integer(w), residual_df = as.integer(residual_df))
    }
  }
}
excluded_results <- if (length(excluded)) do.call(rbind, excluded) else data.frame()
readr::write_csv(excluded_results, file.path(output_dir, "excluded_short_window_candidates.csv"))

forecast_rows_for_task <- function(task) {
  s <- task$spec
  w <- task$window
  origins <- seq.int(w + 1L, nrow(analysis_data) - forecast_horizon)
  fc_rows <- list()
  fit_rows <- list()
  for (origin in origins) {
    train_idx <- seq.int(origin - w + 1L, origin)
    y <- as.data.frame(analysis_data[train_idx, s$Endog_Vars, drop = FALSE])
    if (any(!is.finite(as.matrix(y)))) {
      fit_rows[[length(fit_rows) + 1L]] <- data.frame(
        Model_ID = s$Model_ID, Window = w, Origin_Index = origin,
        Adj_R2_Mean = NA_real_, Max_Root_Modulus = NA_real_, Fit_Failed = TRUE,
        Warning_Count = 0L, Warning_Text = "Non-finite training input", stringsAsFactors = FALSE
      )
      next
    }
    warning_text <- character(0)
    captured <- withCallingHandlers(
      tryCatch({
        fit <- vars::VAR(y, p = s$P, type = "const")
        pred <- stats::predict(fit, n.ahead = forecast_horizon)
        list(fit = fit, pred = pred)
      }, error = function(e) e),
      warning = function(wrn) {
        warning_text <<- c(warning_text, conditionMessage(wrn))
        invokeRestart("muffleWarning")
      }
    )
    if (inherits(captured, "error")) {
      fit_rows[[length(fit_rows) + 1L]] <- data.frame(
        Model_ID = s$Model_ID, Window = w, Origin_Index = origin,
        Adj_R2_Mean = NA_real_, Max_Root_Modulus = NA_real_, Fit_Failed = TRUE,
        Warning_Count = length(warning_text), Warning_Text = conditionMessage(captured), stringsAsFactors = FALSE
      )
      next
    }
    fit <- captured$fit
    pred <- captured$pred
    predicted_transformed <- sapply(s$Endog_Vars, function(v) as.numeric(pred$fcst[[v]][, 1L]))
    if (is.null(dim(predicted_transformed))) predicted_transformed <- matrix(predicted_transformed, ncol = length(s$Endog_Vars))
    colnames(predicted_transformed) <- s$Endog_Vars

    reconstruct <- function(target, col, transform) {
      z <- predicted_transformed[, col]
      if (transform == "Level") return(as.numeric(z))
      if (transform == "Difference") return(as.numeric(analysis_data[[target]][origin] + cumsum(z)))
      if (transform == "LogDifference") return(as.numeric(analysis_data[[target]][origin] * exp(cumsum(z))))
      stop("Unknown target transformation: ", transform, call. = FALSE)
    }
    forecast_ii <- reconstruct("II", s$II_Column, s$II_Transform)
    forecast_ie <- reconstruct("IE", s$IE_Column, s$IE_Transform)
    forecast_nii <- forecast_ii - forecast_ie
    target_forecasts <- list(II = forecast_ii, IE = forecast_ie, NII = forecast_nii)
    root_max <- tryCatch(max(Mod(vars::roots(fit))), error = function(e) NA_real_)
    adj <- vapply(fit$varresult, function(eq) tryCatch(summary(eq)$adj.r.squared, error = function(e) NA_real_), numeric(1))
    fit_rows[[length(fit_rows) + 1L]] <- data.frame(
      Model_ID = s$Model_ID, Window = w, Origin_Index = origin,
      Adj_R2_Mean = mean(adj, na.rm = TRUE), Max_Root_Modulus = root_max,
      Fit_Failed = FALSE, Warning_Count = length(unique(warning_text)),
      Warning_Text = paste(unique(warning_text), collapse = " | "), stringsAsFactors = FALSE
    )
    future_idx <- seq.int(origin + 1L, origin + forecast_horizon)
    actuals <- list(II = analysis_data$II[future_idx], IE = analysis_data$IE[future_idx], NII = analysis_data$NII[future_idx])
    for (target in names(target_forecasts)) {
      scale <- mean(abs(diff(analysis_data[[target]][seq_len(origin)])))
      fc <- target_forecasts[[target]]
      for (h in seq_len(forecast_horizon)) {
        actual <- actuals[[target]][h]
        value <- fc[h]
        fc_rows[[length(fc_rows) + 1L]] <- data.frame(
          Model_ID = s$Model_ID, Window = w, Origin_Index = origin,
          Origin_Quarter = analysis_data$Quarter[origin], Horizon = h,
          Target = target, Actual_Billions = actual, Forecast_Billions = value,
          Error_Billions = value - actual, MASE_Scale_Billions = scale,
          Forecast_Is_Negative = value < 0, stringsAsFactors = FALSE
        )
      }
    }
  }
  list(
    forecasts = if (length(fc_rows)) do.call(rbind, fc_rows) else data.frame(),
    fits = if (length(fit_rows)) do.call(rbind, fit_rows) else data.frame()
  )
}

workers_requested <- suppressWarnings(as.integer(Sys.getenv("PPNR_VAR_WORKERS", "4")))
physical_cores <- parallel::detectCores(logical = FALSE)
if (is.na(physical_cores)) {
  workers <- max(1L, min(4L, workers_requested))
} else {
  workers <- max(1L, min(8L, workers_requested, max(1L, physical_cores - 1L)))
}
cat("Training-only sample:", nrow(analysis_data), "quarters,", analysis_data$Quarter[1L], "to", tail(analysis_data$Quarter, 1L), "\n")
cat("Candidate specifications:", length(specs), "| estimable model-window pairs:", length(tasks),
    "| common forecast origins:", length(common_origins), "(", common_origin_labels[1L], "to", tail(common_origin_labels, 1L), ")\n")
cat("Fitting rolling-origin VARs with", workers, "worker(s); each origin forecasts 9 quarters within training only.\n")
flush.console()

number_of_chunks <- min(workers, length(tasks))
chunk_ids <- ceiling(seq_along(tasks) * number_of_chunks / length(tasks))
task_chunks <- split(tasks, chunk_ids)
all_results <- list()
for (chunk_no in seq_along(task_chunks)) {
  chunk <- task_chunks[[chunk_no]]
  if (.Platform$OS.type == "unix" && workers > 1L) {
    chunk_result <- parallel::mclapply(chunk, forecast_rows_for_task, mc.cores = min(workers, length(chunk)), mc.preschedule = TRUE, mc.set.seed = FALSE)
  } else {
    chunk_result <- lapply(chunk, forecast_rows_for_task)
  }
  all_results <- c(all_results, chunk_result)
  cat("Completed VAR batch", chunk_no, "of", length(task_chunks), "(", length(chunk), "model-window pairs).\n")
  flush.console()
}
all_forecasts <- do.call(rbind, lapply(all_results, `[[`, "forecasts"))
fit_origin_results <- do.call(rbind, lapply(all_results, `[[`, "fits"))

summarize_forecasts <- function(x) {
  keys <- unique(x[, c("Model_ID", "Window"), drop = FALSE])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    z <- x[x$Model_ID == keys$Model_ID[i] & x$Window == keys$Window[i], , drop = FALSE]
    metric <- function(target, field, fun = mean) {
      zz <- z[z$Target == target, , drop = FALSE]
      if (!nrow(zz)) return(NA_real_)
      fun(zz[[field]], na.rm = TRUE)
    }
    mase <- function(target) {
      zz <- z[z$Target == target, , drop = FALSE]
      mean(abs(zz$Error_Billions) / zz$MASE_Scale_Billions, na.rm = TRUE)
    }
    mape <- function(target) {
      zz <- z[z$Target == target, , drop = FALSE]
      mean(abs(zz$Error_Billions) / abs(zz$Actual_Billions), na.rm = TRUE) * 100
    }
    ii_mase <- mase("II"); ie_mase <- mase("IE"); nii_mase <- mase("NII")
    data.frame(
      Model_ID = keys$Model_ID[i], Window = keys$Window[i],
      Forecasts = nrow(z), Origins = length(unique(z$Origin_Index)),
      II_MASE = ii_mase, IE_MASE = ie_mase, NII_MASE = nii_mase,
      Joint_MASE = mean(c(ii_mase, ie_mase, nii_mase), na.rm = TRUE),
      II_MAE_Billions = metric("II", "Error_Billions", function(v, na.rm) mean(abs(v), na.rm = na.rm)),
      IE_MAE_Billions = metric("IE", "Error_Billions", function(v, na.rm) mean(abs(v), na.rm = na.rm)),
      NII_MAE_Billions = metric("NII", "Error_Billions", function(v, na.rm) mean(abs(v), na.rm = na.rm)),
      II_RMSE_Billions = sqrt(metric("II", "Error_Billions", function(v, na.rm) mean(v^2, na.rm = na.rm))),
      IE_RMSE_Billions = sqrt(metric("IE", "Error_Billions", function(v, na.rm) mean(v^2, na.rm = na.rm))),
      NII_RMSE_Billions = sqrt(metric("NII", "Error_Billions", function(v, na.rm) mean(v^2, na.rm = na.rm))),
      II_MAPE_pct = mape("II"), IE_MAPE_pct = mape("IE"),
      Negative_Forecast_Rate = mean(z$Forecast_Is_Negative, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

all_metrics <- summarize_forecasts(all_forecasts)
common_forecasts <- all_forecasts[all_forecasts$Origin_Index %in% common_origins, , drop = FALSE]
common_metrics <- summarize_forecasts(common_forecasts)
names(all_metrics)[!(names(all_metrics) %in% c("Model_ID", "Window"))] <- paste0("All_", names(all_metrics)[!(names(all_metrics) %in% c("Model_ID", "Window"))])
names(common_metrics)[!(names(common_metrics) %in% c("Model_ID", "Window"))] <- paste0("Common_", names(common_metrics)[!(names(common_metrics) %in% c("Model_ID", "Window"))])

fit_summary <- do.call(rbind, lapply(split(fit_origin_results, interaction(fit_origin_results$Model_ID, fit_origin_results$Window, drop = TRUE)), function(z) {
  data.frame(
    Model_ID = z$Model_ID[1L], Window = z$Window[1L],
    Expected_Origins = nrow(z), Failed_Origins = sum(z$Fit_Failed),
    Warned_Origins = sum(z$Warning_Count > 0L),
    Mean_Adj_R2_Rolling = mean(z$Adj_R2_Mean, na.rm = TRUE),
    Stable_Root_Rate_Rolling = mean(z$Max_Root_Modulus < 1, na.rm = TRUE),
    Max_Root_Modulus_Rolling = max(z$Max_Root_Modulus, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
summarize_component_feasibility <- function(x, prefix) {
  keys <- unique(x[, c("Model_ID", "Window"), drop = FALSE])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    z <- x[x$Model_ID == keys$Model_ID[i] & x$Window == keys$Window[i], , drop = FALSE]
    neg_ii <- sum(z$Target == "II" & z$Forecast_Billions < 0, na.rm = TRUE)
    neg_ie <- sum(z$Target == "IE" & z$Forecast_Billions < 0, na.rm = TRUE)
    data.frame(Model_ID = keys$Model_ID[i], Window = keys$Window[i],
               Negative_II_Forecasts = neg_ii, Negative_IE_Forecasts = neg_ie,
               No_Negative_Components = neg_ii == 0L && neg_ie == 0L,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  names(out)[!(names(out) %in% c("Model_ID", "Window"))] <- paste0(prefix, names(out)[!(names(out) %in% c("Model_ID", "Window"))])
  out
}
feasibility_all <- summarize_component_feasibility(all_forecasts, "All_")
feasibility_common <- summarize_component_feasibility(common_forecasts, "Common_")
model_results <- merge(spec_table, merge(all_metrics, common_metrics, by = c("Model_ID", "Window"), all = TRUE), by = "Model_ID", all = TRUE)
model_results <- merge(model_results, fit_summary, by = c("Model_ID", "Window"), all.x = TRUE)
model_results <- merge(model_results, feasibility_all, by = c("Model_ID", "Window"), all.x = TRUE)
model_results <- merge(model_results, feasibility_common, by = c("Model_ID", "Window"), all.x = TRUE)
model_results <- merge(model_results, data.frame(Window = window_grid, Expected_Origins = nrow(analysis_data) - forecast_horizon - window_grid), by = "Window", suffixes = c("", "_Expected"), all.x = TRUE)
model_results$Complete_Common_Backtest <- model_results$Common_Origins == length(common_origins) & model_results$Failed_Origins == 0L
model_results$Rank_Eligible <- model_results$Complete_Common_Backtest & is.finite(model_results$Common_Joint_MASE)
model_results <- model_results[order(model_results$Common_Joint_MASE, model_results$Number_Endogenous, model_results$P), ]
readr::write_csv(model_results, file.path(output_dir, "all_var_model_window_results.csv"))

ranked <- model_results[model_results$Rank_Eligible, , drop = FALSE]
ranked <- ranked[order(ranked$Common_Joint_MASE, ranked$Number_Endogenous, ranked$P), , drop = FALSE]
ranked$Rank_Joint_MASE <- seq_len(nrow(ranked))
top50_joint <- head(ranked, 50L)
readr::write_csv(top50_joint, file.path(output_dir, "top50_var_joint_mase_common_origins.csv"))
ranked_differenced_feasible <- model_results[
  model_results$Rank_Eligible & model_results$Family == "Differenced_Target_VAR" & model_results$Common_No_Negative_Components,
  , drop = FALSE
]
ranked_differenced_feasible <- ranked_differenced_feasible[
  order(ranked_differenced_feasible$Common_Joint_MASE,
        ranked_differenced_feasible$Number_Endogenous, ranked_differenced_feasible$P), , drop = FALSE
]
ranked_differenced_feasible$Rank_Differenced_Feasible <- seq_len(nrow(ranked_differenced_feasible))
top50_differenced_feasible <- head(ranked_differenced_feasible, 50L)
readr::write_csv(top50_differenced_feasible, file.path(output_dir, "top50_var_differenced_feasible_joint_mase_common_origins.csv"))
readr::write_csv(model_results[, c("Model_ID", "Window", "Family", "Target_Transform", "Macro_System",
                                  "Common_Negative_II_Forecasts", "Common_Negative_IE_Forecasts",
                                  "Common_No_Negative_Components", "All_Negative_II_Forecasts",
                                  "All_Negative_IE_Forecasts", "All_No_Negative_Components")],
                 file.path(output_dir, "forecast_feasibility_by_model_window.csv"))
for (target_metric in c("II_MASE", "IE_MASE", "NII_MASE")) {
  metric_col <- paste0("Common_", target_metric)
  top <- ranked[order(ranked[[metric_col]], ranked$Number_Endogenous, ranked$P), , drop = FALSE]
  top$Rank <- seq_len(nrow(top))
  readr::write_csv(head(top, 50L), file.path(output_dir, paste0("top50_var_", tolower(sub("_MASE", "", target_metric)), "_mase_common_origins.csv")))
  feasible_col <- paste0("Common_", target_metric)
  feasible_top <- ranked_differenced_feasible[order(ranked_differenced_feasible[[feasible_col]],
                                                    ranked_differenced_feasible$Number_Endogenous,
                                                    ranked_differenced_feasible$P), , drop = FALSE]
  feasible_top$Rank <- seq_len(nrow(feasible_top))
  readr::write_csv(head(feasible_top, 50L), file.path(output_dir, paste0("top50_var_differenced_feasible_", tolower(sub("_MASE", "", target_metric)), "_mase_common_origins.csv")))
}

# Paired increment from adding each macro system versus the same target transform, lag and window without macros.
base_rows <- model_results[model_results$Macro_System == "Base", , drop = FALSE]
macro_rows <- model_results[model_results$Macro_System != "Base", , drop = FALSE]
macro_comparison <- list()
for (i in seq_len(nrow(macro_rows))) {
  m <- macro_rows[i, ]
  b <- base_rows[base_rows$Target_Transform == m$Target_Transform & base_rows$P == m$P & base_rows$Window == m$Window, , drop = FALSE]
  if (!nrow(b)) next
  macro_comparison[[length(macro_comparison) + 1L]] <- data.frame(
    Model_ID = m$Model_ID, Window = m$Window, Target_Transform = m$Target_Transform,
    Macro_System = m$Macro_System, P = m$P, Base_Joint_MASE = b$Common_Joint_MASE[1L],
    Macro_Joint_MASE = m$Common_Joint_MASE,
    Joint_MASE_Improvement_pct = 100 * (b$Common_Joint_MASE[1L] - m$Common_Joint_MASE) / b$Common_Joint_MASE[1L],
    Base_II_MASE = b$Common_II_MASE[1L], Macro_II_MASE = m$Common_II_MASE,
    Base_IE_MASE = b$Common_IE_MASE[1L], Macro_IE_MASE = m$Common_IE_MASE,
    Base_NII_MASE = b$Common_NII_MASE[1L], Macro_NII_MASE = m$Common_NII_MASE,
    stringsAsFactors = FALSE
  )
}
macro_comparison_results <- if (length(macro_comparison)) do.call(rbind, macro_comparison) else data.frame()
readr::write_csv(macro_comparison_results, file.path(output_dir, "macro_incremental_value_common_origins.csv"))

# Simple no-change and univariate AR benchmarks; forecast origins match the VAR runs.
benchmark_rows <- list()
for (w in window_grid) {
  origins <- seq.int(w + 1L, nrow(analysis_data) - forecast_horizon)
  for (origin in origins) {
    hist_idx <- seq.int(origin - w + 1L, origin)
    for (method in c("Random_Walk", "AR1", "AR2")) {
      component_fc <- list()
      for (target in c("II", "IE")) {
        y <- analysis_data[[target]][hist_idx]
        if (method == "Random_Walk") {
          component_fc[[target]] <- rep(tail(y, 1L), forecast_horizon)
        } else {
          p <- as.integer(sub("AR", "", method))
          response <- y[seq.int(p + 1L, length(y))]
          frame <- data.frame(y = response)
          for (lag in seq_len(p)) frame[[paste0("lag", lag)]] <- y[seq.int(p + 1L - lag, length(y) - lag)]
          fit <- stats::lm(y ~ ., data = frame)
          history <- y
          pred <- numeric(forecast_horizon)
          for (h in seq_len(forecast_horizon)) {
            new_row <- as.data.frame(as.list(vapply(seq_len(p), function(lag) history[length(history) - lag + 1L], numeric(1))))
            names(new_row) <- paste0("lag", seq_len(p))
            pred[h] <- as.numeric(stats::predict(fit, newdata = new_row))
            history <- c(history, pred[h])
          }
          component_fc[[target]] <- pred
        }
      }
      target_fc <- list(II = component_fc$II, IE = component_fc$IE, NII = component_fc$II - component_fc$IE)
      future_idx <- seq.int(origin + 1L, origin + forecast_horizon)
      actuals <- list(II = analysis_data$II[future_idx], IE = analysis_data$IE[future_idx], NII = analysis_data$NII[future_idx])
      for (target in names(target_fc)) {
        scale <- mean(abs(diff(analysis_data[[target]][seq_len(origin)])))
        for (h in seq_len(forecast_horizon)) {
          benchmark_rows[[length(benchmark_rows) + 1L]] <- data.frame(
            Model_ID = paste0("BENCH__", method), Window = w, Origin_Index = origin,
            Origin_Quarter = analysis_data$Quarter[origin], Horizon = h, Target = target,
            Actual_Billions = actuals[[target]][h], Forecast_Billions = target_fc[[target]][h],
            Error_Billions = target_fc[[target]][h] - actuals[[target]][h],
            MASE_Scale_Billions = scale, Forecast_Is_Negative = target_fc[[target]][h] < 0,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
}
benchmark_forecasts <- do.call(rbind, benchmark_rows)
benchmark_all <- summarize_forecasts(benchmark_forecasts)
benchmark_common <- summarize_forecasts(benchmark_forecasts[benchmark_forecasts$Origin_Index %in% common_origins, , drop = FALSE])
names(benchmark_all)[!(names(benchmark_all) %in% c("Model_ID", "Window"))] <- paste0("All_", names(benchmark_all)[!(names(benchmark_all) %in% c("Model_ID", "Window"))])
names(benchmark_common)[!(names(benchmark_common) %in% c("Model_ID", "Window"))] <- paste0("Common_", names(benchmark_common)[!(names(benchmark_common) %in% c("Model_ID", "Window"))])
benchmark_results <- merge(benchmark_all, benchmark_common, by = c("Model_ID", "Window"), all = TRUE)
readr::write_csv(benchmark_results, file.path(output_dir, "benchmark_results.csv"))
readr::write_csv(benchmark_forecasts, file.path(output_dir, "benchmark_origin_forecasts.csv"))

# Full-training explanatory and residual diagnostics for unique Top-50 VAR specifications.
# These descriptive statistics are not used to rank the rolling forecasts.
block_f_p_value <- function(eq_fit, macro_vars) {
  if (!length(macro_vars)) return(NA_real_)
  mf <- stats::model.frame(eq_fit)
  response <- names(mf)[1L]
  labels <- attr(stats::terms(eq_fit), "term.labels")
  macro_terms <- labels[vapply(labels, function(term) any(vapply(macro_vars, function(v) startsWith(term, paste0(v, ".l")), logical(1))), logical(1))]
  if (!length(macro_terms)) return(NA_real_)
  reduced_terms <- setdiff(labels, macro_terms)
  reduced_formula <- stats::reformulate(reduced_terms, response = response, intercept = FALSE)
  reduced <- stats::lm(reduced_formula, data = mf)
  full <- stats::lm(stats::formula(eq_fit), data = mf)
  tab <- stats::anova(reduced, full)
  if (nrow(tab) < 2L) NA_real_ else as.numeric(tab[["Pr(>F)"]][2L])
}
max_vif_and_condition <- function(eq_fit) {
  mm <- stats::model.matrix(eq_fit)
  variable <- apply(mm, 2L, function(x) stats::sd(x) > 1e-10)
  x <- mm[, variable, drop = FALSE]
  if (!ncol(x)) return(c(Max_VIF = NA_real_, Correlation_Condition_Number = NA_real_))
  vifs <- vapply(seq_len(ncol(x)), function(j) {
    y <- x[, j]
    others <- x[, -j, drop = FALSE]
    if (!ncol(others)) return(1)
    aux <- tryCatch(stats::lm.fit(cbind(1, others), y), error = function(e) NULL)
    if (is.null(aux)) return(Inf)
    r2 <- 1 - sum(aux$residuals^2) / sum((y - mean(y))^2)
    if (!is.finite(r2) || r2 >= 1 - 1e-10) Inf else 1 / (1 - r2)
  }, numeric(1))
  x_corr <- tryCatch(stats::cor(x), error = function(e) NA_real_)
  cond <- if (is.matrix(x_corr) && all(is.finite(x_corr))) tryCatch(kappa(x_corr), error = function(e) NA_real_) else NA_real_
  c(Max_VIF = max(vifs), Correlation_Condition_Number = cond)
}
extract_diag_p <- function(expr, field, inner) {
  obj <- tryCatch(expr, error = function(e) NULL)
  if (is.null(obj) || is.null(obj[[inner]]) || is.null(obj[[inner]]$p.value)) return(NA_real_)
  as.numeric(obj[[inner]]$p.value)
}
unique_top_ids <- unique(top50_differenced_feasible$Model_ID)
diagnostic_rows <- lapply(unique_top_ids, function(id) {
  s <- specs[[id]]
  full_y <- as.data.frame(analysis_data[, s$Endog_Vars, drop = FALSE])
  full_y <- full_y[stats::complete.cases(full_y), , drop = FALSE]
  fit <- tryCatch(vars::VAR(full_y, p = s$P, type = "const"), error = function(e) NULL)
  if (is.null(fit)) {
    return(data.frame(Model_ID = id, Full_Training_Fit_Failed = TRUE, stringsAsFactors = FALSE))
  }
  adj <- vapply(fit$varresult, function(eq) tryCatch(summary(eq)$adj.r.squared, error = function(e) NA_real_), numeric(1))
  serial_p <- extract_diag_p(vars::serial.test(fit, lags.pt = 8L, type = "PT.adjusted"), "serial", "serial")
  arch_p <- extract_diag_p(vars::arch.test(fit, lags.multi = 4L), "arch", "arch.mul")
  norm_obj <- tryCatch(vars::normality.test(fit, multivariate.only = TRUE), error = function(e) NULL)
  norm_p <- if (is.null(norm_obj) || is.null(norm_obj$jb.mul) || is.null(norm_obj$jb.mul$JB$p.value)) NA_real_ else as.numeric(norm_obj$jb.mul$JB$p.value)
  root_max <- tryCatch(max(Mod(vars::roots(fit))), error = function(e) NA_real_)
  vifs <- max_vif_and_condition(fit$varresult[[1L]])
  granger_p <- if (length(s$Macro_Vars)) {
    tryCatch(as.numeric(vars::causality(fit, cause = s$Macro_Vars)$Granger$p.value), error = function(e) NA_real_)
  } else NA_real_
  ii_block_p <- block_f_p_value(fit$varresult[[s$II_Column]], s$Macro_Vars)
  ie_block_p <- block_f_p_value(fit$varresult[[s$IE_Column]], s$Macro_Vars)
  data.frame(
    Model_ID = id, Full_Training_N = nrow(full_y), Full_Training_II_Adjusted_R2 = adj[s$II_Column],
    Full_Training_IE_Adjusted_R2 = adj[s$IE_Column], Full_Training_Mean_Adjusted_R2 = mean(adj, na.rm = TRUE),
    Full_Training_Max_Root_Modulus = root_max, Full_Training_Roots_Stable = if (is.finite(root_max)) root_max < 1 else NA,
    Residual_Portmanteau_p = serial_p, Residual_Multivariate_ARCH_p = arch_p,
    Residual_Normality_p = norm_p, Lagged_Macro_Block_II_Equation_p = ii_block_p,
    Lagged_Macro_Block_IE_Equation_p = ie_block_p, System_Granger_Macro_Block_p = granger_p,
    Lagged_Regressor_Max_VIF = unname(vifs["Max_VIF"]), Lagged_Regressor_Correlation_Condition_Number = unname(vifs["Correlation_Condition_Number"]),
    Full_Training_Fit_Failed = FALSE, stringsAsFactors = FALSE
  )
})
diagnostic_results <- do.call(rbind, diagnostic_rows)
readr::write_csv(diagnostic_results, file.path(output_dir, "top50_full_training_diagnostics.csv"))

# Save all forecast rows, model summaries, and run metadata for reproducibility.
readr::write_csv(all_forecasts, file.path(output_dir, "all_var_origin_forecasts.csv"))
readr::write_csv(fit_origin_results, file.path(output_dir, "all_var_origin_fit_status.csv"))
saveRDS(list(
  inputs = list(training_file = train_file, macro_file = macro_file, training_span = c(train$Quarter[1L], tail(train$Quarter, 1L))),
  candidate_specs = spec_table, all_model_results = model_results,
  all_forecasts = all_forecasts, fit_origin_results = fit_origin_results,
  benchmark_results = benchmark_results, benchmark_forecasts = benchmark_forecasts,
  diagnostics = diagnostic_results
), file.path(output_dir, "var_training_results.rds"), compress = "gzip")

metadata <- data.frame(
  Training_Start = train$Quarter[1L], Training_End = tail(train$Quarter, 1L),
  Training_Observations = nrow(train), Horizon_Quarters = forecast_horizon,
  Window_Grid = paste(window_grid, collapse = ", "), Lag_Grid = paste(seq_len(maximum_lag), collapse = ", "),
  Minimum_Residual_DF = minimum_residual_df, Candidate_Specifications = length(specs),
  Estimable_Model_Window_Pairs = length(tasks), Excluded_Model_Window_Pairs = nrow(excluded_results),
  Differenced_Target_Feasible_Model_Window_Pairs = nrow(ranked_differenced_feasible),
  Top50_Unfiltered_Negative_Component_Model_Window_Pairs = sum(!top50_joint$Common_No_Negative_Components),
  Common_Origins = length(common_origins), Common_Origin_Start = common_origin_labels[1L], Common_Origin_End = tail(common_origin_labels, 1L),
  Common_Backtest_Forecasts_Per_Target = length(common_origins) * forecast_horizon,
  Workers = workers, Test_Set_Loaded = FALSE, Fed_Stress_Scenario_Used = FALSE,
  stringsAsFactors = FALSE
)
readr::write_csv(metadata, file.path(output_dir, "run_metadata.csv"))

readme_lines <- c(
  "# Morgan Stanley training-only VAR comparison",
  "",
  paste0("Run date: 2026-11-16. Training outcomes only: ", train$Quarter[1L], "–", tail(train$Quarter, 1L), " (", nrow(train), " quarters)."),
  paste0("Forecast horizon: ", forecast_horizon, " quarters; no Y-9C holdout/test outcome file was loaded."),
  paste0("Common-origin ranking uses ", length(common_origins), " origins from ", common_origin_labels[1L], " to ", tail(common_origin_labels, 1L), "; each 9-quarter forecast ends by 2024Q2."),
  paste0("Rolling windows: ", paste(window_grid, collapse = ", "), ". Errors are equally weighted across origin-quarter forecast errors; no recency weights."),
  paste0("VAR lag orders compared: 1–", maximum_lag, ". Candidate model-window fits require at least ", minimum_residual_df, " residual degrees of freedom."),
  "",
  "## Variables and transformations",
  "",
  "- Targets: interest income (II) and interest expense (IE), both in billions of U.S. dollars; derived net interest income is II minus IE.",
  "- Primary candidate family: four combinations of first differences and log differences for II and IE. Forecasts are converted back to dollar levels before scoring. Some unit-root tests remain borderline/conflicting, especially for IE log differences, so these transformations are candidates rather than proof of stationarity.",
  "- Level VARs are included as a forecast comparison family because unit-root tests flag both target levels; they are not treated as the preferred differenced-target specification.",
  "- Macro systems compare 3-month and 10-year Treasury yields, BBB corporate yield, term spread, BBB spread, and VIX, individually and in selected two-variable combinations. In differenced-target candidates, yield/spread variables enter as first differences; VIX remains in levels per the user's prior instruction.",
  "- The raw 3-month plus 10-year yield system is included as a collinearity sensitivity; top models report VIF and condition-number diagnostics for lagged regressors. The 3M+10Y pair and the 3M+term-spread pair encode the same information, so identical forecasts are duplicate representations, not independent evidence.",
  "- No seasonal dummies are included; the training-only quarter-indicator checks are saved separately.",
  "",
  "## Selection and diagnostics",
  "",
  "- Primary ranking: mean joint MASE across II, IE, and NII, on the common origins, with each origin and forecast horizon equally weighted. MASE scales are computed using data available at each origin.",
  "- Secondary metrics include component and NII MAE/RMSE in billions, component MAPE, negative forecast frequency, adjusted R-squared, and model stability/residual diagnostics.",
  "- VARs are compared with random-walk, AR(1), and AR(2) component benchmarks.",
  "- Full-training adjusted R-squared, Granger/block tests, residual serial-correlation, ARCH, normality, roots, VIF and condition number are descriptive checks; they do not replace rolling forecast performance and do not establish structural causality.",
  "- Johansen trace diagnostics are for II and IE levels only. No VECM is selected automatically; the test results and limitations are saved for review.",
  "",
  "## Important interpretation limits",
  "",
  "- These results select candidate models within the training sample; the 2024Q3–2026Q3 holdout was not loaded or used.",
  "- The common-origin ranking has 21 overlapping rolling origins, so model ranks are uncertain and forecast errors are not independent observations.",
  "- This is an autonomous VAR forecast: macro variables are forecast recursively with the system. It is not yet conditioned on a Fed stress scenario.",
  "- Existing OLS backtest rankings are not directly combined with VAR rankings because their future macro/balance-sheet inputs were realized paths; the information sets differ.",
  "- This first VAR pass covers II and IE only. Noninterest income/expense, full PPNR, uncertainty intervals, and the stress scenario remain later extensions.",
  "",
  "## Files",
  "",
  "- `all_var_model_window_results.csv`: all estimable model-window comparisons.",
  "- `top50_var_joint_mase_common_origins.csv`: 50 leading candidates across all transformations, including level VAR comparators.",
  "- `top50_var_differenced_feasible_joint_mase_common_origins.csv`: primary shortlist after excluding level-target comparators and models with any negative II/IE forecast on a common origin.",
  "- `top50_var_differenced_feasible_ii_mase_common_origins.csv`, `top50_var_differenced_feasible_ie_mase_common_origins.csv`, `top50_var_differenced_feasible_nii_mase_common_origins.csv`: component-specific feasible shortlists.",
  "- `forecast_feasibility_by_model_window.csv`: negative component-forecast counts for every model-window candidate.",
  "- `top50_var_ii_mase_common_origins.csv`, `top50_var_ie_mase_common_origins.csv`, `top50_var_nii_mase_common_origins.csv`: component-specific rankings.",
  "- `all_var_origin_forecasts.csv`: every 9-quarter rolling forecast for every VAR candidate.",
  "- `macro_incremental_value_common_origins.csv`: paired forecast changes versus target-only VARs.",
  "- `benchmark_results.csv`: random-walk and univariate AR benchmarks.",
  "- `top50_full_training_diagnostics.csv`: explanatory/residual checks for unique models appearing in the differenced-target feasible shortlist.",
  "- Stationarity, seasonality, Johansen, lag-selection, exclusions, fit-status, and metadata files preserve decision evidence."
)
writeLines(readme_lines, file.path(output_dir, "README.md"))

cat("\nCompleted training-only VAR comparison.\n")
cat("Successful model-origin fits:", sum(!fit_origin_results$Fit_Failed), "/", nrow(fit_origin_results),
    "| failed origins:", sum(fit_origin_results$Fit_Failed),
    "| warned origins:", sum(fit_origin_results$Warning_Count > 0L), "\n")
cat("Top joint-MASE candidates:\n")
print(utils::head(top50_joint[, c("Rank_Joint_MASE", "Model_ID", "Window", "Common_Joint_MASE", "Common_II_MASE", "Common_IE_MASE", "Common_NII_MASE")], 10L), row.names = FALSE)
cat("Outputs:", normalizePath(output_dir), "\n")
