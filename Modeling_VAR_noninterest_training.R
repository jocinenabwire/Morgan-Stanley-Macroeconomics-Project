# Training-only stationary VAR search for Morgan Stanley non-interest income/expense.
# Uses the same stationary macro pool and two-macro cap as the interest VAR.
# Never loads the Y-9C holdout/test outcome file.

required_packages <- c("readr", "vars", "urca", "tseries")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing R package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

train_file <- file.path("data_splits", "train_y9c_outcomes_2009Q1_2024Q2.csv")
macro_file <- "2025-Table_1A_Historic_Domestic.csv"
output_dir <- file.path("results", "var_noninterest_training_20261116")
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

raw <- readr::read_csv(train_file, show_col_types = FALSE)
train <- data.frame(
  Quarter = quarter_from_date(raw$Date),
  II = as.numeric(raw[["Total interest income"]]) / 1e6,
  IE = as.numeric(raw[["Total interest expense"]]) / 1e6,
  NIR = as.numeric(raw[["Total noninterest income"]]) / 1e6,
  NIE = as.numeric(raw[["Total noninterest expense"]]) / 1e6,
  PPNR_reported = as.numeric(raw[["PPNR"]]) / 1e6,
  stringsAsFactors = FALSE
)
train$NII <- train$II - train$IE
train$Net_Noninterest <- train$NIR - train$NIE
train$PPNR <- train$NII + train$Net_Noninterest
if (nrow(train) != 62L || train$Quarter[1L] != "2009 Q1" ||
    tail(train$Quarter, 1L) != "2024 Q2") {
  stop("Training outcomes do not match the approved 62-quarter span.", call. = FALSE)
}
if (anyDuplicated(train$Quarter) || any(diff(quarter_index(train$Quarter)) != 1L)) {
  stop("Training outcomes contain duplicate or missing quarters.", call. = FALSE)
}
if (any(!is.finite(as.matrix(train[, c("II", "IE", "NIR", "NIE", "PPNR", "PPNR_reported")]))) ||
    any(train$NIR <= 0) || any(train$NIE <= 0)) {
  stop("Training outcomes contain invalid or non-positive non-interest values.", call. = FALSE)
}
if (max(abs(train$PPNR - train$PPNR_reported)) > 1e-8) {
  stop("Four-component PPNR identity does not reconcile in training data.", call. = FALSE)
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
analysis_data <- cbind(train, macro[, c("Rate_3M", "Rate_10Y", "BBB_Yield", "VIX"), drop = FALSE])
analysis_data$Term_Spread <- analysis_data$Rate_10Y - analysis_data$Rate_3M
analysis_data$BBB_Spread <- analysis_data$BBB_Yield - analysis_data$Rate_10Y
analysis_data$d_NIR <- c(NA_real_, diff(analysis_data$NIR))
analysis_data$g_NIR <- c(NA_real_, diff(log(analysis_data$NIR)))
analysis_data$d_NIE <- c(NA_real_, diff(analysis_data$NIE))
analysis_data$g_NIE <- c(NA_real_, diff(log(analysis_data$NIE)))
analysis_data$d_Rate_3M <- c(NA_real_, diff(analysis_data$Rate_3M))
analysis_data$d_Rate_10Y <- c(NA_real_, diff(analysis_data$Rate_10Y))
analysis_data$d_Term_Spread <- c(NA_real_, diff(analysis_data$Term_Spread))
analysis_data$d_BBB_Spread <- c(NA_real_, diff(analysis_data$BBB_Spread))

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
  NIR_first_difference = analysis_data$d_NIR,
  NIR_log_difference = analysis_data$g_NIR,
  NIE_first_difference = analysis_data$d_NIE,
  NIE_log_difference = analysis_data$g_NIE,
  d_Rate_3M = analysis_data$d_Rate_3M,
  d_Rate_10Y = analysis_data$d_Rate_10Y,
  d_Term_Spread = analysis_data$d_Term_Spread,
  d_BBB_Spread = analysis_data$d_BBB_Spread,
  d_BBB_Yield_sensitivity_only = c(NA_real_, diff(analysis_data$BBB_Yield))
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
if (!all(passes_stationarity[c(
  "NIR_first_difference", "NIR_log_difference",
  "NIE_first_difference", "NIE_log_difference"
)])) {
  stop("A new non-interest target transform failed the strict stationarity screen.", call. = FALSE)
}
macro_pool <- c("d_Rate_3M", "d_Rate_10Y", "d_Term_Spread", "d_BBB_Spread")
macro_keep <- macro_pool[passes_stationarity[macro_pool]]
if (!length(macro_keep)) stop("No macro variables passed the strict stationarity screen.", call. = FALSE)

macro_systems <- list(Base = character(0))
for (m in macro_keep) macro_systems[[m]] <- m
if (length(macro_keep) >= 2L) {
  pairs <- combn(macro_keep, 2L, simplify = FALSE)
  for (pair in pairs) macro_systems[[paste(pair, collapse = "_and_")]] <- pair
}
target_grid <- expand.grid(
  NIR_ID = c("NIR_D", "NIR_G"),
  NIE_ID = c("NIE_D", "NIE_G"),
  stringsAsFactors = FALSE
)
transform_info <- list(
  NIR_D = list(column = "d_NIR", reconstruction = "Difference"),
  NIR_G = list(column = "g_NIR", reconstruction = "LogDifference"),
  NIE_D = list(column = "d_NIE", reconstruction = "Difference"),
  NIE_G = list(column = "g_NIE", reconstruction = "LogDifference")
)
specs <- list()
tasks <- list()
for (i in seq_len(nrow(target_grid))) {
  nir_id <- target_grid$NIR_ID[i]
  nie_id <- target_grid$NIE_ID[i]
  for (macro_name in names(macro_systems)) {
    macro_vars <- macro_systems[[macro_name]]
    endog <- c(transform_info[[nir_id]]$column, transform_info[[nie_id]]$column, macro_vars)
    if (length(macro_vars) > maximum_macro_variables) next
    for (p in seq_len(maximum_lag)) {
      residual_df <- window_size - (length(endog) * p + 1L)
      if (residual_df < minimum_residual_df) next
      id <- paste(nir_id, nie_id, macro_name, paste0("VAR", p), sep = "__")
      specs[[id]] <- list(
        Model_ID = id, NIR_ID = nir_id, NIE_ID = nie_id,
        NIR_Column = transform_info[[nir_id]]$column,
        NIE_Column = transform_info[[nie_id]]$column,
        NIR_Reconstruction = transform_info[[nir_id]]$reconstruction,
        NIE_Reconstruction = transform_info[[nie_id]]$reconstruction,
        Macro_System = macro_name, Macro_Vars = macro_vars,
        Endog_Vars = endog, Number_Endogenous = length(endog),
        Number_Macro = length(macro_vars), P = as.integer(p),
        Residual_DF = as.integer(residual_df)
      )
      tasks[[length(tasks) + 1L]] <- list(spec = specs[[id]], window = window_size)
    }
  }
}
spec_table <- do.call(rbind, lapply(specs, function(s) {
  data.frame(
    Model_ID = s$Model_ID, NIR_Transform = s$NIR_ID, NIE_Transform = s$NIE_ID,
    Macro_System = s$Macro_System,
    Macro_Variables = if (length(s$Macro_Vars)) paste(s$Macro_Vars, collapse = " + ") else "None",
    Endogenous_Variables = paste(s$Endog_Vars, collapse = " + "),
    Number_Endogenous = s$Number_Endogenous, Number_Macro = s$Number_Macro,
    P = s$P, Residual_DF = s$Residual_DF, stringsAsFactors = FALSE
  )
}))
readr::write_csv(spec_table, file.path(output_dir, "candidate_specifications.csv"))

# Keep 20 common origins so the interest and non-interest results can be combined
# and later compared with 28- and 24-quarter windows on the same forecast dates.
common_origins <- seq.int(window_size + 2L, nrow(analysis_data) - forecast_horizon)
if (length(common_origins) != 20L) stop("Expected 20 common training origins.", call. = FALSE)
total_fits <- length(tasks) * length(common_origins)
completed_fits <- 0L
last_pct <- 0L
progress_every <- max(1L, ceiling(total_fits * progress_step_pct / 100))
progress_file <- file.path(output_dir, "progress.log")
writeLines(sprintf(
  "Expected VAR origin fits: %d; window=%d; horizon=%d; origins=%d",
  total_fits, window_size, forecast_horizon, length(common_origins)
), progress_file)
cat(sprintf("SEARCH_START specs=%d origins=%d expected_VAR_fits=%d window=%d horizon=%d\n",
            length(tasks), length(common_origins), total_fits, window_size, forecast_horizon))
flush.console()

reconstruct <- function(z, origin, target, transform) {
  level <- analysis_data[[target]][origin]
  if (transform == "Difference") return(level + cumsum(z))
  if (transform == "LogDifference") return(exp(log(level) + cumsum(z)))
  stop("Unknown target reconstruction.", call. = FALSE)
}
forecast_rows <- list()
fit_rows <- list()
for (task in tasks) {
  s <- task$spec
  w <- task$window
  for (origin in common_origins) {
    training_idx <- seq.int(origin - w + 1L, origin)
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
        warning = function(wrn) {
          warning_text <<- c(warning_text, conditionMessage(wrn))
          invokeRestart("muffleWarning")
        }
      )
      if (inherits(attempt, "error")) error_text <- conditionMessage(attempt) else {
        fit <- attempt$fit
        pred <- attempt$pred
      }
    }
    max_root <- if (is.null(fit)) NA_real_ else tryCatch(max(Mod(vars::roots(fit))), error = function(e) NA_real_)
    fit_rows[[length(fit_rows) + 1L]] <- data.frame(
      Model_ID = s$Model_ID, Window = w, Origin_Index = origin,
      Origin_Quarter = analysis_data$Quarter[origin],
      Fit_Failed = !is.null(error_text) && !is.na(error_text),
      Warning_Count = length(unique(warning_text)),
      Warning_Text = paste(unique(warning_text), collapse = " | "),
      Error_Text = if (is.na(error_text)) "" else error_text,
      Max_Root_Modulus = max_root, stringsAsFactors = FALSE
    )
    if (!is.null(fit) && !is.null(pred)) {
      transformed <- vapply(s$Endog_Vars, function(v) {
        as.numeric(pred$fcst[[v]][, 1L])
      }, numeric(forecast_horizon))
      colnames(transformed) <- s$Endog_Vars
      fc_nir <- reconstruct(transformed[, s$NIR_Column], origin, "NIR", s$NIR_Reconstruction)
      fc_nie <- reconstruct(transformed[, s$NIE_Column], origin, "NIE", s$NIE_Reconstruction)
      fc_net <- fc_nir - fc_nie
      future_idx <- seq.int(origin + 1L, origin + forecast_horizon)
      actuals <- list(
        NIR = analysis_data$NIR[future_idx],
        NIE = analysis_data$NIE[future_idx],
        Net_Noninterest = analysis_data$Net_Noninterest[future_idx]
      )
      forecasts <- list(NIR = fc_nir, NIE = fc_nie, Net_Noninterest = fc_net)
      for (target in names(forecasts)) {
        scale <- mean(abs(diff(analysis_data[[target]][seq_len(origin)])))
        for (h in seq_len(forecast_horizon)) {
          value <- forecasts[[target]][h]
          forecast_rows[[length(forecast_rows) + 1L]] <- data.frame(
            Model_ID = s$Model_ID, Window = w, Origin_Index = origin,
            Origin_Quarter = analysis_data$Quarter[origin], Horizon = h,
            Target = target, Actual_Billions = actuals[[target]][h],
            Forecast_Billions = value, Error_Billions = value - actuals[[target]][h],
            MASE_Scale_Billions = scale,
            Forecast_Is_Negative = target %in% c("NIR", "NIE") && is.finite(value) && value < 0,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    completed_fits <- completed_fits + 1L
    if (completed_fits %% progress_every == 0L || completed_fits == total_fits) {
      pct <- min(100L, floor(100 * completed_fits / total_fits))
      if (pct >= last_pct + progress_step_pct || completed_fits == total_fits) {
        line <- sprintf("PROGRESS %d%% (%d/%d VAR fits), elapsed %.1f sec",
                        pct, completed_fits, total_fits, proc.time()[["elapsed"]])
        cat(line, "\n")
        flush.console()
        cat(line, "\n", file = progress_file, append = TRUE)
        last_pct <- pct
      }
    }
  }
}

all_forecasts <- do.call(rbind, forecast_rows)
fit_results <- do.call(rbind, fit_rows)
readr::write_csv(all_forecasts, file.path(output_dir, "all_origin_forecasts.csv"))
readr::write_csv(fit_results, file.path(output_dir, "fit_status_by_model_origin.csv"))

summarize_one <- function(model_id, rows) {
  d <- rows[rows$Model_ID == model_id, , drop = FALSE]
  z1 <- d[d$Target == "NIR", , drop = FALSE]
  z2 <- d[d$Target == "NIE", , drop = FALSE]
  zn <- d[d$Target == "Net_Noninterest", , drop = FALSE]
  f <- fit_results[fit_results$Model_ID == model_id, , drop = FALSE]
  s <- spec_table[spec_table$Model_ID == model_id, , drop = FALSE]
  data.frame(
    Model_ID = model_id, NIR_Transform = s$NIR_Transform, NIE_Transform = s$NIE_Transform,
    Macro_System = s$Macro_System, Macro_Variables = s$Macro_Variables,
    Number_Macro = s$Number_Macro, P = s$P, Residual_DF = s$Residual_DF,
    Common_Origins = length(unique(d$Origin_Index)), Forecast_Rows = nrow(d),
    Failed_Origins = sum(f$Fit_Failed), Warned_Origins = sum(f$Warning_Count > 0L),
    Stable_Root_Rate = mean(f$Max_Root_Modulus < 1, na.rm = TRUE),
    Max_Root_Modulus = if (all(is.na(f$Max_Root_Modulus))) NA_real_ else max(f$Max_Root_Modulus, na.rm = TRUE),
    NIR_MASE = mean(abs(z1$Error_Billions) / z1$MASE_Scale_Billions),
    NIE_MASE = mean(abs(z2$Error_Billions) / z2$MASE_Scale_Billions),
    Net_Noninterest_MASE = mean(abs(zn$Error_Billions) / zn$MASE_Scale_Billions),
    Joint_MASE = mean(c(
      mean(abs(z1$Error_Billions) / z1$MASE_Scale_Billions),
      mean(abs(z2$Error_Billions) / z2$MASE_Scale_Billions),
      mean(abs(zn$Error_Billions) / zn$MASE_Scale_Billions)
    )),
    NIR_MAE_Billions = mean(abs(z1$Error_Billions)),
    NIE_MAE_Billions = mean(abs(z2$Error_Billions)),
    Net_Noninterest_MAE_Billions = mean(abs(zn$Error_Billions)),
    NIR_RMSE_Billions = sqrt(mean(z1$Error_Billions^2)),
    NIE_RMSE_Billions = sqrt(mean(z2$Error_Billions^2)),
    Net_Noninterest_RMSE_Billions = sqrt(mean(zn$Error_Billions^2)),
    Negative_NIR_Forecasts = sum(z1$Forecast_Is_Negative, na.rm = TRUE),
    Negative_NIE_Forecasts = sum(z2$Forecast_Is_Negative, na.rm = TRUE),
    Finite_Forecasts = all(is.finite(d$Forecast_Billions)),
    Complete_Common_Backtest = length(unique(d$Origin_Index)) == length(common_origins) &&
      nrow(d) == length(common_origins) * forecast_horizon * 3L,
    No_Negative_Components = sum(z1$Forecast_Is_Negative, na.rm = TRUE) == 0L &&
      sum(z2$Forecast_Is_Negative, na.rm = TRUE) == 0L,
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

benchmark_rows <- list()
for (origin in common_origins) {
  history_idx <- seq.int(origin - window_size + 1L, origin)
  for (method in c("Random_Walk", "AR1", "AR2")) {
    component_fc <- list()
    for (target in c("NIR", "NIE")) {
      y <- analysis_data[[target]][history_idx]
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
    fc <- list(
      NIR = component_fc$NIR, NIE = component_fc$NIE,
      Net_Noninterest = component_fc$NIR - component_fc$NIE
    )
    future_idx <- seq.int(origin + 1L, origin + forecast_horizon)
    actual <- list(
      NIR = analysis_data$NIR[future_idx], NIE = analysis_data$NIE[future_idx],
      Net_Noninterest = analysis_data$Net_Noninterest[future_idx]
    )
    for (target in names(fc)) {
      scale <- mean(abs(diff(analysis_data[[target]][seq_len(origin)])))
      for (h in seq_len(forecast_horizon)) {
        benchmark_rows[[length(benchmark_rows) + 1L]] <- data.frame(
          Model_ID = paste0("BENCH__", method), Origin_Index = origin,
          Origin_Quarter = analysis_data$Quarter[origin], Horizon = h, Target = target,
          Actual_Billions = actual[[target]][h], Forecast_Billions = fc[[target]][h],
          Error_Billions = fc[[target]][h] - actual[[target]][h],
          MASE_Scale_Billions = scale, stringsAsFactors = FALSE
        )
      }
    }
  }
}
benchmark_forecasts <- do.call(rbind, benchmark_rows)
benchmark_results <- do.call(rbind, lapply(unique(benchmark_forecasts$Model_ID), function(id) {
  d <- benchmark_forecasts[benchmark_forecasts$Model_ID == id, , drop = FALSE]
  m <- vapply(c("NIR", "NIE", "Net_Noninterest"), function(target) {
    z <- d[d$Target == target, , drop = FALSE]
    mean(abs(z$Error_Billions) / z$MASE_Scale_Billions)
  }, numeric(1))
  data.frame(
    Model_ID = id, NIR_MASE = m[["NIR"]], NIE_MASE = m[["NIE"]],
    Net_Noninterest_MASE = m[["Net_Noninterest"]], Joint_MASE = mean(m),
    stringsAsFactors = FALSE
  )
}))
readr::write_csv(benchmark_forecasts, file.path(output_dir, "benchmark_origin_forecasts.csv"))
readr::write_csv(benchmark_results, file.path(output_dir, "benchmark_results.csv"))

metadata <- data.frame(
  Run_Date = as.character(Sys.Date()), Training_Start = train$Quarter[1L],
  Training_End = tail(train$Quarter, 1L), Training_Rows = nrow(train),
  Holdout_Loaded = FALSE, Window = window_size, Horizon = forecast_horizon,
  Common_Origins = length(common_origins),
  First_Common_Origin = analysis_data$Quarter[common_origins[1L]],
  Last_Common_Origin = analysis_data$Quarter[tail(common_origins, 1L)],
  Maximum_Macro_Variables = maximum_macro_variables,
  Candidate_Specifications = length(specs), Expected_VAR_Fits = total_fits,
  Successful_VAR_Fits = sum(!fit_results$Fit_Failed),
  Failed_VAR_Fits = sum(fit_results$Fit_Failed),
  Warned_VAR_Fits = sum(fit_results$Warning_Count > 0L),
  Elapsed_Seconds = proc.time()[["elapsed"]], stringsAsFactors = FALSE
)
readr::write_csv(metadata, file.path(output_dir, "run_metadata.csv"))
readme <- c(
  "# Training-only stationary VAR search: non-interest components",
  "",
  "The holdout/test outcome file was not loaded.",
  paste0("Training span: ", train$Quarter[1L], "–", tail(train$Quarter, 1L), " (", nrow(train), " quarters)."),
  paste0("Window: ", window_size, "; horizon: ", forecast_horizon, "; common origins: ",
         length(common_origins), " (", analysis_data$Quarter[common_origins[1L]], "–",
         analysis_data$Quarter[tail(common_origins, 1L)], ")."),
  paste0("Candidate specifications: ", length(specs), "; expected VAR fits: ", total_fits, "."),
  paste0("Stationary macro pool: ", paste(macro_keep, collapse = ", "),
         "; maximum macro variables per VAR: ", maximum_macro_variables, "."),
  "The grid includes zero-, one- and two-macro systems; the model is not forced to keep two macros.",
  "NIR and NIE first differences and log differences passed the training-only ADF/KPSS 5% screen.",
  "The rolling scores rank candidate specifications inside the training set; the final holdout remains untouched.",
  "This search covers non-interest income, non-interest expense and their difference. It does not apply a Fed stress path.",
  "",
  "Outputs include stationarity_diagnostics_training_only.csv, candidate_specifications.csv,",
  "all_model_results.csv, top50_feasible_joint_mase.csv, all_origin_forecasts.csv,",
  "fit_status_by_model_origin.csv, benchmark_results.csv, run_metadata.csv and var_noninterest_results.rds."
)
writeLines(readme, file.path(output_dir, "README.md"))
saveRDS(
  list(
    stationarity = stationarity_results, specifications = spec_table,
    model_results = model_results, forecasts = all_forecasts,
    fit_status = fit_results, benchmarks = benchmark_results,
    benchmark_forecasts = benchmark_forecasts
  ),
  file.path(output_dir, "var_noninterest_results.rds"), compress = "gzip"
)
cat("\nTOP_10_FEASIBLE\n")
print(utils::head(top50, 10L), row.names = FALSE)
cat("\nBENCHMARKS\n")
print(benchmark_results, row.names = FALSE)
cat("\nRUN_METADATA\n")
print(metadata, row.names = FALSE)
cat("\nSEARCH_COMPLETE\n")
