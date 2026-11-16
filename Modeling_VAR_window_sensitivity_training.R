# Re-estimate the W32 top-50 stationary VAR specifications at W28 and W24.
# Combines selected interest and non-interest pair forecasts to score training PPNR.
# The holdout/test outcome file is never loaded.

required_packages <- c("readr", "vars")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing R package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

interest_dir <- file.path("results", "var_stationary_training_20261116")
noninterest_dir <- file.path("results", "var_noninterest_training_20261116")
output_dir <- file.path("results", "var_window_sensitivity_training_20261116")
train_file <- file.path("data_splits", "train_y9c_outcomes_2009Q1_2024Q2.csv")
macro_file <- "2025-Table_1A_Historic_Domestic.csv"
window_grid <- c(24L, 28L, 32L)
refit_windows <- c(24L, 28L)
window32 <- 32L
forecast_horizon <- 9L
maximum_macro_variables <- 2L
minimum_residual_df <- 8L
progress_step_pct <- 5L

needed_files <- c(
  file.path(interest_dir, "top50_feasible_joint_mase.csv"),
  file.path(interest_dir, "candidate_specifications.csv"),
  file.path(interest_dir, "all_origin_forecasts.csv"),
  file.path(interest_dir, "all_model_results.csv"),
  file.path(noninterest_dir, "top50_feasible_joint_mase.csv"),
  file.path(noninterest_dir, "candidate_specifications.csv"),
  file.path(noninterest_dir, "all_origin_forecasts.csv"),
  file.path(noninterest_dir, "all_model_results.csv"),
  train_file, macro_file
)
missing_files <- needed_files[!file.exists(needed_files)]
if (length(missing_files)) stop("Missing prerequisite result/input file(s): ", paste(missing_files, collapse = ", "), call. = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

quarter_from_date <- function(x) {
  d <- as.Date(x)
  paste0(format(d, "%Y"), " Q", ((as.integer(format(d, "%m")) - 1L) %/% 3L) + 1L)
}
raw <- readr::read_csv(train_file, show_col_types = FALSE)
train <- data.frame(
  Quarter = quarter_from_date(raw$Date),
  II = as.numeric(raw[["Total interest income"]]) / 1e6,
  IE = as.numeric(raw[["Total interest expense"]]) / 1e6,
  NIR = as.numeric(raw[["Total noninterest income"]]) / 1e6,
  NIE = as.numeric(raw[["Total noninterest expense"]]) / 1e6,
  stringsAsFactors = FALSE
)
train$NII <- train$II - train$IE
train$Net_Noninterest <- train$NIR - train$NIE
train$PPNR <- train$NII + train$Net_Noninterest
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
analysis_data <- cbind(train, macro[, c("Rate_3M", "Rate_10Y", "BBB_Yield", "VIX"), drop = FALSE])
analysis_data$Term_Spread <- analysis_data$Rate_10Y - analysis_data$Rate_3M
analysis_data$BBB_Spread <- analysis_data$BBB_Yield - analysis_data$Rate_10Y
analysis_data$d_II <- c(NA_real_, diff(analysis_data$II))
analysis_data$g_II <- c(NA_real_, diff(log(analysis_data$II)))
analysis_data$dd_IE <- c(NA_real_, NA_real_, diff(analysis_data$IE, differences = 2L))
analysis_data$ddlog_IE <- c(NA_real_, NA_real_, diff(log(analysis_data$IE), differences = 2L))
analysis_data$d_NIR <- c(NA_real_, diff(analysis_data$NIR))
analysis_data$g_NIR <- c(NA_real_, diff(log(analysis_data$NIR)))
analysis_data$d_NIE <- c(NA_real_, diff(analysis_data$NIE))
analysis_data$g_NIE <- c(NA_real_, diff(log(analysis_data$NIE)))
analysis_data$d_Rate_3M <- c(NA_real_, diff(analysis_data$Rate_3M))
analysis_data$d_Rate_10Y <- c(NA_real_, diff(analysis_data$Rate_10Y))
analysis_data$d_Term_Spread <- c(NA_real_, diff(analysis_data$Term_Spread))
analysis_data$d_BBB_Spread <- c(NA_real_, diff(analysis_data$BBB_Spread))

interest_top50 <- readr::read_csv(file.path(interest_dir, "top50_feasible_joint_mase.csv"), show_col_types = FALSE)
interest_specs <- readr::read_csv(file.path(interest_dir, "candidate_specifications.csv"), show_col_types = FALSE)
interest_fc32 <- readr::read_csv(file.path(interest_dir, "all_origin_forecasts.csv"), show_col_types = FALSE)
interest_results32 <- readr::read_csv(file.path(interest_dir, "all_model_results.csv"), show_col_types = FALSE)
noninterest_top50 <- readr::read_csv(file.path(noninterest_dir, "top50_feasible_joint_mase.csv"), show_col_types = FALSE)
noninterest_specs <- readr::read_csv(file.path(noninterest_dir, "candidate_specifications.csv"), show_col_types = FALSE)
noninterest_fc32 <- readr::read_csv(file.path(noninterest_dir, "all_origin_forecasts.csv"), show_col_types = FALSE)
noninterest_results32 <- readr::read_csv(file.path(noninterest_dir, "all_model_results.csv"), show_col_types = FALSE)

parse_macro_vars <- function(x) {
  x <- as.character(x)
  if (!length(x) || is.na(x) || x == "None" || !nzchar(x)) character(0) else trimws(strsplit(x, "\\+", perl = TRUE)[[1L]])
}
interest_map <- list(
  II_D = list(column = "d_II", raw = "II", transform = "Difference"),
  II_G = list(column = "g_II", raw = "II", transform = "LogDifference"),
  IE_D2 = list(column = "dd_IE", raw = "IE", transform = "SecondDifference"),
  IE_G2 = list(column = "ddlog_IE", raw = "IE", transform = "SecondLogDifference")
)
noninterest_map <- list(
  NIR_D = list(column = "d_NIR", raw = "NIR", transform = "Difference"),
  NIR_G = list(column = "g_NIR", raw = "NIR", transform = "LogDifference"),
  NIE_D = list(column = "d_NIE", raw = "NIE", transform = "Difference"),
  NIE_G = list(column = "g_NIE", raw = "NIE", transform = "LogDifference")
)

normalize_specs <- function(pair, top, spec_table) {
  lapply(top$Model_ID, function(id) {
    s <- spec_table[spec_table$Model_ID == id, , drop = FALSE]
    if (nrow(s) != 1L) stop("Could not find unique specification for ", id)
    if (pair == "Interest") {
      a_id <- s$II_Transform[1L]
      b_id <- s$IE_Transform[1L]
      a <- interest_map[[a_id]]
      b <- interest_map[[b_id]]
      a_name <- "II"; b_name <- "IE"; net_name <- "NII"
    } else {
      a_id <- s$NIR_Transform[1L]
      b_id <- s$NIE_Transform[1L]
      a <- noninterest_map[[a_id]]
      b <- noninterest_map[[b_id]]
      a_name <- "NIR"; b_name <- "NIE"; net_name <- "Net_Noninterest"
    }
    list(
      Pair = pair, Model_ID = id, A_ID = a_id, B_ID = b_id,
      A_Column = a$column, B_Column = b$column,
      A_Raw = a$raw, B_Raw = b$raw,
      A_Transform = a$transform, B_Transform = b$transform,
      A_Name = a_name, B_Name = b_name, Net_Name = net_name,
      Macro_System = as.character(s$Macro_System[1L]),
      Macro_Vars = parse_macro_vars(s$Macro_Variables[1L]),
      P = as.integer(s$P[1L]), Number_Macro = as.integer(s$Number_Macro[1L]),
      stringsAsFactors = FALSE
    )
  })
}
interest_top_specs <- normalize_specs("Interest", interest_top50, interest_specs)
noninterest_top_specs <- normalize_specs("Noninterest", noninterest_top50, noninterest_specs)
top_spec_map <- c(interest_top_specs, noninterest_top_specs)

common_origins <- seq.int(window32 + 2L, nrow(analysis_data) - forecast_horizon)
if (length(common_origins) != 20L) stop("Expected 20 common rolling origins.", call. = FALSE)

tasks <- list()
excluded <- list()
for (s in top_spec_map) {
  for (w in refit_windows) {
    residual_df <- w - ((2L + s$Number_Macro) * s$P + 1L)
    if (residual_df < minimum_residual_df) {
      excluded[[length(excluded) + 1L]] <- data.frame(
        Pair = s$Pair, Model_ID = s$Model_ID, Window = w,
        P = s$P, Number_Macro = s$Number_Macro,
        Residual_DF = residual_df, Reason = "Below minimum residual degrees of freedom",
        stringsAsFactors = FALSE
      )
    } else {
      tasks[[length(tasks) + 1L]] <- list(spec = s, window = as.integer(w), residual_df = residual_df)
    }
  }
}
if (!length(tasks)) stop("No top-50 specifications satisfy the window sensitivity degrees-of-freedom rule.")
excluded_results <- if (length(excluded)) do.call(rbind, excluded) else data.frame()
readr::write_csv(excluded_results, file.path(output_dir, "excluded_short_window_models.csv"))

reconstruct <- function(z, origin, raw_target, transform) {
  level <- analysis_data[[raw_target]][origin]
  previous <- analysis_data[[raw_target]][origin - 1L]
  if (transform == "Difference") return(level + cumsum(z))
  if (transform == "LogDifference") return(exp(log(level) + cumsum(z)))
  if (transform == "SecondDifference") {
    previous_change <- level - previous
    return(level + cumsum(previous_change + cumsum(z)))
  }
  if (transform == "SecondLogDifference") {
    previous_growth <- log(level) - log(previous)
    return(exp(log(level) + cumsum(previous_growth + cumsum(z))))
  }
  stop("Unknown transformation: ", transform, call. = FALSE)
}

reuse_saved_fits <- identical(Sys.getenv("REUSE_SAVED_SENSITIVITY_FITS"), "1") &&
  file.exists(file.path(output_dir, "top50_reestimated_origin_forecasts.csv")) &&
  file.exists(file.path(output_dir, "top50_reestimated_fit_status.csv"))
if (reuse_saved_fits) {
  sensitivity_forecasts <- readr::read_csv(
    file.path(output_dir, "top50_reestimated_origin_forecasts.csv"),
    show_col_types = FALSE
  )
  sensitivity_fits <- readr::read_csv(
    file.path(output_dir, "top50_reestimated_fit_status.csv"),
    show_col_types = FALSE
  )
  total_fits <- nrow(sensitivity_fits)
  cat(sprintf("REUSED_SAVED_FITS=%d; no VAR refits requested.\n", total_fits))
} else {
  total_fits <- length(tasks) * length(common_origins)
  completed_fits <- 0L
  last_pct <- 0L
  progress_every <- max(1L, ceiling(total_fits * progress_step_pct / 100))
  progress_file <- file.path(output_dir, "progress.log")
  writeLines(sprintf(
    "Expected top-50 re-estimation fits: %d; windows=%s; origins=%d",
    total_fits, paste(refit_windows, collapse = ","), length(common_origins)
  ), progress_file)
  cat(sprintf("SENSITIVITY_START specs=%d model_window_tasks=%d origins=%d expected_VAR_fits=%d\n",
              length(top_spec_map), length(tasks), length(common_origins), total_fits))
  flush.console()

  forecast_rows <- list()
  fit_rows <- list()
  for (task in tasks) {
  s <- task$spec
  w <- task$window
  for (origin in common_origins) {
    training_idx <- seq.int(origin - w + 1L, origin)
    endog <- c(s$A_Column, s$B_Column, s$Macro_Vars)
    y <- as.data.frame(analysis_data[training_idx, endog, drop = FALSE])
    fit <- NULL; pred <- NULL; warning_text <- character(0); error_text <- NA_character_
    if (any(!is.finite(as.matrix(y)))) {
      error_text <- "Non-finite input inside rolling window."
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
      Pair = s$Pair, Model_ID = s$Model_ID, Window = w,
      Origin_Index = origin, Origin_Quarter = analysis_data$Quarter[origin],
      Fit_Failed = !is.null(error_text) && !is.na(error_text),
      Warning_Count = length(unique(warning_text)),
      Warning_Text = paste(unique(warning_text), collapse = " | "),
      Error_Text = if (is.na(error_text)) "" else error_text,
      Max_Root_Modulus = max_root, stringsAsFactors = FALSE
    )
    if (!is.null(fit) && !is.null(pred)) {
      transformed <- vapply(endog, function(v) {
        as.numeric(pred$fcst[[v]][, 1L])
      }, numeric(forecast_horizon))
      colnames(transformed) <- endog
      fc_a <- reconstruct(transformed[, s$A_Column], origin, s$A_Raw, s$A_Transform)
      fc_b <- reconstruct(transformed[, s$B_Column], origin, s$B_Raw, s$B_Transform)
      fc_net <- fc_a - fc_b
      future_idx <- seq.int(origin + 1L, origin + forecast_horizon)
      actuals <- list(
        A = analysis_data[[s$A_Name]][future_idx],
        B = analysis_data[[s$B_Name]][future_idx],
        Net = analysis_data[[s$Net_Name]][future_idx]
      )
      forecasts <- list(A = fc_a, B = fc_b, Net = fc_net)
      target_names <- c(A = s$A_Name, B = s$B_Name, Net = s$Net_Name)
      for (target_key in names(forecasts)) {
        target <- target_names[[target_key]]
        scale <- mean(abs(diff(analysis_data[[target]][seq_len(origin)])))
        for (h in seq_len(forecast_horizon)) {
          value <- forecasts[[target_key]][h]
          forecast_rows[[length(forecast_rows) + 1L]] <- data.frame(
            Pair = s$Pair, Model_ID = s$Model_ID, Window = w,
            Origin_Index = origin, Origin_Quarter = analysis_data$Quarter[origin],
            Horizon = h, Target = target,
            Actual_Billions = actuals[[target_key]][h],
            Forecast_Billions = value, Error_Billions = value - actuals[[target_key]][h],
            MASE_Scale_Billions = scale,
            Forecast_Is_Negative = target_key %in% c("A", "B") && is.finite(value) && value < 0,
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

  sensitivity_forecasts <- if (length(forecast_rows)) do.call(rbind, forecast_rows) else data.frame()
  sensitivity_fits <- do.call(rbind, fit_rows)
  readr::write_csv(sensitivity_forecasts, file.path(output_dir, "top50_reestimated_origin_forecasts.csv"))
  readr::write_csv(sensitivity_fits, file.path(output_dir, "top50_reestimated_fit_status.csv"))
}

summarize_sensitivity_one <- function(pair, model_id, window, rows) {
  d <- rows[rows$Pair == pair & rows$Model_ID == model_id & rows$Window == window, , drop = FALSE]
  s <- top_spec_map[[which(vapply(top_spec_map, function(z) z$Pair == pair && z$Model_ID == model_id, logical(1)))[1L]]]
  target_keys <- c(s$A_Name, s$B_Name, s$Net_Name)
  tstat <- lapply(target_keys, function(target) {
    z <- d[d$Target == target, , drop = FALSE]
    data.frame(
      Target = target,
      MASE = mean(abs(z$Error_Billions) / z$MASE_Scale_Billions),
      MAE_Billions = mean(abs(z$Error_Billions)),
      RMSE_Billions = sqrt(mean(z$Error_Billions^2)),
      stringsAsFactors = FALSE
    )
  })
  tstat <- do.call(rbind, tstat)
  f <- sensitivity_fits[
    sensitivity_fits$Pair == pair & sensitivity_fits$Model_ID == model_id &
      sensitivity_fits$Window == window, , drop = FALSE
  ]
  result <- data.frame(
    Pair = pair, Model_ID = model_id, Window = window,
    A_Transform = s$A_ID, B_Transform = s$B_ID,
    Macro_System = s$Macro_System, Number_Macro = s$Number_Macro, P = s$P,
    Residual_DF = window - ((2L + s$Number_Macro) * s$P + 1L),
    Common_Origins = length(unique(d$Origin_Index)), Forecast_Rows = nrow(d),
    Failed_Origins = sum(f$Fit_Failed), Warned_Origins = sum(f$Warning_Count > 0L),
    Stable_Root_Rate = mean(f$Max_Root_Modulus < 1, na.rm = TRUE),
    Max_Root_Modulus = if (all(is.na(f$Max_Root_Modulus))) NA_real_ else max(f$Max_Root_Modulus, na.rm = TRUE),
    Joint_MASE = mean(tstat$MASE), Failed_Origins_Expected = 0L,
    Finite_Forecasts = all(is.finite(d$Forecast_Billions)),
    Complete_Common_Backtest = length(unique(d$Origin_Index)) == length(common_origins) &&
      nrow(d) == length(common_origins) * forecast_horizon * 3L,
    No_Negative_Components = sum(d$Forecast_Is_Negative, na.rm = TRUE) == 0L,
    stringsAsFactors = FALSE
  )
  result$Component1_Name <- s$A_Name
  result$Component2_Name <- s$B_Name
  result$Net_Component_Name <- s$Net_Name
  result$Component1_MASE <- tstat$MASE[1L]
  result$Component2_MASE <- tstat$MASE[2L]
  result$Net_Component_MASE <- tstat$MASE[3L]
  result$Component1_MAE_Billions <- tstat$MAE_Billions[1L]
  result$Component2_MAE_Billions <- tstat$MAE_Billions[2L]
  result$Net_Component_MAE_Billions <- tstat$MAE_Billions[3L]
  result$Component1_RMSE_Billions <- tstat$RMSE_Billions[1L]
  result$Component2_RMSE_Billions <- tstat$RMSE_Billions[2L]
  result$Net_Component_RMSE_Billions <- tstat$RMSE_Billions[3L]
  result
}
sensitivity_results <- do.call(rbind, lapply(tasks, function(task) {
  summarize_sensitivity_one(
    task$spec$Pair, task$spec$Model_ID, task$window, sensitivity_forecasts
  )
}))
sensitivity_results$Rank_Eligible <- with(
  sensitivity_results,
  Complete_Common_Backtest & Finite_Forecasts & Failed_Origins == 0L &
    No_Negative_Components & Residual_DF >= minimum_residual_df
)
readr::write_csv(sensitivity_results, file.path(output_dir, "top50_reestimated_model_results.csv"))

# Normalize the already-computed W32 results to the re-estimated W28/W24 schema.
interest_w32 <- data.frame(
  Pair = "Interest", Model_ID = interest_top50$Model_ID, Window = 32L,
  Joint_MASE = interest_top50$Joint_MASE,
  Rank_Eligible = interest_top50$Rank_Eligible,
  Stable_Root_Rate = interest_top50$Stable_Root_Rate,
  Number_Macro = interest_top50$Number_Macro, P = interest_top50$P,
  stringsAsFactors = FALSE
)
noninterest_w32 <- data.frame(
  Pair = "Noninterest", Model_ID = noninterest_top50$Model_ID, Window = 32L,
  Joint_MASE = noninterest_top50$Joint_MASE,
  Rank_Eligible = noninterest_top50$Rank_Eligible,
  Stable_Root_Rate = noninterest_top50$Stable_Root_Rate,
  Number_Macro = noninterest_top50$Number_Macro, P = noninterest_top50$P,
  stringsAsFactors = FALSE
)
window_results <- rbind(
  interest_w32, noninterest_w32,
  sensitivity_results[, c("Pair", "Model_ID", "Window", "Joint_MASE",
                          "Rank_Eligible", "Stable_Root_Rate", "Number_Macro", "P")]
)
window_results <- window_results[window_results$Window %in% window_grid, , drop = FALSE]
window_results$Window_Rank <- NA_integer_
for (pair in unique(window_results$Pair)) {
  for (w in window_grid) {
    idx <- which(window_results$Pair == pair & window_results$Window == w & window_results$Rank_Eligible)
    window_results$Window_Rank[idx] <- rank(window_results$Joint_MASE[idx], ties.method = "min")
  }
}
readr::write_csv(window_results, file.path(output_dir, "top50_window_comparison_results.csv"))

window_summary_rows <- list()
matched_rank_rows <- list()
for (pair in unique(window_results$Pair)) {
  for (w in window_grid) {
    d <- window_results[window_results$Pair == pair & window_results$Window == w &
                          window_results$Rank_Eligible, , drop = FALSE]
    sorted <- d[order(d$Joint_MASE), , drop = FALSE]
    window_summary_rows[[length(window_summary_rows) + 1L]] <- data.frame(
      Pair = pair, Window = w, Eligible_Top50_Models = nrow(d),
      Best_Joint_MASE = if (nrow(d)) sorted$Joint_MASE[1L] else NA_real_,
      Mean_Top10_Joint_MASE = if (nrow(d)) mean(head(sorted$Joint_MASE, 10L)) else NA_real_,
      Median_Top10_Joint_MASE = if (nrow(d)) median(head(sorted$Joint_MASE, 10L)) else NA_real_,
      Best_Model_ID = if (nrow(d)) sorted$Model_ID[1L] else NA_character_,
      Best_Model_Stable_Rate = if (nrow(d)) sorted$Stable_Root_Rate[1L] else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  eligible_sets <- lapply(window_grid, function(w) {
    window_results$Model_ID[
      window_results$Pair == pair & window_results$Window == w & window_results$Rank_Eligible
    ]
  })
  common_ids <- if (length(eligible_sets) && all(lengths(eligible_sets) > 0L)) {
    Reduce(intersect, eligible_sets)
  } else character(0)
  if (length(common_ids)) {
    for (id in common_ids) {
      scores <- vapply(window_grid, function(w) {
        window_results$Joint_MASE[
          window_results$Pair == pair & window_results$Model_ID == id &
            window_results$Window == w & window_results$Rank_Eligible
        ][1L]
      }, numeric(1))
      ranks <- rank(scores, ties.method = "min")
      for (j in seq_along(window_grid)) {
        matched_rank_rows[[length(matched_rank_rows) + 1L]] <- data.frame(
          Pair = pair, Model_ID = id, Window = window_grid[j],
          Joint_MASE = scores[j], Rank_Within_Specification = ranks[j],
          stringsAsFactors = FALSE
        )
      }
    }
  }
}
window_summary <- do.call(rbind, window_summary_rows)
readr::write_csv(window_summary, file.path(output_dir, "window_summary_top50.csv"))
matched_ranks <- if (length(matched_rank_rows)) do.call(rbind, matched_rank_rows) else data.frame()
readr::write_csv(matched_ranks, file.path(output_dir, "matched_specification_window_ranks.csv"))
matched_window_summary <- if (nrow(matched_ranks)) {
  aggregate(
    cbind(Joint_MASE, Rank_Within_Specification) ~ Pair + Window,
    data = matched_ranks, FUN = mean
  )
} else data.frame()
readr::write_csv(matched_window_summary, file.path(output_dir, "matched_specification_window_summary.csv"))

# Select the best stable feasible shortlisted model per component pair and window.
selected_rows <- list()
for (pair in unique(window_results$Pair)) {
  for (w in window_grid) {
    d <- window_results[window_results$Pair == pair & window_results$Window == w &
                          window_results$Rank_Eligible, , drop = FALSE]
    if (!nrow(d)) next
    stable <- d[d$Stable_Root_Rate >= 1 - 1e-12, , drop = FALSE]
    selection_rule <- "Lowest joint MASE among feasible models stable at all common origins"
    if (!nrow(stable)) {
      stable <- d
      selection_rule <- "No fully stable feasible model; lowest joint MASE among feasible models"
    }
    chosen <- stable[which.min(stable$Joint_MASE), , drop = FALSE]
    selected_rows[[length(selected_rows) + 1L]] <- data.frame(
      Pair = pair, Window = w, Model_ID = chosen$Model_ID,
      Pair_Joint_MASE = chosen$Joint_MASE,
      Stable_Root_Rate = chosen$Stable_Root_Rate,
      Selection_Rule = selection_rule, stringsAsFactors = FALSE
    )
  }
}
selected_models <- do.call(rbind, selected_rows)
readr::write_csv(selected_models, file.path(output_dir, "selected_pair_models_by_window.csv"))

read_forecast_source <- function(pair, model_id, w) {
  if (w == 32L && pair == "Interest") {
    f <- interest_fc32
    if (!"Window" %in% names(f)) f$Window <- 32L
  } else if (w == 32L && pair == "Noninterest") {
    f <- noninterest_fc32
  } else {
    f <- sensitivity_forecasts
  }
  d <- f[f$Model_ID == model_id & f$Window == w, , drop = FALSE]
  if (pair == "Interest") d <- d[d$Target %in% c("II", "IE"), , drop = FALSE]
  if (pair == "Noninterest") d <- d[d$Target %in% c("NIR", "NIE"), , drop = FALSE]
  d
}

ppnr_forecast_rows <- list()
for (w in window_grid) {
  int_id <- selected_models$Model_ID[selected_models$Pair == "Interest" & selected_models$Window == w]
  non_id <- selected_models$Model_ID[selected_models$Pair == "Noninterest" & selected_models$Window == w]
  if (length(int_id) != 1L || length(non_id) != 1L) next
  f_int <- read_forecast_source("Interest", int_id, w)
  f_non <- read_forecast_source("Noninterest", non_id, w)
  key <- c("Origin_Index", "Horizon")
  ii <- f_int[f_int$Target == "II", c(key, "Actual_Billions", "Forecast_Billions")]
  names(ii)[3:4] <- c("Actual_II", "Forecast_II")
  ie <- f_int[f_int$Target == "IE", c(key, "Actual_Billions", "Forecast_Billions")]
  names(ie)[3:4] <- c("Actual_IE", "Forecast_IE")
  nir <- f_non[f_non$Target == "NIR", c(key, "Actual_Billions", "Forecast_Billions")]
  names(nir)[3:4] <- c("Actual_NIR", "Forecast_NIR")
  nie <- f_non[f_non$Target == "NIE", c(key, "Actual_Billions", "Forecast_Billions")]
  names(nie)[3:4] <- c("Actual_NIE", "Forecast_NIE")
  joined <- Reduce(function(x, y) merge(x, y, by = key, all = FALSE), list(ii, ie, nir, nie))
  joined$Quarter <- analysis_data$Quarter[joined$Origin_Index + joined$Horizon]
  joined$Actual_PPNR <- joined$Actual_II - joined$Actual_IE + joined$Actual_NIR - joined$Actual_NIE
  joined$Forecast_PPNR <- joined$Forecast_II - joined$Forecast_IE + joined$Forecast_NIR - joined$Forecast_NIE
  joined$Error_PPNR <- joined$Forecast_PPNR - joined$Actual_PPNR
  joined$MASE_Scale_PPNR <- vapply(joined$Origin_Index, function(origin) {
    mean(abs(diff(analysis_data$PPNR[seq_len(origin)])))
  }, numeric(1))
  joined$Window <- w
  joined$Interest_Model_ID <- int_id
  joined$Noninterest_Model_ID <- non_id
  ppnr_forecast_rows[[length(ppnr_forecast_rows) + 1L]] <- joined
}
ppnr_forecasts <- if (length(ppnr_forecast_rows)) do.call(rbind, ppnr_forecast_rows) else data.frame()
readr::write_csv(ppnr_forecasts, file.path(output_dir, "ppnr_component_composite_backtest_forecasts.csv"))

ppnr_summary_rows <- list()
for (w in window_grid) {
  d <- ppnr_forecasts[ppnr_forecasts$Window == w, , drop = FALSE]
  if (!nrow(d)) next
  ppnr_summary_rows[[length(ppnr_summary_rows) + 1L]] <- data.frame(
    Forecast_Type = "Selected component-pair VAR composite",
    Window = w, Origins = length(unique(d$Origin_Index)), Forecasts = nrow(d),
    MASE = mean(abs(d$Error_PPNR) / d$MASE_Scale_PPNR),
    MAE_Billions = mean(abs(d$Error_PPNR)),
    RMSE_Billions = sqrt(mean(d$Error_PPNR^2)),
    Mean_Actual_PPNR_Billions = mean(d$Actual_PPNR),
    Mean_Forecast_PPNR_Billions = mean(d$Forecast_PPNR),
    Interest_Model_ID = d$Interest_Model_ID[1L],
    Noninterest_Model_ID = d$Noninterest_Model_ID[1L],
    stringsAsFactors = FALSE
  )
  benchmark_forecast <- analysis_data$PPNR[d$Origin_Index]
  benchmark_error <- benchmark_forecast - d$Actual_PPNR
  ppnr_summary_rows[[length(ppnr_summary_rows) + 1L]] <- data.frame(
    Forecast_Type = "Random walk PPNR benchmark",
    Window = w, Origins = length(unique(d$Origin_Index)), Forecasts = nrow(d),
    MASE = mean(abs(benchmark_error) / d$MASE_Scale_PPNR),
    MAE_Billions = mean(abs(benchmark_error)),
    RMSE_Billions = sqrt(mean(benchmark_error^2)),
    Mean_Actual_PPNR_Billions = mean(d$Actual_PPNR),
    Mean_Forecast_PPNR_Billions = mean(benchmark_forecast),
    Interest_Model_ID = NA_character_,
    Noninterest_Model_ID = NA_character_,
    stringsAsFactors = FALSE
  )
}
ppnr_summary <- if (length(ppnr_summary_rows)) do.call(rbind, ppnr_summary_rows) else data.frame()
readr::write_csv(ppnr_summary, file.path(output_dir, "ppnr_performance_by_window.csv"))

metadata <- data.frame(
  Run_Date = as.character(Sys.Date()), Training_Start = train$Quarter[1L],
  Training_End = tail(train$Quarter, 1L), Training_Rows = nrow(train),
  Holdout_Loaded = FALSE, Windows = paste(window_grid, collapse = ","),
  Horizon = forecast_horizon, Common_Origins = length(common_origins),
  First_Common_Origin = analysis_data$Quarter[common_origins[1L]],
  Last_Common_Origin = analysis_data$Quarter[tail(common_origins, 1L)],
  Top50_Forms_Per_Pair = 50L,
  Refit_Windows = paste(refit_windows, collapse = ","),
  Model_Window_Tasks = length(tasks), Expected_VAR_Fits = total_fits,
  Successful_VAR_Fits = sum(!sensitivity_fits$Fit_Failed),
  Failed_VAR_Fits = sum(sensitivity_fits$Fit_Failed),
  Warned_VAR_Fits = sum(sensitivity_fits$Warning_Count > 0L),
  stringsAsFactors = FALSE
)
readr::write_csv(metadata, file.path(output_dir, "run_metadata.csv"))
readme <- c(
  "# Training-only VAR window sensitivity and PPNR component composite",
  "",
  "W32 stationary searches for interest and non-interest pairs selected Top-50 feasible formula sets.",
  "Those same formula sets were re-estimated at W28 and W24 using the same 20 rolling origins.",
  "This is a focused window sensitivity of W32-selected formulas, not a full model-grid search at W28/W24.",
  "The 2024 Q3–2026 Q3 Y-9C holdout was not loaded.",
  "",
  paste0("Common origins: ", length(common_origins), " (", analysis_data$Quarter[common_origins[1L]],
         "–", analysis_data$Quarter[tail(common_origins, 1L)], "), forecast horizon: ", forecast_horizon, "."),
  paste0("Top-50 re-estimation fits: ", total_fits, "; failed: ", sum(sensitivity_fits$Fit_Failed),
         "; warnings: ", sum(sensitivity_fits$Warning_Count > 0L), "."),
  "",
  "PPNR composite selects the best fully stable feasible shortlisted model for each component pair and window,",
  "then adds II - IE + non-interest income - non-interest expense. The two pairwise VARs forecast their macro",
  "variables separately, so this is a component-wise autonomous forecast portfolio, not a single shared macro path.",
  "A common Fed stress path is a later exercise.",
  "",
  "Outputs: top50_reestimated_model_results.csv, top50_reestimated_origin_forecasts.csv,",
  "top50_window_comparison_results.csv, window_summary_top50.csv, matched_specification_window_ranks.csv,",
  "matched_specification_window_summary.csv, selected_pair_models_by_window.csv,",
  "ppnr_component_composite_backtest_forecasts.csv, ppnr_performance_by_window.csv,",
  "excluded_short_window_models.csv, top50_reestimated_fit_status.csv, run_metadata.csv."
)
writeLines(readme, file.path(output_dir, "README.md"))
saveRDS(
  list(
    sensitivity_model_results = sensitivity_results,
    sensitivity_forecasts = sensitivity_forecasts,
    sensitivity_fit_status = sensitivity_fits,
    window_results = window_results, window_summary = window_summary,
    matched_window_summary = matched_window_summary,
    selected_models = selected_models,
    ppnr_forecasts = ppnr_forecasts, ppnr_summary = ppnr_summary
  ),
  file.path(output_dir, "var_window_sensitivity_results.rds"),
  compress = "gzip"
)
cat("\nWINDOW_SUMMARY_TOP50\n")
print(window_summary, row.names = FALSE)
cat("\nSELECTED_MODELS_BY_WINDOW\n")
print(selected_models, row.names = FALSE)
cat("\nPPNR_PERFORMANCE\n")
print(ppnr_summary, row.names = FALSE)
cat("\nRUN_METADATA\n")
print(metadata, row.names = FALSE)
cat("\nSENSITIVITY_COMPLETE\n")
