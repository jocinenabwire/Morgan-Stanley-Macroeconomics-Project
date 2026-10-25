# Morgan Stanley PPNR | Domestic-only exhaustive forecasts | 2026-10-25
# Source this file to define the functions. The search runs only when requested.
# All files and results stay local; this script performs no Git operations.

ppnr_search_dir <- local({
  paths <- vapply(sys.frames(), function(f) if (is.null(f$ofile)) "" else f$ofile[1], "")
  dirs <- c(dirname(rev(paths[nzchar(paths)])), "ppnr_domestic", ".")
  normalizePath(dirs[file.exists(file.path(dirs, "subset_search.cpp"))][1], mustWork = TRUE)
})

load_ppnr_search_inputs <- function(include_own = TRUE) {
  inputs <- new.env(parent = globalenv())
  sys.source(file.path(ppnr_search_dir, "restore_inputs.R"), inputs, chdir = TRUE)
  all_data <- inputs$ms_macro_2025
  all_data$PPNR <- with(all_data, (Total.interest.income - Total.interest.expense +
    Total.noninterest.income - Total.noninterest.expense) / 1e6)
  stopifnot(all(all_data$Total.interest.income - all_data$Total.interest.expense ==
    all_data$Net.interest.income), all(is.finite(all_data$PPNR)))
  data <- all_data[all_data$macro_matched, ]
  macro <- inputs$dictionary$r_name
  stopifnot(length(macro) == 16L, all(grepl("^us_", macro)), nrow(data) == 64L,
    identical(range(data$Date), as.Date(c("2009-03-31", "2024-12-31"))))
  prices <- c("us_dow_jones_total_market", "us_house_price_index", "us_cre_price_index")
  rates <- c("us_treasury_3m", "us_treasury_5y", "us_treasury_10y",
    "us_bbb_corporate_yield", "us_mortgage_rate", "us_prime_rate")
  delta <- function(z) c(NA_real_, diff(z))
  x <- data[macro]
  x[prices] <- lapply(x[prices], function(z) 100 * log(z))
  names(x)[match(prices, macro)] <- paste0("log_", prices)
  for (nm in prices) x[[paste0("return_", nm)]] <- delta(100 * log(data[[nm]]))
  for (nm in rates) x[[paste0("d_", nm)]] <- delta(data[[nm]])
  x$credit_spread <- data$us_bbb_corporate_yield - data$us_treasury_10y
  x$term_spread <- data$us_treasury_10y - data$us_treasury_3m
  x$d_credit_spread <- delta(x$credit_spread)
  x$d_term_spread <- delta(x$term_spread)
  stopifnot(ncol(x) == 29L)
  if (include_own) {
    x$PPNR <- data$PPNR
    x$d_PPNR <- delta(data$PPNR)
  }
  x <- as.matrix(x)
  stopifnot(all(is.finite(x[-1, ])))
  list(data = data, all_data = all_data, x = x, y = data$PPNR,
    dictionary = inputs$dictionary, manifest = inputs$manifest, include_own = include_own)
}

# Calendar-based windows preserve the earlier selection and check periods.
# For every fit, the latest training response is at or before its forecast origin.
ppnr_search_origins <- function(data, h, period = c("selection", "check")) {
  period <- match.arg(period)
  stopifnot(h %in% 1:9)
  bounds <- if (period == "selection") c("2017-12-31", "2022-12-31") else
    c("2022-12-31", "2024-12-31")
  positions <- match(as.Date(bounds), data$Date)
  stopifnot(!anyNA(positions))
  if (positions[2] - h < positions[1]) return(integer())
  seq.int(positions[1], positions[2] - h)
}

ppnr_future_quarters <- function(date, horizons) {
  index <- as.integer(format(date, "%Y")) * 4L +
    as.integer(format(date, "%m")) %/% 3L - 1L + horizons
  paste(index %/% 4L, paste0("Q", index %% 4L + 1L))
}

# The nine-quarter check uses the same six post-selection origins at every
# horizon. Later company outcomes are observable without later macro predictors.
ppnr_evaluation_origins <- function(input, h, period, max_horizon = 4L) {
  if (period == "selection" || max_horizon <= 4L)
    return(ppnr_search_origins(input$data, h, period))
  first <- match(as.Date("2022-12-31"), input$data$Date)
  last <- min(nrow(input$data), nrow(input$all_data) - max_horizon)
  stopifnot(first <= last)
  seq.int(first, last)
}

ppnr_search_folds <- function(input, h, target = "level", period = "selection") {
  x <- input$x
  # With a free PPNR coefficient, level and change equations are identical.
  # The change family excludes that duplicate and counts its fixed PPNR baseline.
  if (target == "change") x <- x[, colnames(x) != "PPNR", drop = FALSE]
  origins <- ppnr_search_origins(input$data, h, period)
  lapply(origins, function(o) {
    i <- 2:(o - h)
    stopifnot(max(i + h) == o)
    response <- input$y[i + h] - if (target == "change") input$y[i] else 0
    centre <- colMeans(x[i, , drop = FALSE])
    z <- sweep(x[i, , drop = FALSE], 2, centre)
    scale <- sqrt(colSums(z^2)); scale[scale < 1e-12] <- 1
    z <- sweep(z, 2, scale, "/")
    list(gram = crossprod(z), cy = as.numeric(crossprod(z, response - mean(response))),
      future = (x[o, ] - centre) / scale,
      base_prediction = mean(response) + if (target == "change") input$y[o] else 0,
      actual = input$y[o + h], origin = o, selection = TRUE)
  })
}

# Independent QR/OLS fitting is used for all retained results and final forecasts.
ppnr_search_predict <- function(input, variables, origin, h, target = "level") {
  i <- 2:(origin - h)
  response <- input$y[i + h] - if (target == "change") input$y[i] else 0
  baseline <- if (target == "change") input$y[origin] else 0
  if (!length(variables)) return(mean(response) + baseline)
  z <- input$x[i, variables, drop = FALSE]
  centre <- colMeans(z); z <- sweep(z, 2, centre)
  scale <- sqrt(colSums(z^2)); scale[scale < 1e-12] <- 1
  fit <- lm.fit(cbind(1, sweep(z, 2, scale, "/")), response)
  if (fit$rank < length(variables) + 1L) return(NA_real_)
  sum(c(1, (input$x[origin, variables] - centre) / scale) * fit$coefficients) + baseline
}

compile_ppnr_search <- function() {
  if (!requireNamespace("Rcpp", quietly = TRUE)) stop("Rcpp is required")
  cache <- file.path(tempdir(), "ppnr_cpp_cache")
  Rcpp::sourceCpp(file.path(ppnr_search_dir, "subset_search.cpp"),
    env = environment(compile_ppnr_search), cacheDir = cache, showOutput = FALSE)
  invisible(TRUE)
}

run_ppnr_horizons <- function(include_own = TRUE, max_factors = 8L,
    threads = 6L, minutes = 120, output_dir = NULL, max_horizon = 4L) {
  stopifnot(max_factors %in% 1:8, threads >= 1L, max_horizon %in% c(4L, 9L))
  horizons <- seq_len(max_horizon)
  started <- proc.time()[3]
  old_limit <- mem.maxVSize()
  invisible(mem.maxVSize(min(old_limit, 3072)))
  on.exit(invisible(mem.maxVSize(old_limit)), add = TRUE)
  input <- load_ppnr_search_inputs(include_own)
  if (is.null(output_dir)) output_dir <- file.path(ppnr_search_dir,
    if (max_horizon == 4L) "results_local_2026-10-25" else "results_nine_quarters_local_2026-10-25")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  compile_ppnr_search()
  tasks <- expand.grid(horizon = horizons,
    target = if (include_own) c("level", "change") else "level", stringsAsFactors = FALSE)
  folds <- lapply(seq_len(nrow(tasks)), function(j)
    ppnr_search_folds(input, tasks$horizon[j], tasks$target[j]))
  candidates <- rounds <- list()
  cat(sprintf("Fed 2025 Domestic only: 16 source series, %d derived candidates.\n", ncol(input$x)))
  final_origin <- min(nrow(input$data), nrow(input$all_data) - max_horizon)
  cat("Selection: 2018-2022 expanding windows.\n")
  cat(sprintf("Check origins: %s to %s. Fully observed path: %s + %d quarters.\n",
    input$data$quarter[min(ppnr_evaluation_origins(input, 1L, "check", max_horizon))],
    input$data$quarter[max(ppnr_evaluation_origins(input, 1L, "check", max_horizon))],
    input$data$quarter[final_origin], max_horizon))
  cat("Ranking uses selection RMSE only; the intercept is excluded from factor counts.\n")
  cat("Change equations count the fixed PPNR baseline toward the eight-factor limit.\n")
  for (size in seq_len(max_factors)) {
    for (target in unique(tasks$target)) {
      free_size <- size - as.integer(target == "change")
      ids <- which(tasks$target == target)
      pool <- colnames(input$x)
      if (target == "change") pool <- setdiff(pool, "PPNR")
      remaining <- minutes * 60 - (proc.time()[3] - started)
      if (remaining <= 0) stop("Time budget reached; local checkpoints retained")
      cat(sprintf("Round %d/%d | %s | %s combinations x %d horizons\n", size, max_factors,
        target, format(choose(length(pool), free_size), big.mark = ",", scientific = FALSE), max_horizon))
      flush.console()
      if (free_size == 0L) {
        native <- lapply(folds[ids], function(ff) list(covered = 1, excluded = 0, total = 1,
          top = list(list(variables = integer(), predictions = vapply(ff, `[[`, 0.0, "base_prediction"),
            MSE = mean(vapply(ff, function(f) (f$base_prediction - f$actual)^2, 0.0))))))
        elapsed <- 0; peak <- NA_real_
      } else {
        round <- ppnr_search_round(folds[ids], free_size, threads, remaining)
        if (round$status != "complete") stop(paste("Incomplete search:", round$status))
        native <- round$tasks; elapsed <- round$seconds; peak <- round$peak_bytes / 2^20
      }
      for (j in seq_along(ids)) {
        h <- tasks$horizon[ids[j]]
        stopifnot(native[[j]]$covered == choose(length(pool), free_size))
        rounds[[length(rounds) + 1L]] <- data.frame(factors = size, target, horizon = h,
          combinations = native[[j]]$covered, excluded = native[[j]]$excluded,
          seconds = elapsed, peak_MiB = peak)
        ff <- folds[[ids[j]]]
        for (r in seq_along(native[[j]]$top)) {
          best <- native[[j]]$top[[r]]
          variables <- pool[best$variables]
          prediction <- vapply(ff, function(f)
            ppnr_search_predict(input, variables, f$origin, h, target), 0.0)
          actual <- vapply(ff, `[[`, 0.0, "actual")
          stopifnot(all(is.finite(prediction)), max(abs(prediction - best$predictions)) < 1e-6,
            abs(mean((prediction - actual)^2) - best$MSE) < 1e-6)
          own <- target == "change" || any(variables %in% c("PPNR", "d_PPNR"))
          macro <- any(!variables %in% c("PPNR", "d_PPNR"))
          candidates[[length(candidates) + 1L]] <- data.frame(factors = size,
            free_regressors = free_size, horizon = h, target,
            model_type = if (own) if (macro) "ARX" else "AR" else "Macro OLS",
            estimation = "OLS", selection_n = length(actual),
            selection_RMSE = sqrt(mean((prediction - actual)^2)),
            selection_MAE = mean(abs(prediction - actual)), variables = paste(variables, collapse = ";"),
            fixed_PPNR_baseline = target == "change", stringsAsFactors = FALSE)
        }
      }
      saveRDS(list(candidates = candidates, rounds = rounds), file.path(output_dir, "checkpoint.rds"))
      cat(sprintf("  Complete | %.1f s\n", elapsed)); flush.console()
    }
  }
  candidates <- do.call(rbind, candidates)
  top3 <- do.call(rbind, lapply(split(candidates, list(candidates$factors, candidates$horizon)),
    function(z) {
      z <- z[order(z$selection_RMSE, z$target, z$variables), ]
      z <- head(z, 3L); z$rank <- seq_len(nrow(z)); z
    }))
  rownames(top3) <- NULL
  top3$model_id <- sprintf("K%d_H%d_R%d", top3$factors, top3$horizon, top3$rank)
  # Freeze selection before reading later evaluation outcomes.
  winners <- do.call(rbind, lapply(split(top3, top3$horizon), function(z)
    z[order(z$selection_RMSE, z$factors, z$variables)[1L], ]))
  predictions <- final <- latest <- list()
  for (r in seq_len(nrow(top3))) {
    model <- top3[r, ]; h <- model$horizon
    variables <- if (nzchar(model$variables)) strsplit(model$variables, ";", fixed = TRUE)[[1]] else character()
    for (period in c("selection", "check")) {
      origins <- ppnr_evaluation_origins(input, h, period, max_horizon)
      predicted <- vapply(origins, function(o)
        ppnr_search_predict(input, variables, o, h, model$target), 0.0)
      actual <- input$all_data$PPNR[origins + h]; errors <- predicted - actual
      top3[r, paste0(period, "_RMSE")] <- sqrt(mean(errors^2))
      top3[r, paste0(period, "_MAE")] <- mean(abs(errors))
      top3[r, paste0(period, "_n")] <- length(errors)
      predictions[[length(predictions) + 1L]] <- data.frame(model_id = model$model_id, period,
        horizon = h, origin = input$data$quarter[origins],
        quarter = input$all_data$quarter[origins + h], actual, predicted)
    }
    # Latest completely observed path. This path overlaps the rolling check;
    # it is a visualization of that check, not an additional independent test.
    origin <- final_origin
    actual <- input$all_data$PPNR[origin + h]
    predicted <- ppnr_search_predict(input, variables, origin, h, model$target)
    final[[r]] <- data.frame(model_id = model$model_id, factors = model$factors, rank = model$rank,
      horizon = h, origin = input$data$quarter[origin],
      quarter = input$all_data$quarter[origin + h], actual, predicted,
      error = predicted - actual, selected_winner = model$model_id %in% winners$model_id)
    origin <- nrow(input$data)
    predicted <- ppnr_search_predict(input, variables, origin, h, model$target)
    actual <- input$all_data$PPNR[origin + h]
    latest[[r]] <- data.frame(model_id = model$model_id, factors = model$factors,
      rank = model$rank, horizon = h, origin = input$data$quarter[origin],
      quarter = ppnr_future_quarters(input$data$Date[origin], h), actual, predicted,
      actual_available = is.finite(actual), error = predicted - actual,
      selected_winner = model$model_id %in% winners$model_id)
  }
  winners <- top3[match(winners$model_id, top3$model_id), ]
  benchmark <- list()
  for (period in c("selection", "check", "final")) for (h in horizons) {
    origins <- if (period == "final") final_origin else
      ppnr_evaluation_origins(input, h, period, max_horizon)
    for (o in origins) {
      benchmark[[length(benchmark) + 1L]] <- data.frame(period, horizon = h,
        origin = input$data$quarter[o], quarter = input$all_data$quarter[o + h],
        actual = input$all_data$PPNR[o + h], Naive = input$y[o],
        Seasonal_naive = input$y[o - ((4L - h) %% 4L)],
        Historical_mean = mean(input$y[seq_len(o)]),
        AR1 = ppnr_search_predict(input = within(input, x <- cbind(PPNR = y)),
          variables = "PPNR", origin = o, h = h))
    }
  }
  benchmark <- do.call(rbind, benchmark)
  scores <- do.call(rbind, lapply(split(benchmark, list(benchmark$period, benchmark$horizon)), function(z) {
    error <- sweep(as.matrix(z[c("Naive", "Seasonal_naive", "Historical_mean", "AR1")]), 1, z$actual)
    data.frame(period = z$period[1], horizon = z$horizon[1], n = nrow(z),
      model = colnames(error), RMSE = sqrt(colMeans(error^2)), MAE = colMeans(abs(error)))
  }))
  result <- list(settings = list(vintage = 2025L, scope = "Domestic only", include_own = include_own,
      max_factors = max_factors, max_completed = max(rounds[[length(rounds)]]$factors),
      pool_size = ncol(input$x), horizons = horizons, selection = "2018-2022",
      check = if (max_horizon == 4L) "2023-2024 targets" else "2022 Q4-2024 Q1 common forecast origins",
      final_origin = input$data$quarter[final_origin],
      final_targets = ppnr_future_quarters(input$data$Date[final_origin], horizons),
      fully_observed_path_overlaps_check = max_horizon > 4L,
      latest_origin = tail(input$data$quarter, 1), threads = threads,
      elapsed_seconds = unname(proc.time()[3] - started),
      fixed_baseline_counts_as_factor = TRUE),
    top3 = top3, winners = winners, predictions = do.call(rbind, predictions),
    final_predictions = do.call(rbind, final), latest_predictions = do.call(rbind, latest),
    benchmarks = benchmark, benchmark_scores = scores,
    rounds = do.call(rbind, rounds), candidate_values = input$x, input_manifest = input$manifest)
  result$final_winners <- result$final_predictions[result$final_predictions$selected_winner, ]
  result$latest_winners <- result$latest_predictions[result$latest_predictions$selected_winner, ]
  result$final_path_RMSE <- sqrt(mean(result$final_winners$error^2))
  saveRDS(result, file.path(output_dir,
    if (max_horizon == 4L) "ppnr_four_quarters.rds" else "ppnr_nine_quarters.rds"))
  for (nm in c("top3", "winners", "final_predictions", "final_winners", "latest_winners", "rounds", "benchmark_scores"))
    write.csv(result[[nm]], file.path(output_dir, paste0(nm, ".csv")), row.names = FALSE)
  cat(sprintf("Completed all 1-%d factor rounds | %.1f min | %s\n", max_factors,
    result$settings$elapsed_seconds / 60, format(sum(result$rounds$combinations), big.mark = ",")))
  print(result$winners[c("horizon", "factors", "model_type", "selection_RMSE", "check_RMSE")], row.names = FALSE)
  print(result$final_winners, row.names = FALSE)
  result
}

# Keep the existing four-quarter entry point and its saved outputs compatible.
run_ppnr_four_quarters <- function(include_own = TRUE, max_factors = 8L,
    threads = 6L, minutes = 120, output_dir = NULL) {
  run_ppnr_horizons(include_own, max_factors, threads, minutes, output_dir, max_horizon = 4L)
}

plot_ppnr_four_quarters <- function(result) {
  final <- result$final_winners[order(result$final_winners$horizon), ]
  baseline <- result$benchmarks[result$benchmarks$period == "final", ]
  values <- cbind(Actual = final$actual, Selected = final$predicted,
    Naive = baseline$Naive, Seasonal_naive = baseline$Seasonal_naive)
  matplot(1:4, values, type = "b", pch = c(16, 17, 1, 2), lty = c(1, 1, 2, 3),
    col = c("black", "#0072B2", "#888888", "#D55E00"), xaxt = "n", xlab = "Target quarter",
    ylab = "PPNR (USD billion per quarter)", main = "Four-quarter historical forecast | origin: 2024 Q4")
  axis(1, at = 1:4, labels = final$quarter)
  legend("topleft", legend = c("Actual", "Selected on 2018-2022", "Naive", "Seasonal naive"),
    col = c("black", "#0072B2", "#888888", "#D55E00"), lty = c(1, 1, 2, 3),
    pch = c(16, 17, 1, 2), bty = "n", cex = 0.8)
  invisible(result)
}
