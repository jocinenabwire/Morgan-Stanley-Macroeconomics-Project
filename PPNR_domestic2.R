# Morgan Stanley PPNR, experiment 2 | Source from the RStudio project root.
# Three components: net interest income + noninterest income - noninterest expense.
# Only Fed 2025 Historic Domestic macros are used. The original analysis is read-only.
# Source opens saved results. Set options(ppnr2.rebuild = TRUE) to repeat this experiment.

ppnr2 <- local({
  folder <- "PPNR_domestic2"
  stopifnot("Open the repository project first." = file.exists("run_ppnr.R"))
  for (package in c("vars", "urca"))
    if (!requireNamespace(package, quietly = TRUE)) stop("Required R package: ", package)
  dir.create(file.path(folder, "results"), showWarnings = FALSE)
  horizon <- 9L
  components <- c("NII", "Noninterest_income", "Noninterest_expense")
  signs <- c(1, 1, -1)
  quarter_number <- function(date) as.integer(format(date, "%m")) %/% 3L
  future_dates <- function(date, h = horizon) {
    q <- as.integer(format(date, "%Y")) * 4L + quarter_number(date) - 1L + seq_len(h)
    as.Date(paste0(q %/% 4L, c("-03-31", "-06-30", "-09-30", "-12-31")[q %% 4L + 1L]))
  }
  seasons <- function(date) {
    q <- quarter_number(date)
    cbind(Q1 = as.numeric(q == 1L), Q2 = as.numeric(q == 2L), Q3 = as.numeric(q == 3L))
  }
  change <- function(z, lag = 1L) c(rep(NA_real_, lag), diff(z, lag = lag))

  # 1. Verified inputs. All money below is USD billions; ratios are quarterly percent.
  original <- new.env(parent = environment())
  sys.source("ppnr_domestic/restore_inputs.R", original, chdir = TRUE)
  company <- original$ms_macro_2025
  macro <- original$fed_macro_2025
  assets_file <- file.path(folder, "data/total_assets.csv")
  stopifnot("Assets input changed; review its provenance first." =
    unname(tools::md5sum(assets_file)) == "cceb8e5d82ea59ebcca238c129ac069f")
  assets <- read.csv(assets_file)
  stopifnot(identical(assets$Date, as.character(company$Date)), nrow(assets) == 70L,
    all(is.finite(assets$total_assets_thousands)), all(assets$total_assets_thousands > 0))
  y <- cbind(NII = company$Total.interest.income - company$Total.interest.expense,
    Noninterest_income = company$Total.noninterest.income,
    Noninterest_expense = company$Total.noninterest.expense) / 1e6
  stopifnot(all(is.finite(y)), all(y[, "NII"] == company$Net.interest.income / 1e6))
  # Transform the complete macro history first: even 2009 Q1 has earlier macro lags.
  features <- with(macro, data.frame(short_rate = us_treasury_3m,
    term_spread = us_treasury_10y - us_treasury_3m,
    equity_return = change(100 * log(us_dow_jones_total_market)),
    credit_change = change(us_bbb_corporate_yield - us_treasury_10y),
    vix_year_change = change(us_vix_max, 4L),
    gdp_growth = us_real_gdp_growth, unemployment_change = change(us_unemployment_rate)))
  x <- as.matrix(features[match(company$Date, macro$quarter_end), ])
  matched <- which(company$macro_matched)
  stopifnot(identical(matched, 1:64), all(is.finite(x[matched, ])), all(is.na(x[-matched, ])))
  input <- list(dates = company$Date, y = y, x = x,
    assets = assets$total_assets_thousands / 1e6, ppnr = drop(y %*% signs))
  # Equal-length paths: all development targets end by 2022 Q4. No check targets select models.
  selection_origins <- 36:47  # 2017 Q4-2020 Q3; 12 complete nine-quarter paths.
  check_origins <- 56:61     # 2022 Q4-2024 Q1; the original six common check origins.
  latest_origin <- 64L      # 2024 Q4, the final observed Domestic macro quarter.
  stopifnot(company$Date[36] == as.Date("2017-12-31"),
    company$Date[47 + horizon] == as.Date("2022-12-31"),
    company$Date[61 + horizon] == as.Date("2026-06-30"))

  # 2. A small, declared menu of models, rather than another exhaustive subset search.
  # The component ARX models forecast macros from past data; VARs forecast them jointly.
  macro_sets <- list(none = character(), rates = c("short_rate", "term_spread"),
    markets = c("equity_return", "credit_change", "vix_year_change"),
    activity = c("gdp_growth", "unemployment_change"), equity = "equity_return",
    risk = c("vix_year_change", "credit_change"))
  specs <- list()
  add_spec <- function(family, basis, transform = "level", own = "none", block = "none",
      seasonal = FALSE, p = 1L, benchmark = "") {
    k <- 3L + length(macro_sets[[block]])
    own_terms <- switch(own, none = 0L, AR1 = 1L, AR2 = 2L, YearAR = 1L)
    parameters <- if (family == "Benchmark") 0L else if (family == "VAR")
      k * (1L + k * p + 3L * seasonal) else
      3L * (1L + own_terms + length(macro_sets[[block]]) + 3L * seasonal) +
        2L * length(macro_sets[[block]])
    specs[[length(specs) + 1L]] <<- data.frame(family, basis, transform, own, block,
      seasonal, p, benchmark, parameters, stringsAsFactors = FALSE)
  }
  for (basis in c("dollars", "assets")) {
    for (method in c("Naive", "Seasonal", "Mean4", "Median8")) {
      if (basis == "assets" && method == "Naive") next # Identical to dollar naive.
      add_spec("Benchmark", basis, benchmark = method)
    }
    for (transform in c("level", "difference")) for (seasonal in c(FALSE, TRUE)) {
      for (own in c("none", "AR1", "AR2", "YearAR"))
        for (block in c("none", "rates", "markets", "activity")) {
          if ((own == "none" && block == "none") || (own == "AR2" && block != "none")) next
          add_spec(if (block == "none") "Component AR" else "Component ARX",
            basis, transform, own, block, seasonal)
        }
      for (block in c("none", "rates", "activity", "equity", "risk")) for (p in 1:2)
        add_spec("VAR", basis, transform, block = block, seasonal = seasonal, p = p)
    }
  }
  specs <- do.call(rbind, specs)
  specs$model_id <- sprintf("M%03d", seq_len(nrow(specs)))
  specs$label <- with(specs, ifelse(family == "Benchmark", paste(benchmark, basis),
    paste(family, ifelse(family == "VAR", paste0("p", p), own), block,
      transform, basis, ifelse(seasonal, "seasonal", "no season"))))
  settings <- list(horizon = horizon, scope = "Fed 2025 Historic Domestic only",
    primary_metric = "RMSE across equal-length nine-quarter development paths",
    selection_origins = as.character(input$dates[selection_origins]),
    check_origins = as.character(input$dates[check_origins]),
    latest_origin = as.character(input$dates[latest_origin]),
    max_companion_root = 0.999, max_forecast_multiple = 10,
    parsimony_tolerance = 0.05, macro_forecast = "AR(1), or flat if its root is not below 0.999",
    assets_projection = "Each origin's observed assets held fixed for nine quarters",
    assets_source = "SEC consolidated GAAP assets; 2009 Q1 NIC; two FR Y-9C overlap checks",
    software = c(R = as.character(getRversion()), vars = as.character(packageVersion("vars")),
      urca = as.character(packageVersion("urca"))),
    interpretation = "Revised-history backtest; overlapping check paths already explored previously")
  source_paths <- c("PPNR_domestic2.R", "ppnr_domestic/restore_inputs.R",
    "ppnr_domestic/data/2025-Table_1A_Historic_Domestic.csv",
    "ppnr_domestic/data/domestic_variables.csv", "ppnr_domestic/data/source_manifest.csv",
    "Morgan_Stanley_PPNR_Quarterly_2009_2026.csv", assets_file,
    "ppnr_domestic/results_nine_quarters_local_2026-10-25/ppnr_nine_quarters.rds")
  signature <- list(files = tools::md5sum(source_paths), specs = specs, settings = settings)

  # 3. Forecast one path. The estimator receives history only, never future observations.
  ar_radius <- function(phi) {
    if (!length(phi)) return(0)
    p <- length(phi)
    companion <- matrix(0, p, p); companion[1, ] <- phi
    if (p > 1L) companion[2:p, 1:(p - 1L)] <- diag(p - 1L)
    max(Mod(eigen(companion, only.values = TRUE)$values))
  }
  fit_ols <- function(design, response) {
    fit <- lm.fit(design, response)
    if (fit$rank < ncol(design) || fit$df.residual < 8L || anyNA(fit$coefficients))
      stop("Insufficient independent training observations")
    fit
  }
  own_values <- function(z, t, own) switch(own, none = numeric(),
    AR1 = c(lag1 = z[t - 1L]), AR2 = c(lag1 = z[t - 1L], lag2 = z[t - 2L]),
    YearAR = c(mean4 = mean(z[(t - 4L):(t - 1L)])))
  macro_path <- function(history) {
    answer <- matrix(NA_real_, horizon, ncol(history), dimnames = list(NULL, colnames(history)))
    if (!ncol(history)) return(answer)
    for (j in seq_len(ncol(history))) {
      z <- history[, j]
      fit <- lm.fit(cbind(1, head(z, -1L)), tail(z, -1L))
      beta <- fit$coefficients
      if (anyNA(beta) || abs(beta[2]) >= settings$max_companion_root) beta <- c(0, 1)
      current <- tail(z, 1)
      for (h in seq_len(horizon)) { current <- beta[1] + beta[2] * current; answer[h, j] <- current }
    }
    answer
  }
  estimate_path <- function(spec, history, dates) {
    scaled <- if (spec$basis == "assets") 100 * history$y / history$assets else history$y
    z <- if (spec$transform == "difference") apply(scaled, 2, change) else scaled
    n <- nrow(z); q_future <- seasons(dates)
    vars <- macro_sets[[spec$block]]
    root <- 0; fitted_models <- list()
    if (spec$family == "Benchmark") {
      predicted <- switch(spec$benchmark,
        Naive = matrix(rep(scaled[n, ], each = horizon), horizon),
        Seasonal = scaled[n - ((4L - seq_len(horizon)) %% 4L), , drop = FALSE],
        Mean4 = matrix(rep(colMeans(tail(scaled, 4L)), each = horizon), horizon),
        Median8 = matrix(rep(apply(tail(scaled, 8L), 2, median), each = horizon), horizon))
    } else if (spec$family == "VAR") {
      design <- cbind(z, history$x[, vars, drop = FALSE])
      valid <- complete.cases(design)
      fit <- vars::VAR(design[valid, , drop = FALSE], p = spec$p, type = "const",
        exogen = if (spec$seasonal) seasons(history$dates[valid]) else NULL)
      # predict.varest evaluates this stored argument; retain data rather than a local expression.
      fit$call$exogen <- if (spec$seasonal) seasons(history$dates[valid]) else NULL
      if (any(vapply(fit$varresult, function(m) m$df.residual < 8L || anyNA(coef(m)), TRUE)))
        stop("Insufficient VAR training degrees of freedom")
      root <- max(vars::roots(fit))
      if (root >= settings$max_companion_root) return(list(valid = FALSE, root = root, reason = "Unstable VAR"))
      prediction <- predict(fit, n.ahead = horizon, dumvar = if (spec$seasonal) q_future else NULL)
      predicted <- sapply(components, function(nm) prediction$fcst[[nm]][, "fcst"])
      fitted_models <- list(VAR = fit)
    } else {
      macro_future <- macro_path(history$x[, vars, drop = FALSE])
      predicted <- matrix(NA_real_, horizon, 3L)
      start <- switch(spec$own, none = 1L, AR1 = 2L, AR2 = 3L, YearAR = 5L)
      if (spec$transform == "difference") start <- start + 1L
      times <- seq.int(start, n)
      seasonal_design <- seasons(history$dates)
      for (j in 1:3) {
        design <- do.call(rbind, lapply(times, function(t) c(const = 1,
          own_values(z[, j], t, spec$own), history$x[t, vars],
          if (spec$seasonal) seasonal_design[t, ] else numeric())))
        fit <- fit_ols(design, z[times, j]); beta <- fit$coefficients
        own_terms <- switch(spec$own, none = 0L, AR1 = 1L, AR2 = 2L, YearAR = 1L)
        phi <- if (own_terms) beta[seq_len(own_terms) + 1L] else numeric()
        if (spec$own == "YearAR") phi <- rep(phi / 4, 4L)
        root <- max(root, ar_radius(phi))
        if (root >= settings$max_companion_root)
          return(list(valid = FALSE, root = root, reason = "Unstable component AR"))
        extended <- z[, j]
        for (h in seq_len(horizon)) {
          row <- c(1, own_values(extended, length(extended) + 1L, spec$own),
            macro_future[h, ], if (spec$seasonal) q_future[h, ] else numeric())
          predicted[h, j] <- sum(row * beta)
          extended <- c(extended, predicted[h, j])
        }
        fitted_models[[components[j]]] <- fit
      }
    }
    if (spec$transform == "difference")
      predicted <- sweep(apply(predicted, 2, cumsum), 2, scaled[n, ], "+")
    if (spec$basis == "assets") predicted <- predicted * tail(history$assets, 1) / 100
    colnames(predicted) <- components
    valid <- all(is.finite(predicted)) && all(predicted[, "Noninterest_expense"] >= 0) &&
      max(abs(predicted)) <= settings$max_forecast_multiple * max(abs(history$y))
    list(valid = valid, components = predicted, root = root, models = fitted_models,
      reason = if (valid) "" else "Nonfinite, negative expense or extreme forecast")
  }
  forecast_from <- function(model_id, origin, data = input, keep_models = FALSE) {
    spec <- specs[match(model_id, specs$model_id), ]
    stopifnot(nrow(spec) == 1L, !is.na(spec$model_id), origin >= 24L, origin <= latest_origin)
    rows <- seq_len(origin)
    history <- list(y = data$y[rows, , drop = FALSE], x = data$x[rows, , drop = FALSE],
      dates = data$dates[rows], assets = data$assets[rows])
    dates <- future_dates(tail(history$dates, 1))
    attempt <- tryCatch(estimate_path(spec, history, dates),
      error = function(e) list(valid = FALSE, root = NA_real_, reason = conditionMessage(e)))
    fallback <- !isTRUE(attempt$valid)
    # A frozen operational fallback keeps every check origin; no failures disappear from RMSE.
    if (fallback) attempt$components <- history$y[
      origin - ((4L - seq_len(horizon)) %% 4L), , drop = FALSE]
    if (!keep_models) attempt$models <- NULL
    attempt$ppnr <- drop(attempt$components %*% signs)
    attempt$fallback <- fallback
    attempt$dates <- dates
    attempt
  }

  # 4. Rolling paths and selection. A five-percent tolerance favors simpler models.
  evaluate <- function(ids, origins, phase) {
    rows <- vector("list", length(ids) * length(origins)); i <- 0L
    for (id in ids) for (o in origins) {
      p <- forecast_from(id, o); i <- i + 1L
      rows[[i]] <- data.frame(model_id = id, phase, origin = input$dates[o],
        horizon = seq_len(horizon), date = p$dates, actual = input$ppnr[o + seq_len(horizon)],
        predicted = p$ppnr, p$components, actual_NII = y[o + seq_len(horizon), 1],
        actual_Noninterest_income = y[o + seq_len(horizon), 2],
        actual_Noninterest_expense = y[o + seq_len(horizon), 3],
        max_root = p$root, fallback = p$fallback, reason = p$reason, train_end = input$dates[o])
    }
    do.call(rbind, rows)
  }
  score <- function(predictions) {
    do.call(rbind, lapply(split(predictions, predictions$model_id), function(z) {
      error <- z$predicted - z$actual
      sum_error <- tapply(error, z$origin, sum)
      data.frame(model_id = z$model_id[1], RMSE = sqrt(mean(error^2)), MAE = mean(abs(error)),
        H9_RMSE = sqrt(mean(error[z$horizon == horizon]^2)),
        Sum9_RMSE = sqrt(mean(sum_error^2)),
        Path_error_SD = sd(tapply(error, z$origin, mean)),
        fallback_origins = length(unique(z$origin[z$fallback])),
        paths = length(unique(z$origin)), errors = nrow(z), row.names = NULL)
    }))
  }
  choose_model <- function(scores, ids) {
    z <- merge(specs[specs$model_id %in% ids, ], scores, by = "model_id")
    z <- z[z$fallback_origins == 0L & is.finite(z$RMSE), ]
    if (!nrow(z)) return(NA_character_)
    z <- z[z$RMSE <= min(z$RMSE) * (1 + settings$parsimony_tolerance), ]
    z$model_id[order(z$parameters, z$RMSE, z$model_id)[1L]]
  }
  reference_predictions <- function(origins, phase) {
    old <- readRDS(tail(source_paths, 1))
    dates <- input$dates[origins]
    labels <- paste(format(dates, "%Y"), paste0("Q", quarter_number(dates)))
    z <- old$predictions[old$predictions$model_id %in% old$winners$model_id &
      old$predictions$period == if (phase == "selection") "selection" else "check", ]
    z <- z[z$origin %in% labels, ]
    z$origin <- dates[match(z$origin, labels)]
    stopifnot(nrow(z) == length(origins) * horizon, !anyDuplicated(z[c("origin", "horizon")]))
    data.frame(model_id = "Original_search", phase, origin = z$origin, horizon = z$horizon,
      date = as.Date(vapply(seq_len(nrow(z)), function(i)
        as.character(future_dates(z$origin[i])[z$horizon[i]]), "")),
      actual = z$actual, predicted = z$predicted, fallback = FALSE)
  }
  run_experiment <- function(stage = "full") {
    dev_file <- file.path(folder, "results/development.rds")
    dev <- if (file.exists(dev_file)) readRDS(dev_file) else NULL
    if (is.null(dev) || !identical(dev$signature, signature)) {
      message("Development: ", nrow(specs), " declared models, 12 nine-quarter paths each.")
      development <- evaluate(specs$model_id, selection_origins, "selection")
      development_scores <- score(development)
      groups <- list(Selected_new = specs$model_id,
        Selected_component = specs$model_id[specs$family %in% c("Component AR", "Component ARX")],
        Selected_ARX = specs$model_id[specs$family == "Component ARX"],
        Selected_VAR = specs$model_id[specs$family == "VAR"],
        Selected_dollars = specs$model_id[specs$basis == "dollars" & specs$family != "Benchmark"],
        Selected_assets = specs$model_id[specs$basis == "assets" & specs$family != "Benchmark"])
      chosen <- vapply(groups, function(ids) choose_model(development_scores, ids), "")
      dev <- list(signature = signature, predictions = development,
        scores = development_scores, chosen = chosen)
      saveRDS(dev, dev_file)
    }
    # This file is written BEFORE any new later-period evaluation or latest-path fitting.
    lock <- list(signature = signature, chosen = dev$chosen, scores = dev$scores,
      frozen_at = as.character(Sys.time()), rule = "Selection errors only; five-percent parsimony tolerance")
    saveRDS(lock, file.path(folder, "results/selection_lock.rds"))
    if (stage == "development") return(dev)
    message("Selection frozen. Evaluating the six previously explored check origins.")
    check <- evaluate(specs$model_id, check_origins, "check")
    reference <- rbind(reference_predictions(selection_origins, "selection"),
      reference_predictions(check_origins, "check"))
    ref_dev <- score(reference[reference$phase == "selection", ])
    ref_check <- score(reference[reference$phase == "check", ])
    model_table <- merge(specs, dev$scores, by = "model_id", sort = FALSE)
    names(model_table)[match(names(dev$scores)[-1], names(model_table))] <- paste0("selection_", names(dev$scores)[-1])
    check_scores <- score(check)
    model_table <- merge(model_table, check_scores, by = "model_id", sort = FALSE)
    names(model_table)[match(names(check_scores)[-1], names(model_table))] <- paste0("check_", names(check_scores)[-1])
    model_table <- model_table[order(model_table$selection_fallback_origins > 0L,
      model_table$selection_RMSE, model_table$parameters), ]
    selected_ids <- unique(na.omit(c(dev$chosen, specs$model_id[specs$family == "Benchmark"])))
    comparison <- model_table[match(selected_ids, model_table$model_id), ]
    comparison$role <- vapply(comparison$model_id, function(id) {
      roles <- names(dev$chosen)[dev$chosen == id & !is.na(dev$chosen)]
      if (length(roles)) paste(roles, collapse = "; ") else "Benchmark"
    }, "")
    original_row <- comparison[1, ]; original_row[1, ] <- NA
    original_row$model_id <- "Original_search"; original_row$family <- "Previous exhaustive search"
    original_row$label <- "Original horizon-specific winners"; original_row$role <- "Frozen reference"
    for (nm in names(ref_dev)[-1]) original_row[[paste0("selection_", nm)]] <- ref_dev[[nm]]
    for (nm in names(ref_check)[-1]) original_row[[paste0("check_", nm)]] <- ref_check[[nm]]
    comparison <- rbind(comparison, original_row)
    naive_id <- specs$model_id[specs$benchmark == "Naive" & specs$basis == "dollars"]
    naive_rmse <- comparison$check_RMSE[match(naive_id, comparison$model_id)]
    comparison$check_gain_vs_naive_percent <- 100 * (1 - comparison$check_RMSE / naive_rmse)
    # Component errors may offset: signed MSE contributions add to the total PPNR MSE.
    component_accuracy <- do.call(rbind, lapply(c("selection", "check"), function(phase) {
      paths <- if (phase == "selection") dev$predictions else check
      do.call(rbind, lapply(selected_ids, function(id) {
        z <- paths[paths$model_id == id, ]
        error <- as.matrix(z[components]) - as.matrix(z[paste0("actual_", components)])
        signed_error <- sweep(error, 2, signs, "*")
        data.frame(model_id = id, phase, component = components,
          RMSE = sqrt(colMeans(error^2)), MAE = colMeans(abs(error)),
          PPNR_MSE_contribution = colMeans(signed_error * rowSums(signed_error)), row.names = NULL)
      }))
    }))
    latest <- lapply(selected_ids, function(id) {
      p <- forecast_from(id, latest_origin)
      data.frame(model_id = id, date = p$dates, horizon = seq_len(horizon),
        predicted = p$ppnr, p$components, fallback = p$fallback, max_root = p$root)
    })
    latest <- do.call(rbind, latest)
    latest_models <- lapply(unique(na.omit(dev$chosen)), forecast_from, origin = latest_origin, keep_models = TRUE)
    names(latest_models) <- unique(na.omit(dev$chosen))
    old <- readRDS(tail(source_paths, 1))
    selected <- latest[latest$model_id == dev$chosen[["Selected_new"]], ]
    selected$actual <- input$ppnr[latest_origin + selected$horizon]
    selected$original_search <- old$latest_winners$predicted[match(selected$horizon, old$latest_winners$horizon)]
    selected$seasonal_naive <- latest$predicted[latest$model_id ==
      specs$model_id[specs$benchmark == "Seasonal" & specs$basis == "dollars"]]
    # Stationarity is descriptive: both level/difference candidates were declared before scoring.
    diagnostics <- do.call(rbind, lapply(c("dollars", "assets"), function(basis)
      do.call(rbind, lapply(components, function(nm) {
        values <- input$y[1:56, nm]
        if (basis == "assets") values <- 100 * values / input$assets[1:56]
        do.call(rbind, lapply(c("level", "difference"), function(form) {
          z <- if (form == "difference") diff(values) else values
          adf <- urca::ur.df(z, type = "drift", lags = 4, selectlags = "BIC")
          kpss <- urca::ur.kpss(z, type = "mu", lags = "short")
          data.frame(component = nm, basis, form, sample_end = "2022 Q4",
            ADF_stat = adf@teststat[1, "tau2"], ADF_5pct = adf@cval["tau2", "5pct"],
            KPSS_stat = kpss@teststat[1], KPSS_5pct = kpss@cval[1, "5pct"])
        }))
      }))))
    list(settings = settings, signature = signature, input = input, specs = specs,
      models = model_table, comparison = comparison, chosen = dev$chosen,
      development = dev$predictions, check = check, reference = reference,
      forecast = selected, latest_paths = latest, fitted_models = latest_models,
      component_accuracy = component_accuracy, diagnostics = diagnostics, selection_lock = lock,
      checks = c(company_quarters = nrow(company), matched_quarters = length(matched),
        macro_source_series = nrow(original$dictionary), declared_models = nrow(specs),
        development_paths_per_model = length(selection_origins), check_paths_per_model = length(check_origins),
        latest_known_actuals = sum(!is.na(selected$actual)),
        max_component_identity_error = max(abs(input$ppnr - drop(input$y %*% signs)))))
  }

  # 5. Source uses a cache tied to the script, inputs and declared model menu.
  cache_file <- file.path(folder, "results/analysis.rds")
  result <- if (file.exists(cache_file)) readRDS(cache_file) else NULL
  stage <- getOption("ppnr2.stage", "full")
  if (stage == "development") result <- run_experiment(stage) else
    if (isTRUE(getOption("ppnr2.rebuild", FALSE)) || is.null(result) || !identical(result$signature, signature)) {
      result <- run_experiment()
      saveRDS(result, cache_file)
      write.csv(result$models, file.path(folder, "results/model_comparison.csv"), row.names = FALSE)
      write.csv(result$comparison, file.path(folder, "results/selected_comparison.csv"), row.names = FALSE)
      write.csv(result$forecast, file.path(folder, "results/latest_forecast.csv"), row.names = FALSE)
      write.csv(result$diagnostics, file.path(folder, "results/stationarity.csv"), row.names = FALSE)
      write.csv(result$component_accuracy, file.path(folder, "results/component_accuracy.csv"), row.names = FALSE)
    }

  # 6. One plotting command. All data and helper names stay inside this private environment.
  result$plot <- function(which = c("forecast", "accuracy", "components", "history")) {
    which <- match.arg(which)
    old <- par(no.readonly = TRUE); on.exit(par(old))
    par(mfrow = c(1, 1), mar = c(5, 4.5, 3.5, 1))
    id <- result$chosen[["Selected_new"]]
    if (which == "forecast") {
      z <- result$forecast
      matplot(z$horizon, cbind(z$actual, z$predicted, z$original_search, z$seasonal_naive),
        type = "o", lty = c(1, 1, 2, 3), pch = c(16, 1, NA, NA),
        col = c("black", "#007A87", "#B55B19", "grey55"),
        xlab = "Quarters after 2024 Q4", ylab = "PPNR (USD billion per quarter)",
        main = "PPNR: nine-quarter forecast")
      legend("topleft", c("Observed", "Selected on development data", "Original search", "Seasonal naive"),
        col = c("black", "#007A87", "#B55B19", "grey55"), lty = c(1, 1, 2, 3), bty = "n", cex = 0.8)
      mtext("Last three actuals unavailable; forecast origin remains 2024 Q4", side = 3, cex = 0.7)
    } else if (which == "accuracy") {
      ids <- unique(na.omit(c(id, result$chosen[["Selected_VAR"]], "Original_search",
        specs$model_id[specs$benchmark == "Seasonal" & specs$basis == "dollars"])))
      by_horizon <- sapply(ids, function(i) {
        z <- if (i == "Original_search") result$reference[result$reference$phase == "check", ] else
          result$check[result$check$model_id == i, ]
        vapply(seq_len(horizon), function(h) sqrt(mean((z$predicted[z$horizon == h] - z$actual[z$horizon == h])^2)), 0.0)
      })
      matplot(seq_len(horizon), by_horizon, type = "o", lty = 1:4, pch = 1:4,
        xlab = "Forecast horizon (quarters)", ylab = "Check RMSE (USD billion)",
        main = "Forecast error by horizon")
      labels <- ifelse(ids == id, "Selected new model", ifelse(ids == "Original_search",
        "Original search", ifelse(ids == result$chosen[["Selected_VAR"]], "Selected VAR", "Seasonal naive")))
      legend("topleft", labels, col = seq_along(ids), lty = seq_along(ids),
        pch = seq_along(ids), bty = "n", cex = 0.8)
      mtext("Six overlapping historical check paths; lower is better", side = 3, cex = 0.7)
    } else if (which == "components") {
      par(mfrow = c(3, 1), mar = c(3, 4.5, 2, 1))
      for (j in 1:3) {
        o <- (latest_origin - 11L):latest_origin
        z <- result$forecast
        dates <- c(input$dates[o], z$date)
        observed <- c(input$y[o, j], input$y[match(z$date, input$dates), j])
        plot(dates, observed, type = "o", pch = 16, cex = 0.5,
          ylim = range(c(observed, z[[components[j]]]), na.rm = TRUE),
          xlab = "", ylab = "USD billion", main = components[j])
        lines(c(input$dates[latest_origin], z$date), c(input$y[latest_origin, j], z[[components[j]]]),
          col = "#007A87", lty = 2, lwd = 2)
        abline(v = input$dates[latest_origin], col = "grey65", lty = 3)
      }
    } else {
      o <- tail(check_origins, 1); z <- result$check[result$check$model_id == id & result$check$origin == input$dates[o], ]
      matplot(z$horizon, cbind(z$actual, z$predicted), type = "o", lty = c(1, 2), pch = c(16, 1),
        col = c("black", "#007A87"), xlab = "Quarters after 2024 Q1",
        ylab = "PPNR (USD billion per quarter)", main = "Fully observed path | overlaps the historical check")
      legend("topleft", c("Observed", "Selected on development data"), lty = c(1, 2),
        col = c("black", "#007A87"), bty = "n")
    }
    invisible(NULL)
  }
  result$forecast_from <- forecast_from
  result
})

# Five useful names; the existing ppnr objects are preserved.
ppnr2_models <- ppnr2$models
ppnr2_scores <- ppnr2$comparison
ppnr2_forecast <- ppnr2$forecast
ppnr2_plot <- ppnr2$plot

# View(ppnr2_scores)          # Development-selected models and fixed benchmarks.
# View(ppnr2_models)          # Every declared candidate, ordered by development error.
# View(ppnr2_forecast)        # The selected nine-quarter path and available actuals.
# ppnr2_plot("components")   # Where the three component forecasts differ from outcomes.
# ppnr2_plot("accuracy")     # Forecast error at horizons 1-9.
# ppnr2_plot("history")      # A fully observed nine-quarter path.
if (getOption("ppnr2.stage", "full") == "full") {
  print(ppnr2$checks)
  print(ppnr2_scores[c("model_id", "role", "selection_RMSE", "check_RMSE", "check_H9_RMSE", "check_Sum9_RMSE")], row.names = FALSE)
  if (interactive()) ppnr2_plot("forecast")
}
