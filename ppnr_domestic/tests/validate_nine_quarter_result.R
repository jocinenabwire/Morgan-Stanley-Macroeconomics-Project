# Verify the completed nine-quarter result without rerunning the search.
source("ppnr_domestic/search_nine_quarters.R")
output_dir <- file.path(ppnr_search_dir, "results_nine_quarters_local_2026-10-25")
r <- readRDS(file.path(output_dir, "ppnr_nine_quarters.rds"))
d <- load_ppnr_search_inputs(r$settings$include_own)
stopifnot(r$settings$max_completed == 8L, nrow(r$top3) == 216L,
  all(table(r$top3$factors, r$top3$horizon) == 3L),
  identical(r$settings$horizons, 1:9), all(r$top3$check_n == 6L),
  r$settings$final_origin == "2024 Q1", sum(r$latest_winners$actual_available) == 6L)
expected <- 9 * (sum(choose(31, 1:8)) + sum(choose(30, 0:7)))
stopifnot(sum(r$rounds$combinations) == expected, expected == 128384640)
largest_difference <- 0; verified <- 0L
for (j in seq_len(nrow(r$top3))) {
  model <- r$top3[j, ]; h <- model$horizon
  variables <- if (nzchar(model$variables)) strsplit(model$variables, ";", fixed = TRUE)[[1]] else character()
  stopifnot(length(variables) + as.integer(model$fixed_PPNR_baseline) == model$factors)
  for (period in c("selection", "check", "observed_path", "latest_path")) {
    origins <- switch(period, observed_path = 61L, latest_path = 64L,
      ppnr_evaluation_origins(d, h, period, 9L))
    actual <- d$all_data$PPNR[origins + h]
    predicted <- vapply(origins, function(o) {
      rows <- seq.int(2L, o - h)
      stopifnot(max(rows + h) == o)
      response <- d$y[rows + h] - if (model$target == "change") d$y[rows] else 0
      frame <- data.frame(response, d$x[rows, variables, drop = FALSE])
      fit <- lm(response ~ ., data = frame)
      new <- as.data.frame(d$x[o, variables, drop = FALSE])
      if (!length(variables)) new <- data.frame(dummy = 1)
      as.numeric(predict(fit, newdata = new)) + if (model$target == "change") d$y[o] else 0
    }, 0.0)
    saved <- switch(period, observed_path = r$final_predictions,
      latest_path = r$latest_predictions, r$predictions[r$predictions$period == period, ])
    saved <- saved[saved$model_id == model$model_id, ]
    stopifnot(identical(is.na(actual), is.na(saved$actual)))
    if (any(is.finite(actual))) stopifnot(max(abs(actual - saved$actual), na.rm = TRUE) == 0)
    difference <- max(abs(predicted - saved$predicted))
    largest_difference <- max(largest_difference, difference)
    stopifnot(difference < 1e-6)
    if (period %in% c("selection", "check")) stopifnot(
      abs(sqrt(mean((predicted - actual)^2)) - model[[paste0(period, "_RMSE")]]) < 1e-6)
    verified <- verified + length(origins)
  }
}
for (h in 1:9) {
  rows <- r$top3[r$top3$horizon == h, ]
  stopifnot(r$winners$model_id[r$winners$horizon == h] == rows$model_id[which.min(rows$selection_RMSE)])
  for (k in 1:8) {
    z <- rows[rows$factors == k, ]; z <- z[order(z$rank), ]
    stopifnot(all(diff(z$selection_RMSE) >= 0))
  }
}
# Independent seasonal-naive construction: repeat the four last observed seasons.
for (j in seq_len(nrow(r$benchmarks))) {
  row <- r$benchmarks[j, ]
  o <- match(row$origin, d$data$quarter)
  cycle <- rep(d$y[(o - 3L):o], length.out = 12L)
  stopifnot(row$Seasonal_naive == cycle[row$horizon], row$Naive == d$y[o])
}
# Extending the horizon must preserve the previous selection rankings at h <= 4.
old <- readRDS(file.path(ppnr_search_dir, "results_local_2026-10-25", "ppnr_four_quarters.rds"))
new <- r$top3[r$top3$horizon <= 4L, ]
new <- new[match(old$top3$model_id, new$model_id), ]
stopifnot(identical(old$top3$variables, new$variables),
  max(abs(old$top3$selection_RMSE - new$selection_RMSE)) < 1e-12)
comparison <- ppnr_nine_comparison(r)
write.csv(comparison, file.path(output_dir, "horizon_comparison.csv"), row.names = FALSE)
qa <- list(status = "passed", checked_predictions = verified,
  independent_method = "stats::lm and predict.lm with unscaled regressors",
  max_absolute_prediction_difference = largest_difference,
  covered = sum(r$rounds$combinations), excluded = sum(r$rounds$excluded),
  scored = sum(r$rounds$combinations - r$rounds$excluded),
  old_four_horizon_selection_preserved = TRUE, seasonal_naive_uses_only_observed_values = TRUE,
  source_md5 = d$manifest[c("relative_path", "md5")],
  code_md5 = tools::md5sum(c("run_search_nine_quarters.R",
    "ppnr_domestic/search_nine_quarters.R", "ppnr_domestic/search_four_quarters.R",
    "ppnr_domestic/subset_search.cpp")))
saveRDS(qa, file.path(output_dir, "validation.rds"))
print(qa)
