# Independent verification of the completed local result, without another search.
source("ppnr_domestic/search_four_quarters.R")
output_dir <- file.path(ppnr_search_dir, "results_local_2026-10-25")
r <- readRDS(file.path(output_dir, "ppnr_four_quarters.rds"))
d <- load_ppnr_search_inputs(r$settings$include_own)
stopifnot(r$settings$max_completed == 8L, nrow(r$top3) == 96L,
  all(table(r$top3$factors, r$top3$horizon) == 3L), nrow(r$final_winners) == 4L,
  identical(r$settings$horizons, 1:4), all(r$top3$factors <= 8L))
expected <- 4 * (sum(choose(31, 1:8)) + sum(choose(30, 0:7)))
stopifnot(sum(r$rounds$combinations) == expected, expected == 57059840)
largest_difference <- 0
verified <- 0L
for (j in seq_len(nrow(r$top3))) {
  model <- r$top3[j, ]; h <- model$horizon
  variables <- if (nzchar(model$variables)) strsplit(model$variables, ";", fixed = TRUE)[[1]] else character()
  stopifnot(length(variables) + as.integer(model$fixed_PPNR_baseline) == model$factors)
  for (period in c("selection", "check", "final")) {
    origins <- if (period == "final") 64L else ppnr_search_origins(d$data, h, period)
    actual <- d$all_data$PPNR[origins + h]
    predicted <- vapply(origins, function(o) {
      rows <- seq.int(2L, o - h)
      response <- d$y[rows + h] - if (model$target == "change") d$y[rows] else 0
      frame <- data.frame(response, d$x[rows, variables, drop = FALSE])
      fit <- lm(response ~ ., data = frame)
      new <- as.data.frame(d$x[o, variables, drop = FALSE])
      if (!length(variables)) new <- data.frame(dummy = 1)
      as.numeric(predict(fit, newdata = new)) + if (model$target == "change") d$y[o] else 0
    }, 0.0)
    saved <- if (period == "final") r$final_predictions else r$predictions
    saved <- saved[saved$model_id == model$model_id &
      if (period == "final") TRUE else saved$period == period, ]
    difference <- max(abs(predicted - saved$predicted))
    largest_difference <- max(largest_difference, difference)
    stopifnot(difference < 1e-6, max(abs(actual - saved$actual)) == 0)
    if (period != "final") stopifnot(abs(sqrt(mean((predicted - actual)^2)) -
      model[[paste0(period, "_RMSE")]]) < 1e-6)
    verified <- verified + length(origins)
  }
}
for (h in 1:4) {
  rows <- r$top3[r$top3$horizon == h, ]
  stopifnot(r$winners$model_id[r$winners$horizon == h] == rows$model_id[which.min(rows$selection_RMSE)])
  for (k in 1:8) {
    z <- rows[rows$factors == k, ]; z <- z[order(z$rank), ]
    stopifnot(all(diff(z$selection_RMSE) >= 0))
  }
}
stopifnot(all(r$final_predictions$origin == "2024 Q4"),
  identical(r$final_winners$quarter, c("2025 Q1", "2025 Q2", "2025 Q3", "2025 Q4")))
qa <- list(status = "passed", checked_predictions = verified,
  independent_method = "stats::lm and predict.lm on unscaled source regressors",
  max_absolute_prediction_difference = largest_difference,
  covered = sum(r$rounds$combinations), excluded = sum(r$rounds$excluded),
  scored = sum(r$rounds$combinations - r$rounds$excluded),
  source_md5 = d$manifest[c("relative_path", "md5")],
  code_md5 = tools::md5sum(c("run_search_four_quarters.R",
    "ppnr_domestic/search_four_quarters.R", "ppnr_domestic/subset_search.cpp")))
saveRDS(qa, file.path(output_dir, "validation.rds"))
print(qa)
