# Check the added horizons against independently fitted, exhaustive QR models.
source("ppnr_domestic/search_nine_quarters.R")
d <- load_ppnr_search_inputs()
stopifnot(identical(vapply(1:9, function(h) length(ppnr_search_origins(d$data, h)), 0L), 20:12),
  all(vapply(1:9, function(h) length(ppnr_evaluation_origins(d, h, "check", 9L)), 0L) == 6L),
  length(ppnr_search_origins(d$data, 9L, "check")) == 0L,
  identical(ppnr_future_quarters(as.Date("2024-12-31"), 1:9),
    c("2025 Q1", "2025 Q2", "2025 Q3", "2025 Q4", "2026 Q1", "2026 Q2",
      "2026 Q3", "2026 Q4", "2027 Q1")))
compile_ppnr_search()
pool <- c("us_treasury_3m", "us_treasury_10y", "term_spread", "us_vix_max",
  "PPNR", "d_PPNR", "credit_spread")
small <- d; small$x <- small$x[, pool]
qr_prediction <- function(v, o, h, target) {
  i <- 2:(o - h)
  response <- small$y[i + h] - if (target == "change") small$y[i] else 0
  fit <- lm.fit(cbind(1, small$x[i, v, drop = FALSE]), response)
  if (fit$rank != length(v) + 1L) return(NA_real_)
  sum(c(1, small$x[o, v]) * fit$coefficients) + if (target == "change") small$y[o] else 0
}
checked <- 0L
for (h in 5:9) for (target in c("level", "change")) {
  ff <- ppnr_search_folds(small, h, target)
  origins <- ppnr_search_origins(d$data, h)
  current_pool <- if (target == "change") setdiff(pool, "PPNR") else pool
  for (k in 1:4) {
    native <- ppnr_search_round(list(ff), k, threads = 1L, verbose = FALSE)
    subsets <- combn(current_pool, k, simplify = FALSE)
    scores <- vapply(subsets, function(v) {
      p <- vapply(origins, function(o) qr_prediction(v, o, h, target), 0.0)
      if (anyNA(p)) Inf else mean((p - small$y[origins + h])^2)
    }, 0.0)
    stopifnot(native$status == "complete", native$tasks[[1]]$covered == length(subsets),
      max(abs(sort(scores)[1:3] - vapply(native$tasks[[1]]$top, `[[`, 0.0, "MSE"))) < 1e-7)
    checked <- checked + length(subsets)
  }
  # Outcomes and predictors after 2022 cannot affect specification selection.
  altered <- d
  after <- d$data$Date > as.Date("2022-12-31")
  altered$y[after] <- -9999
  altered$x[after, ] <- -9999
  stopifnot(identical(ppnr_search_folds(d, h, target), ppnr_search_folds(altered, h, target)))
}
# The seasonal benchmark at any horizon must use an observed matching season.
o <- 61L
indices <- o - ((4L - 1:9) %% 4L)
stopifnot(all(indices <= o), all(indices >= o - 3L))
for (h in 1:9) stopifnot((indices[h] - o - h) %% 4L == 0L)
cat(sprintf("PASS: %d added subset/horizon/target cases independently ranked by QR.\n", checked))
cat("PASS: nine horizons, six common check origins, quarter labels and leakage guards.\n")
