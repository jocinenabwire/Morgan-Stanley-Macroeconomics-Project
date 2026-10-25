# Run from the repository root with Rscript --vanilla.
source("ppnr_domestic/search_four_quarters.R")
input <- load_ppnr_search_inputs(TRUE)
stopifnot(nrow(input$data) == 64L, nrow(input$all_data) == 70L,
  ncol(input$x) == 31L, ncol(load_ppnr_search_inputs(FALSE)$x) == 29L,
  identical(vapply(1:4, function(h) length(ppnr_search_origins(input$data, h)), 0L), 20:17),
  identical(vapply(1:4, function(h) length(ppnr_search_origins(input$data, h, "check")), 0L), 8:5))
compile_ppnr_search()

# Compare the native implementation against an exhaustive, independently fitted
# QR implementation. Include exact Treasury/spread dependencies and own PPNR.
pool <- c("us_treasury_3m", "us_treasury_10y", "term_spread", "us_vix_max",
  "PPNR", "d_PPNR", "credit_spread")
small <- input
small$x <- input$x[, pool]
qr_prediction <- function(data, variables, origin, h, target) {
  rows <- seq.int(2L, origin - h)
  response <- data$y[rows + h] - if (target == "change") data$y[rows] else 0
  regressors <- data$x[rows, variables, drop = FALSE]
  fit <- lm.fit(cbind(Intercept = 1, regressors), response)
  if (fit$rank != length(variables) + 1L) return(NA_real_)
  sum(c(1, data$x[origin, variables]) * fit$coefficients) +
    if (target == "change") data$y[origin] else 0
}
verified <- 0L
for (target in c("level", "change")) for (h in 1:4) {
  ff <- ppnr_search_folds(small, h, target)
  current_pool <- if (target == "change") setdiff(pool, "PPNR") else pool
  origins <- ppnr_search_origins(small$data, h)
  for (k in 1:4) {
    native <- ppnr_search_round(list(ff), k, threads = 1L, verbose = FALSE)
    stopifnot(native$status == "complete", native$tasks[[1]]$covered == choose(length(current_pool), k))
    subsets <- combn(current_pool, k, simplify = FALSE)
    scores <- vapply(subsets, function(v) {
      p <- vapply(origins, function(o) qr_prediction(small, v, o, h, target), 0.0)
      if (anyNA(p)) Inf else mean((p - small$y[origins + h])^2)
    }, 0.0)
    expected <- sort(scores)[1:3]
    observed <- vapply(native$tasks[[1]]$top, `[[`, 0.0, "MSE")
    stopifnot(max(abs(expected - observed)) < 1e-7)
    for (best in native$tasks[[1]]$top) {
      p <- vapply(origins, function(o)
        qr_prediction(small, current_pool[best$variables], o, h, target), 0.0)
      stopifnot(max(abs(p - best$predictions)) < 1e-7)
    }
    verified <- verified + length(subsets)
  }
}

# Perturb all data after selection: selection calculations must remain unchanged.
altered <- input
after <- input$data$Date > as.Date("2022-12-31")
altered$x[after, ] <- altered$x[after, ] + 1000
altered$y[after] <- altered$y[after] + 1000
for (h in 1:4) for (target in c("level", "change"))
  stopifnot(identical(ppnr_search_folds(input, h, target),
    ppnr_search_folds(altered, h, target)))
forecast <- ppnr_search_predict(input, c("PPNR", "us_vix_max"), 64L, 4L)
altered <- input
altered$all_data$PPNR[65:70] <- -999999
stopifnot(identical(forecast, ppnr_search_predict(altered,
  c("PPNR", "us_vix_max"), 64L, 4L)), is.finite(forecast))

cat(sprintf("PASS: %d subset/horizon/target cases independently ranked by QR.\n", verified))
cat("PASS: four horizons, exact collinearity, top-three retention, and no future-target leakage.\n")
