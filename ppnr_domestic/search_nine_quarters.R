# Nine-quarter extension of the local Domestic subset search.
# Same candidate transformations, OLS estimation and one-to-eight-factor limit.
# Level/change response forms and optional past PPNR are retained.
source("ppnr_domestic/search_four_quarters.R")

run_ppnr_nine_quarters <- function(include_own = TRUE, max_factors = 8L,
    threads = 6L, minutes = 120, output_dir = NULL) {
  run_ppnr_horizons(include_own, max_factors, threads, minutes, output_dir, max_horizon = 9L)
}

ppnr_nine_comparison <- function(result) {
  w <- result$winners[order(result$winners$horizon), ]
  naive <- result$benchmark_scores[result$benchmark_scores$period == "check" &
    result$benchmark_scores$model == "Naive", ]
  w$naive_check_RMSE <- naive$RMSE[match(w$horizon, naive$horizon)]
  w$check_RMSE_gain_percent <- 100 * (1 - w$check_RMSE / w$naive_check_RMSE)
  w[c("horizon", "factors", "model_type", "target", "selection_n", "selection_RMSE",
    "check_n", "check_RMSE", "naive_check_RMSE", "check_RMSE_gain_percent", "variables")]
}

ppnr_ninth_horizon_review <- function(result) {
  z <- subset(result$top3, horizon == 9L)
  z <- z[order(z$check_RMSE, z$factors, z$rank), ]
  z[c("factors", "rank", "model_type", "target", "selection_RMSE", "check_n",
    "check_RMSE", "check_MAE", "variables", "model_id")]
}

plot_ppnr_ninth_model_size <- function(result) {
  z <- subset(result$top3, horizon == 9L & rank == 1L)
  z <- z[order(z$factors), ]
  naive <- subset(result$benchmark_scores, period == "check" & horizon == 9L & model == "Naive")$RMSE
  values <- cbind(Selection = z$selection_RMSE, Check = z$check_RMSE)
  matplot(z$factors, values, type = "b", pch = c(1, 16), lty = c(2, 1),
    col = c("#009E73", "#0072B2"), ylim = c(0, max(values, naive) * 1.1), xaxt = "n",
    xlab = "Total factors (intercept excluded)", ylab = "RMSE (USD billion)",
    main = "Nine-quarter horizon | forecast error by model size")
  axis(1, at = 1:8)
  abline(h = naive, lty = 3, col = "#777777")
  legend("topleft", c("Earlier selection", "Later check: six origins", "Last observed PPNR"),
    col = c("#009E73", "#0072B2", "#777777"), lty = c(2, 1, 3),
    pch = c(1, 16, NA), bty = "n", cex = 0.8)
  invisible(result)
}

plot_ppnr_nine_accuracy <- function(result) {
  z <- ppnr_nine_comparison(result)
  values <- cbind(Selected = z$check_RMSE, Naive = z$naive_check_RMSE)
  matplot(z$horizon, values, type = "b", pch = c(16, 1), lty = c(1, 2),
    col = c("#0072B2", "#777777"), ylim = c(0, max(values) * 1.1), xaxt = "n",
    xlab = "Forecast horizon (quarters)", ylab = "Rolling-check RMSE (USD billion)",
    main = "1-9 quarter accuracy | six common forecast origins")
  axis(1, at = 1:9)
  legend("topleft", c("Selected on 2018-2022", "Last observed PPNR"),
    col = c("#0072B2", "#777777"), lty = c(1, 2), pch = c(16, 1), bty = "n")
  invisible(result)
}

plot_ppnr_nine_path <- function(result, latest = FALSE) {
  z <- if (latest) result$latest_winners else result$final_winners
  z <- z[order(z$horizon), ]
  values <- cbind(Actual = z$actual, Selected = z$predicted)
  matplot(z$horizon, values, type = "n", xaxt = "n", xlab = "Target quarter",
    ylab = "PPNR (USD billion per quarter)",
    main = paste("Nine-quarter historical forecast | origin:", z$origin[1]))
  if (latest && any(!z$actual_available)) {
    limits <- par("usr")
    rect(min(z$horizon[!z$actual_available]) - 0.5, limits[3], limits[2], limits[4],
      col = "#EEEEEE", border = NA)
    text(8, limits[4], "No company actuals", pos = 1, cex = 0.8)
  }
  matlines(z$horizon, values, type = "b", pch = c(16, 17), lty = 1,
    col = c("black", "#0072B2"))
  axis(1, at = 1:9, labels = z$quarter, cex.axis = 0.75)
  legend("topleft", c("Actual", "Selected on 2018-2022"),
    col = c("black", "#0072B2"), lty = 1, pch = c(16, 17), bty = "n", cex = 0.85)
  invisible(result)
}
