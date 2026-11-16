# Plot the existing training-only rolling window comparisons.
# This script reads saved result tables only; it does not refit models or load the holdout.

required_packages <- c("readr", "dplyr", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing R package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

window_grid <- c(20L, 24L, 28L, 32L)
regression_dir <- file.path("results", "rolling_backtest")
var_dir <- file.path("results", "var_window_sensitivity_training_20261116")
output_dir <- file.path("results", "window_sensitivity_figures_20261116")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

model_result_paths <- function(path) {
  if (file.exists(path)) return(path)
  window_parts <- Sys.glob(sub(
    "_all_model_results\\.csv$", "_all_model_results_window_*.csv.gz", path
  ))
  if (length(window_parts)) return(window_parts)
  compressed_path <- paste0(path, ".gz")
  if (file.exists(compressed_path)) return(compressed_path)
  character()
}
read_model_results <- function(path) {
  paths <- model_result_paths(path)
  if (!length(paths)) stop("No saved model result file found for ", path, call. = FALSE)
  dplyr::bind_rows(lapply(paths, function(file) {
    readr::read_csv(file, show_col_types = FALSE)
  }))
}

regression_files <- c(
  "Interest income (II)" = file.path(regression_dir, "interest_income_all_model_results.csv"),
  "Interest expense (IE)" = file.path(regression_dir, "interest_expense_all_model_results.csv")
)
missing_files <- c(
  regression_files[!vapply(regression_files, function(path) {
    length(model_result_paths(path)) > 0L
  }, logical(1))],
  file.path(var_dir, c("window_summary_top50.csv", "ppnr_performance_by_window.csv"))[
    !file.exists(file.path(var_dir, c("window_summary_top50.csv", "ppnr_performance_by_window.csv")))
  ]
)
if (length(missing_files)) {
  stop("Missing saved result file(s): ", paste(missing_files, collapse = ", "), call. = FALSE)
}

# Regression: compare each window's ten lowest common-origin MAPE results.
# Every candidate is evaluated on the same origins within its component.
regression_rows <- list()
for (outcome in names(regression_files)) {
  d <- read_model_results(regression_files[[outcome]])
  needed <- c("Window", "Variables", "Average_MAPE_Common", "Failed_Origins_Common")
  if (!all(needed %in% names(d))) {
    stop("Unexpected regression result columns in ", regression_files[[outcome]], call. = FALSE)
  }
  for (w in window_grid) {
    z <- d[d$Window == w & is.finite(d$Average_MAPE_Common) &
             d$Failed_Origins_Common == 0L, , drop = FALSE]
    z <- z[order(z$Average_MAPE_Common), , drop = FALSE]
    top <- head(z, 10L)
    if (nrow(top) < 10L) {
      stop("Fewer than ten complete candidates for ", outcome, " at window ", w, call. = FALSE)
    }
    origins <- if (grepl("income", regression_files[[outcome]], ignore.case = TRUE) &&
                    !grepl("expense", regression_files[[outcome]], ignore.case = TRUE)) 20L else 21L
    regression_rows[[length(regression_rows) + 1L]] <- data.frame(
      Outcome = outcome, Window = w, Eligible_Models = nrow(z),
      Common_Origins = origins, Top10_Mean_MAPE_pct = mean(top$Average_MAPE_Common),
      Top10_Median_MAPE_pct = median(top$Average_MAPE_Common),
      Best_MAPE_pct = top$Average_MAPE_Common[1L],
      Tenth_Best_MAPE_pct = top$Average_MAPE_Common[10L],
      Best_Model = top$Variables[1L], stringsAsFactors = FALSE
    )
  }
}
regression_summary <- dplyr::bind_rows(regression_rows)
readr::write_csv(regression_summary, file.path(output_dir, "regression_window_summary.csv"))

regression_long <- dplyr::bind_rows(
  regression_summary |>
    dplyr::transmute(Outcome, Window, Series = "Mean of top 10 models", Value = Top10_Mean_MAPE_pct),
  regression_summary |>
    dplyr::transmute(Outcome, Window, Series = "Best single model", Value = Best_MAPE_pct)
)
regression_plot <- ggplot2::ggplot(
  regression_long,
  ggplot2::aes(x = Window, y = Value, color = Series, group = Series)
) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 2.8) +
  ggplot2::facet_wrap(~Outcome, scales = "free_y", ncol = 1L) +
  ggplot2::scale_x_continuous(breaks = window_grid, limits = c(19L, 33L)) +
  ggplot2::scale_color_manual(values = c(
    "Mean of top 10 models" = "#2C7FB8", "Best single model" = "#D95F02"
  )) +
  ggplot2::labs(
    title = "Regression performance by rolling training-window length",
    subtitle = "Training-only, nine-quarter rolling forecasts; lower MAPE is better",
    x = "Training window (quarters)", y = "Average MAPE (%)",
    color = NULL,
    caption = paste(
      c(
        "Top 10 and best model are selected separately within each window using common-origin MAPE.",
        "II uses 20 common origins; IE uses 21. Holdout outcomes were not loaded.",
        "Training validation only; this is not an independent test-set result."
      ),
      collapse = "\n"
    )
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "top", plot.title = ggplot2::element_text(face = "bold"),
    plot.caption = ggplot2::element_text(size = 9, hjust = 0),
    panel.grid.minor = ggplot2::element_blank(), strip.text = ggplot2::element_text(face = "bold")
  )

var_window_summary <- readr::read_csv(
  file.path(var_dir, "window_summary_top50.csv"), show_col_types = FALSE
)
var_ppnr_summary <- readr::read_csv(
  file.path(var_dir, "ppnr_performance_by_window.csv"), show_col_types = FALSE
)
var_panels <- c(
  "Interest pair (II, IE, NII)" = "Interest",
  "Non-interest pair (NIR, NIE, net non-interest)" = "Noninterest"
)
var_rows <- list()
for (panel in names(var_panels)) {
  pair <- var_panels[[panel]]
  d <- var_window_summary[var_window_summary$Pair == pair, , drop = FALSE]
  if (!nrow(d)) stop("Missing VAR sensitivity rows for ", pair, call. = FALSE)
  var_rows[[length(var_rows) + 1L]] <- data.frame(
    Panel = panel, Window = d$Window, Series = "Mean of top 10 models",
    Value = d$Mean_Top10_Joint_MASE, stringsAsFactors = FALSE
  )
  var_rows[[length(var_rows) + 1L]] <- data.frame(
    Panel = panel, Window = d$Window, Series = "Best feasible model",
    Value = d$Best_Joint_MASE, stringsAsFactors = FALSE
  )
}
ppnr <- var_ppnr_summary[var_ppnr_summary$Window %in% c(24L, 28L, 32L), , drop = FALSE]
ppnr$Series <- ifelse(
  ppnr$Forecast_Type == "Selected component-pair VAR composite",
  "Selected VAR composite", "Random-walk benchmark"
)
var_rows[[length(var_rows) + 1L]] <- data.frame(
  Panel = "Total PPNR composite", Window = ppnr$Window,
  Series = ppnr$Series, Value = ppnr$MASE, stringsAsFactors = FALSE
)
var_long <- dplyr::bind_rows(var_rows)
readr::write_csv(var_long, file.path(output_dir, "var_window_summary.csv"))

missing_w20_labels <- var_long |>
  dplyr::group_by(Panel) |>
  dplyr::summarise(Value = max(Value, na.rm = TRUE) * 1.08, .groups = "drop") |>
  dplyr::mutate(Window = 20L, Label = "W20 not run")

var_plot <- ggplot2::ggplot(
  var_long,
  ggplot2::aes(x = Window, y = Value, color = Series, group = Series)
) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 2.8) +
  ggplot2::geom_text(
    data = missing_w20_labels,
    ggplot2::aes(x = Window, y = Value, label = Label),
    inherit.aes = FALSE, color = "#777777", size = 3.2, hjust = 0.5
  ) +
  ggplot2::facet_wrap(~Panel, scales = "free_y", ncol = 1L) +
  ggplot2::scale_x_continuous(breaks = window_grid, limits = c(19L, 33L)) +
  ggplot2::scale_color_manual(values = c(
    "Mean of top 10 models" = "#2C7FB8", "Best feasible model" = "#D95F02",
    "Selected VAR composite" = "#9B59B6", "Random-walk benchmark" = "#333333"
  )) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.08, 0.24))) +
  ggplot2::labs(
    title = "Stationary VAR performance by rolling training-window length",
    subtitle = "Top-50 W32 shortlist re-estimated on the same 20 training origins; nine-quarter horizon",
    x = "Training window (quarters)", y = "Panel-specific error measure",
    color = NULL,
    caption = paste(
      c(
        "Pair panels show Joint MASE (mean component MASE); lower is better.",
        "PPNR panel shows direct composite PPNR MASE against a random-walk benchmark.",
        "VAR W20 was not re-estimated. Holdout outcomes were not loaded."
      ),
      collapse = "\n"
    )
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "top", plot.title = ggplot2::element_text(face = "bold"),
    plot.caption = ggplot2::element_text(size = 9, hjust = 0),
    panel.grid.minor = ggplot2::element_blank(), strip.text = ggplot2::element_text(face = "bold")
  )

readr::write_csv(
  data.frame(
    Method = c("Regression", "Stationary VAR", "Stationary VAR PPNR composite"),
    Windows_Available = c("20, 24, 28, 32", "24, 28, 32", "24, 28, 32"),
    Common_Origins = c("II: 20; IE: 21", "20", "20"),
    Forecast_Horizon_Quarters = 9L,
    Holdout_Used = FALSE,
    stringsAsFactors = FALSE
  ),
  file.path(output_dir, "figure_scope.csv")
)

ggplot2::ggsave(
  file.path(output_dir, "regression_window_comparison.png"),
  regression_plot, width = 10.5, height = 8.2, dpi = 300, bg = "white"
)
ggplot2::ggsave(
  file.path(output_dir, "var_window_comparison.png"),
  var_plot, width = 10.5, height = 10.0, dpi = 300, bg = "white"
)
ggplot2::ggsave(
  file.path(output_dir, "regression_window_comparison.pdf"),
  regression_plot, width = 10.5, height = 8.2, bg = "white"
)
ggplot2::ggsave(
  file.path(output_dir, "var_window_comparison.pdf"),
  var_plot, width = 10.5, height = 10.0, bg = "white"
)

cat("Created training-only window comparison figures in:", output_dir, "\n")
cat("Regression origins: II = 20; IE = 21. VAR origins = 20.\n")
cat("VAR 20-quarter sensitivity is not present in the saved results; marked as not run.\n")
print(regression_summary)
print(var_window_summary[, c("Pair", "Window", "Eligible_Top50_Models",
                             "Best_Joint_MASE", "Mean_Top10_Joint_MASE")])
print(var_ppnr_summary[, c("Forecast_Type", "Window", "MASE", "MAE_Billions", "RMSE_Billions")])
print(regression_plot)
print(var_plot)
