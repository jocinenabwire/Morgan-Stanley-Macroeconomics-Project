# Training-only VAR window sensitivity and PPNR component composite

W32 stationary searches for interest and non-interest pairs selected Top-50 feasible formula sets.
Those same formula sets were re-estimated at W28 and W24 using the same 20 rolling origins.
This is a focused window sensitivity of W32-selected formulas, not a full model-grid search at W28/W24.
The 2024 Q3–2026 Q3 Y-9C holdout was not loaded.

Common origins: 20 (2017 Q2–2022 Q1), forecast horizon: 9.
Top-50 re-estimation fits: 3960; failed: 0; warnings: 0.

PPNR composite selects the best fully stable feasible shortlisted model for each component pair and window,
then adds II - IE + non-interest income - non-interest expense. The two pairwise VARs forecast their macro
variables separately, so this is a component-wise autonomous forecast portfolio, not a single shared macro path.
A common Fed stress path is a later exercise.

Outputs: top50_reestimated_model_results.csv, top50_reestimated_origin_forecasts.csv,
top50_window_comparison_results.csv, window_summary_top50.csv, matched_specification_window_ranks.csv,
matched_specification_window_summary.csv, selected_pair_models_by_window.csv,
ppnr_component_composite_backtest_forecasts.csv, ppnr_performance_by_window.csv,
excluded_short_window_models.csv, top50_reestimated_fit_status.csv, run_metadata.csv.
