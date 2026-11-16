# Stationary training-only VAR search

Training outcomes: 2009 Q1–2024 Q2 (62 quarters).
Window: 32 observations; forecast horizon: 9 quarters.
Common origins: 20 (2017 Q2–2022 Q1).
The holdout/test outcome file was not loaded.

Candidate specifications: 176; expected model-origin fits: 3520.
Macro-variable cap: 2 per VAR; VAR lags: 1–4.
Stationary macro variables retained: d_Rate_3M, d_Rate_10Y, d_Term_Spread, d_BBB_Spread.
II transformations retained: II_D, II_G.
IE transformations retained: IE_D2, IE_G2.

The top table ranks complete, finite, nonnegative II/IE forecasts by the equal-weight mean MASE for II, IE and derived NII.
Forecast origins overlap, so rolling errors are dependent and the best score is a training-only selection result.
The current search does not apply a Fed stress scenario and does not model noninterest income/expense.

Files: candidate_specifications.csv, stationarity_diagnostics_training_only.csv, all_model_results.csv,
top50_feasible_joint_mase.csv, all_origin_forecasts.csv, fit_status_by_model_origin.csv,
benchmark_results.csv, benchmark_origin_forecasts.csv, macro_incremental_value.csv, run_metadata.csv,
var_stationary_results.rds, progress.log.
