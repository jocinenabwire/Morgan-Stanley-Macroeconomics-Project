# Rolling backtest output

## Search performed

- Targets: interest income (`II`) and interest expense (`IE`).
- Training data end: 2024 Q2. The final holdout beginning 2024 Q3 was excluded from fitting, model ranking, and rolling evaluation.
- Forecast horizon: 9 quarters, forecast recursively at each origin.
- Candidate sizes: 4 to 6 explanatory variables. An autoregressive lag counts toward this limit; the regression intercept does not.
- Windows: 32, 28, 24, 20, 16, and 12 quarters. The same candidate variable sets are re-estimated at every eligible quarterly origin and for each window length.
- Candidate specifications: 14,939 for interest income and 73,772 for interest expense.
- Model-origin regression fits: 2,689,020 for income and 13,721,592 for expense; 16,410,612 total. Each fitted regression produces a recursive 9-quarter forecast.
- The smaller-window results use all eligible origins. `Common` metrics compare windows on the same origins; `All` metrics also include origins available only to shorter windows. Errors receive equal weight.

## Interpretation and caveats

The validation quarters and their realized predictor values fall within the training period. These scores therefore evaluate each model conditionally on the realized macro and balance-sheet paths; they are not real-time forecasts of those predictors. No final holdout outcome was used, and this run does not yet apply the Fed stress scenario.

VIF above 5 is recorded as a review flag, but does not automatically exclude a candidate from the search. Many high-ranking candidates also have VIF flags, and some maximum VIF values are very large. Use forecast ranking together with the VIF flag and economic interpretation before choosing a final model.

The completed run recorded zero failed and zero warned rolling origins in both components.

## Files

For each component, the committed all-model CSV result contains every model-by-window result, including the formula, MAPE and MAE rankings, common-origin and all-origin errors, horizon-specific common-origin errors, failed/warned-origin counts, and maximum VIF summary. The interest-expense table is stored as six compressed CSV files, one per window, to keep every GitHub file below its size limit. The plotting scripts transparently read these window parts; when a locally generated unsplit CSV is present, they use it instead.

`*_top50_mape_all_windows.csv` and `*_top50_mae_all_windows.csv` give the 50 best model-window pairs across the grid. The corresponding `*_by_window.csv` files give the 50 best per window (300 rows per metric).

`*_window_<n>_checkpoint.rds` preserves each completed window separately. Income outputs also include the rate-change comparison tables. Expense outputs include the quarterly 3-month/10-year rate correlations and pairwise VIF diagnostics.

The `*_run_metadata.csv` files record the data span, horizon, windows, candidate count, fit count, worker count, and holdout status. RDS files and per-window checkpoints are local run caches; the committed CSV tables are the portable results.
