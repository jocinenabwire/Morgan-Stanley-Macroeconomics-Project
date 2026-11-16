# Training-only forecast accuracy figures

Run `Rscript plot_training_forecast_results.R` from the repository root. The script
reuses saved VAR results and replays only the selected W32 interest-income and
interest-expense regressions. It does not read the holdout file. Raw regression
inputs are truncated at 2024 Q2, and the forecast replay reproduces the saved
common-origin regression MAPE values before writing any figures.

## Metric definitions

- **MAPE** is the mean of `abs(actual - forecast) / abs(actual) * 100` over all
  origin-quarter forecast rows. PPNR actuals are nonzero and positive in this
  comparison; the smallest is $1.869bn.
- **Within 25%** is the share of individual origin-quarter forecasts whose
  absolute percentage error is at most 25%. The script also reports the share
  of origins where all nine horizon forecasts are within 25%.
- **MASE** is a scaled absolute-error measure, not a percentage. The script
  independently reconciles saved PPNR MASE, MAE, and RMSE before reporting MAPE.

## PPNR results

The three VAR component composites produce PPNR MAPE of 47.20% (W24), 44.76%
(W28), and 44.36% (W32), against 28.69% for the random-walk PPNR benchmark.
None meets the 25% mean-MAPE target. None of the 20 nine-quarter origins has all
nine errors within 25%. At W32, the mean PPNR MAPE exceeds 25% at every horizon.

The VAR forecasts use the four Y-9C components and reconstruct PPNR by its
accounting identity. This is a training-only rolling-origin comparison, not a
Fed stress-scenario forecast or an untouched final test.

## Existing regression results

The regression figures cover only the already searched interest-income and
interest-expense equations. Non-interest regression searches are not present in
these results; the four-component VAR figures cover those non-interest outcomes.
At W32, the lowest-MAPE II regression has MAPE 11.06% and maximum VIF 565.57;
the lowest-MAPE candidate with VIF <= 5 has MAPE 12.19% and VIF 4.59. For IE,
the corresponding results are 18.19% with VIF 13.45, and 23.54% with VIF 3.79.
MAPE is averaged across the nine horizons, so some individual forecasts can
still exceed 25%; the horizon chart and accuracy CSVs show those cases.

These regressions use realized predictor values for future quarters inside the
training sample. Their scores are conditional on those historical predictor
paths and do not establish performance when future macro and balance-sheet
predictors must themselves be forecast.

## Files

- `ppnr_mape_by_horizon.png`: PPNR MAPE by horizon, W24/W28/W32, and random walk.
- `ppnr_latest_training_origin.png`: actual and forecast PPNR for the final
  internal origin (2022 Q1), through 2024 Q2.
- `var_w32_component_mape_by_horizon.png`: MAPE by horizon for all four W32 VAR
  components.
- `regression_w32_mape_by_horizon.png`: MAPE by horizon for the best MAPE and
  best VIF <= 5 W32 regression candidates.
- `regression_w32_latest_training_origin.png`: actual and forecast II/IE for the
  final common training origin.
- `forecast_accuracy_summary.csv` and `forecast_accuracy_by_horizon.csv`:
  aggregate accuracy, 25% hit rates, and horizon-level MAPE.
- `training_forecast_rows.csv`: the plotted training-only forecast rows.
- `selected_regression_specs_w32.csv` and `regression_selected_forecasts_w32.csv`:
  exact selected formulas and replayed forecasts.
