# Morgan Stanley training-only VAR comparison

Run date: 2026-11-16. Training outcomes only: 2009 Q1–2024 Q2 (62 quarters).
Forecast horizon: 9 quarters; no Y-9C holdout/test outcome file was loaded.
Common-origin ranking uses 21 origins from 2017 Q1 to 2022 Q1; each 9-quarter forecast ends by 2024Q2.
Rolling windows: 12, 16, 20, 24, 28, 32. Errors are equally weighted across origin-quarter forecast errors; no recency weights.
VAR lag orders compared: 1–4. Candidate model-window fits require at least 8 residual degrees of freedom.

## Variables and transformations

- Targets: interest income (II) and interest expense (IE), both in billions of U.S. dollars; derived net interest income is II minus IE.
- Primary candidate family: four combinations of first differences and log differences for II and IE. Forecasts are converted back to dollar levels before scoring. Some unit-root tests remain borderline/conflicting, especially for IE log differences, so these transformations are candidates rather than proof of stationarity.
- Level VARs are included as a forecast comparison family because unit-root tests flag both target levels; they are not treated as the preferred differenced-target specification.
- Macro systems compare 3-month and 10-year Treasury yields, BBB corporate yield, term spread, BBB spread, and VIX, individually and in selected two-variable combinations. In differenced-target candidates, yield/spread variables enter as first differences; VIX remains in levels per the user's prior instruction.
- The raw 3-month plus 10-year yield system is included as a collinearity sensitivity; top models report VIF and condition-number diagnostics for lagged regressors. The 3M+10Y pair and the 3M+term-spread pair encode the same information, so identical forecasts are duplicate representations, not independent evidence.
- No seasonal dummies are included; the training-only quarter-indicator checks are saved separately.

## Selection and diagnostics

- Primary ranking: mean joint MASE across II, IE, and NII, on the common origins, with each origin and forecast horizon equally weighted. MASE scales are computed using data available at each origin.
- Secondary metrics include component and NII MAE/RMSE in billions, component MAPE, negative forecast frequency, adjusted R-squared, and model stability/residual diagnostics.
- VARs are compared with random-walk, AR(1), and AR(2) component benchmarks.
- Full-training adjusted R-squared, Granger/block tests, residual serial-correlation, ARCH, normality, roots, VIF and condition number are descriptive checks; they do not replace rolling forecast performance and do not establish structural causality.
- Johansen trace diagnostics are for II and IE levels only. No VECM is selected automatically; the test results and limitations are saved for review.

## Important interpretation limits

- These results select candidate models within the training sample; the 2024Q3–2026Q3 holdout was not loaded or used.
- The common-origin ranking has 21 overlapping rolling origins, so model ranks are uncertain and forecast errors are not independent observations.
- This is an autonomous VAR forecast: macro variables are forecast recursively with the system. It is not yet conditioned on a Fed stress scenario.
- Existing OLS backtest rankings are not directly combined with VAR rankings because their future macro/balance-sheet inputs were realized paths; the information sets differ.
- This first VAR pass covers II and IE only. Noninterest income/expense, full PPNR, uncertainty intervals, and the stress scenario remain later extensions.

## Files

- `all_var_model_window_results.csv`: all estimable model-window comparisons.
- `top50_var_joint_mase_common_origins.csv`: 50 leading candidates across all transformations, including level VAR comparators.
- `top50_var_differenced_feasible_joint_mase_common_origins.csv`: primary shortlist after excluding level-target comparators and models with any negative II/IE forecast on a common origin.
- `top50_var_differenced_feasible_ii_mase_common_origins.csv`, `top50_var_differenced_feasible_ie_mase_common_origins.csv`, `top50_var_differenced_feasible_nii_mase_common_origins.csv`: component-specific feasible shortlists.
- `forecast_feasibility_by_model_window.csv`: negative component-forecast counts for every model-window candidate.
- `top50_var_ii_mase_common_origins.csv`, `top50_var_ie_mase_common_origins.csv`, `top50_var_nii_mase_common_origins.csv`: component-specific rankings.
- `all_var_origin_forecasts.csv`: every 9-quarter rolling forecast for every VAR candidate.
- `macro_incremental_value_common_origins.csv`: paired forecast changes versus target-only VARs.
- `benchmark_results.csv`: random-walk and univariate AR benchmarks.
- `top50_full_training_diagnostics.csv`: explanatory/residual checks for unique models appearing in the differenced-target feasible shortlist.
- Stationarity, seasonality, Johansen, lag-selection, exclusions, fit-status, and metadata files preserve decision evidence.
