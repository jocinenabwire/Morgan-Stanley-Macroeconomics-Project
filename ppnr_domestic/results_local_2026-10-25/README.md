# Local Domestic PPNR search — 25 October 2026

Completed locally in native RStudio. No commit or push was performed.
Open the repository project and source `view_search_four_quarters.R` to reopen
the saved results without running another search. `run_search_four_quarters.R`
starts the complete search again.

## Inputs and scope

- Company input: `Morgan_Stanley_PPNR_Quarterly_2009_2026.csv`, incorporated
  from main in the preceding integration; 70 quarterly rows, 2009 Q1–2026 Q2.
- Macro input: unchanged Fed 2025 Historic Domestic file, 16 US series,
  ending in 2024 Q4. No International series or scenarios are loaded.
- Matched modelling sample: 64 quarters, 2009 Q1–2024 Q4. No missing macro
  quarters are filled or extrapolated.
- PPNR = interest income − interest expense + noninterest income −
  noninterest expense. Original USD thousands are divided by 1,000,000
  to obtain USD billions per quarter.

## Search and ranking

The earlier candidate construction is retained: growth/rate/VIX levels,
100 × log price-index levels, price log changes, yield changes, and two
yield spreads with their changes. This gives 29 Domestic macro candidates.
Observed PPNR and its quarterly change are optional, giving 31 candidates.

Direct OLS forecasts use predictors at origin t to predict PPNR at t+h,
for h = 1, 2, 3, 4. Training responses never extend beyond the origin.
Each training design is centred and scaled within that training window.
Rows start at 2009 Q2 so changes exist; earlier PPNR enters their construction.

Every subset of one through eight candidates is covered for level equations.
Change equations forecast the cumulative h-quarter change and add PPNR[t]
back with coefficient fixed at one. That baseline counts as one factor,
so at most seven other regressors are used. Change equations with a free
PPNR coefficient duplicate a level equation and are represented there.
The intercept is excluded from the factor count.

Specifications are ranked only by expanding-window forecast RMSE in the
2018–2022 selection window, separately for each horizon and factor count.
The three best combinations across the two response forms are retained:
8 factor counts × 4 horizons × 3 ranks = **96 rows** in `top3.csv`.
Equivalent regressor parameterizations, such as substitutions of yield spreads,
may produce the same predictions; these are combinations, not necessarily
96 statistically distinct forecast functions.

Selection origins begin in 2017 Q4, with targets ending by 2022 Q4.
There are 20, 19, 18, 17 selection errors at horizons 1–4. The subsequent
2023–2024 check uses 8, 7, 6, 5 errors. Coefficients expand using past data;
the specification is frozen. Later errors never determine the ranking.

**57,059,840** subset/horizon cases are covered. **2,424,668** are excluded
because the training design is singular or numerically dependent at the
native Cholesky threshold of 1e-10; **54,635,172** are scored.
The tested family is exhaustive; this is not a search over every possible
model class, lag order, transformation, or seasonal specification.

## Four-quarter final check

The latest complete four-quarter path uses one frozen origin, **2024 Q4**,
to forecast **2025 Q1–Q4**. Future macros are unnecessary in these direct
equations. No 2025 company outcomes enter specification selection or any of
the four forecast fits. The 2026 company rows cannot extend this path past horizon four
without additional macro observations or a different forecasting design.

One specification per horizon is selected on the earlier selection RMSE.
The four forecasts therefore form a path from four horizon-specific models.

| Target | Selected factors | Forecast | Actual | Forecast minus actual |
| --- | ---: | ---: | ---: | ---: |
| 2025 Q1 | 8 | 5.266 | 5.685 | -0.419 |
| 2025 Q2 | 8 | 5.902 | 4.839 | 1.063 |
| 2025 Q3 | 7 | 4.790 | 6.063 | -1.273 |
| 2025 Q4 | 8 | 5.976 | 5.788 | 0.188 |

Units: USD billion per quarter. Path RMSE is **0.860**, versus **0.734**
for carrying 2024 Q4 PPNR forward unchanged. The search does not beat that
simple benchmark on this four-quarter path. In the earlier 2023–2024 check,
the selected models beat that benchmark at horizons 1, 2, 4, but not 3.

This is an exploratory historical backtest using revised, quarterly data.
It is not a real-time vintage/release-calendar test, a causal estimate,
or proof of durable forecast superiority. Four final errors are a small
evaluation sample; the 2023–2024 period was also inspected in previous work.
Do not reselect on 2025 errors and then call those errors an untouched test.

## Reading the RStudio tables

- `ppnr_top3_four_quarters`: all 96 shortlisted combinations. `rank` is
  within the same `factors` and `horizon`; smaller RMSE/MAE is better.
  `selection_RMSE` chooses the model; `check_RMSE` measures its later errors.
- `ppnr_winners_four_quarters`: the four selection winners, including
  their variable lists, response form and later check scores.
- `ppnr_2025_forecast_check`: the four frozen-origin forecasts versus actuals.
- `target = change` includes the fixed PPNR baseline even when `PPNR` is
  absent from `variables`; `fixed_PPNR_baseline` makes this explicit.
- `d_` means a quarterly difference, `return_` means 100 × log change,
  and `log_` means 100 × log level. `ARX` uses macro and own PPNR information;
  all coefficients are estimated by OLS, with the stated fixed baseline exception.

## Verification

`tests/test_four_quarters.R` independently ranked 616 small-pool
subset/horizon/response cases using QR, including exact dependencies,
and verified that later observations cannot change selection folds.
`tests/validate_four_quarter_result.R` independently recomputed all 2,496
retained selection/check/final predictions with `stats::lm` and `predict.lm`
on unscaled regressors. Maximum absolute discrepancy: 4.36e-14 USD billion.
It also checked exact coverage, three ranks per size/horizon, factor counts,
frozen winners, final dates and input hashes. See `validation.rds`.

Input hashes, code hashes, full numerical results, progress and round counts
are retained in this local folder and its `ppnr_four_quarters.rds`,
`validation.rds`, and `search_progress.log`. Plot files are not exported.
