# Local nine-quarter Domestic PPNR search — 25 October 2026

Completed in native RStudio. All work stays local; no commit or push was made.
Source `view_search_nine_quarters.R` from the repository root to reopen the
saved tables and plots without another search. `run_search_nine_quarters.R`
runs the complete search again. The preceding four-quarter result is preserved
in `../results_local_2026-10-25/`.

## Inputs and model family

The verified inputs are unchanged: 70 quarterly Morgan Stanley observations,
2009 Q1–2026 Q2, and Fed 2025 Historic Domestic with 16 US macro series through
2024 Q4. Only 64 quarters have both company outcomes and Domestic macros.
No International series, scenarios, filled macro values or new vintage are used.

The same transformations give 29 macro candidates, plus optional observed PPNR
and its change. Direct OLS subset models predict PPNR[t+h] from information
at origin t, now for h = 1 through 9. Every training response is observed at
or before t. Changes start in 2009 Q2. Units are USD billions per quarter.

All subsets up to eight total factors are covered. The intercept is excluded.
Change-response equations include a fixed coefficient of one on PPNR[t],
counted toward the eight-factor limit. Their remaining predictors exclude a
free PPNR coefficient, because that duplicates a level-response equation.
This is exhaustive within this fixed OLS family, not all possible model classes.

128,384,640 subset/horizon cases are covered; 18,026,595 are excluded as
singular/numerically dependent in a selection fold (Cholesky threshold 1e-10).
110,358,045 cases are scored. Native search runtime was approximately 18 seconds.
All 8 sizes × 9 horizons × 3 retained ranks give **216 shortlisted combinations**.
Equivalent regressor parameterizations can give identical predictions.

## Selection, checking and forecast paths

Selection retains the previous calendar rule: origins begin in 2017 Q4, with
all selection targets ending by 2022 Q4. Thus selection error counts decrease
from 20 at h=1 to 12 at h=9. Specifically h=9 uses 2020 Q1–2022 Q4 target
outcomes, forecast from origins 2017 Q4–2020 Q3. Models are ranked by expanding-
window forecast RMSE, not in-sample fit. Specification selection never uses
the later check errors. The h=1–4 selection rankings match the preceding run.

The previous 2023–2024 target-only check is too short to assess nine-quarter
forecasts made after the selection cutoff. The new check uses **six common
origins, 2022 Q4–2024 Q1**, for every horizon. Target dates vary with h. At h=9
they are 2025 Q1–2026 Q2. These later company outcomes can be scored without
having macros for those later dates, because the regressors are at the origin.
This check differs from the previous run; do not compare its scores as if the
evaluation windows were unchanged. Pooling all nine horizons over those six
origins gives 54 forecast errors: RMSE **1.483** for the previously selected
horizon-specific models, **1.984** for unchanged PPNR, **1.846** for seasonal
naive and **2.137** for direct AR(1). These errors overlap and are not 54
independent observations. See `nine_path_summary.csv`.

The latest fully observed nine-quarter path starts at **2024 Q1**, with targets
**2024 Q2–2026 Q2**. This path is one of the six rolling-check origins and is
not an additional independent holdout. Its nine horizon-specific, previously
selected models give path RMSE **1.466**, versus **1.666** for unchanged PPNR.

The latest macro origin is **2024 Q4**. Its nine predictions cover
**2025 Q1–2027 Q1**. Only the first six company actuals are available; the last
three remain NA and are shaded in the plot. These are historical-origin
estimates, not a model updated to the present day. No full-path score is
claimed for this partially observed path.

## Ninth-quarter results

| Model | Selection RMSE | Later-check RMSE |
| --- | ---: | ---: |
| Earlier selection winner: eight macro factors | 0.505 | 1.729 |
| One factor: 100 × log House Price Index | 0.926 | 1.202 |
| Two factors: BBB yield + 100 × log CRE Price Index | 0.856 | 1.201 |
| Last observed PPNR benchmark | — | 3.138 |

All RMSE values are in USD billions. The earlier selection winner reduces
ninth-quarter check RMSE by 44.9% relative to unchanged PPNR. Nevertheless,
simpler shortlisted specifications have smaller errors in this later window.
The two-factor specification shown is rank 3 within its earlier-selection size
group, not the earlier overall winner. Its tiny advantage over the one-factor
model is insufficient evidence of a durable advantage.

The eight-variable earlier winner contains real GDP growth, nominal GDP growth,
BBB yield, log Dow Jones total market index, log CRE price index, change in
10-year Treasury yield, the 10-year minus 3-month term spread and its change.
It does not select own PPNR, although own PPNR was allowed.

The later-error ordering in `horizon9_check_review.csv` is exploratory. Selecting
a model using that ordering would reuse the check sample for model selection;
the same scores must not then be called an untouched final test. There are only
six errors per horizon, overlapping forecast windows and previously inspected
company outcomes. The historical macro vintage is revised and the analysis
does not implement real-time release timing or establish causal relationships.

## RStudio objects and plots

- `ppnr_top3_nine_quarters`: all 216 retained combinations, ranked on earlier RMSE.
- `ppnr_horizon9_top3`: the 24 ninth-quarter shortlisted combinations.
- `ppnr_horizon9_check_review`: those 24 ordered by later check RMSE; exploratory.
- `ppnr_nine_quarter_comparison`: nine earlier winners versus the naive benchmark.
- `ppnr_nine_observed_path`: the fully observed 2024 Q1-origin path.
- `ppnr_nine_latest_path`: the partly observed 2024 Q4-origin path.

The last plot shows error by model size for h=9, using the selection winner at
each size. Previous plots show accuracy by horizon and both nine-quarter paths.
Use RStudio's previous-plot arrow to inspect them. Plot files are not exported.

## Verification and regulatory context

The added horizons passed 770 independent small-pool QR ranking checks; the
existing four-horizon checks also passed. All **5,184** retained selection,
rolling-check and path predictions were independently recomputed using
`stats::lm`/`predict.lm` on unscaled regressors. Maximum discrepancy was
1.73e-13 USD billion. Tests checked exact search coverage, factor counts,
quarter labels, missing future actuals, six common origins, preserved previous
selection rankings and seasonal benchmarks based only on observed seasons.
See `validation.rds`, the progress log, CSVs and `ppnr_nine_quarters.rds`.

The Federal Reserve PPNR documentation describes a nine-quarter projection
horizon and supervisory macroeconomic scenarios. It also discusses nine-quarter
out-of-sample RMSE. The present direct-forecast exercise assesses historical
forecasting ability; it is not an implementation of the supervisory stress-test
model, a scenario-conditional stress forecast or a regulatory-compliance finding.

Official reference, accessed 25 October 2026: Federal Reserve,
*Supervisory Stress Test Model Documentation: Pre-Provision Net Revenue Model*,
updated December 2025, printed pages 6–8 and 22:
https://www.federalreserve.gov/supervisionreg/files/pre-provision-net-revenue-models.pdf
