# Training-only stationary VAR search: non-interest components

The holdout/test outcome file was not loaded.
Training span: 2009 Q1–2024 Q2 (62 quarters).
Window: 32; horizon: 9; common origins: 20 (2017 Q2–2022 Q1).
Candidate specifications: 176; expected VAR fits: 3520.
Stationary macro pool: d_Rate_3M, d_Rate_10Y, d_Term_Spread, d_BBB_Spread; maximum macro variables per VAR: 2.
The grid includes zero-, one- and two-macro systems; the model is not forced to keep two macros.
NIR and NIE first differences and log differences passed the training-only ADF/KPSS 5% screen.
The rolling scores rank candidate specifications inside the training set; the final holdout remains untouched.
This search covers non-interest income, non-interest expense and their difference. It does not apply a Fed stress path.

The ten highest-scoring feasible W32 specifications were refit descriptively on all 61 complete transformed
training rows. Their VAR roots were stable, Portmanteau p-values ranged from 0.281 to 0.912, multivariate ARCH
p-values from 0.133 to 0.694, and maximum lagged-regressor VIFs from 1.60 to 2.55. Multivariate residual
normality was strongly rejected for all ten (reported p-values round to 0); this weakens conventional
Gaussian inference and prediction intervals, but does not by itself invalidate point-forecast comparisons.
These full-training diagnostics did not change the rolling-forecast ranking.

Outputs include stationarity_diagnostics_training_only.csv, candidate_specifications.csv,
all_model_results.csv, top50_feasible_joint_mase.csv, all_origin_forecasts.csv,
fit_status_by_model_origin.csv, benchmark_results.csv, top10_full_training_diagnostics.csv,
diagnose_top10_full_training.R, run_metadata.csv and var_noninterest_results.rds.
