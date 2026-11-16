# Training-only window-length comparison

The figures use previously saved rolling-backtest outputs. They do not refit models and do not load the 2024Q3–2026Q3 holdout.

## Regression figure

- Window lengths: 20, 24, 28, and 32 quarters.
- Each point summarizes nine-quarter rolling forecasts on the common training origins for that component: 20 origins for interest income (II) and 21 for interest expense (IE).
- Within each window, candidate specifications are ranked by common-origin MAPE. The plot shows both the single best model and the mean of that window's top 10 models.
- This is a training model-search summary. Selecting the best models separately in each window means it is not an independent test of the window choice.

## Stationary VAR figure

- The saved sensitivity run re-estimated the W32-selected top-50 feasible formula sets at W28 and W24, on the same 20 training origins, with a nine-quarter horizon.
- W20 was not evaluated in that VAR run and is explicitly marked as not run. The figure must not be read as evidence about VAR W20.
- Interest and non-interest panels show the mean Joint MASE of the top 10 eligible candidates and the best eligible candidate. The interest Joint MASE averages II, IE, and NII errors; the non-interest Joint MASE averages NIR, NIE, and net non-interest errors.
- The PPNR panel shows direct four-component PPNR MASE for the selected pair-model composite and the random-walk PPNR benchmark.

## What the saved results say

Lower errors are better.

| Result | 20-quarter | 24-quarter | 28-quarter | 32-quarter |
|---|---:|---:|---:|---:|
| II regression mean MAPE of top 10 (%) | 12.26 | 12.66 | 12.19 | 12.14 |
| IE regression mean MAPE of top 10 (%) | 21.88 | 23.35 | 21.56 | 20.03 |
| II regression best MAPE (%) | 11.97 | 12.36 | 11.50 | 11.06 |
| IE regression best MAPE (%) | 20.83 | 22.04 | 20.55 | 18.19 |
| VAR interest pair mean Joint MASE of top 10 | not run | 10.97 | 10.12 | 9.47 |
| VAR non-interest pair mean Joint MASE of top 10 | not run | 1.29 | 1.18 | 1.15 |
| Direct PPNR composite MASE | not run | 1.58 | 1.48 | 1.49 |
| PPNR random-walk benchmark MASE | not run | 1.00 | 1.00 | 1.00 |

The regression evidence favors W32 on both the best-model and top-10-average MAPE summaries, though II W28 is close on the top-10 mean. In the VAR component-pair comparison, W32 has the lowest top-10 mean Joint MASE among the evaluated windows. At the total PPNR level, however, W28 has slightly lower MASE than W32, and the random-walk benchmark has lower error than every VAR composite. So W32 is supported as a strong component-model training window, but it is not established as the universal PPNR-optimal window.

## Files

- `regression_window_comparison.png` and `.pdf`
- `var_window_comparison.png` and `.pdf`
- `regression_window_summary.csv`
- `var_window_summary.csv`
- `figure_scope.csv`
- Source R script: `plot_window_sensitivity.R` at the project root
