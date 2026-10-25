# Morgan Stanley PPNR: Domestic-only workflow

This is the current workflow: **Fed 2025 historical Domestic data only**. It uses 16 US macro series, 196 historical quarters (1976 Q1–2024 Q4), and 64 matched company quarters (2009 Q1–2024 Q4). International data and hypothetical scenarios are not loaded.

## Start from GitHub

Clone the working branch once:

```sh
git clone --branch ppnr-portable-analysis-2026-10-22 https://github.com/jocinenabwire/Morgan-Stanley-Macroeconomics-Project.git
```

Open `Morgan_Stanley_Macroeconomics.Rproj` in RStudio. Install once, if needed:

```r
install.packages(c("urca", "sandwich"))
```

Open **`run_ppnr.R`** and click **Source**, or run:

```r
source("run_ppnr.R")
```

The script reads version-controlled files from your local Git clone, checks their hashes, joins the company and Domestic macro data by quarter, and recreates the existing diagnostics, five plots and three-macro trial model. It does not rely on an old workspace or files elsewhere on your computer. It does not start either large search.

Before later work, run `git pull` from the repository terminal. This updates the local files; the analysis does not download changing data silently during each run.

## Optional searches

### Completed four- and nine-quarter searches

The 25 October 2026 searches used the 2009 company history and Domestic inputs
described above. The four-quarter search retained 96 combinations; the
nine-quarter search retained 216. Each keeps the three lowest selection-RMSE
combinations per factor count and horizon, up to eight total factors, with
past PPNR optional. The fixed PPNR baseline in change equations counts toward
that limit. Saved rankings and later error checks were independently verified.

To inspect these completed results without rerunning them, source either file
from the repository root in RStudio:

```r
source("view_search_four_quarters.R")
source("view_search_nine_quarters.R")
```

To deliberately rerun a complete search, use `run_search_four_quarters.R` or
`run_search_nine_quarters.R`. These require `Rcpp` and a working C++ compiler.
The shared implementation is in `search_four_quarters.R` and
`subset_search.cpp`; `search_nine_quarters.R` adds the nine-quarter entry point
and plots. The existing `run_ppnr.R` still runs ordinary diagnostics and its
original trial model without starting either search.

Detailed periods, results and limits are recorded with the saved
[four-quarter results](results_local_2026-10-25/README.md) and
[nine-quarter results](results_nine_quarters_local_2026-10-25/README.md).
The nine-quarter check uses six common origins, 2022 Q4–2024 Q1; it differs
from the earlier check window. Ordering models by those later errors is
exploratory, not a new untouched test. Temporary checkpoints remain local.

### Earlier three-quarter search functions

With `Rcpp` and a working C++ compiler installed, run either independently:

```r
ppnr_search_2025 <- run_ppnr_mixed()
ppnr_arx_search_2025 <- run_ppnr_arx()
```

The pool now contains 29 US macro candidates: 16 published variables (three price indices represented in logs), three log changes, six yield/rate changes, and four spread terms. The mixed search adds optional PPNR and its change (31 candidates); ARX always includes past PPNR plus up to eight macro factors. All other transformations, OLS fitting, horizons and selection rules are retained. The EUR benchmark is excluded. Existing international-model rankings are not Domestic-only results.

For a short check, add `max_factors = 2, threads = 2, minutes = 2` to either call. The default remains six threads and 120 minutes per search; progress is printed. Native compilation and the RSS memory guard are validated on macOS only.

## Sources and interpretation

The unmodified [Historic Domestic CSV](https://www.federalreserve.gov/supervisionreg/files/2025-Table_1A_Historic_Domestic.csv) is included in `data/`. The [Fed definitions](https://www.federalreserve.gov/supervisionreg/files/domestic-data-definitions-2025.pdf) explain units and quarterly timing. `domestic_variables.csv` maps the 16 original headings to English R names; `source_manifest.csv` records the source URLs and file hashes.

Company input is `Morgan_Stanley_PPNR_Quarterly_2009_2026.csv` at repository root, brought in from `main` commit `829e1c18dac2d60aba27f427e84a0ede2e71c527`. It contains 70 company quarters (2009 Q1–2026 Q2). All values in the overlapping 2010–2026 period agree with the previous company CSV. The six company quarters after 2024 Q4 retain missing macro values and are excluded, without filling or extrapolating them. PPNR outputs use USD billions per quarter.

The additional 2009 quarters extend the training history. Evaluation periods remain fixed by calendar date: selection/development in 2018–2022 and the later historical check in 2023–2024. Seasonal summaries now contain 16 observations for each quarter. The older CSV and the `ppnr/` workflow remain available for reproducibility.

To verify the input integration and ordinary analysis without running either search, run from the repository root:

```sh
Rscript --vanilla ppnr_domestic/tests/test_2009_input.R
```

The earlier three-quarter functions use 2018–2022 selection errors and an already-inspected 2023–2024 exploratory check (8/7/6 observations for horizons 1/2/3). Revised historical data are not a real-time backtest. Forecasts from the 2024 Q4 origin are historical estimates. The completed four- and nine-quarter runs are documented above; their later-check windows and available actuals are stated separately. The previous combined-input workflow remains in `ppnr/` for reproducibility.
