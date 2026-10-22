# Morgan Stanley PPNR: Domestic-only workflow

This is the current workflow: **Fed 2025 historical Domestic data only**. It uses 16 US macro series, 196 historical quarters (1976 Q1–2024 Q4), and 60 matched company quarters (2010 Q1–2024 Q4). International data and hypothetical scenarios are not loaded.

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

With `Rcpp` and a working C++ compiler installed, run either independently:

```r
ppnr_search_2025 <- run_ppnr_mixed()
ppnr_arx_search_2025 <- run_ppnr_arx()
```

The pool now contains 29 US macro candidates: 16 published variables (three price indices represented in logs), three log changes, six yield/rate changes, and four spread terms. The mixed search adds optional PPNR and its change (31 candidates); ARX always includes past PPNR plus up to eight macro factors. All other transformations, OLS fitting, horizons and selection rules are retained. The EUR benchmark is excluded. Existing international-model rankings are not Domestic-only results.

For a short check, add `max_factors = 2, threads = 2, minutes = 2` to either call. The default remains six threads and 120 minutes per search; progress is printed. Native compilation and the RSS memory guard are validated on macOS only.

## Sources and interpretation

The unmodified [Historic Domestic CSV](https://www.federalreserve.gov/supervisionreg/files/2025-Table_1A_Historic_Domestic.csv) is included in `data/`. The [Fed definitions](https://www.federalreserve.gov/supervisionreg/files/domestic-data-definitions-2025.pdf) explain units and quarterly timing. `domestic_variables.csv` maps the 16 original headings to English R names; `source_manifest.csv` records the source URLs and file hashes.

Company input is the existing `Morgan_Stanley_PPNR_Quarterly_Final.csv` at repository root. Its financial aggregates agree with the earlier model input. The six company quarters after 2024 Q4 retain missing macro values and are excluded, without filling or extrapolating them. PPNR outputs use USD billions per quarter.

Selection uses 2018–2022 errors; the already-inspected 2023–2024 check remains exploratory (8/7/6 observations for horizons 1/2/3). Revised historical data are not a real-time backtest. Forecasts start after 2024 Q4 and are not current forecasts. Large searches have not been rerun for this Domestic-only scope. The previous combined-input workflow remains in `ppnr/` for reproducibility.
