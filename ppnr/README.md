# Morgan Stanley PPNR analysis

Exploratory OLS models using Federal Reserve **2025 historical data** and 60 matched quarters, **2010 Q1–2024 Q4**. PPNR is interest income minus interest expense, plus noninterest income minus noninterest expense; model outputs use USD billions per quarter.

## Run the analysis

Install once, if needed:

```r
install.packages(c("urca", "sandwich"))
```

In RStudio, open `2026-10-17_ppnr.R` in this folder and click **Source**. It loads the included inputs, recreates the existing diagnostics, five plots and trial model, and defines the search functions. **Neither large search starts automatically.** No saved workspace or working-directory change is needed when sourcing the file by its path.

Alternatively, from the repository root:

```r
source("ppnr/2026-10-17_ppnr.R")
```

## Run either search separately

Searches additionally require `Rcpp` and a working C++ compiler. The native search and its RSS memory guard have been tested on macOS; Windows/Linux support has not been validated.

```r
install.packages("Rcpp")  # once, if needed

# Mixed OLS search: own PPNR predictors are optional, up to 8 total factors.
ppnr_search_2025 <- run_ppnr_mixed()

# ARX-only search: mandatory past PPNR plus up to 8 macro factors.
ppnr_arx_search_2025 <- run_ppnr_arx()
```

Choose one call; they are independent. Each retains the existing six-thread, 120-minute budget and progress reporting. For a short check, use `max_factors = 2, threads = 2, minutes = 2` in either call. Inspect results, for example:

```r
ppnr_arx_search_2025$best_by_size
ppnr_arx_search_2025$validation
```

## Included inputs and limits

- `joined_2025/ms_macro_2025.rds`: frozen input used by the existing models; 66 company rows, 60 matched to macro history.
- `ready_for_R/fed_macro_historical_2025.csv`: 196 historical quarters and 28 macro variables. Source: [Federal Reserve 2025 stress-test historical domestic and international tables](https://www.federalreserve.gov/supervisionreg/dfa-stress-tests-2025.htm).
- `restore_inputs.R`: input-only loader, run from this folder with `source("restore_inputs.R")`.

Keep these files and subfolders together. The frozen input is intentionally retained: some detailed components differ from the newer company CSV at repository root, while the five shared financial aggregates agree. Do not silently substitute that CSV or hypothetical scenario data.

Model equations, transformations and selection rules are unchanged. Selection uses 2018–2022 forecast errors; the later 2023–2024 check has already been inspected and is exploratory. Revised history is not a real-time vintage backtest. Forecasts start after the sample ends in 2024 Q4; they are not current forecasts. No final model is endorsed.
