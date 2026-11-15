# Approved Y-9C outcome split

## Windows

| Set | Quarters | Observations |
| --- | --- | ---: |
| Training | 2009Q1–2024Q2 | 62 |
| Holdout test | 2024Q3–2026Q3 | 9 |

The split is chronological. No Y-9C outcome from the test period is included in training. It uses the latest nine available company quarters and preserves the approved training cutoff of 2024Q2.

## Target fields

Both files contain the four quarterly Y-9C components: total interest income, total interest expense, total noninterest income, and total noninterest expense. The script derives net interest income as interest income minus interest expense, then derives PPNR as net interest income plus noninterest income minus noninterest expense. Values are in thousands of U.S. dollars.

Run `source("create_y9c_outcome_split.R")` from the repository root to recreate the files. Outputs are `train_y9c_outcomes_2009Q1_2024Q2.csv` and `test_y9c_outcomes_2024Q3_2026Q3.csv`.

## Model-readiness limit

These are outcome-only splits. They establish the approved chronology and the complete Y-9C target data, but do not yet contain the macro or balance-sheet predictors required for model estimation and forecast evaluation. The branch's 2025 Historic Domestic macro file contains Actual observations only through 2024Q4; the existing `selection_data.csv` and `selection_expense.csv` stop at 2025Q1 and 2024Q4. Their predictor definitions and timing must be reconciled against the complete Y-9C series before they are used for this holdout.

The chosen 2024Q3–2026Q3 holdout is not the same window as the 2025 Fed stress test's nine-quarter outcome period, 2025Q1–2027Q1. The current company data cover seven quarters of that Fed outcome period, through 2026Q3.
