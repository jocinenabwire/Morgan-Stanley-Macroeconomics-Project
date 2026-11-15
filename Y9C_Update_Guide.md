# Recent Morgan Stanley FR Y-9C refresh

## Scope

The 2025Q1–2026Q3 rows in the three maintained PPNR and balance-sheet CSVs are sourced from Morgan Stanley's institution-specific quarterly FR Y-9C reports, identified by RSSD ID 2162966. This refresh does not use earnings releases as Y-9C data. The comparison with company earnings releases remains a separate analysis.

The source snapshot in `y9c_source/Morgan_Stanley_Y9C_2025Q1_2026Q3_source.csv` records the report date, institution, RSSD ID, retrieval date, official FFIEC source URL, and the selected Y-9C line-item codes. Values are reported in thousands of U.S. dollars.

## Reproduce the recent refresh

Open this repository's `.Rproj` file in RStudio, then run:

```r
source("update_y9c_recent.R")
```

The script uses base R and performs no package installation. It checks the institution and source URLs, expected quarter coverage, required numeric fields, YTD-to-quarter conversion, component subtotals, the net-interest-income identity, and equality of all overlapping 2025Q1–2026Q2 values against the existing files. It writes the three maintained CSVs only after all checks pass.

FR Y-9C income-statement values are year-to-date in the regulatory report. The script converts them to standalone quarterly flows by differencing adjacent quarters within the same calendar year. Balance-sheet values are quarter-end stocks and are not differenced.

## Limits and provenance

This source snapshot makes the recent seven quarters and their update reproducible. It does not bundle the full historical institution-level FR Y-9C archive used to construct pre-2025 values. Those earlier observations remain the existing project baseline and should not be described as re-created by this recent update. The historical raw-data workflow in `Y9C_Data.R` remains unchanged.

For download timing, definitions, and the institution identifier, see the [FFIEC Financial Reports Help](https://www.ffiec.gov/npw/Help/FinancialReports) and [Morgan Stanley's FFIEC NIC profile](https://www.ffiec.gov/npw/Institution/Profile/2162966).
