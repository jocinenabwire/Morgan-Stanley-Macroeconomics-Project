# Morgan Stanley FR Y-9C data: coverage, rebuild, and refresh

## Current coverage

The maintained company data now cover **2009Q1–2026Q3**. The selected source snapshot, `y9c_source/Morgan_Stanley_Y9C_2009Q1_2026Q3_source.csv`, contains 71 Morgan Stanley observations (RSSD ID 2162966), 60 selected Y-9C codes, and provenance fields for every quarter: report date, institution, RSSD ID, retrieval date, and source URL.

The 64 observations from 2009Q1–2024Q4 come from the official FFIEC quarterly holding-company bulk archives. The seven observations from 2025Q1–2026Q3 come from Morgan Stanley's institution-specific FR Y-9C reports. The bulk ZIPs include other institutions, so the build script retains only the Morgan Stanley row and selected codes; the large all-institution ZIPs are kept locally and are not part of the repository. The source snapshot does not use earnings releases as Y-9C data. Values are reported in thousands of U.S. dollars.

## Rebuild from the official source data

Open this repository's `.Rproj` file in RStudio. To recreate the selected source snapshot, run:

```r
source("build_y9c_source_snapshot.R")
```

The script uses base R, downloads any missing FFIEC bulk ZIPs for 2009Q1–2024Q4, and checks each archive for exactly one Morgan Stanley RSSD 2162966 row with the expected institution name and reporting date. It combines those historical rows with the saved institution-specific 2025Q1–2026Q3 source snapshot. The script records the official source URL for every quarter. The FFIEC portal describes its quarterly holding-company archive format and download process: [Financial Data Download](https://www.ffiec.gov/npw/FinancialReport/FinancialDataDownload).

To recreate the three maintained data files from the selected source snapshot, run:

```r
source("rebuild_y9c_outputs.R")
```

This script checks the exact quarter sequence, reporting institution, RSSD ID, numeric source fields, the reported net-interest-income identity, and each component subtotal when all of its source codes are present. It produces:

- `Morgan_Stanley_PPNR_Quarterly_2009_2026.csv`: 71 rows, 2009Q1–2026Q3.
- `Morgan_Stanley_PPNR_Quarterly_Final.csv`: 67 rows, 2010Q1–2026Q3, preserving the existing file's start date.
- `Morgan_Stanley_Balance_Sheet.csv`: 71 quarter-end rows, 2009Q1–2026Q3.

FR Y-9C income-statement values are year-to-date in the regulatory report. The rebuild converts them to standalone quarterly flows by subtracting the previous quarter within the same calendar year. Balance-sheet values are quarter-end stocks and are not differenced.

## Known source-form gaps and corrected values

The deposit-expense codes `BHCKHK03` and `BHCKHK04` are absent from the downloaded reports through 2016Q4. Their source values remain `NA` through that period; they are not treated as zero. The held-to-maturity securities code `BHCKD961` is not available before 2015Q1. The Federal Reserve data dictionary identifies it as total held-to-maturity securities: [BHCKD961 definition and reporting history](https://www.federalreserve.gov/apps/mdrm/data-dictionary/search/series?date_end=20190808&date_start=20190808&rep_period=Before&rep_state=Opened&rep_status=All&show_conf=False&show_short_title=False&sid=259).

The previous balance-sheet CSV had 16 blank `BHCKD961` values from 2015Q1 through 2018Q4 even though the values are present in the official source. `rebuild_y9c_outputs.R` restores those values. No other PPNR or balance-sheet numeric values changed in the rebuild; the two PPNR files are numerically identical to their prior versions.

For institution identity and reporting details, see the [FFIEC Financial Reports Help](https://www.ffiec.gov/npw/Help/FinancialReports) and [Morgan Stanley's FFIEC NIC profile](https://www.ffiec.gov/npw/Institution/Profile/2162966). The older `Y9C_Data.R` remains in the repository; the two scripts documented above provide the reproducible path for the complete selected snapshot and maintained outputs.
