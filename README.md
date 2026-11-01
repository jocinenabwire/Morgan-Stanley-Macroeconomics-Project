# Morgan Stanley: Preliminary PPNR Analysis

## 1. Project Overview

This branch contains the preliminary R analysis for the group project, **Macroeconometric Modeling and Forecasting of PPNR Components**.

The project investigates the relationship between Morgan Stanley's Pre-Provision Net Revenue (PPNR) components, balance-sheet characteristics and macroeconomic conditions, with the broader objective of developing econometric models for explanation and forecasting.

## 2. Purpose of This Branch

The `preliminary-analysis` branch contains the initial empirical analysis conducted separately from the main project workflow.

The analysis focuses on:

- Exploring the four PPNR components.
- Investigating potential macroeconomic and balance-sheet determinants.
- Examining the time-series properties of the variables.
- Estimating preliminary component-level regression models.
- Conducting regression diagnostics.
- Beginning the investigation of long-run relationships between PPNR components and balance-sheet variables.

## 3. File Included

| File | Description |
|---|---|
| `01_preliminary_analysis.R` | R script containing data preparation, descriptive analysis, stationarity tests, correlation analysis, preliminary regressions, diagnostic tests and initial long-run relationship analysis. |

## 4. Data Requirements

The analysis uses the merged quarterly Morgan Stanley dataset covering **2009 Q1–2024 Q4**, comprising 64 quarterly observations.

The dataset combines:

- Morgan Stanley PPNR financial data.
- Morgan Stanley balance-sheet information.
- US macroeconomic variables.

**Note:** The dataset is not included in this branch. The R script requires the corresponding merged CSV dataset to be available locally when executed.

## 5. PPNR Components Examined

The analysis considers four components:

1. Total interest income.
2. Total interest expense.
3. Total noninterest income.
4. Total noninterest expense.

PPNR is calculated as:

\[
PPNR = (Interest\ Income - Interest\ Expense)
\]

\[
+ (Noninterest\ Income - Noninterest\ Expense)
\]

## 6. Preliminary Methodology

The analysis follows several stages:

1. Data preparation and validation.
2. Descriptive statistics and visual exploration.
3. Correlation analysis and explanatory variable selection.
4. Stationarity testing using ADF and KPSS tests.
5. Preliminary structural-break investigation.
6. Component-level OLS regression estimation.
7. Diagnostic testing, including serial correlation, heteroskedasticity and multicollinearity.
8. Robust inference using Newey–West standard errors.
9. Initial investigation of long-run relationships involving total assets and PPNR components.

## 7. Preliminary Findings

The initial regression results indicate that:

- Interest income and interest expense have relatively strong preliminary explanatory models, with the three-month Treasury rate showing a strong positive association with their quarterly changes.
- Noninterest income has moderate explanatory power, although residual serial correlation requires further investigation.
- Noninterest expense has relatively weak explanatory power under the preliminary specification.

The analysis also identifies non-stationarity in the level series and motivates further investigation of cointegration and dynamic relationships.

These findings are preliminary and should not be interpreted as final model results or causal relationships.

## 8. Next Steps

The subsequent analysis will focus on:

- Completing the long-run relationship investigation.
- Further incorporating balance-sheet variables, particularly total assets.
- Developing an appropriate VAR or VECM framework, depending on the integration and cointegration results.
- Evaluating model stability and diagnostic performance.
- Developing and assessing forecasting models.

## 9. Branch Information

**Branch:** `preliminary-analysis`

**Main project repository:** [Morgan-Stanley-Macroeconomics-Project](https://github.com/jocinenabwire/Morgan-Stanley-Macroeconomics-Project)

This branch is maintained separately from `main` to allow the preliminary analysis to be reviewed independently before any integration into the main project workflow.
