#### Research Plan
### Macroeconometric Modeling and Forecasting of PPNR Components
# Attila Benjámin Vörös, László Bendegúz Krihó, Marco Romanelli, Ogoya Jocine Fanice Nabwire
## Objective
This project develops and validates econometric models to explain and forecast selected PPNR (Pre-Provision Net Revenue) components, such as interest income and interest expense, for Morgan Stanley using macroeconomic variables as predictors. The analysis will initially focus on Morgan Stanley, with a potential extension to a panel of large U.S. bank holding companies to examine whether these relationships differ across institutions.

The empirical analysis will begin with descriptive analysis and a single-entity time-series specification for Morgan Stanley. Depending on the properties of the data and initial results, the analysis may subsequently be extended using VAR/VECM and/or panel-data methods.
Research Question

To what extent can macroeconomic conditions such as interest rates, the yield-curve slope, market volatility and GDP growth explain and predict selected PPNR components at Morgan Stanley, and how do these relationships compare with those of other large U.S. banks and commercial banks?

## Hypotheses
H1: Macroeconomic sensitivity
H1: Selected PPNR components at Morgan Stanley are systematically related to macroeconomic and financial conditions.

H2: Different channels
H2: Different PPNR components respond differently to macroeconomic and financial conditions, reflecting the distinct economic channels underlying interest income, trading activity, investment-banking activity.
For example:
•	interest-related components → interest rates/yield curve
•	trading revenue → volatility/financial-market conditions
•	investment-banking fees → broader economic/market conditions

H3: Bank heterogeneity / possible extension
H3: The sensitivity of PPNR components to macroeconomic and financial conditions differs between Morgan Stanley, other large U.S banks and commercial banks, reflecting differences in their business models and balance-sheet structures.

### Data
## Item	Description
Dependent variables	Selected quarterly PPNR components of Morgan Stanley and other US banks and commercial banks. Candidate variables include net interest income / interest income, interest expense, trading revenue, and investment-banking fees. The final selection will be based on data availability and economic relevance. Where appropriate, PPNR components will be scaled by an appropriate balance-sheet measure (e.g. average total assets) to improve comparability over time and across institutions.

Bank data	Quarterly FR Y-9C regulatory data, initially for Morgan Stanley. The current Morgan Stanley dataset contains the relevant PPNR income and expense components and can be extended with balance-sheet variables required for scaling and further analysis.
Macroeconomic / financial predictors	Candidate predictors include short- and long-term Treasury yields, yield-curve slope, corporate yields/credit spreads, market volatility (VIX), and real GDP growth. The final set of predictors will depend on the PPNR component being modelled and the economic channel considered.
Potential cross-bank extension	If feasible and useful after the Morgan Stanley analysis, the dataset will be extended to a panel of large U.S. bank holding companies. This would allow comparison of macroeconomic sensitivities across banks with different business models rather than treating the panel analysis as a predetermined part of the core model.

## Methodology
1.	Data preparation and variable construction.
Select economically relevant PPNR components and corresponding macroeconomic and financial predictors. Where appropriate, PPNR components will be scaled by relevant balance-sheet measures to account for changes in bank size.

3.	Descriptive and exploratory analysis.
Examine the selected variables using summary statistics, time-series plots and correlation analysis to identify trends, potential structural changes and preliminary relationships between PPNR components and macroeconomic conditions.

4.	Time-series properties and model preparation.
Test relevant series for stationarity using standard unit-root/stationarity tests (e.g. ADF and KPSS). Appropriate transformations, such as first differences, will be considered where necessary. Cointegration will be investigated for relevant non-stationary variables.

5.	Morgan Stanley baseline estimation.
Estimate parsimonious time-series models for the selected Morgan Stanley PPNR components. Autoregressive specifications will provide a baseline against which models incorporating macroeconomic and financial predictors can be evaluated.

6.	Macroeconomic model estimation and evaluation.
Assess whether macroeconomic and financial variables improve the explanation and prediction of the selected PPNR components relative to the autoregressive baseline. Model performance will be evaluated using appropriate diagnostics and, where feasible, out-of-sample forecasting.

7.	Potential model extensions.
Depending on the time-series properties and results of the baseline analysis, multivariate approaches such as VAR or VECM may be considered to capture dynamic relationships between PPNR components and macroeconomic variables.

8.	Potential cross-bank extension.
If feasible, the analysis will be extended to other large U.S. bank holding companies using panel-data methods. This would allow comparison of macroeconomic sensitivities across institutions and business models.

## Timeline
•	Week 4: Data analysis: Finalize the construction and scaling of the selected Morgan Stanley PPNR components and macroeconomic variables. Conduct descriptive analysis, correlations and preliminary time-series diagnostics.

•	Weeks 6-7: Model development: Refine the initial specifications based on diagnostics and feedback, compare alternative specifications, and prepare the methodology and preliminary results.

•	Milestone 1: Finalized working dataset, documented methodology, initial model estimates and program code.

•	Second quarter: Extensions and validation: Develop more advanced specifications where supported by the data, conduct robustness and forecasting tests, and potentially extend the analysis to other large U.S. banks to compare results with Morgan Stanley.

•	Final stage: Select and validate the preferred models, interpret the results, and complete the final report and presentation.


