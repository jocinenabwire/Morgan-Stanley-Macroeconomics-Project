
# ============================================================
# MORGAN STANLEY PPNR PROJECT
# Stage 1: Import and preliminary data analysis
# Sample: 2009 Q1 - 2024 Q4
# ============================================================

# 1. Load packages --------------------------------------------

library(tidyverse)
library(lubridate)

# 2. Import master dataset ------------------------------------

data <- read.csv(
  "Morgan_Stanley_Master_Quarterly_Dataset_2009Q1_2024Q4.csv",
  stringsAsFactors = FALSE
)

# Convert date column
data$Date <- as.Date(data$Date)

# Sort observations chronologically
data <- data %>%
  arrange(Date)

# 3. Initial data inspection ----------------------------------

dim(data)
names(data)
head(data)
tail(data)
str(data)

# 4. Verify quarterly observations ---------------------------

cat("Number of observations:", nrow(data), "\n")
cat("Number of unique dates:", n_distinct(data$Date), "\n")
cat("First observation:", as.character(min(data$Date)), "\n")
cat("Last observation:", as.character(max(data$Date)), "\n")

# Check for duplicate dates
sum(duplicated(data$Date))

# Check quarterly continuity
expected_dates <- seq.Date(
  from = as.Date("2009-03-31"),
  to = as.Date("2024-12-31"),
  by = "quarter"
)

all(data$Date == expected_dates)


# 5. Prepare the four PPNR components -------------------------

data <- data %>%
  mutate(
    interest_income_bn = Total_interest_income_USD_BN,
    
    interest_expense_bn = Total_interest_expense_USD_BN,
    
    noninterest_income_bn = Total_noninterest_income_USD_BN,
    
    noninterest_expense_bn = Total_noninterest_expense_USD_BN,
    
    net_interest_income_bn = Net_interest_income_USD_BN
  )

# 6. Calculate aggregate PPNR ---------------------------------

data <- data %>%
  mutate(
    ppnr_bn = interest_income_bn -
      interest_expense_bn +
      noninterest_income_bn -
      noninterest_expense_bn
  )

# 7. Check calculations ---------------------------------------

summary(data$ppnr_bn)

# Display the five variables
data %>%
  select(
    Date,
    interest_income_bn,
    interest_expense_bn,
    noninterest_income_bn,
    noninterest_expense_bn,
    ppnr_bn
  ) %>%
  head()

# Check the PPNR calculation
summary(data$ppnr_bn)

summary(
  data$net_interest_income_bn -
    (data$interest_income_bn - data$interest_expense_bn)
)

# 8. Descriptive statistics -----------------------------------

ppnr_variables <- c(
  "interest_income_bn",
  "interest_expense_bn",
  "noninterest_income_bn",
  "noninterest_expense_bn",
  "ppnr_bn"
)

descriptive_stats <- data %>%
  summarise(
    across(
      all_of(ppnr_variables),
      list(
        Mean = ~mean(.x, na.rm = TRUE),
        SD = ~sd(.x, na.rm = TRUE),
        Min = ~min(.x, na.rm = TRUE),
        Max = ~max(.x, na.rm = TRUE)
      )
    )
  )

print(descriptive_stats)


# 9. Plot the four PPNR components ----------------------------

ppnr_long <- data %>%
  select(Date, all_of(ppnr_variables)) %>%
  pivot_longer(
    cols = -Date,
    names_to = "Component",
    values_to = "Value"
  )

ggplot(ppnr_long, aes(x = Date, y = Value)) +
  geom_line() +
  facet_wrap(~ Component, scales = "free_y", ncol = 2) +
  labs(
    title = "Morgan Stanley: PPNR Components",
    subtitle = "Quarterly data, 2009 Q1 - 2024 Q4",
    x = "Quarter",
    y = "USD billions"
  ) +
  theme_minimal()

# 10. Save prepared dataset -----------------------------------

write.csv(
  data,
  "Morgan_Stanley_Preliminary_Analysis.csv",
  row.names = FALSE
)

# ============================================================
# STAGE 2: STATIONARITY TESTS
# ============================================================

library(tseries)
library(urca)

# Select the four PPNR components

ppnr_variables <- c(
  "interest_income_bn",
  "interest_expense_bn",
  "noninterest_income_bn",
  "noninterest_expense_bn"
)

# ADF tests at levels

for (variable in ppnr_variables) {
  
  cat("\n====================================\n")
  cat("ADF TEST:", variable, "\n")
  cat("====================================\n")
  
  print(adf.test(data[[variable]]))
}

# KPSS tests at levels

for (variable in ppnr_variables) {
  
  cat("\n====================================\n")
  cat("KPSS TEST:", variable, "\n")
  cat("====================================\n")
  
  print(kpss.test(data[[variable]], null = "Level"))
}


# ============================================================
# STAGE 2B: STATIONARITY TESTS AT FIRST DIFFERENCES
# ============================================================

# Create first differences of PPNR components

for (variable in ppnr_variables) {
  
  new_variable <- paste0("d_", variable)
  
  data[[new_variable]] <- c(
    NA,
    diff(data[[variable]])
  )
}

# ADF tests at first differences

for (variable in ppnr_variables) {
  
  diff_variable <- paste0("d_", variable)
  
  cat("\n====================================\n")
  cat("ADF TEST - FIRST DIFFERENCE:", variable, "\n")
  cat("====================================\n")
  
  print(adf.test(na.omit(data[[diff_variable]])))
}

# KPSS tests at first differences

for (variable in ppnr_variables) {
  
  diff_variable <- paste0("d_", variable)
  
  cat("\n====================================\n")
  cat("KPSS TEST - FIRST DIFFERENCE:", variable, "\n")
  cat("====================================\n")
  
  print(
    kpss.test(
      na.omit(data[[diff_variable]]),
      null = "Level"
    )
  )
}


# ============================================================
# STAGE 2C: ADDITIONAL ADF TESTS
# Interest income and interest expense
# ============================================================

library(urca)

interest_variables <- c(
  "interest_income_bn",
  "interest_expense_bn"
)

for (variable in interest_variables) {
  
  cat("\n====================================\n")
  cat("ADF WITH DRIFT:", variable, "\n")
  cat("====================================\n")
  
  test_drift <- ur.df(
    data[[variable]],
    type = "drift",
    lags = 4,
    selectlags = "AIC"
  )
  
  print(summary(test_drift))
  
  cat("\n====================================\n")
  cat("ADF WITH TREND:", variable, "\n")
  cat("====================================\n")
  
  test_trend <- ur.df(
    data[[variable]],
    type = "trend",
    lags = 4,
    selectlags = "AIC"
  )
  
  print(summary(test_trend))
}


# ============================================================
# STAGE 2D: PLOT FIRST DIFFERENCES
# ============================================================

diff_long <- data %>%
  select(
    Date,
    d_interest_income_bn,
    d_interest_expense_bn,
    d_noninterest_income_bn,
    d_noninterest_expense_bn
  ) %>%
  pivot_longer(
    cols = -Date,
    names_to = "Component",
    values_to = "Change"
  )

ggplot(diff_long, aes(x = Date, y = Change)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~Component, scales = "free_y", ncol = 2) +
  labs(
    title = "Quarterly Changes in Morgan Stanley PPNR Components",
    subtitle = "First differences, 2009 Q2 - 2024 Q4",
    x = "Quarter",
    y = "Change in USD billions"
  ) +
  theme_minimal()



# ============================================================
# STAGE 2E: STRUCTURAL BREAK INVESTIGATION
# ============================================================

install.packages("strucchange")
library(strucchange)

for (variable in ppnr_variables) {
  
  cat("\n====================================\n")
  cat("STRUCTURAL BREAK TEST:", variable, "\n")
  cat("====================================\n")
  
  break_model <- breakpoints(
    data[[variable]] ~ 1,
    h = 0.15
  )
  
  print(summary(break_model))
  
  # Plot estimated break structure
  plot(
    break_model,
    main = paste("Structural Breaks:", variable)
  )
}


# ============================================================
# STAGE 3: EXPLANATORY VARIABLE SELECTION
# ============================================================

# 1. Define candidate explanatory variables

interest_income_drivers <- c(
  "Total_Assets_USD_BN",
  "Loans_Leases_USD_BN",
  "X3.month.Treasury.rate",
  "X10.year.Treasury.yield",
  "Real.GDP.growth"
)

interest_expense_drivers <- c(
  "Total_Liabilities_USD_BN",
  "Total_Assets_USD_BN",
  "X3.month.Treasury.rate",
  "X10.year.Treasury.yield",
  "Prime.rate"
)

noninterest_income_drivers <- c(
  "Total_Assets_USD_BN",
  "Real.GDP.growth",
  "Dow.Jones.Total.Stock.Market.Index..Level.",
  "Market.Volatility.Index..Level.",
  "Unemployment.rate"
)

noninterest_expense_drivers <- c(
  "Total_Assets_USD_BN",
  "Nominal.GDP.growth",
  "CPI.inflation.rate",
  "Unemployment.rate",
  "Real.disposable.income.growth"
)


# 2. Check that all selected variables exist

all_drivers <- unique(c(
  interest_income_drivers,
  interest_expense_drivers,
  noninterest_income_drivers,
  noninterest_expense_drivers
))

missing_variables <- setdiff(
  all_drivers,
  names(data)
)

if (length(missing_variables) > 0) {
  stop(
    paste(
      "These variables are missing:",
      paste(missing_variables, collapse = ", ")
    )
  )
} else {
  cat("All explanatory variables are available.\n")
}


# 3. Inspect explanatory variable summaries

summary(
  data[, all_drivers]
)


# 4. Check missing observations

missing_summary <- data %>%
  summarise(
    across(
      all_of(all_drivers),
      ~sum(is.na(.x))
    )
  )

print(missing_summary)


# 5. Correlation analysis: interest income

cor_interest_income <- cor(
  data[, c(
    "interest_income_bn",
    interest_income_drivers
  )],
  use = "complete.obs"
)

print(round(cor_interest_income, 3))


# 6. Correlation analysis: interest expense

cor_interest_expense <- cor(
  data[, c(
    "interest_expense_bn",
    interest_expense_drivers
  )],
  use = "complete.obs"
)

print(round(cor_interest_expense, 3))


# 7. Correlation analysis: noninterest income

cor_noninterest_income <- cor(
  data[, c(
    "noninterest_income_bn",
    noninterest_income_drivers
  )],
  use = "complete.obs"
)

print(round(cor_noninterest_income, 3))


# 8. Correlation analysis: noninterest expense

cor_noninterest_expense <- cor(
  data[, c(
    "noninterest_expense_bn",
    noninterest_expense_drivers
  )],
  use = "complete.obs"
)

print(round(cor_noninterest_expense, 3))


# ============================================================
# STAGE 3B: STATIONARITY OF EXPLANATORY VARIABLES
# ============================================================

candidate_variables <- c(
  "Total_Assets_USD_BN",
  "Total_Liabilities_USD_BN",
  "X3.month.Treasury.rate",
  "X10.year.Treasury.yield",
  "Real.GDP.growth",
  "Nominal.GDP.growth",
  "CPI.inflation.rate",
  "Market.Volatility.Index..Level."
)

# ADF tests at levels

for (variable in candidate_variables) {
  
  cat("\n====================================\n")
  cat("ADF TEST:", variable, "\n")
  cat("====================================\n")
  
  print(adf.test(data[[variable]]))
}

# KPSS tests at levels

for (variable in candidate_variables) {
  
  cat("\n====================================\n")
  cat("KPSS TEST:", variable, "\n")
  cat("====================================\n")
  
  print(kpss.test(data[[variable]], null = "Level"))
}


# Variables requiring further investigation
variables_to_difference <- c(
  "Total_Assets_USD_BN",
  "Total_Liabilities_USD_BN",
  "X3.month.Treasury.rate",
  "X10.year.Treasury.yield",
  "CPI.inflation.rate",
  "Market.Volatility.Index..Level."
)

# ADF tests on first differences
for (variable in variables_to_difference) {
  
  cat("\n====================================\n")
  cat("ADF FIRST DIFFERENCE:", variable, "\n")
  cat("====================================\n")
  
  print(adf.test(diff(data[[variable]])))
}

# KPSS tests on first differences
for (variable in variables_to_difference) {
  
  cat("\n====================================\n")
  cat("KPSS FIRST DIFFERENCE:", variable, "\n")
  cat("====================================\n")
  
  print(kpss.test(diff(data[[variable]]), null = "Level"))
}

# ==========================================
#  PREPARE VARIABLES FOR PRELIMINARY MODELS
# ==========================================

library(dplyr)

data <- data %>%
  arrange(Date) %>%
  mutate(
    # Dependent variables: quarterly changes in PPNR components
    d_interest_income = c(NA, diff(interest_income_bn)),
    d_interest_expense = c(NA, diff(interest_expense_bn)),
    d_noninterest_income = c(NA, diff(noninterest_income_bn)),
    d_noninterest_expense = c(NA, diff(noninterest_expense_bn)),
    
    # Balance-sheet variables: quarterly changes
    d_total_assets = c(NA, diff(Total_Assets_USD_BN)),
    d_total_liabilities = c(NA, diff(Total_Liabilities_USD_BN)),
    
    # Interest rates: quarterly changes
    d_3m_treasury = c(NA, diff(X3.month.Treasury.rate)),
    d_10y_treasury = c(NA, diff(X10.year.Treasury.yield)),
    
    # CPI inflation: quarterly changes
    d_cpi = c(NA, diff(CPI.inflation.rate))
  )

# Remove first observation lost through differencing
model_data <- data %>%
  filter(!is.na(d_interest_income))

# Check the resulting sample
nrow(model_data)
head(model_data)

# ==========================================
#  MODEL 1: INTEREST INCOME
# ==========================================

model_interest_income <- lm(
  d_interest_income ~
    d_total_assets +
    d_3m_treasury +
    d_10y_treasury +
    Real.GDP.growth,
  data = model_data
)

summary(model_interest_income)


# ==========================================
#  MODEL 2: INTEREST EXPENSE
# ==========================================

model_interest_expense <- lm(
  d_interest_expense ~
    d_total_liabilities +
    d_3m_treasury +
    d_10y_treasury,
  data = model_data
)

summary(model_interest_expense)

library(lmtest)
library(car)
library(sandwich)

# ==========================================
# DIAGNOSTICS: MODEL 1 - INTEREST INCOME
# ==========================================

cat("\nMODEL 1: INTEREST INCOME\n")

# Serial correlation
bgtest(model_interest_income, order = 4)

# Heteroskedasticity
bptest(model_interest_income)

# Multicollinearity
vif(model_interest_income)


# ==========================================
# DIAGNOSTICS: MODEL 2 - INTEREST EXPENSE
# ==========================================

cat("\nMODEL 2: INTEREST EXPENSE\n")

# Serial correlation
bgtest(model_interest_expense, order = 4)

# Heteroskedasticity
bptest(model_interest_expense)

# Multicollinearity
vif(model_interest_expense)


## Calculate robust standard errors
# Model 1
coeftest(
  model_interest_income,
  vcov = NeweyWest(
    model_interest_income,
    lag = 4,
    prewhite = FALSE
  )
)

# Model 2
coeftest(
  model_interest_expense,
  vcov = NeweyWest(
    model_interest_expense,
    lag = 4,
    prewhite = FALSE
  )
)