install.packages("dplyr")
install.packages("readr")
install.packages("lmtest")
install.packages("tidyr")

library(lmtest)
library(dplyr)
library(readr)
library(tidyr)

getwd()
list.files(pattern = "\\.csv$")
rm(list = ls())

# 1. Import datasets
ppnr <- read_csv("Morgan_Stanley_PPNR_Quarterly_2009_2026.csv")
bs <- read_csv("Morgan_Stanley_Balance_Sheet.csv")
macro <- read_csv("2025-Table_1A_Historic_Domestic.csv")

# 2. Prepare interest expense
expense <- ppnr %>%
  transmute(
    Quarter = paste0(format(as.Date(Date), "%Y"), " Q",
                     (as.integer(format(as.Date(Date), "%m")) - 1) %/% 3 + 1),
    IE = `Total interest expense`
  ) %>%
  mutate(IE_lag = lag(IE))

# 3. Prepare balance sheet variables
balance <- bs %>%
  transmute(
    Quarter = Date,
    Deposits_US = `Interest-bearing domestic deposits`,
    Deposits_Foreign = `Interest-bearing foreign deposits`,
    Repos = Repos,
    Other_Borrowing = `Other borrowed money`,
    Subordinated_Debt = `Subordinated debt`,
    Trading_Liabilities = `Trading liabilities`,
    Total_Liabilities = `Total liabilities`
  )

# 4. Prepare macroeconomic variables
rates <- macro %>%
  transmute(
    Quarter = Date,
    Rate_3M = `3-month Treasury rate`,
    Rate_10Y = `10-year Treasury yield`,
    BBB_Yield = `BBB corporate yield`,
    VIX = `Market Volatility Index (Level)`
  ) %>%
  arrange(Quarter) %>%
  mutate(Rate_Change = Rate_3M - lag(Rate_3M))

# 5. Create selection dataset
selection_expense <- expense %>%
  left_join(balance, by = "Quarter") %>%
  left_join(rates, by = "Quarter") %>%
  drop_na() %>%
  arrange(Quarter)

# 6. Save and inspect
saveRDS(selection_expense, "selection_expense.rds")
View(selection_expense)
dim(selection_expense)

# Add interest-rate interaction terms
selection_expense <- selection_expense %>%
  mutate(
    Rate_Deposits = Rate_3M * Deposits_US,
    Rate_Repos = Rate_3M * Repos,
    Rate_Borrowing = Rate_3M * Other_Borrowing
  )

# Save updated dataset
saveRDS(selection_expense, "selection_expense.rds")
write_csv(selection_expense, "selection_expense.csv")

# 7. Settings
window <- 32
horizon <- 9
starts <- c(37, 46, 55)

variables <- setdiff(names(selection_expense), c("Quarter", "IE"))

combinations <- unlist(
  lapply(3:5, function(k) combn(variables, k, simplify = FALSE)),
  recursive = FALSE
)

# 8. Function to evaluate one model
evaluate_model <- function(selected) {
  
  scores <- numeric(length(starts))
  
  for (j in seq_along(starts)) {
    
    start <- starts[j]
    
    train <- selection_expense[(start - window):(start - 1), ]
    test <- selection_expense[start:(start + horizon - 1), ]
    
    model <- lm(
      reformulate(selected, response = "IE"),
      data = train
    )
    
    last_IE <- tail(train$IE, 1)
    errors <- numeric(horizon)
    
    for (h in 1:horizon) {
      
      new_row <- test[h, , drop = FALSE]
      
      if ("IE_lag" %in% selected) {
        new_row$IE_lag <- last_IE
      }
      
      prediction <- as.numeric(
        predict(model, newdata = new_row)
      )
      
      errors[h] <- abs(test$IE[h] - prediction) /
        abs(test$IE[h]) * 100
      
      last_IE <- prediction
    }
    
    scores[j] <- mean(errors)
  }
  
  scores
}

# 9. Evaluate all combinations
scores <- t(sapply(combinations, evaluate_model))

results_expense <- data.frame(
  Variables = sapply(combinations, paste, collapse = " + "),
  Test_1 = scores[, 1],
  Test_2 = scores[, 2],
  Test_3 = scores[, 3]
)

# 10. Rank models
results_expense <- results_expense %>%
  mutate(
    Average_MAPE = rowMeans(across(Test_1:Test_3)),
    Rank = rank(Average_MAPE, ties.method = "first")
  ) %>%
  arrange(Rank) %>%
  select(Rank, Variables, Test_1, Test_2, Test_3, Average_MAPE)

# 11. Show best 10 models
View(head(results_expense, 10))


# 12. Plot the three best models separately

top3 <- head(results_expense, 3)

for (i in 1:3) {
  
  vars <- trimws(strsplit(top3$Variables[i], " \\+ ")[[1]])
  
  actual <- numeric(0)
  predicted <- numeric(0)
  quarters <- character(0)
  
  for (s in starts) {
    
    train <- selection_expense[(s-window):(s-1), ]
    test <- selection_expense[s:(s+horizon-1), ]
    
    model <- lm(reformulate(vars, response = "IE"), data = train)
    
    forecasts <- numeric(horizon)
    
    for (h in 1:horizon) {
      
      new_row <- test[h, ]
      
      if ("IE_lag" %in% vars) {
        new_row$IE_lag <- if (h == 1) {
          tail(train$IE, 1)
        } else {
          forecasts[h-1]
        }
      }
      
      forecasts[h] <- predict(model, newdata = new_row)
    }
    
    actual <- c(actual, test$IE)
    predicted <- c(predicted, forecasts)
    quarters <- c(quarters, test$Quarter)
  }
  
  x <- seq_along(actual)
  
  plot(x, actual / 1e6,
       type = "l",
       col = "black", lwd = 2,
       ylim = range(c(actual, predicted) / 1e6),
       xaxt = "n",
       xlab = "Year",
       ylab = "Interest Expense (USD billions)",
       main = paste0("Model ", i,
                     " - Average MAPE: ",
                     round(top3$Average_MAPE[i], 2), "%"))
  
  lines(x, predicted / 1e6,
        col = "blue", lwd = 2, lty = 2)
  
  abline(v = c(9.5, 18.5), col = "grey75", lty = 3)
  
  year_positions <- which(grepl("Q1", quarters))
  
  axis(1,
       at = year_positions,
       labels = substr(quarters[year_positions], 1, 4))
  
  text(c(5, 14, 23),
       rep(max(c(actual, predicted) / 1e6) * 0.97, 3),
       labels = c("Test 1", "Test 2", "Test 3"),
       cex = 0.9)
  
  legend("topleft",
         legend = c("Actual", "Forecast"),
         col = c("black", "blue"),
         lty = c(1, 2),
         lwd = 2,
         bty = "n")
}


# 12. Rank models by MAE (USD billions)

mae_scores <- matrix(NA_real_, nrow = length(combinations), ncol = 3)

for (i in seq_along(combinations)) {
  
  vars <- combinations[[i]]
  formula <- reformulate(vars, response = "IE")
  
  for (j in 1:3) {
    
    start <- starts[j]
    actual <- predicted <- numeric(horizon)
    
    for (h in 1:horizon) {
      t <- start + h - 1
      
      model <- lm(formula, data = selection_expense[1:(t - 1), ])
      predicted[h] <- predict(model, newdata = selection_expense[t, ])
      actual[h] <- selection_expense$IE[t]
    }
    
    mae_scores[i, j] <- mean(abs(actual - predicted)) / 1e6
  }
}

results_mae <- data.frame(
  Variables = sapply(combinations, paste, collapse = " + "),
  Test_1 = mae_scores[, 1],
  Test_2 = mae_scores[, 2],
  Test_3 = mae_scores[, 3]
) %>%
  mutate(
    Average_MAE = rowMeans(across(Test_1:Test_3))
  ) %>%
  arrange(Average_MAE) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, everything())

View(head(results_mae, 10))


# 13. Plot the three best MAE models individually

par(mfrow = c(1, 1))

top3_mae <- head(results_mae, 3)

for (i in 1:3) {
  
  vars <- trimws(strsplit(top3_mae$Variables[i], " \\+ ")[[1]])
  formula <- reformulate(vars, response = "IE")
  
  actual <- numeric(0)
  predicted <- numeric(0)
  quarters <- character(0)
  
  for (s in starts) {
    
    for (h in 1:horizon) {
      
      t <- s + h - 1
      
      model <- lm(formula, data = selection_expense[1:(t - 1), ])
      
      predicted <- c(predicted,
                     as.numeric(predict(model, newdata = selection_expense[t, ])))
      
      actual <- c(actual, selection_expense$IE[t])
      quarters <- c(quarters, selection_expense$Quarter[t])
    }
  }
  
  x <- seq_along(actual)
  
  plot(x, actual / 1e6,
       type = "l",
       col = "black", lwd = 2,
       ylim = range(c(actual, predicted) / 1e6),
       xaxt = "n",
       xlab = "Year",
       ylab = "Interest Expense (USD billions)",
       main = paste0("Model ", i,
                     " - Average MAE: $",
                     round(top3_mae$Average_MAE[i], 3),
                     " billion"))
  
  lines(x, predicted / 1e6,
        col = "blue", lwd = 2, lty = 2)
  
  abline(v = c(9.5, 18.5), col = "grey75", lty = 3)
  
  year_positions <- which(grepl("Q1", quarters))
  
  axis(1,
       at = year_positions,
       labels = substr(quarters[year_positions], 1, 4))
  
  text(c(5, 14, 23),
       rep(max(c(actual, predicted) / 1e6) * 0.97, 3),
       labels = c("Test 1", "Test 2", "Test 3"),
       cex = 0.9)
  
  legend("topleft",
         legend = c("Actual", "Forecast"),
         col = c("black", "blue"),
         lty = c(1, 2),
         lwd = 2,
         bty = "n")
}
