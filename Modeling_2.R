install.packages("dplyr")
install.packages("readr")
install.packages("lmtest")

library(lmtest)
library(dplyr)
library(readr)

getwd()

selection_data <- read_csv("selection_data.csv")

# Create interest-rate interaction variables
selection_data <- selection_data %>%
  mutate(
    Rate_Trading = Rate_3M * Trading,   
    Rate_Repos = Rate_3M * Reverse_Repos,
    Rate_Loans = Rate_3M * Loans
  )

# Create quarterly interest-rate change
selection_data <- selection_data %>%
  mutate(Rate_Change = Rate_3M - lag(Rate_3M))

selection_data <- selection_data %>%
  filter(!is.na(Rate_Change))

# 1. Settings
window <- 32
horizon <- 9
starts <- c(36, 45, 54)
variables <- setdiff(names(selection_data), c("Quarter", "II"))
combinations <- unlist(
  lapply(3:5, function(k) combn(variables, k, simplify = FALSE)),
  recursive = FALSE
)

combinations <- Filter(function(x) {
  (!"Rate_Trading" %in% x || all(c("Rate_3M", "Trading") %in% x)) &&
    (!"Rate_Repos" %in% x || all(c("Rate_3M", "Reverse_Repos") %in% x)) &&
    (!"Rate_Loans" %in% x || all(c("Rate_3M", "Loans") %in% x))
}, combinations)

# 2. Function to evaluate one model
evaluate_model <- function(selected) {
  mape <- numeric(length(starts))
  for (j in seq_along(starts)) {
    start <- starts[j]
    train <- selection_data[(start - window):(start - 1), ]
    test <- selection_data[start:(start + horizon - 1), ]
    model <- lm(reformulate(selected, response = "II"), data = train)
    last_II <- tail(train$II, 1)
    errors <- numeric(horizon)
    for (h in 1:horizon) {
      new_row <- test[h, , drop = FALSE]
      if ("II_lag" %in% selected) new_row$II_lag <- last_II
      prediction <- as.numeric(predict(model, newdata = new_row))
      errors[h] <- abs(test$II[h] - prediction) / abs(test$II[h]) * 100
      last_II <- prediction
    }
    mape[j] <- mean(errors)
  }
  return(mape)
}

# 3. Evaluate all combinations
scores <- t(sapply(combinations, evaluate_model))
results_rolling <- data.frame(
  Variables = sapply(combinations, paste, collapse = " + "),
  Test_1 = scores[, 1],
  Test_2 = scores[, 2],
  Test_3 = scores[, 3]
)

# 4. Rank models and show the best 10
results_rolling <- results_rolling %>%
  mutate(
    Average_MAPE = rowMeans(across(Test_1:Test_3)),
    Rank = rank(Average_MAPE, ties.method = "first")
  ) %>%
  arrange(Rank) %>%
  select(Rank, Variables, Test_1, Test_2, Test_3, Average_MAPE)

View(head(results_rolling, 10))


# 5. Plot the 3 best models

for (i in 1:3) {
  
  selected <- strsplit(results_rolling$Variables[i], " \\+ ")[[1]]
  
  actual <- numeric(0)
  predicted <- numeric(0)
  quarters <- character(0)
  
  for (j in seq_along(starts)) {
    
    start <- starts[j]
    
    train <- selection_data[(start - window):(start - 1), ]
    test <- selection_data[start:(start + horizon - 1), ]
    
    model <- lm(reformulate(selected, response = "II"), data = train)
    
    last_II <- tail(train$II, 1)
    
    for (h in 1:horizon) {
      
      new_row <- test[h, , drop = FALSE]
      
      if ("II_lag" %in% selected) {
        new_row$II_lag <- last_II
      }
      
      prediction <- as.numeric(predict(model, newdata = new_row))
      
      predicted <- c(predicted, prediction)
      actual <- c(actual, test$II[h])
      quarters <- c(quarters, test$Quarter[h])
      
      last_II <- prediction
    }
  }
  
  # Plot
  x <- seq_along(actual)
  
  plot(x, actual / 1e6,
       type = "l", col = "black", lwd = 2,
       ylim = range(c(actual, predicted)) / 1e6,
       xaxt = "n",
       xlab = "Quarter",
       ylab = "Interest Income (USD billions)",
       main = paste0("Model ", i,
                     " - Average MAPE: ",
                     round(results_rolling$Average_MAPE[i], 2), "%"))
  
  lines(x, predicted / 1e6,
        col = "blue", lwd = 2, lty = 2)
  
  # Test boundaries
  abline(v = c(9.5, 18.5), col = "gray80", lty = 3)
  
  # Year labels
  year_positions <- which(grepl("Q1", quarters))
  
  axis(1,
       at = year_positions,
       labels = substr(quarters[year_positions], 1, 4))
  
  # Test labels
  text(c(5, 14, 23),
       rep(max(c(actual, predicted)) / 1e6 * 0.97, 3),
       labels = c("Test 1", "Test 2", "Test 3"),
       cex = 0.9)
  
  legend("topleft",
         legend = c("Actual", "Forecast"),
         col = c("black", "blue"),
         lty = c(1, 2),
         lwd = 2,
         bty = "n")
}





# 5. Test the top 3 models with Rate_Change
top_3 <- lapply(results_rolling$Variables[1:3], function(x) {
  trimws(strsplit(x, " + ", fixed = TRUE)[[1]])
})

comparison_change <- do.call(rbind, lapply(1:3, function(i) {
  original <- top_3[[i]]
  extended <- unique(c(original, "Rate_Change"))
  old <- evaluate_model(original)
  new <- evaluate_model(extended)
  data.frame(
    Model = i,
    Version = c("Original", "With Rate_Change"),
    Test_1 = c(old[1], new[1]),
    Test_2 = c(old[2], new[2]),
    Test_3 = c(old[3], new[3]),
    Average_MAPE = c(mean(old), mean(new))
  )
}))

View(comparison_change)



# 6. Compare Model 1 forecasts with and without Rate_Change
original <- c("II_lag", "Rate_3M", "Rate_10Y", "Loans", "Rate_Loans")
extended <- c(original, "Rate_Change")

get_forecasts <- function(selected) {
  predictions <- c()
  for (start in starts) {
    train <- selection_data[(start - window):(start - 1), ]
    test <- selection_data[start:(start + horizon - 1), ]
    model <- lm(reformulate(selected, response = "II"), data = train)
    last_II <- tail(train$II, 1)
    for (h in 1:horizon) {
      new_row <- test[h, , drop = FALSE]
      if ("II_lag" %in% selected) new_row$II_lag <- last_II
      prediction <- as.numeric(predict(model, newdata = new_row))
      predictions <- c(predictions, prediction)
      last_II <- prediction
    }
  }
  predictions
}

# Calculate forecasts
actual <- unlist(lapply(starts, function(s) selection_data$II[s:(s + horizon - 1)]))
quarters <- unlist(lapply(starts, function(s) selection_data$Quarter[s:(s + horizon - 1)]))
forecast_original <- get_forecasts(original)
forecast_extended <- get_forecasts(extended)

# Plot
x <- seq_along(actual)
years <- substr(quarters, 1, 4)
year_positions <- which(!duplicated(years))

plot(x, actual / 1e6, type = "l", col = "black", lwd = 2,
     ylim = range(c(actual, forecast_original, forecast_extended)) / 1e6,
     xaxt = "n", xlab = "Year", ylab = "Interest income (USD billions)",
     main = "Model 1: Effect of Rate_Change")
lines(x, forecast_original / 1e6, col = "blue", lwd = 2, lty = 2)
lines(x, forecast_extended / 1e6, col = "darkorange", lwd = 2, lty = 3)
axis(1, at = year_positions, labels = years[year_positions])
abline(v = c(9.5, 18.5), col = "gray70", lty = 3)
text(c(5, 14, 23), par("usr")[4] - 0.4,
     c("Test 1", "Test 2", "Test 3"), cex = 0.9)
legend("topleft",
       legend = c("Actual", "Original model", "With Rate_Change"),
       col = c("black", "blue", "darkorange"),
       lty = c(1, 2, 3), lwd = 2, bty = "n")
