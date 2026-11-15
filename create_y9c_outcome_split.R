# Create the approved chronological train/test split for the four Y-9C PPNR
# outcome components. Macro and balance-sheet predictors are intentionally not
# included here; see data_splits/SPLIT_SPEC.md for the remaining feature work.

source_file <- "Morgan_Stanley_PPNR_Quarterly_2009_2026.csv"
output_dir <- "data_splits"
train_file <- file.path(output_dir, "train_y9c_outcomes_2009Q1_2024Q2.csv")
test_file <- file.path(output_dir, "test_y9c_outcomes_2024Q3_2026Q3.csv")
train_end <- as.Date("2024-06-30")
test_start <- as.Date("2024-09-30")
test_end <- as.Date("2026-09-30")

stop_if <- function(condition, message) {
  if (condition) stop(message, call. = FALSE)
}

stop_if(!file.exists(source_file), paste("Missing Y-9C output:", source_file))
ppnr <- read.csv(source_file, check.names = FALSE, stringsAsFactors = FALSE,
                 na.strings = c("NA", ""))
dates <- as.Date(ppnr$Date)
stop_if(anyNA(dates) || anyDuplicated(dates), "Invalid or duplicate Y-9C dates.")
stop_if(length(dates) != 71L || dates[1] != as.Date("2009-03-31") ||
          tail(dates, 1) != test_end,
        "Expected 71 quarterly Y-9C observations from 2009Q1 through 2026Q3.")

component_names <- c(
  "Total interest income",
  "Total interest expense",
  "Total noninterest income",
  "Total noninterest expense"
)
stop_if(!all(component_names %in% names(ppnr)),
        "One or more of the four required Y-9C outcome components are missing.")
stop_if(anyNA(ppnr[, component_names, drop = FALSE]),
        "A required Y-9C outcome component is missing in the 71-quarter period.")

targets <- data.frame(Date = format(dates, "%Y-%m-%d"),
                      ppnr[, component_names, drop = FALSE],
                      check.names = FALSE)
targets[["Net interest income"]] <-
  targets[["Total interest income"]] - targets[["Total interest expense"]]
targets[["PPNR"]] <- targets[["Net interest income"]] +
  targets[["Total noninterest income"]] - targets[["Total noninterest expense"]]
targets <- targets[, c("Date", component_names, "Net interest income", "PPNR"),
                   drop = FALSE]

reported_nii <- ppnr[["Net interest income"]]
stop_if(any(targets[["Net interest income"]] != reported_nii),
        "Derived net interest income does not match the reported Y-9C total.")
stop_if(any(!is.finite(targets[["PPNR"]])), "Derived PPNR contains non-finite values.")

train <- targets[dates <= train_end, , drop = FALSE]
test <- targets[dates >= test_start & dates <= test_end, , drop = FALSE]
stop_if(nrow(train) != 62L || train$Date[1] != "2009-03-31" ||
          tail(train$Date, 1) != "2024-06-30",
        "Training set does not match the approved 62-quarter window.")
stop_if(nrow(test) != 9L || test$Date[1] != "2024-09-30" ||
          tail(test$Date, 1) != "2026-09-30",
        "Test set does not match the approved 9-quarter window.")

write_split_csv <- function(data, path) {
  previous_scipen <- getOption("scipen")
  options(scipen = 999)
  write.csv(data, path, row.names = FALSE, na = "NA")
  options(scipen = previous_scipen)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_split_csv(train, train_file)
write_split_csv(test, test_file)

cat("Created target-only Y-9C split.\n")
cat("Training:", nrow(train), "quarters,", train$Date[1], "through", tail(train$Date, 1), "\n")
cat("Test:", nrow(test), "quarters,", test$Date[1], "through", tail(test$Date, 1), "\n")
cat("Four Y-9C components, reported NII reconciliation, and derived PPNR checked.\n")
