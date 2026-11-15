# Refresh the recent Morgan Stanley PPNR and balance-sheet rows from official
# institution-specific FFIEC FR Y-9C reports. This script uses base R only.

source_file <- file.path("y9c_source", "Morgan_Stanley_Y9C_2025Q1_2026Q3_source.csv")
ppnr_files <- c("Morgan_Stanley_PPNR_Quarterly_Final.csv",
                "Morgan_Stanley_PPNR_Quarterly_2009_2026.csv")
balance_file <- "Morgan_Stanley_Balance_Sheet.csv"

required_dates <- as.Date(c("2025-03-31", "2025-06-30", "2025-09-30",
                            "2025-12-31", "2026-03-31", "2026-06-30",
                            "2026-09-30"))

ppnr_map <- c(
  "Interest income - Fed funds & reverse repos" = "BHCK4020",
  "Interest income - Depository institution balances" = "BHCK4115",
  "Interest income - US Treasury & agency securities" = "BHCKB488",
  "Interest income - Mortgage-backed securities" = "BHCKB489",
  "Interest income - Other securities" = "BHCK4060",
  "Interest income - Trading assets" = "BHCK4069",
  "Interest income - Other" = "BHCK4518",
  "Interest income - Residential real estate loans" = "BHCK4435",
  "Interest income - Other real estate loans" = "BHCK4436",
  "Interest income - Other domestic loans" = "BHCKF821",
  "Interest income - Foreign loans" = "BHCK4059",
  "Interest income - Lease financing" = "BHCK4065",
  "Interest expense - Domestic time deposits <=250k" = "BHCKHK03",
  "Interest expense - Domestic time deposits >250k" = "BHCKHK04",
  "Interest expense - Other domestic deposits" = "BHCK6761",
  "Interest expense - Foreign deposits" = "BHCK4172",
  "Interest expense - Fed funds & repos" = "BHCK4180",
  "Interest expense - Other borrowed money" = "BHCK4185",
  "Interest expense - Subordinated debt & convertible securities" = "BHCK4397",
  "Interest expense - Other" = "BHCK4398",
  "Noninterest income - Trading revenue" = "BHCKA220",
  "Noninterest income - Fiduciary activities" = "BHCK4070",
  "Noninterest income - Investment banking" = "BHCKC888",
  "Noninterest income - Net servicing fees" = "BHCKB492",
  "Noninterest income - Service charges on deposits" = "BHCK4483",
  "Noninterest income - Other" = "BHCKB497",
  "Noninterest income - Securities brokerage" = "BHCKC886",
  "Noninterest income - Annuity sales" = "BHCKC887",
  "Noninterest income - Insurance underwriting" = "BHCKC386",
  "Noninterest income - Other insurance activities" = "BHCKC387",
  "Noninterest income - Venture capital" = "BHCKB491",
  "Noninterest income - Net securitization" = "BHCKB493",
  "Noninterest income - Gains on sales of loans and leases" = "BHCK8560",
  "Noninterest income - Gains on sales of other real estate" = "BHCK8561",
  "Noninterest income - Gains on sales of other assets" = "BHCKB496",
  "Noninterest expense - Salaries and employee benefits" = "BHCK4135",
  "Noninterest expense - Premises and fixed assets" = "BHCK4217",
  "Noninterest expense - Other" = "BHCK4092",
  "Noninterest expense - Goodwill impairment" = "BHCKC216",
  "Noninterest expense - Other intangible assets" = "BHCKC232",
  "Total interest income" = "BHCK4107",
  "Total interest expense" = "BHCK4073",
  "Net interest income" = "BHCK4074",
  "Total noninterest income" = "BHCK4079",
  "Total noninterest expense" = "BHCK4093"
)

balance_map <- c(
  "Interest-bearing balances US" = "BHCK0395",
  "Interest-bearing balances foreign" = "BHCK0397",
  "Available-for-sale securities" = "BHCK1773",
  "Reverse repos" = "BHCKB989",
  "Loans held for investment" = "BHCKB528",
  "Trading assets" = "BHCK3545",
  "Interest-bearing domestic deposits" = "BHDM6636",
  "Interest-bearing foreign deposits" = "BHFN6636",
  "Repos" = "BHCKB995",
  "Other borrowed money" = "BHCK3190",
  "Subordinated debt" = "BHCK4062",
  "Trading liabilities" = "BHCK3548",
  "Total assets" = "BHCK2170",
  "Total liabilities" = "BHCK2948",
  "Held-to-maturity securities" = "BHCKD961"
)

stop_if <- function(condition, message) {
  if (condition) stop(message, call. = FALSE)
}

stop_if(!file.exists(source_file), paste("Missing source file:", source_file))
source <- read.csv(source_file, check.names = FALSE, stringsAsFactors = FALSE,
                   na.strings = c("", "NA"))
stop_if(!all(c("ReportDate", "Institution", "RSSD_ID", "SourceURL") %in% names(source)),
        "The source snapshot is missing required provenance columns.")
source_dates <- as.Date(source$ReportDate)
stop_if(anyNA(source_dates) || !identical(source_dates, required_dates),
        "The source snapshot must contain exactly 2025Q1 through 2026Q3 in order.")
stop_if(any(source$Institution != "MORGAN STANLEY") || any(source$RSSD_ID != 2162966),
        "The source snapshot must contain only Morgan Stanley (RSSD ID 2162966).")
expected_urls <- paste0("https://www.ffiec.gov/npw/FinancialReport/ReturnFinancialReportCSV?",
                        "rpt=FRY9C&id=2162966&dt=", format(required_dates, "%Y%m%d"))
stop_if(!identical(source$SourceURL, expected_urls),
        "A source URL does not match the official FFIEC Morgan Stanley FR Y-9C endpoint.")

needed_codes <- unique(c(unname(ppnr_map), unname(balance_map)))
stop_if(!all(needed_codes %in% names(source)),
        paste("Source snapshot is missing codes:",
              paste(setdiff(needed_codes, names(source)), collapse = ", ")))
for (code in needed_codes) {
  source[[code]] <- suppressWarnings(as.numeric(source[[code]]))
  stop_if(anyNA(source[[code]]), paste("Missing or non-numeric Y-9C value for", code))
}

years <- as.integer(format(source_dates, "%Y"))
quarters <- as.integer(format(source_dates, "%m")) %/% 3L
quarterly_flows <- source[, unname(ppnr_map), drop = FALSE]
for (i in seq_along(source_dates)) {
  if (quarters[i] > 1L) {
    prior <- which(years == years[i] & quarters == quarters[i] - 1L)
    stop_if(length(prior) != 1L,
            paste("Cannot convert YTD flow to quarterly value for", source_dates[i]))
    quarterly_flows[i, ] <- source[i, unname(ppnr_map), drop = FALSE] -
      source[prior, unname(ppnr_map), drop = FALSE]
  }
}
names(quarterly_flows) <- names(ppnr_map)
quarterly_flows <- data.frame(Date = format(source_dates, "%Y-%m-%d"),
                              quarterly_flows, check.names = FALSE)

balance_rows <- data.frame(
  Date = paste(years, paste0("Q", quarters)),
  source[, unname(balance_map), drop = FALSE],
  check.names = FALSE
)
names(balance_rows) <- c("Date", names(balance_map))

close_enough <- function(x, y) isTRUE(all.equal(as.numeric(x), as.numeric(y),
                                               tolerance = 0, check.attributes = FALSE))
row_sum <- function(data, cols) rowSums(data[, cols, drop = FALSE])
income_components <- names(ppnr_map)[1:12]
expense_components <- names(ppnr_map)[13:20]
noninterest_income_components <- names(ppnr_map)[21:35]
noninterest_expense_components <- names(ppnr_map)[36:40]
reconciliations <- list(
  "interest income components" = close_enough(row_sum(quarterly_flows, income_components),
                                                quarterly_flows[["Total interest income"]]),
  "interest expense components" = close_enough(row_sum(quarterly_flows, expense_components),
                                                  quarterly_flows[["Total interest expense"]]),
  "net interest income identity" = close_enough(
    quarterly_flows[["Total interest income"]] - quarterly_flows[["Total interest expense"]],
    quarterly_flows[["Net interest income"]]),
  "noninterest income components" = close_enough(
    row_sum(quarterly_flows, noninterest_income_components),
    quarterly_flows[["Total noninterest income"]]),
  "noninterest expense components" = close_enough(
    row_sum(quarterly_flows, noninterest_expense_components),
    quarterly_flows[["Total noninterest expense"]])
)
stop_if(!all(unlist(reconciliations)),
        paste("Y-9C component reconciliation failed:",
              paste(names(reconciliations)[!unlist(reconciliations)], collapse = ", ")))

update_output <- function(path, recent_rows, expected_columns) {
  stop_if(!file.exists(path), paste("Missing existing output:", path))
  old <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                  na.strings = c("", "NA"))
  stop_if(!identical(names(old), expected_columns),
          paste("Unexpected column names or order in", path))
  stop_if(anyDuplicated(old$Date) > 0L, paste("Duplicate dates in", path))
  is_balance_sheet <- grepl("Balance_Sheet", path, fixed = TRUE)
  recent_dates <- recent_rows$Date
  new_overlap <- recent_rows[recent_rows$Date %in% old$Date, , drop = FALSE]
  if (nrow(new_overlap)) {
    old_overlap <- old[match(new_overlap$Date, old$Date), , drop = FALSE]
    for (column in expected_columns[-1]) {
      stop_if(!close_enough(old_overlap[[column]], new_overlap[[column]]),
              paste("Existing value mismatch in", path, "column", column))
    }
  }
  missing_rows <- recent_rows[!recent_dates %in% old$Date, expected_columns, drop = FALSE]
  if (nrow(missing_rows)) {
    date_value <- function(labels) {
      if (!is_balance_sheet) return(as.Date(labels))
      as.Date(paste0(substr(labels, 1, 4), "-",
                     c(Q1 = "03-31", Q2 = "06-30", Q3 = "09-30", Q4 = "12-31")[
                       sub(".* ", "", labels)]))
    }
    stop_if(max(date_value(old$Date)) >= min(date_value(missing_rows$Date)),
            paste("New rows are not strictly after the existing data in", path,
                  "; refusing to reorder historical rows."))
  }
  missing_rows
}

ppnr_missing <- lapply(ppnr_files, function(path) {
  old_header <- names(read.csv(path, nrows = 0, check.names = FALSE))
  update_output(path, quarterly_flows, old_header)
})
names(ppnr_missing) <- ppnr_files
balance_header <- names(read.csv(balance_file, nrows = 0, check.names = FALSE))
balance_missing <- update_output(balance_file, balance_rows, balance_header)

append_missing_rows <- function(path, missing_rows) {
  if (!nrow(missing_rows)) return(invisible(NULL))
  is_balance_sheet <- grepl("Balance_Sheet", path, fixed = TRUE)
  old_options <- options(scipen = 999)
  on.exit(options(old_options), add = TRUE)
  # Append only missing observations, preserving all existing rows and the
  # project's established date conventions (plain ISO PPNR, quoted B/S labels).
  write.table(missing_rows, file = path, append = TRUE, sep = ",",
              row.names = FALSE, col.names = FALSE,
              quote = if (is_balance_sheet) 1L else FALSE, na = "NA")
}

# Validate every destination before changing any of them.
for (path in ppnr_files) append_missing_rows(path, ppnr_missing[[path]])
append_missing_rows(balance_file, balance_missing)

cat("Updated recent rows from official Morgan Stanley FR Y-9C source data.\n")
cat("Source coverage:", format(min(source_dates)), "to", format(max(source_dates)), "\n")
cat("PPNR by quarter ($000):\n")
print(data.frame(Date = quarterly_flows$Date,
                 PPNR = quarterly_flows[["Net interest income"]] +
                   quarterly_flows[["Total noninterest income"]] -
                   quarterly_flows[["Total noninterest expense"]]))
cat("Reconciliations passed:", paste(names(reconciliations), collapse = "; "), "\n")
cat("Rows appended:", paste(c(vapply(ppnr_missing, nrow, integer(1)),
                               nrow(balance_missing)), collapse = ", "), "\n")
