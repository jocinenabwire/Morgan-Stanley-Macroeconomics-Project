# Rebuild maintained PPNR and balance-sheet CSVs from the selected, provenance-
# tagged Morgan Stanley FR Y-9C source snapshot. Uses base R only.

source_file <- file.path("y9c_source", "Morgan_Stanley_Y9C_2009Q1_2026Q3_source.csv")
ppnr_file <- "Morgan_Stanley_PPNR_Quarterly_2009_2026.csv"
final_ppnr_file <- "Morgan_Stanley_PPNR_Quarterly_Final.csv"
balance_file <- "Morgan_Stanley_Balance_Sheet.csv"

stop_if <- function(condition, message) {
  if (condition) stop(message, call. = FALSE)
}

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

stop_if(!file.exists(source_file), paste("Missing source data:", source_file))
source <- read.csv(source_file, check.names = FALSE, stringsAsFactors = FALSE,
                   na.strings = c("", "NA"))
required_metadata <- c("ReportDate", "Institution", "RSSD_ID", "RetrievedOn", "SourceURL")
stop_if(!all(required_metadata %in% names(source)),
        "Source data is missing one or more provenance columns.")
report_dates <- as.Date(source$ReportDate)
quarter_index <- seq.int(2009L * 4L, 2026L * 4L + 2L)
years <- quarter_index %/% 4L
quarters <- quarter_index %% 4L + 1L
ends <- c("03-31", "06-30", "09-30", "12-31")[quarters]
expected_dates <- as.Date(paste(years, ends, sep = "-"))
stop_if(!identical(report_dates, expected_dates),
        "Source data must contain every quarter from 2009Q1 through 2026Q3, in order.")
stop_if(any(source$Institution != "MORGAN STANLEY") || any(source$RSSD_ID != 2162966),
        "Source data must contain only Morgan Stanley (RSSD ID 2162966).")
stop_if(anyDuplicated(report_dates) > 0L, "Duplicate reporting dates in source data.")

needed_codes <- unique(c(unname(ppnr_map), unname(balance_map)))
stop_if(!all(needed_codes %in% names(source)),
        paste("Missing Y-9C codes:", paste(setdiff(needed_codes, names(source)), collapse = ", ")))
for (code in needed_codes) {
  raw_value <- trimws(as.character(source[[code]]))
  parsed <- suppressWarnings(as.numeric(raw_value))
  stop_if(any(!is.na(raw_value) & nzchar(raw_value) & is.na(parsed)),
          paste("Non-numeric value in source field", code))
  source[[code]] <- parsed
}

# Income-statement observations in the report are year-to-date; convert them
# into standalone quarterly amounts by subtracting the previous quarter.
quarterly_flows <- source[, unname(ppnr_map), drop = FALSE]
for (i in seq_along(report_dates)) {
  if (quarters[i] > 1L) {
    prior <- i - 1L
    stop_if(years[prior] != years[i] || quarters[prior] != quarters[i] - 1L,
            paste("Cannot find prior YTD quarter for", report_dates[i]))
    quarterly_flows[i, ] <- source[i, unname(ppnr_map), drop = FALSE] -
      source[prior, unname(ppnr_map), drop = FALSE]
  }
}
names(quarterly_flows) <- names(ppnr_map)
ppnr <- data.frame(Date = format(report_dates, "%Y-%m-%d"), quarterly_flows,
                   check.names = FALSE)

balance <- data.frame(
  Date = paste(years, paste0("Q", quarters)),
  source[, unname(balance_map), drop = FALSE],
  check.names = FALSE
)
names(balance) <- c("Date", names(balance_map))
final_ppnr <- ppnr[report_dates >= as.Date("2010-03-31"), , drop = FALSE]

# Check reported identities wherever all inputs are present. Missing cells are
# preserved as NA because the reporting forms did not include every code in
# every historical period.
check_identity <- function(lhs, rhs, label) {
  complete <- !is.na(lhs) & !is.na(rhs)
  stop_if(any(lhs[complete] != rhs[complete]),
          paste("Reported identity failed:", label))
  sum(complete)
}
checked_n <- c(
  net_interest_income = check_identity(
    ppnr[["Total interest income"]] - ppnr[["Total interest expense"]],
    ppnr[["Net interest income"]], "interest income - expense = net interest income")
)
check_components <- function(component_names, total_name, label) {
  complete <- complete.cases(ppnr[, component_names, drop = FALSE]) &
    !is.na(ppnr[[total_name]])
  component_total <- rowSums(ppnr[complete, component_names, drop = FALSE])
  stop_if(any(component_total != ppnr[[total_name]][complete]),
          paste("Reported component subtotal failed:", label))
  sum(complete)
}
checked_n <- c(
  checked_n,
  interest_income_components = check_components(names(ppnr_map)[1:12],
                                                  "Total interest income",
                                                  "interest income components"),
  interest_expense_components = check_components(names(ppnr_map)[13:20],
                                                   "Total interest expense",
                                                   "interest expense components"),
  noninterest_income_components = check_components(names(ppnr_map)[21:35],
                                                    "Total noninterest income",
                                                    "noninterest income components"),
  noninterest_expense_components = check_components(names(ppnr_map)[36:40],
                                                     "Total noninterest expense",
                                                     "noninterest expense components")
)

# Do not overwrite any maintained file until all source and identity checks pass.
write_project_csv <- function(data, path, quote_dates = FALSE) {
  old_options <- options(scipen = 999)
  on.exit(options(old_options), add = TRUE)
  write.table(data, path, sep = ",", quote = FALSE, row.names = FALSE,
              col.names = TRUE, na = "NA")
  lines <- readLines(path, warn = FALSE)
  lines[1] <- paste0('"', names(data), '"', collapse = ",")
  if (quote_dates) lines[-1] <- sub("^([^,]*),", '"\\1",', lines[-1])
  writeLines(lines, path, useBytes = TRUE)
}
write_project_csv(ppnr, ppnr_file)
write_project_csv(final_ppnr, final_ppnr_file)
write_project_csv(balance, balance_file, quote_dates = TRUE)

cat("Rebuilt:", ppnr_file, nrow(ppnr), "rows;", final_ppnr_file,
    nrow(final_ppnr), "rows;", balance_file, nrow(balance), "rows.\n")
cat("Source coverage:", format(min(report_dates)), "through", format(max(report_dates)), "\n")
cat("Identity checks with observed values:", paste(names(checked_n), checked_n, collapse = "; "), "\n")
cat("Missing source fields retained as NA: BHCKHK03/HK04 in the early-form period where not reported.\n")
