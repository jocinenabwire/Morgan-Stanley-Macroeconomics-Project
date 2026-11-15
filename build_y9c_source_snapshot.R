# Rebuild the selected Morgan Stanley FR Y-9C source snapshot for 2009Q1-2026Q3.
# Historical quarterly FFIEC bulk archives are downloaded only when missing.
# The bulk ZIPs contain all holding companies and stay local; only Morgan
# Stanley's selected line items are written into the project source CSV.

archive_dir <- file.path("y9c_source", "ffiec_bulk_archives")
recent_file <- file.path("y9c_source", "Morgan_Stanley_Y9C_2025Q1_2026Q3_source.csv")
output_file <- file.path("y9c_source", "Morgan_Stanley_Y9C_2009Q1_2026Q3_source.csv")
rssd_id <- "2162966"
retrieval_date <- format(Sys.Date(), "%Y-%m-%d")

stop_if <- function(condition, message) {
  if (condition) stop(message, call. = FALSE)
}

quarter_ends <- function(from_year, from_quarter, to_year, to_quarter) {
  idx <- seq.int(from_year * 4L + from_quarter - 1L,
                 to_year * 4L + to_quarter - 1L)
  years <- idx %/% 4L
  quarters <- idx %% 4L + 1L
  month_day <- c("03-31", "06-30", "09-30", "12-31")[quarters]
  as.Date(paste(years, month_day, sep = "-"))
}

dates <- quarter_ends(2009L, 1L, 2026L, 3L)
historical_dates <- dates[dates <= as.Date("2024-12-31")]
historical_codes <- c(
  "BHCK4020", "BHCK4115", "BHCKB488", "BHCKB489", "BHCK4060", "BHCK4069",
  "BHCK4518", "BHCK4435", "BHCK4436", "BHCKF821", "BHCK4059", "BHCK4065",
  "BHCKHK03", "BHCKHK04", "BHCK6761", "BHCK4172", "BHCK4180", "BHCK4185",
  "BHCK4397", "BHCK4398", "BHCKA220", "BHCK4070", "BHCKC888", "BHCKB492",
  "BHCK4483", "BHCKB497", "BHCKC886", "BHCKC887", "BHCKC386", "BHCKC387",
  "BHCKB491", "BHCKB493", "BHCK8560", "BHCK8561", "BHCKB496", "BHCK4135",
  "BHCK4217", "BHCK4092", "BHCKC216", "BHCKC232", "BHCK4107", "BHCK4073",
  "BHCK4074", "BHCK4079", "BHCK4093", "BHCK0395", "BHCK0397", "BHCK1773",
  "BHCKB989", "BHCKB528", "BHCK3545", "BHDM6636", "BHFN6636", "BHCKB995",
  "BHCK3190", "BHCK4062", "BHCK3548", "BHCK2170", "BHCK2948", "BHCKD961"
)

dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
historical_rows <- vector("list", length(historical_dates))
names(historical_rows) <- format(historical_dates, "%Y-%m-%d")

for (i in seq_along(historical_dates)) {
  report_date <- historical_dates[i]
  date_key <- format(report_date, "%Y%m%d")
  archive_name <- paste0("BHCF", date_key, ".ZIP")
  archive_path <- file.path(archive_dir, archive_name)
  if (!file.exists(archive_path)) {
    url <- paste0("https://www.ffiec.gov/npw/FinancialReport/ReturnBHCFZipFiles?zipfilename=",
                  archive_name)
    download.file(url, archive_path, mode = "wb", quiet = TRUE)
  }
  stop_if(!file.exists(archive_path) || file.info(archive_path)$size == 0,
          paste("Missing or empty FFIEC archive:", archive_name))

  text_name <- paste0("BHCF", date_key, ".txt")
  connection <- unz(archive_path, text_name, open = "r")
  raw <- read.delim(connection, sep = "^", quote = "", check.names = FALSE,
                     colClasses = "character", comment.char = "",
                     na.strings = c("", "NA"), stringsAsFactors = FALSE)
  close(connection)
  stop_if(!all(c("RSSD9001", "RSSD9017", "RSSD9999") %in% names(raw)),
          paste("Required institution identifiers are absent from", archive_name))

  selected <- which(raw$RSSD9001 == rssd_id)
  stop_if(length(selected) != 1L,
          paste("Expected exactly one Morgan Stanley row in", archive_name,
                "but found", length(selected)))
  row <- raw[selected, , drop = FALSE]
  stop_if(row$RSSD9017 != "MORGAN STANLEY",
          paste("RSSD ID does not identify Morgan Stanley in", archive_name))
  stop_if(row$RSSD9999 != date_key,
          paste("Report date mismatch in", archive_name))

  values <- setNames(vector("list", length(historical_codes)), historical_codes)
  for (code in historical_codes) {
    value <- if (code %in% names(row)) row[[code]] else NA_character_
    value <- trimws(value)
    parsed <- suppressWarnings(as.numeric(value))
    stop_if(!is.na(value) && nzchar(value) && is.na(parsed),
            paste("Non-numeric", code, "in", archive_name, ":", value))
    values[[code]] <- parsed
  }
  source_url <- paste0("https://www.ffiec.gov/npw/FinancialReport/ReturnBHCFZipFiles?zipfilename=",
                       archive_name)
  historical_rows[[i]] <- data.frame(
    ReportDate = format(report_date, "%Y-%m-%d"),
    Institution = row$RSSD9017,
    RSSD_ID = as.integer(rssd_id),
    RetrievedOn = retrieval_date,
    SourceURL = source_url,
    as.data.frame(values, check.names = FALSE),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (i %% 8L == 0L || i == length(historical_dates)) {
    cat("Read", i, "of", length(historical_dates), "historical quarters\n")
  }
}

historical <- do.call(rbind, historical_rows)
stop_if(!file.exists(recent_file), paste("Missing recent source snapshot:", recent_file))
recent <- read.csv(recent_file, check.names = FALSE, stringsAsFactors = FALSE,
                   na.strings = c("", "NA"))
metadata <- c("ReportDate", "Institution", "RSSD_ID", "RetrievedOn", "SourceURL")
stop_if(!identical(names(historical), names(recent)),
        "Historical and recent source snapshots have different column sets or order.")
recent_dates <- as.Date(recent$ReportDate)
expected_recent_urls <- paste0(
  "https://www.ffiec.gov/npw/FinancialReport/ReturnFinancialReportCSV?",
  "rpt=FRY9C&id=2162966&dt=", format(recent_dates, "%Y%m%d")
)
stop_if(!identical(recent$SourceURL, expected_recent_urls),
        "A recent source URL does not match the official institution-specific FR Y-9C endpoint.")

combined <- rbind(historical, recent)
combined_dates <- as.Date(combined$ReportDate)
stop_if(anyNA(combined_dates) || anyDuplicated(combined_dates),
        "The combined source contains invalid or duplicate reporting quarters.")
stop_if(!identical(combined_dates, dates),
        "The combined source does not contain every quarter from 2009Q1 to 2026Q3.")
stop_if(any(combined$Institution != "MORGAN STANLEY") ||
          any(combined$RSSD_ID != as.integer(rssd_id)),
        "The combined source contains a row for another institution.")

for (code in setdiff(names(combined), metadata)) {
  raw_value <- trimws(as.character(combined[[code]]))
  parsed <- suppressWarnings(as.numeric(raw_value))
  stop_if(any(!is.na(raw_value) & nzchar(raw_value) & is.na(parsed)),
          paste("Non-numeric value found in source field", code))
  combined[[code]] <- parsed
}

previous_scipen <- getOption("scipen")
options(scipen = 999)
write.csv(combined, output_file, row.names = FALSE, na = "NA")
options(scipen = previous_scipen)
cat("Wrote", nrow(combined), "Morgan Stanley Y-9C quarters to", output_file, "\n")
cat("Coverage:", format(min(combined_dates)), "through", format(max(combined_dates)), "\n")
cat("Selected Y-9C line items:", length(setdiff(names(combined), metadata)), "\n")
