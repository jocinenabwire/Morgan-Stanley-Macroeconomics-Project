# Run from ppnr_domestic; the main script handles its location automatically.
manifest <- read.csv("data/source_manifest.csv", stringsAsFactors = FALSE)
stopifnot(all(file.exists(manifest$relative_path)),
          identical(unname(tools::md5sum(manifest$relative_path)), manifest$md5))
dictionary <- read.csv("data/domestic_variables.csv", stringsAsFactors = FALSE)
raw <- read.csv("data/2025-Table_1A_Historic_Domestic.csv", check.names = FALSE,
                stringsAsFactors = FALSE, na.strings = c("", "NA"))
stopifnot(nrow(raw) == 196, ncol(raw) == 18, nrow(dictionary) == 16,
          identical(names(raw)[-(1:2)], dictionary$source_column),
          all(raw[["Scenario Name"]] == "Actual"), !anyDuplicated(raw$Date),
          all(grepl("^us_", dictionary$r_name)))
year <- as.integer(substr(raw$Date, 1, 4))
quarter <- as.integer(sub(".*Q", "", raw$Date))
stopifnot(all(diff(year * 4 + quarter) == 1))
fed_macro_2025 <- data.frame(quarter = raw$Date,
  quarter_end = as.Date(paste0(year, c("-03-31", "-06-30", "-09-30", "-12-31")[quarter])),
  data_status = "historical_as_published", raw[dictionary$source_column], check.names = FALSE)
names(fed_macro_2025)[4:19] <- dictionary$r_name
stopifnot(identical(range(fed_macro_2025$quarter_end), as.Date(c("1976-03-31", "2024-12-31"))))

company <- read.csv("../Morgan_Stanley_PPNR_Quarterly_Final.csv", stringsAsFactors = FALSE)
company$Date <- as.Date(company$Date)
company <- company[order(company$Date), ]
stopifnot(nrow(company) == 66, !anyNA(company$Date), !anyDuplicated(company$Date))
index <- match(company$Date, fed_macro_2025$quarter_end)
ms_macro_2025 <- cbind(company,
  quarter = paste(format(company$Date, "%Y"),
    paste0("Q", as.integer(format(company$Date, "%m")) / 3)),
  macro_matched = !is.na(index), fed_macro_2025[index, dictionary$r_name, drop = FALSE])
rownames(ms_macro_2025) <- NULL
stopifnot(sum(ms_macro_2025$macro_matched) == 60,
  all(is.finite(as.matrix(ms_macro_2025[ms_macro_2025$macro_matched, dictionary$r_name]))),
  all(is.na(as.matrix(ms_macro_2025[!ms_macro_2025$macro_matched, dictionary$r_name]))))
