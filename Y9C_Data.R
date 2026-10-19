install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
library(dplyr)
library(tidyr)
library(lubridate)



# Import Y-9C data for 2024 Q4

data <- read.delim(
    "BHCF20241231.txt",
    sep = "^",
  header = TRUE,
  check.names = FALSE
)

View(data)
names(data)[grep("RSSD", names(data))]


# Find all quarterly Y-9C files

files <- list.files(
  path = "Y9C_Data",
  pattern = "\\.txt$",
  full.names = TRUE
)

length(files)


# Test: keep only Morgan Stanley from one quarter

test_data <- read.delim(
  files[1],
  sep = "^",
  header = TRUE,
  check.names = FALSE
)

morgan_test <- test_data[test_data$RSSD9001 == 2162966, ]
dim(morgan_test)


# Extract Morgan Stanley from every quarter

morgan_list <- lapply(files, function(file) {
  d <- read.delim(
    file,
    sep = "^",
    header = TRUE,
    check.names = FALSE
  )
  d[d$RSSD9001 == 2162966, ]
})

sapply(morgan_list, nrow)
files[sapply(morgan_list, nrow) == 0]


problem_data <- read.delim(
  "Y9C_Data/BHCF20110331.txt",
  sep = "^",
  header = TRUE,
  check.names = FALSE
)

dim(problem_data)
"RSSD9001" %in% names(problem_data)
head(problem_data$RSSD9001)


problem_data2 <- read.delim(
  "Y9C_Data/BHCF20110331.txt",
  sep = "^",
  header = TRUE,
  check.names = FALSE,
  quote = ""
)

dim(problem_data2)
problem_data2[problem_data2$RSSD9001 == 2162966, ]


# Extract Morgan Stanley from every quarter

morgan_list <- lapply(files, function(file) {
  d <- read.delim(
    file,
    sep = "^",
    header = TRUE,
    check.names = FALSE,
    quote = ""
  )
  d[d$RSSD9001 == 2162966, ]
})
sapply(morgan_list, nrow)


# Combine all Morgan Stanley quarters into one dataset

all_columns <- unique(unlist(lapply(morgan_list, names)))

morgan_list2 <- lapply(morgan_list, function(x) {
  missing_columns <- setdiff(all_columns, names(x))
  x[missing_columns] <- NA
  x[, all_columns]
})

morgan_data <- do.call(rbind, morgan_list2)
dim(morgan_data)


# Add quarter date from filenames

morgan_data$Date <- as.Date(
  sub(".*BHCF([0-9]{8})\\.txt", "\\1", files),
  format = "%Y%m%d"
)

# Put Date as the first column

morgan_data <- morgan_data[, c("Date", setdiff(names(morgan_data), "Date"))]
head(morgan_data$Date)
tail(morgan_data$Date)
unique(morgan_data$RSSD9001)


# Candidate PPNR variables to check

ppnr_codes <- c(
  "BHCK4020",
  "BHCK4115",
  "BHCKB488",
  "BHCKB489",
  "BHCK4060",
  "BHCK4069",
  "BHCK4518",
  "BHCKHK03",
  "BHCKHK04",
  "BHCK6761",
  "BHCK4172",
  "BHCK4180",
  "BHCK4185",
  "BHCK4397",
  "BHCK4398",
  "BHCKA220",
  "BHCK4070",
  "BHCKC888",
  "BHCKB492",
  "BHCK4483",
  "BHCKB497",
  "BHCK4135",
  "BHCK4217",
  "BHCK4092",
  "BHCK4107",
  "BHCK4073",
  "BHCK4074",
  "BHCK4079",
  "BHCK4093"
)

ppnr_codes %in% names(morgan_data)


# Check availability of each PPNR variable over time

coverage <- data.frame(
  Code = ppnr_codes,
  Available_Quarters = sapply(
    morgan_data[, ppnr_codes],
    function(x) sum(!is.na(x))
  )
)

coverage$Total_Quarters <- nrow(morgan_data)
coverage$Coverage_Percent <- round(
  100 * coverage$Available_Quarters / coverage$Total_Quarters,
  1
)

coverage
sapply(morgan_data[, ppnr_codes], class)


# Check coverage period of the two variables with missing observations

morgan_data[
  is.na(morgan_data$BHCKHK03) | is.na(morgan_data$BHCKHK04),
  c("Date", "BHCKHK03", "BHCKHK04")
]

morgan_data[
  !is.na(morgan_data$BHCKHK03) | !is.na(morgan_data$BHCKHK04),
  c("Date", "BHCKHK03", "BHCKHK04")
][1:5, ]


# Create clean Morgan Stanley PPNR research dataset

ppnr_data <- morgan_data[, c("Date", ppnr_codes)]

# Sort chronologically

ppnr_data <- ppnr_data[order(ppnr_data$Date), ]

# Reset row numbers

rownames(ppnr_data) <- NULL

# Check dimensions and first observations

dim(ppnr_data)
head(ppnr_data)


# Create PPNR variable dictionary

ppnr_dictionary <- data.frame(
  Code = ppnr_codes,
  stringsAsFactors = FALSE
)

View(ppnr_dictionary)


# Add readable variable names

ppnr_dictionary$Name <- c(
  "Interest income - Fed funds & reverse repos",
  "Interest income - Other securities",
  "Interest income - US Treasury & agency securities",
  "Interest income - Mortgage-backed securities",
  "Interest income - Other interest-bearing balances",
  "Interest income - Trading assets",
  "Interest income - Other",
  "Interest expense - Domestic time deposits",
  "Interest expense - Other domestic deposits",
  "Interest expense - Subordinated debt",
  "Interest expense - Foreign deposits",
  "Interest expense - Fed funds & repos",
  "Interest expense - Other borrowed money",
  "Interest expense - Trading liabilities",
  "Interest expense - Other",
  "Trading revenue",
  "Noninterest income - Fiduciary activities",
  "Noninterest income - Net servicing fees",
  "Noninterest income - Investment banking",
  "Noninterest income - Service charges on deposits",
  "Noninterest income - Other",
  "Compensation expense",
  "Fixed asset expense",
  "Other noninterest expense",
  "Total interest income",
  "Total interest expense",
  "Net interest income",
  "Total noninterest income",
  "Total noninterest expense"
)

View(ppnr_dictionary)


# Add broad PPNR categories

ppnr_dictionary$Category <- c(
  rep("Interest Income", 7),
  rep("Interest Expense", 8),
  "Trading Revenue",
  rep("Noninterest Income", 5),
  rep("Noninterest Expense", 3),
  "Total / Aggregate",
  "Total / Aggregate",
  "Total / Aggregate",
  "Total / Aggregate",
  "Total / Aggregate"
)

View(ppnr_dictionary)


# Create readable version of the PPNR dataset

ppnr_clean <- ppnr_data
names(ppnr_clean)[-1] <- ppnr_dictionary$Name
View(ppnr_clean)


# Convert YTD values into standalone quarterly values

ppnr_quarterly <- ppnr_clean
years <- format(ppnr_quarterly$Date, "%Y")
quarters <- format(ppnr_quarterly$Date, "%m")

for (j in 2:ncol(ppnr_quarterly)) {
  ytd <- ppnr_clean[[j]]
  quarterly <- ytd
  for (i in 2:nrow(ppnr_quarterly)) {
    # Q2, Q3 and Q4 = current YTD minus previous YTD
    if (years[i] == years[i - 1] && quarters[i] != "03") {
      quarterly[i] <- ytd[i] - ytd[i - 1]
    }
  }
  ppnr_quarterly[[j]] <- quarterly
}

View(ppnr_quarterly)


# Check accounting identity after quarterly conversion

identity_check <- with(
  ppnr_quarterly,
  `Total interest income` -
    `Total interest expense` -
    `Net interest income`
)

summary(identity_check)


# Summary statistics for main PPNR aggregates

main_ppnr <- ppnr_quarterly[, c(
  "Date",
  "Total interest income",
  "Total interest expense",
  "Net interest income",
  "Total noninterest income",
  "Total noninterest expense"
)]

summary(main_ppnr)


# Plot Net Interest Income over time

plot(
  main_ppnr$Date,
  main_ppnr$`Net interest income`,
  type = "l",
  xlab = "Date",
  ylab = "Net Interest Income",
  main = "Morgan Stanley - Quarterly Net Interest Income"
)

abline(h = 0, lty = 2)


# Export cleaned quarterly Morgan Stanley dataset

write.csv(
  ppnr_quarterly,
  "Morgan_Stanley_PPNR_Quarterly.csv",
  row.names = FALSE
)


# See all available variables first

names(ppnr_quarterly)

install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)

income_ranking <- ppnr_quarterly %>%
  select(-Date) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Average_Quarterly_Amount"
  ) %>%
    arrange(desc(Average_Quarterly_Amount))
income_ranking


# Find the interest-income variables

interest_income_vars <- ppnr_dictionary %>%
  filter(Category == "Interest Income") %>%
  pull(Name)

interest_income_vars


interest_income_ranking <- ppnr_quarterly %>%
  select(all_of(interest_income_vars)) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Interest_Income_Component",
    values_to = "Average_Quarterly_Income"
  ) %>%
  arrange(desc(Average_Quarterly_Income))

print(interest_income_ranking, n = Inf)


# Search column names for anything containing "trading"

grep(
  "trading",
  names(morgan_data),
  value = TRUE,
  ignore.case = TRUE
)
"BHCK3545" %in% names(morgan_data)
morgan_data[, c("Date", "BHCK3545")]
trading_analysis <- morgan_data %>%
  select(Date, BHCK3545) %>%
  rename(Trading_Assets_End = BHCK3545) %>%
  arrange(Date) %>%
  mutate(
    Trading_Assets_Beginning = lag(Trading_Assets_End)
  ) %>%
  left_join(
    ppnr_quarterly %>%
      select(Date, `Interest income - Trading assets`) %>%
      rename(
        Trading_Interest_Income = `Interest income - Trading assets`
      ),
    by = "Date"
  ) %>%
  mutate(
    Trading_Asset_Yield =
      (Trading_Interest_Income / Trading_Assets_Beginning) * 100
  ) %>%
  select(
    Date,
    Trading_Assets_Beginning,
    Trading_Interest_Income,
    Trading_Asset_Yield
  )


print(trading_analysis, n = Inf)


View(trading_analysis)


head(trading_analysis, 10)

summary(trading_analysis)


plot(
  trading_analysis$Date,
  trading_analysis$Trading_Asset_Yield,
  type = "l",
  lwd = 2,
  xlab = "Year",
  ylab = "Interest income / beginning trading assets (%)",
  main = "Morgan Stanley - Trading Asset Interest Yield"
)


plot(
  trading_analysis$Date,
  trading_analysis$Trading_Assets_Beginning,
  type = "l",
  lwd = 2,
  xlab = "Year",
  ylab = "Beginning trading assets",
  main = "Morgan Stanley - Trading Assets"
)


plot(
  trading_analysis$Date,
  trading_analysis$Trading_Interest_Income,
  type = "l",
  lwd = 2,
  xlab = "Year",
  ylab = "Quarterly interest income",
  main = "Morgan Stanley - Interest Income from Trading Assets"
)


write.csv(
  ppnr_quarterly,
  "Morgan_Stanley_PPNR_Quarterly.csv",
  row.names = FALSE
)


write.csv(
  trading_analysis,
  "Morgan_Stanley_Trading_Asset_Analysis.csv",
  row.names = FALSE
)


ppnr_quarterly %>%
  filter(Date == as.Date("2026-06-30")) %>%
  select(
    Date,
    `Net interest income`,
    `Total noninterest income`,
    `Total noninterest expense`
  )


ppnr_quarterly %>%
  filter(Date == as.Date("2026-03-31")) %>%
  mutate(
    PPNR = (
      `Net interest income` +
        `Total noninterest income` -
        `Total noninterest expense`
    ) / 1000
  ) %>%
  select(Date, PPNR)


ppnr_check <- ppnr_quarterly %>%
  filter(Date %in% as.Date(c("2026-03-31", "2026-06-30"))) %>%
  transmute(
    Date,
    Net_Interest_Income = `Net interest income` / 1000,
    Noninterest_Income = `Total noninterest income` / 1000,
    Noninterest_Expense = `Total noninterest expense` / 1000,
    Net_Revenues = Net_Interest_Income + Noninterest_Income,
    PPNR = Net_Revenues - Noninterest_Expense
  )

print(ppnr_check)


print(ppnr_dictionary, row.names = FALSE)


interest_income_check <- ppnr_quarterly %>%
  filter(Date %in% as.Date(c("2026-03-31", "2026-06-30"))) %>%
  mutate(
    Sum_Components =
      `Interest income - Fed funds & reverse repos` +
      `Interest income - Other securities` +
      `Interest income - US Treasury & agency securities` +
      `Interest income - Mortgage-backed securities` +
      `Interest income - Other interest-bearing balances` +
      `Interest income - Trading assets` +
      `Interest income - Other`,
    Reported_Total = `Total interest income`,
    Difference = Sum_Components - Reported_Total
  ) %>%
    select(Date, Sum_Components, Reported_Total, Difference)

print(interest_income_check)

"BHCK4010" %in% names(morgan_data)
grep("4010", names(morgan_data), value = TRUE)
grep("^BHCK40", names(morgan_data), value = TRUE)


missing_interest_codes <- c(
  "BHCK4435",
  "BHCK4436",
  "BHCKF821",
  "BHCK4059",
  "BHCK4065"
)

data.frame(
  Code = missing_interest_codes,
  Available = missing_interest_codes %in% names(morgan_data)
)


missing_interest_quarterly <- morgan_data %>%
  select(Date, all_of(missing_interest_codes)) %>%
  arrange(Date) %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  mutate(
    across(
      all_of(missing_interest_codes),
      ~ .x - lag(.x, default = 0)
      )
  ) %>%
  ungroup() %>%
  select(-Year)

tail(missing_interest_quarterly)


ppnr_dictionary <- ppnr_dictionary %>%
  mutate(
    Name = case_when(
      Code == "BHCK4115" ~ "Interest income - Depository institution balances",
      Code == "BHCK4060" ~ "Interest income - Other securities",
      TRUE ~ Name
    )
  )

print(ppnr_dictionary, row.names = FALSE)


ppnr_quarterly <- ppnr_quarterly %>%
  rename(
    `Interest income - Depository institution balances` =
      `Interest income - Other securities`,
    `Interest income - Other securities` =
      `Interest income - Other interest-bearing balances`
  )


new_interest_dictionary <- data.frame(
  Code = c("BHCK4435", "BHCK4436", "BHCKF821",
           "BHCK4059", "BHCK4065"),
  Name = c(
    "Interest income - Residential real estate loans",
    "Interest income - Other real estate loans",
    "Interest income - Other domestic loans",
    "Interest income - Foreign loans",
    "Interest income - Lease financing"
  ),
  Category = rep("Interest Income", 5)
)

ppnr_dictionary <- bind_rows(
  ppnr_dictionary,
  new_interest_dictionary
)


ppnr_quarterly <- ppnr_quarterly %>%
  left_join(missing_interest_quarterly, by = "Date")



interest_codes <- ppnr_dictionary %>%
  filter(Category == "Interest Income") %>%
  pull(Code)
interest_names <- ppnr_dictionary %>%
  filter(Category == "Interest Income") %>%
  pull(Name)

# Five new variables still have their BHCK codes as column names

interest_columns <- ifelse(
  interest_codes %in% names(ppnr_quarterly),
  interest_codes,
  interest_names
)

interest_income_check_updated <- ppnr_quarterly %>%
  filter(Date %in% as.Date(c("2026-03-31", "2026-06-30"))) %>%
  mutate(
    Sum_Components = rowSums(
      pick(all_of(interest_columns))
    ),
    Reported_Total = `Total interest income`,
    Difference = Sum_Components - Reported_Total
  ) %>%
  select(Date, Sum_Components, Reported_Total, Difference)

print(interest_income_check_updated)



ppnr_quarterly <- ppnr_quarterly %>%
  rename(
    `Interest income - Residential real estate loans` = BHCK4435,
    `Interest income - Other real estate loans` = BHCK4436,
    `Interest income - Other domestic loans` = BHCKF821,
    `Interest income - Foreign loans` = BHCK4059,
    `Interest income - Lease financing` = BHCK4065
  ) %>%
  relocate(
    starts_with("Interest income -"),
    .after = Date
  )

names(ppnr_quarterly)[1:13]


ppnr_quarterly %>%
  filter(Date %in% as.Date(c("2026-03-31", "2026-06-30"))) %>%
  mutate(
    Sum_Components = rowSums(
      pick(starts_with("Interest income -"))
    ),
    Reported_Total = `Total interest income`,
    Difference = Sum_Components - Reported_Total
  ) %>%
  select(Date, Sum_Components, Reported_Total, Difference) %>%
  print()


ppnr_quarterly %>%
  filter(Date %in% as.Date(c("2026-03-31", "2026-06-30"))) %>%
  mutate(
    Sum_Components = rowSums(
      pick(starts_with("Interest expense -"))
    ),
    Reported_Total = `Total interest expense`,
    Difference = Sum_Components - Reported_Total
  ) %>%
  select(Date, Sum_Components, Reported_Total, Difference) %>%
  print()


ppnr_quarterly <- ppnr_quarterly %>%
  rename(
    `Interest expense - Domestic time deposits <=250k` =
      `Interest expense - Domestic time deposits`,
    `Interest expense - Domestic time deposits >250k` =
      `Interest expense - Other domestic deposits`,
    `Interest expense - Other domestic deposits` =
      `Interest expense - Subordinated debt`,
    `Interest expense - Subordinated debt & convertible securities` =
      `Interest expense - Trading liabilities`
  )


names(ppnr_quarterly)[grepl("^Interest expense -", names(ppnr_quarterly))]


ppnr_quarterly %>%
  filter(Date %in% as.Date(c("2026-03-31", "2026-06-30"))) %>%
  mutate(
    Sum_Components = rowSums(
      pick(starts_with("Interest expense -"))
    ),
    Reported_Total = `Total interest expense`,
    Difference = Sum_Components - Reported_Total
  ) %>%
  select(Date, Sum_Components, Reported_Total, Difference) %>%
  print()


ppnr_quarterly <- ppnr_quarterly %>%
  rename(
    `Noninterest income - Trading revenue` = `Trading revenue`
  )


ppnr_dictionary <- ppnr_dictionary %>%
  mutate(
    Name = if_else(
      Code == "BHCKA220",
      "Noninterest income - Trading revenue",
      Name
    ),
    Category = if_else(
      Code == "BHCKA220",
      "Noninterest Income",
      Category
    )
  )


# Variables in ppnr_quarterly (excluding Date)
quarterly_names <- setdiff(names(ppnr_quarterly), "Date")

# Variable names recorded in our dictionary
dictionary_names <- ppnr_dictionary$Name

# Check both directions
data.frame(
  Check = c(
    "Variables in ppnr_quarterly",
    "Variables in ppnr_dictionary",
    "Missing from dictionary",
    "Missing from ppnr_quarterly",
    "Duplicate dictionary names"
  ),
  Result = c(
    length(quarterly_names),
    length(dictionary_names),
    length(setdiff(quarterly_names, dictionary_names)),
    length(setdiff(dictionary_names, quarterly_names)),
    sum(duplicated(dictionary_names))
  )
)



# Names in ppnr_quarterly that are NOT in the dictionary
setdiff(
  names(ppnr_quarterly)[names(ppnr_quarterly) != "Date"],
  ppnr_dictionary$Name
)

# Names in the dictionary that are NOT in ppnr_quarterly
setdiff(
  ppnr_dictionary$Name,
  names(ppnr_quarterly)
)


ppnr_dictionary <- ppnr_dictionary %>%
  mutate(
    Name = case_when(
      Code == "BHCKHK03" ~ "Interest expense - Domestic time deposits <=250k",
      Code == "BHCKHK04" ~ "Interest expense - Domestic time deposits >250k",
      Code == "BHCK4397" ~ "Interest expense - Subordinated debt & convertible securities",
      TRUE ~ Name
    )
  )


# Names in ppnr_quarterly missing from dictionary
setdiff(
  names(ppnr_quarterly)[names(ppnr_quarterly) != "Date"],
  ppnr_dictionary$Name
)

# Names in dictionary missing from ppnr_quarterly
setdiff(
  ppnr_dictionary$Name,
  names(ppnr_quarterly)
)


ppnr_dictionary <- ppnr_dictionary %>%
  mutate(
    Name = if_else(
      Code == "BHCK6761",
      "Interest expense - Other domestic deposits",
      Name
    )
  )


# Names in ppnr_quarterly missing from dictionary
setdiff(
  names(ppnr_quarterly)[names(ppnr_quarterly) != "Date"],
  ppnr_dictionary$Name
)

# Names in dictionary missing from ppnr_quarterly
setdiff(
  ppnr_dictionary$Name,
  names(ppnr_quarterly)
)


ppnr_quarterly %>%
  filter(Date %in% as.Date(c("2026-03-31", "2026-06-30"))) %>%
  mutate(
    Sum_Components = rowSums(
      pick(starts_with("Noninterest income -"))
    ),
    Reported_Total = `Total noninterest income`,
    Difference = Sum_Components - Reported_Total
  ) %>%
  select(Date, Sum_Components, Reported_Total, Difference) %>%
  print()


# Correct names in ppnr_quarterly
names(ppnr_quarterly) <- dplyr::recode(
  names(ppnr_quarterly),
  "Noninterest income - Net servicing fees" =
    "Noninterest income - Investment banking",
  "Noninterest income - Investment banking" =
    "Noninterest income - Net servicing fees"
)

# Correct names in ppnr_dictionary
ppnr_dictionary <- ppnr_dictionary %>%
  mutate(
    Name = case_when(
      Code == "BHCKC888" ~ "Noninterest income - Investment banking",
      Code == "BHCKB492" ~ "Noninterest income - Net servicing fees",
      TRUE ~ Name
    )
  )

setdiff(names(ppnr_quarterly)[-1], ppnr_dictionary$Name)
setdiff(ppnr_dictionary$Name, names(ppnr_quarterly))


# Missing noninterest income codes from official Schedule HI
missing_nii_codes <- c(
  "BHCKC886", "BHCKC887", "BHCKC386",
  "BHCKC387", "BHCKKX46", "BHCKKX47",
  "BHCKB491", "BHCKB493", "BHCK8560",
  "BHCK8561", "BHCKB496"
)

# Check availability in our original Y-9C dataset
data.frame(
  Code = missing_nii_codes,
  Available = missing_nii_codes %in% names(morgan_data)
)


# Official noninterest income codes to add
new_income_codes <- c(
  "BHCKC886", "BHCKC887", "BHCKC386",
  "BHCKC387", "BHCKB491", "BHCKB493",
  "BHCK8560", "BHCK8561", "BHCKB496"
)

# Extract the year-to-date values
new_income_quarterly <- morgan_data %>%
  select(Date, all_of(new_income_codes)) %>%
  arrange(Date) %>%
  mutate(Year = as.integer(format(Date, "%Y"))) %>%
  group_by(Year) %>%
  mutate(
    across(
      all_of(new_income_codes),
      ~ .x - lag(.x, default = 0)
    )
  ) %>%
  ungroup() %>%
  select(-Year)

# Check date formats
class(morgan_data$Date)
class(ppnr_quarterly$Date)

# Check whether both datasets contain identical dates
setequal(morgan_data$Date, ppnr_quarterly$Date)

# Check for duplicate dates
anyDuplicated(morgan_data$Date)
anyDuplicated(ppnr_quarterly$Date)


sapply(
  morgan_data[missing_nii_codes],
  class
)


morgan_data %>%
  filter(Date >= as.Date("2025-12-31")) %>%
  select(
    Date,
    BHCKC886,
    BHCK4079
  ) %>%
  arrange(Date) %>%
  print()



# Assign official names to the 9 new noninterest income variables
new_income_names <- c(
  BHCKC886 = "Noninterest income - Securities brokerage",
  BHCKC887 = "Noninterest income - Annuity sales",
  BHCKC386 = "Noninterest income - Insurance underwriting",
  BHCKC387 = "Noninterest income - Other insurance activities",
  BHCKB491 = "Noninterest income - Venture capital",
  BHCKB493 = "Noninterest income - Net securitization",
  BHCK8560 = "Noninterest income - Gains on sales of loans and leases",
  BHCK8561 = "Noninterest income - Gains on sales of other real estate",
  BHCKB496 = "Noninterest income - Gains on sales of other assets"
)

# Rename columns in our prepared quarterly dataset
names(new_income_quarterly)[-1] <- unname(new_income_names[new_income_codes])


# Add the 9 missing variables to our main quarterly dataset
ppnr_quarterly <- ppnr_quarterly %>%
  left_join(new_income_quarterly, by = "Date")


# Add the 9 new variables to our dictionary
ppnr_dictionary <- ppnr_dictionary %>%
  bind_rows(
    data.frame(
      Code = new_income_codes,
      Name = unname(new_income_names[new_income_codes]),
      Category = "Noninterest Income"
    )
  )



# Reconcile noninterest income for Q1 and Q2 2026

ppnr_quarterly %>%
  filter(Date %in% as.Date(c("2026-03-31", "2026-06-30"))) %>%
  mutate(
    Sum_Components = rowSums(
      pick(starts_with("Noninterest income -"))
    ),
    Reported_Total = `Total noninterest income`,
    Difference = Sum_Components - Reported_Total
  ) %>%
  select(Date, Sum_Components, Reported_Total, Difference) %>%
  print()



# Reconcile noninterest expense for Q1 and Q2 2026

ppnr_quarterly %>%
  filter(Date %in% as.Date(c("2026-03-31", "2026-06-30"))) %>%
  mutate(
    Sum_Components = rowSums(
      pick(starts_with("Noninterest expense -"))
    ),
    Reported_Total = `Total noninterest expense`,
    Difference = Sum_Components - Reported_Total
  ) %>%
  select(Date, Sum_Components, Reported_Total, Difference) %>%
  print()


# Export current quarterly PPNR dataset to CSV

write.csv(
  ppnr_quarterly,
  file = "ppnr_quarterly.csv",
  row.names = FALSE
)


# Rename noninterest expense variables

ppnr_quarterly <- ppnr_quarterly %>%
  rename(
    `Noninterest expense - Salaries and employee benefits` = `Compensation expense`,
    `Noninterest expense - Premises and fixed assets` = `Fixed asset expense`,
    `Noninterest expense - Other` = `Other noninterest expense`
  )


# Update noninterest expense names in dictionary

ppnr_dictionary <- ppnr_dictionary %>%
  mutate(
    Name = case_when(
      Code == "BHCK4135" ~ "Noninterest expense - Salaries and employee benefits",
      Code == "BHCK4217" ~ "Noninterest expense - Premises and fixed assets",
      Code == "BHCK4092" ~ "Noninterest expense - Other",
      TRUE ~ Name
    )
  )


# Organize quarterly dataset by financial category

ppnr_quarterly <- ppnr_quarterly %>%
  select(
    Date,
    starts_with("Interest income -"),
    starts_with("Interest expense -"),
    starts_with("Noninterest income -"),
    starts_with("Noninterest expense -"),
    everything()
  )


names(ppnr_quarterly)


# Reconcile noninterest expense for Q1 and Q2 2026

ppnr_quarterly %>%
  filter(Date %in% as.Date(c("2026-03-31", "2026-06-30"))) %>%
  mutate(
    Sum_Components = rowSums(
      pick(starts_with("Noninterest expense -"))
    ),
    Reported_Total = `Total noninterest expense`,
    Difference = Sum_Components - Reported_Total
  ) %>%
  select(Date, Sum_Components, Reported_Total, Difference) %>%
  print()



# Check availability of missing noninterest expense variables
missing_expense_codes <- c("BHCKC216", "BHCKC232")
data.frame(
  Code = missing_expense_codes,
  Available = missing_expense_codes %in% names(morgan_data)
)


# Inspect missing expense components for 2026
morgan_data %>%
  filter(Date %in% as.Date(c("2026-03-31", "2026-06-30"))) %>%
  select(Date, BHCKC216, BHCKC232) %>%
  print()



# Convert missing noninterest expense variables from YTD to quarterly
new_expense_codes <- c("BHCKC216", "BHCKC232")
new_expense_quarterly <- morgan_data %>%
  select(Date, all_of(new_expense_codes)) %>%
  arrange(Date) %>%
  mutate(Year = as.integer(format(Date, "%Y"))) %>%
  group_by(Year) %>%
  mutate(
    across(
      all_of(new_expense_codes),
      ~ .x - lag(.x, default = 0)
    )
  ) %>%
  ungroup() %>%
  select(-Year)


# Assign names to the two new noninterest expense variables
new_expense_names <- c(
  BHCKC216 = "Noninterest expense - Goodwill impairment",
  BHCKC232 = "Noninterest expense - Other intangible assets"
)
names(new_expense_quarterly)[-1] <-
  unname(new_expense_names[new_expense_codes])


# Add the two variables to our main quarterly dataset
ppnr_quarterly <- ppnr_quarterly %>%
    left_join(new_expense_quarterly, by = "Date")


# Organize columns by category again

ppnr_quarterly <- ppnr_quarterly %>%
  select(
    Date,
    starts_with("Interest income -"),
    starts_with("Interest expense -"),
    starts_with("Noninterest income -"),
    starts_with("Noninterest expense -"),
    everything()
  )


# Add the two variables to our dictionary
ppnr_dictionary <- ppnr_dictionary %>%
  bind_rows(
    data.frame(
      Code = new_expense_codes,
      Name = unname(new_expense_names[new_expense_codes]),
      Category = "Noninterest Expense"
    )
  )


# Final reconciliation of noninterest expense
ppnr_quarterly %>%
  filter(Date %in% as.Date(c("2026-03-31", "2026-06-30"))) %>%
  mutate(
    Sum_Components = rowSums(
      pick(starts_with("Noninterest expense -"))
    ),
    Reported_Total = `Total noninterest expense`,
    Difference = Sum_Components - Reported_Total
  ) %>%
  select(Date, Sum_Components, Reported_Total, Difference) %>%
  print()


# Verify Net Interest Income for Q1 and Q2 2026

ppnr_quarterly %>%
  filter(Date %in% as.Date(c("2026-03-31", "2026-06-30"))) %>%
  mutate(
    Calculated_NII = `Total interest income` - `Total interest expense`,
    Reported_NII = `Net interest income`,
    Difference = Calculated_NII - Reported_NII
  ) %>%
  select(Date, Calculated_NII, Reported_NII, Difference) %>%
  print()


# Export updated quarterly PPNR dataset
write.csv(
  ppnr_quarterly,
  file = "Morgan_Stanley_PPNR_Quarterly_Final.csv",
  row.names = FALSE
)



# Calculate PPNR and compare with Morgan Stanley earnings releases
ppnr_quarterly %>%
  filter(Date %in% as.Date(c("2026-03-31", "2026-06-30"))) %>%
  mutate(
    PPNR_Y9C = (
      `Net interest income` +
        `Total noninterest income` -
        `Total noninterest expense`
    ) / 1000,
    PPNR_Earnings = c(7109, 7446),
    Difference = PPNR_Y9C - PPNR_Earnings,
    Difference_Percent = round(
      100 * Difference / PPNR_Earnings, 2
    )
  ) %>%
  select(
    Date,
    PPNR_Y9C,
    PPNR_Earnings,
    Difference,
    Difference_Percent
  ) %>%
  print()


# Compare Y-9C data with Morgan Stanley's official earnings release

# All values in USD millions

# Official Morgan Stanley figures (Q1 and Q2 2026)
ms_official <- data.frame(
  Date = as.Date(c("2026-03-31", "2026-06-30")),
  `Interest income` = c(15273, 15902),
  `Interest expense` = c(12570, 13122),
  `Net interest income` = c(2703, 2780),
  `Noninterest income` = c(17877, 18568),
  `Noninterest expense` = c(13471, 13902),
  `Total PPNR` = c(7109, 7446),
  check.names = FALSE
)

# Extract the corresponding Y-9C figures
y9c_values <- ppnr_quarterly %>%
  filter(Date %in% ms_official$Date) %>%
  transmute(
    Date,
    `Interest income` = `Total interest income` / 1000,
    `Interest expense` = `Total interest expense` / 1000,
    `Net interest income` = `Net interest income` / 1000,
    `Noninterest income` = `Total noninterest income` / 1000,
    `Noninterest expense` = `Total noninterest expense` / 1000,
    `Total PPNR` = (
      `Total interest income` -
        `Total interest expense` +
        `Total noninterest income` -
        `Total noninterest expense`
    ) / 1000
  )

# Create comparison table
ppnr_comparison <- y9c_values %>%
  pivot_longer(
    cols = -Date,
    names_to = "Variable",
    values_to = "Y9C"
  ) %>%
  left_join(
    ms_official %>%
      pivot_longer(
        cols = -Date,
        names_to = "Variable",
        values_to = "Morgan_Stanley"
      ),
    by = c("Date", "Variable")
  ) %>%
  mutate(
    Difference = Y9C - Morgan_Stanley,
    Difference_Percent = round(
      100 * Difference / Morgan_Stanley, 2
    )
  ) %>%
  arrange(Date)
print(ppnr_comparison, n = Inf)



# Compare total net revenues across the two reporting sources
ppnr_comparison %>%
  filter(Variable %in% c("Net interest income", "Noninterest income")) %>%
  group_by(Date) %>%
  summarise(
    Y9C_Net_Revenues = sum(Y9C),
    MS_Net_Revenues = sum(Morgan_Stanley),
    Difference = Y9C_Net_Revenues - MS_Net_Revenues,
    Difference_Percent = round(
      100 * Difference / MS_Net_Revenues, 2
    ),
    .groups = "drop"
  ) %>%
  print()


# Export comparison with Morgan Stanley's official earnings releases

# All financial values in USD millions

write.csv(
  ppnr_comparison,
  "Morgan_Stanley_Y9C_vs_Official_2026.csv",
  row.names = FALSE
)



# Create final comparison table with clear units and definitions
ppnr_comparison_final <- ppnr_comparison %>%
  bind_rows(
    ppnr_comparison %>%
      filter(Variable %in% c("Net interest income", "Noninterest income")) %>%
      group_by(Date) %>%
      summarise(
        Variable = "Total net revenues",
        Y9C = sum(Y9C),
        Morgan_Stanley = sum(Morgan_Stanley),
        Difference = Y9C - Morgan_Stanley,
        Difference_Percent = round(100 * Difference / Morgan_Stanley, 2),
        .groups = "drop"
      )
  ) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        "Interest income",
        "Interest expense",
        "Net interest income",
        "Noninterest income",
        "Noninterest expense",
        "Total net revenues",
        "Total PPNR"
      )
    )
  ) %>%
  arrange(Date, Variable) %>%
  mutate(Variable = as.character(Variable)) %>%
  rename(
    `Y9C (USD millions)` = Y9C,
    `Morgan Stanley (USD millions)` = Morgan_Stanley,
    `Difference (USD millions)` = Difference,
    `Difference (%)` = Difference_Percent
  )

View(ppnr_comparison_final)



# Revenue classification differences: Y-9C vs Morgan Stanley
# Positive = Y-9C reports more than Morgan Stanley
# Negative = Y-9C reports less than Morgan Stanley
# All values in USD millions
revenue_differences <- ppnr_comparison_final %>%
  filter(Variable %in% c(
    "Interest income",
    "Noninterest income",
    "Total net revenues"
  )) %>%
  select(
    Date,
    Variable,
    `Difference (USD millions)`
  ) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        "Interest income",
        "Noninterest income",
        "Total net revenues"
      )
    )
  ) %>%
  arrange(Date, Variable) %>%
  mutate(Variable = as.character(Variable))

View(revenue_differences)



# Export final PPNR comparison with official Morgan Stanley figures
# All financial values in USD millions
write.csv(
  ppnr_comparison_final,
  "Morgan_Stanley_PPNR_Comparison_2026.csv",
  row.names = FALSE
)

# Export revenue differences (classification comparison)
# All financial values in USD millions
write.csv(
  revenue_differences,
  "Morgan_Stanley_Revenue_Differences_2026.csv",
  row.names = FALSE
)



# Inspect individual interest income components
# Q1 and Q2 2026, USD millions
ppnr_quarterly %>%
  filter(Date %in% as.Date(c("2026-03-31", "2026-06-30"))) %>%
  select(Date, starts_with("Interest income -")) %>%
  pivot_longer(
    cols = -Date,
    names_to = "Component",
    values_to = "Amount"
  ) %>%
  mutate(Amount = Amount / 1000) %>%
  pivot_wider(
    names_from = Date,
    values_from = Amount
  ) %>%
  print(n = Inf)
