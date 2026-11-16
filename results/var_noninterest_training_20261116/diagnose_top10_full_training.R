# Descriptive full-training diagnostics for the ten leading feasible non-interest VARs.
# This reads only the approved training outcomes and the macro source.

required_packages <- c("readr", "vars")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing R package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}
train_raw <- readr::read_csv(
  file.path("data_splits", "train_y9c_outcomes_2009Q1_2024Q2.csv"),
  show_col_types = FALSE
)
macro_raw <- readr::read_csv("2025-Table_1A_Historic_Domestic.csv", show_col_types = FALSE)
top10 <- utils::head(readr::read_csv(
  "results/var_noninterest_training_20261116/top50_feasible_joint_mase.csv",
  show_col_types = FALSE
), 10L)
specs <- readr::read_csv(
  "results/var_noninterest_training_20261116/candidate_specifications.csv",
  show_col_types = FALSE
)
quarter_from_date <- function(x) {
  d <- as.Date(x)
  paste0(format(d, "%Y"), " Q", ((as.integer(format(d, "%m")) - 1L) %/% 3L) + 1L)
}
train <- data.frame(
  Quarter = quarter_from_date(train_raw$Date),
  NIR = as.numeric(train_raw[["Total noninterest income"]]) / 1e6,
  NIE = as.numeric(train_raw[["Total noninterest expense"]]) / 1e6,
  stringsAsFactors = FALSE
)
macro <- data.frame(
  Quarter = trimws(as.character(macro_raw$Date)),
  Rate_3M = as.numeric(macro_raw[["3-month Treasury rate"]]),
  Rate_10Y = as.numeric(macro_raw[["10-year Treasury yield"]]),
  BBB_Yield = as.numeric(macro_raw[["BBB corporate yield"]]),
  VIX = as.numeric(macro_raw[["Market Volatility Index (Level)"]]),
  stringsAsFactors = FALSE
)
macro <- macro[match(train$Quarter, macro$Quarter), , drop = FALSE]
d <- cbind(train, macro[, c("Rate_3M", "Rate_10Y", "BBB_Yield", "VIX"), drop = FALSE])
d$Term_Spread <- d$Rate_10Y - d$Rate_3M
d$BBB_Spread <- d$BBB_Yield - d$Rate_10Y
d$d_NIR <- c(NA_real_, diff(d$NIR))
d$g_NIR <- c(NA_real_, diff(log(d$NIR)))
d$d_NIE <- c(NA_real_, diff(d$NIE))
d$g_NIE <- c(NA_real_, diff(log(d$NIE)))
d$d_Rate_3M <- c(NA_real_, diff(d$Rate_3M))
d$d_Rate_10Y <- c(NA_real_, diff(d$Rate_10Y))
d$d_Term_Spread <- c(NA_real_, diff(d$Term_Spread))
d$d_BBB_Spread <- c(NA_real_, diff(d$BBB_Spread))

max_vif_and_condition <- function(eq_fit) {
  mm <- stats::model.matrix(eq_fit)
  keep <- apply(mm, 2L, function(x) stats::sd(x) > 1e-10)
  x <- mm[, keep, drop = FALSE]
  vifs <- vapply(seq_len(ncol(x)), function(j) {
    y <- x[, j]
    others <- x[, -j, drop = FALSE]
    if (!ncol(others)) return(1)
    aux <- tryCatch(stats::lm.fit(cbind(1, others), y), error = function(e) NULL)
    if (is.null(aux)) return(Inf)
    r2 <- 1 - sum(aux$residuals^2) / sum((y - mean(y))^2)
    if (!is.finite(r2) || r2 >= 1 - 1e-10) Inf else 1 / (1 - r2)
  }, numeric(1))
  cc <- tryCatch(stats::cor(x), error = function(e) NA_real_)
  condition <- if (is.matrix(cc) && all(is.finite(cc))) tryCatch(kappa(cc), error = function(e) NA_real_) else NA_real_
  c(Max_VIF = max(vifs), Condition_Number = condition)
}
extract_p <- function(expr, inner) {
  x <- tryCatch(expr, error = function(e) NULL)
  if (is.null(x) || is.null(x[[inner]]) || is.null(x[[inner]]$p.value)) return(NA_real_)
  as.numeric(x[[inner]]$p.value)
}
rows <- lapply(top10$Model_ID, function(id) {
  s <- specs[specs$Model_ID == id, , drop = FALSE]
  endog <- trimws(strsplit(s$Endogenous_Variables[1L], "\\+", perl = TRUE)[[1L]])
  y <- as.data.frame(d[, endog, drop = FALSE])
  y <- y[stats::complete.cases(y), , drop = FALSE]
  fit <- tryCatch(vars::VAR(y, p = as.integer(s$P[1L]), type = "const"), error = function(e) NULL)
  if (is.null(fit)) return(data.frame(Model_ID = id, Full_Training_Fit_Failed = TRUE))
  eq_vif <- lapply(fit$varresult, max_vif_and_condition)
  roots <- tryCatch(max(Mod(vars::roots(fit))), error = function(e) NA_real_)
  normality <- tryCatch(vars::normality.test(fit, multivariate.only = TRUE), error = function(e) NULL)
  normality_p <- if (is.null(normality) || is.null(normality$jb.mul) ||
                     is.null(normality$jb.mul$JB$p.value)) NA_real_ else as.numeric(normality$jb.mul$JB$p.value)
  nir_column <- if (s$NIR_Transform[1L] == "NIR_D") "d_NIR" else "g_NIR"
  nie_column <- if (s$NIE_Transform[1L] == "NIE_D") "d_NIE" else "g_NIE"
  data.frame(
    Model_ID = id, Full_Training_N = nrow(y), P = as.integer(s$P[1L]),
    NIR_Adjusted_R2 = summary(fit$varresult[[nir_column]])$adj.r.squared,
    NIE_Adjusted_R2 = summary(fit$varresult[[nie_column]])$adj.r.squared,
    Max_Root_Modulus = roots, Roots_Stable = is.finite(roots) && roots < 1,
    Residual_Portmanteau_p = extract_p(vars::serial.test(fit, lags.pt = 8L, type = "PT.adjusted"), "serial"),
    Residual_Multivariate_ARCH_p = extract_p(vars::arch.test(fit, lags.multi = 4L), "arch.mul"),
    Residual_Normality_p = normality_p,
    Lagged_Regressor_Max_VIF = max(vapply(eq_vif, function(x) x[["Max_VIF"]], numeric(1)), na.rm = TRUE),
    Lagged_Regressor_Max_Condition_Number = max(vapply(eq_vif, function(x) x[["Condition_Number"]], numeric(1)), na.rm = TRUE),
    Full_Training_Fit_Failed = FALSE, stringsAsFactors = FALSE
  )
})
diagnostics <- do.call(rbind, rows)
readr::write_csv(
  diagnostics,
  "results/var_noninterest_training_20261116/top10_full_training_diagnostics.csv"
)
print(diagnostics, row.names = FALSE)
