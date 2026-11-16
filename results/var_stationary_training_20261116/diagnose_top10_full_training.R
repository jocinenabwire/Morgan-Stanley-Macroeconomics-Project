# Descriptive full-training diagnostics for the ten leading feasible VARs.
# This helper reads training inputs and saved model specifications only.
# It does not read the Y-9C holdout/test outcome file.

required_packages <- c("readr", "vars", "urca", "tseries")
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
top10 <- utils::head(readr::read_csv("results/var_stationary_training_20261116/top50_feasible_joint_mase.csv",
                                     show_col_types = FALSE), 10L)
specs <- readr::read_csv("results/var_stationary_training_20261116/candidate_specifications.csv",
                         show_col_types = FALSE)

quarter_from_date <- function(x) {
  d <- as.Date(x)
  paste0(format(d, "%Y"), " Q", ((as.integer(format(d, "%m")) - 1L) %/% 3L) + 1L)
}
train <- data.frame(
  Quarter = quarter_from_date(train_raw$Date),
  II = as.numeric(train_raw[["Total interest income"]]) / 1e6,
  IE = as.numeric(train_raw[["Total interest expense"]]) / 1e6,
  NII_reported = as.numeric(train_raw[["Net interest income"]]) / 1e6,
  stringsAsFactors = FALSE
)
train$NII <- train$II - train$IE
macro <- data.frame(
  Quarter = trimws(as.character(macro_raw$Date)),
  Rate_3M = as.numeric(macro_raw[["3-month Treasury rate"]]),
  Rate_10Y = as.numeric(macro_raw[["10-year Treasury yield"]]),
  BBB_Yield = as.numeric(macro_raw[["BBB corporate yield"]]),
  VIX = as.numeric(macro_raw[["Market Volatility Index (Level)"]]),
  stringsAsFactors = FALSE
)
macro <- macro[match(train$Quarter, macro$Quarter), , drop = FALSE]
analysis_data <- cbind(train, macro[, c("Rate_3M", "Rate_10Y", "BBB_Yield", "VIX"), drop = FALSE])
analysis_data$Term_Spread <- analysis_data$Rate_10Y - analysis_data$Rate_3M
analysis_data$BBB_Spread <- analysis_data$BBB_Yield - analysis_data$Rate_10Y
analysis_data$d_II <- c(NA_real_, diff(analysis_data$II))
analysis_data$g_II <- c(NA_real_, diff(log(analysis_data$II)))
analysis_data$dd_IE <- c(NA_real_, NA_real_, diff(analysis_data$IE, differences = 2L))
analysis_data$ddlog_IE <- c(NA_real_, NA_real_, diff(log(analysis_data$IE), differences = 2L))
analysis_data$d_Rate_3M <- c(NA_real_, diff(analysis_data$Rate_3M))
analysis_data$d_Rate_10Y <- c(NA_real_, diff(analysis_data$Rate_10Y))
analysis_data$d_Term_Spread <- c(NA_real_, diff(analysis_data$Term_Spread))
analysis_data$d_BBB_Spread <- c(NA_real_, diff(analysis_data$BBB_Spread))

max_vif_and_condition <- function(eq_fit) {
  mm <- stats::model.matrix(eq_fit)
  variable <- apply(mm, 2L, function(x) stats::sd(x) > 1e-10)
  x <- mm[, variable, drop = FALSE]
  if (!ncol(x)) return(c(Max_VIF = NA_real_, Condition_Number = NA_real_))
  vifs <- vapply(seq_len(ncol(x)), function(j) {
    y <- x[, j]
    others <- x[, -j, drop = FALSE]
    if (!ncol(others)) return(1)
    aux <- tryCatch(stats::lm.fit(cbind(1, others), y), error = function(e) NULL)
    if (is.null(aux)) return(Inf)
    r2 <- 1 - sum(aux$residuals^2) / sum((y - mean(y))^2)
    if (!is.finite(r2) || r2 >= 1 - 1e-10) Inf else 1 / (1 - r2)
  }, numeric(1))
  x_corr <- tryCatch(stats::cor(x), error = function(e) NA_real_)
  condition <- if (is.matrix(x_corr) && all(is.finite(x_corr))) {
    tryCatch(kappa(x_corr), error = function(e) NA_real_)
  } else NA_real_
  c(Max_VIF = max(vifs), Condition_Number = condition)
}
extract_diag_p <- function(expr, inner, field = "p.value") {
  obj <- tryCatch(expr, error = function(e) NULL)
  if (is.null(obj) || is.null(obj[[inner]]) || is.null(obj[[inner]][[field]])) return(NA_real_)
  as.numeric(obj[[inner]][[field]])
}

diagnostics <- lapply(top10$Model_ID, function(id) {
  s <- specs[specs$Model_ID == id, , drop = FALSE]
  endog <- trimws(strsplit(s$Endogenous_Variables[1L], "\\+")[[1L]])
  y <- as.data.frame(analysis_data[, endog, drop = FALSE])
  y <- y[stats::complete.cases(y), , drop = FALSE]
  fit <- tryCatch(vars::VAR(y, p = as.integer(s$P[1L]), type = "const"), error = function(e) NULL)
  if (is.null(fit)) {
    return(data.frame(Model_ID = id, Full_Training_Fit_Failed = TRUE))
  }
  adj <- vapply(fit$varresult, function(eq) tryCatch(summary(eq)$adj.r.squared, error = function(e) NA_real_), numeric(1))
  vif_cond <- lapply(fit$varresult, max_vif_and_condition)
  serial_p <- extract_diag_p(vars::serial.test(fit, lags.pt = 8L, type = "PT.adjusted"), "serial")
  arch_p <- extract_diag_p(vars::arch.test(fit, lags.multi = 4L), "arch.mul")
  normality <- tryCatch(vars::normality.test(fit, multivariate.only = TRUE), error = function(e) NULL)
  normality_p <- if (is.null(normality) || is.null(normality$jb.mul) ||
                     is.null(normality$jb.mul$JB$p.value)) NA_real_ else as.numeric(normality$jb.mul$JB$p.value)
  roots <- tryCatch(max(Mod(vars::roots(fit))), error = function(e) NA_real_)
  data.frame(
    Model_ID = id, Full_Training_N = nrow(y), P = as.integer(s$P[1L]),
    Mean_Adjusted_R2 = mean(adj, na.rm = TRUE), Max_Root_Modulus = roots,
    Roots_Stable = is.finite(roots) && roots < 1,
    Residual_Portmanteau_p = serial_p, Residual_Multivariate_ARCH_p = arch_p,
    Residual_Normality_p = normality_p,
    Lagged_Regressor_Max_VIF = max(vapply(vif_cond, function(z) z[["Max_VIF"]], numeric(1)), na.rm = TRUE),
    Lagged_Regressor_Max_Condition_Number = max(vapply(vif_cond, function(z) z[["Condition_Number"]], numeric(1)), na.rm = TRUE),
    Full_Training_Fit_Failed = FALSE, stringsAsFactors = FALSE
  )
})
diagnostics <- do.call(rbind, diagnostics)
readr::write_csv(
  diagnostics,
  "results/var_stationary_training_20261116/top10_full_training_diagnostics.csv"
)
print(diagnostics, row.names = FALSE)
