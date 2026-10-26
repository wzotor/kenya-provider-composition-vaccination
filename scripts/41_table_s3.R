# 41_table_s3
# Purpose: S3 Table. Complete regression results (Section A) and model
#          summary statistics (Section B) for all eight specifications.

library(dplyr); library(lme4); library(survey)

mods <- readRDS("data-clean/models_main.rds")
spec <- readRDS("data-clean/models_spec.rds")
m4   <- readRDS("data-clean/model_s4.rds")
sw   <- readRDS("data-clean/model_survey.rds")
uw   <- readRDS("data-clean/model_uw.rds")

models <- list(
  "M0 Exposure-only, county RE"        = mods$m0,
  "M1 Adjusted, county RE"             = mods$m1,
  "M2 Adjusted, county + constit RE"   = mods$m2,
  "S2 VCT included"                    = spec$s2,
  "S3 Four constituencies excluded"    = spec$s3,
  "S4 Five counties excluded"          = m4,
  "County indicators, unweighted"      = uw,
  "County indicators, survey-weighted" = sw
)

# ---- AIC method check for the survey-weighted model ----
# AIC() on an svyglm returns a named vector (eff.p, AIC, deltabar), which is
# why fitrow() extracts the AIC element by name below.
cat("=== AIC METHOD CHECK, survey-weighted model ===\n")
cat("Class:", paste(class(sw), collapse = ", "), "\n")
cat("AIC() returns:\n"); print(AIC(sw))
cat("\n")

# ---- Section A: coefficients ----
extract <- function(m, label) {
  s  <- summary(m)$coefficients
  b  <- as.numeric(s[, 1])
  se <- as.numeric(s[, 2])
  p  <- as.numeric(s[, 4])
  data.frame(
    model = label,
    term  = rownames(s),
    coef  = sprintf("%.4f", b),
    se    = sprintf("%.4f", se),
    or    = sprintf("%.3f", exp(b)),
    ci_lo = sprintf("%.3f", exp(b - 1.96 * se)),
    ci_hi = sprintf("%.3f", exp(b + 1.96 * se)),
    p     = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

s3a <- bind_rows(Map(extract, models, names(models)))

# ---- Section B: model summary ----
fitrow <- function(m, label) {
  if (inherits(m, "glmerMod")) {
    vc <- as.data.frame(VarCorr(m))
    data.frame(
      model = label,
      n = format(nobs(m), big.mark = ","),
      random_intercept_variances =
        paste(paste0(vc$grp, ": ", sprintf("%.3f", vc$vcov)), collapse = "; "),
      logLik = sprintf("%.1f", as.numeric(logLik(m))),
      AIC = sprintf("%.1f", AIC(m)),
      stringsAsFactors = FALSE)
  } else if (inherits(m, "svyglm")) {
    data.frame(
      model = label, n = format(nobs(m), big.mark = ","),
      random_intercept_variances = "--",
      logLik = "--",
      AIC = sprintf("%.1f", AIC(m)[["AIC"]]),
      stringsAsFactors = FALSE)
  } else {
    data.frame(
      model = label, n = format(nobs(m), big.mark = ","),
      random_intercept_variances = "--",
      logLik = sprintf("%.1f", as.numeric(logLik(m))),
      AIC = sprintf("%.1f", AIC(m)),
      stringsAsFactors = FALSE)
  }
}

s3b <- bind_rows(Map(fitrow, models, names(models)))

cat("=== SECTION B: MODEL SUMMARY ===\n")
print(as.data.frame(s3b), row.names = FALSE)

cat("\n=== SECTION A: rows =", nrow(s3a), "===\n")
print(as.data.frame(head(s3a, 10)), row.names = FALSE)

write.csv(s3a, "output/S3a_Table.csv", row.names = FALSE)
write.csv(s3b, "output/S3b_Table.csv", row.names = FALSE)
cat("\nSaved to output/S3a_Table.csv and output/S3b_Table.csv\n")