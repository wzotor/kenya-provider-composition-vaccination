# 30_predictions
# Purpose: model-based predicted probabilities and percentage-point
#          differences, averaged over the analytic sample.

library(dplyr); library(lme4); library(survey)

dat <- readRDS("data-clean/model_data.rds")
mods <- readRDS("data-clean/models_main.rds")
sw   <- readRDS("data-clean/model_survey.rds")

# ---- G-computation over the analytic sample ----
# Set everyone to each exposure level in turn, average the predictions.
gcomp <- function(model, data, type = c("merMod","svyglm")) {
  type <- match.arg(type)
  d0 <- d1 <- data
  d0$exposure <- factor("Government only",
                        levels = levels(data$exposure))
  d1$exposure <- factor("Government + NGO/FBO",
                        levels = levels(data$exposure))
  if (type == "merMod") {
    p0 <- predict(model, newdata = d0, type = "response", re.form = NA)
    p1 <- predict(model, newdata = d1, type = "response", re.form = NA)
  } else {
    p0 <- predict(model, newdata = d0, type = "response")
    p1 <- predict(model, newdata = d1, type = "response")
  }
  c(gov_only = mean(p0), both = mean(p1), diff_pp = 100*(mean(p1) - mean(p0)))
}

cat("=== ADJUSTED PREDICTED PROBABILITIES ===\n")
cat(sprintf("(averaged over all %s children in the analytic sample,\n",
            format(nrow(dat), big.mark = ",")))
cat(" random effects set to zero / county held at observed values)\n\n")

r1 <- gcomp(mods$m1, dat, "merMod")
cat(sprintf("M1 county RE      gov-only %.1f%%   both %.1f%%   diff %+.1f pp\n",
            100*r1[1], 100*r1[2], r1[3]))

r2 <- gcomp(mods$m2, dat, "merMod")
cat(sprintf("M2 +constituency  gov-only %.1f%%   both %.1f%%   diff %+.1f pp\n",
            100*r2[1], 100*r2[2], r2[3]))

rs <- gcomp(sw, dat, "svyglm")
cat(sprintf("Survey, county FE gov-only %.1f%%   both %.1f%%   diff %+.1f pp\n",
            100*rs[1], 100*rs[2], rs[3]))

# ---- Observed crude coverage for comparison ----
cat("\n=== OBSERVED (crude, weighted) ===\n")
des <- svydesign(ids=~v021, strata=~v022, weights=~I(v005/1e6),
                 data=dat, nest=TRUE)
options(survey.lonely.psu="adjust")
print(svyby(~vaccinated, ~exposure, des, svymean))

saveRDS(list(m1 = r1, m2 = r2, survey = rs), "data-clean/predictions.rds")