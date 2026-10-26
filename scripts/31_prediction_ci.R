# 31_prediction_ci.R
# Purpose: confidence intervals for the adjusted percentage-point
#          differences, from simulation draws of the fixed-effect
#          coefficients.
#          Averages here are unweighted; the survey-weighted versions
#          reported in the paper are produced in 32_predictions_final.R.

library(dplyr); library(lme4); library(survey); library(MASS)

dat  <- readRDS("data-clean/model_data.rds")
mods <- readRDS("data-clean/models_main.rds")
sw   <- readRDS("data-clean/model_survey.rds")

set.seed(2026)
B <- 1000

# Draw coefficient vectors from the sampling distribution, recompute
# the g-computation contrast for each draw.
boot_diff <- function(model, data, is_svy = FALSE) {
  d0 <- d1 <- data
  d0$exposure <- factor("Government only",      levels = levels(data$exposure))
  d1$exposure <- factor("Government + NGO/FBO", levels = levels(data$exposure))
  
  if (is_svy) {
    beta <- coef(model); V <- vcov(model)
    X0 <- model.matrix(delete.response(terms(model)), d0)
    X1 <- model.matrix(delete.response(terms(model)), d1)
  } else {
    beta <- fixef(model); V <- as.matrix(vcov(model))
    X0 <- model.matrix(delete.response(terms(model)), d0)
    X1 <- model.matrix(delete.response(terms(model)), d1)
  }
  
  draws <- MASS::mvrnorm(B, mu = beta, Sigma = V)
  inv <- function(x) 1/(1+exp(-x))
  
  diffs <- apply(draws, 1, function(b) {
    100 * (mean(inv(X1 %*% b)) - mean(inv(X0 %*% b)))
  })
  c(est = 100*(mean(inv(X1 %*% beta)) - mean(inv(X0 %*% beta))),
    lo  = quantile(diffs, 0.025, names = FALSE),
    hi  = quantile(diffs, 0.975, names = FALSE))
}

cat("=== ADJUSTED PERCENTAGE-POINT DIFFERENCE (both minus gov-only) ===\n")
cat(sprintf("Averaged over all %s children; 95%% CI from %s draws\n\n",
            format(nrow(dat), big.mark = ","), format(B, big.mark = ",")))

r1 <- boot_diff(mods$m1, dat)
cat(sprintf("M1 county RE       %+.1f pp   95%% CI %+.1f to %+.1f\n", r1[1], r1[2], r1[3]))

r2 <- boot_diff(mods$m2, dat)
cat(sprintf("M2 +constituency   %+.1f pp   95%% CI %+.1f to %+.1f\n", r2[1], r2[2], r2[3]))

rs <- boot_diff(sw, dat, is_svy = TRUE)
cat(sprintf("Survey, county FE  %+.1f pp   95%% CI %+.1f to %+.1f\n", rs[1], rs[2], rs[3]))

saveRDS(list(m1 = r1, m2 = r2, survey = rs), "data-clean/prediction_ci.rds")