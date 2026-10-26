# 32_predictions_final
# Purpose: final adjusted predictions under equal and survey-weighted
#          averaging. The survey-weighted rows are those reported in Table 3.

library(dplyr); library(lme4); library(survey); library(MASS)

dat  <- readRDS("data-clean/model_data.rds")
mods <- readRDS("data-clean/models_main.rds")
sw   <- readRDS("data-clean/model_survey.rds")

set.seed(2026)
B <- 1000
inv <- function(x) 1/(1+exp(-x))

contrast <- function(model, data, is_svy = FALSE, wts = NULL) {
  d0 <- d1 <- data
  d0$exposure <- factor("Government only",      levels = levels(data$exposure))
  d1$exposure <- factor("Government + NGO/FBO", levels = levels(data$exposure))
  
  X0 <- model.matrix(delete.response(terms(model)), d0)
  X1 <- model.matrix(delete.response(terms(model)), d1)
  beta <- if (is_svy) coef(model) else fixef(model)
  V    <- as.matrix(vcov(model))
  
  w <- if (is.null(wts)) rep(1, nrow(data)) else wts
  w <- w / sum(w)
  
  pt <- function(b) c(sum(w*inv(X0 %*% b)), sum(w*inv(X1 %*% b)))
  e  <- pt(beta)
  
  draws <- MASS::mvrnorm(B, mu = beta, Sigma = V)
  dif <- apply(draws, 1, function(b) { p <- pt(b); 100*(p[2]-p[1]) })
  
  c(p0 = 100*e[1], p1 = 100*e[2], diff = 100*(e[2]-e[1]),
    lo = quantile(dif, .025, names=FALSE), hi = quantile(dif, .975, names=FALSE))
}

show <- function(r, lab) {
  cat(sprintf("%-34s gov-only %.1f%%  both %.1f%%  diff %+.1f pp (%+.1f to %+.1f)\n",
              lab, r[1], r[2], r[3], r[4], r[5]))
}

cat("=== EQUAL WEIGHTING OF CHILDREN ===\n")
show(contrast(mods$m1, dat), "M1 county RE, unweighted avg")
show(contrast(mods$m2, dat), "M2 +constit RE, unweighted avg")
show(contrast(sw, dat, is_svy=TRUE), "Survey model, unweighted avg")

cat("\n=== SURVEY-WEIGHTED AVERAGING OF CHILDREN ===\n")
w <- dat$v005/1e6
show(contrast(mods$m1, dat, wts=w), "M1 county RE, wtd avg")
show(contrast(mods$m2, dat, wts=w), "M2 +constit RE, wtd avg")
show(contrast(sw, dat, is_svy=TRUE, wts=w), "Survey model, wtd avg")

saveRDS(list(
  note = paste("Random effects set to zero (re.form = NA): predictions are for",
               "a county and constituency with average random effect, not",
               "integrated over their distributions. Intervals are",
               "simulation-based from 1000 draws of the fixed-effect",
               "coefficient vector; they do not propagate uncertainty in",
               "random-effect variances. Predicting both exposure conditions",
               "in all counties involves extrapolation where only one was",
               "observed; these are model-standardised, not observed, rates.")
), "data-clean/prediction_notes.rds")