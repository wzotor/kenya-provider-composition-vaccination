# 24_models
# Purpose: unadjusted and adjusted association between provider
#          co-presence and full basic-antigen vaccination.

library(dplyr)
library(lme4)

dat <- readRDS("data-clean/model_data.rds")

# BOBYQA with the function-evaluation limit raised to 200,000, as reported
# in Methods; convergence is rechecked against Nelder-Mead in 28_s4_optimizer.R.
ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# ---- M0: unadjusted ----
m0 <- glmer(vaccinated ~ exposure + (1 | county),
            data = dat, family = binomial, control = ctrl)
print(summary(m0)$coefficients)

# ---- M1: adjusted, county random effect ----
m1 <- glmer(vaccinated ~ exposure + child_age + female + birth_ord +
              mat_educ + mat_age + wealth + urban +
              log_travel + log_popden + log_lights + malaria_prev +
              (1 | county),
            data = dat, family = binomial, control = ctrl)
print(summary(m1)$coefficients)

# ---- M2: adjusted, county + constituency random effects ----
m2 <- glmer(vaccinated ~ exposure + child_age + female + birth_ord +
              mat_educ + mat_age + wealth + urban +
              log_travel + log_popden + log_lights + malaria_prev +
              (1 | county/constit),
            data = dat, family = binomial, control = ctrl)
print(summary(m2)$coefficients)

# ---- Convergence check ----
cat("M0 messages:", length(m0@optinfo$conv$lme4$messages), "\n")
cat("M1 messages:", length(m1@optinfo$conv$lme4$messages), "\n")
cat("M2 messages:", length(m2@optinfo$conv$lme4$messages), "\n")

# ---- Odds ratios for the exposure term ----
or_row <- function(m, lab) {
  s <- summary(m)$coefficients
  i <- grep("^exposure", rownames(s))
  cat(sprintf("%-30s OR %.2f  CI %.2f-%.2f  p=%.3f\n", lab,
              exp(s[i,1]), exp(s[i,1] - 1.96*s[i,2]),
              exp(s[i,1] + 1.96*s[i,2]), s[i,4]))
}

or_row(m0, "M0 unadjusted")
or_row(m1, "M1 adjusted, county RE")
or_row(m2, "M2 adjusted, county+constit")

# ---- Random effect variances ----
print(VarCorr(m2))

saveRDS(list(m0 = m0, m1 = m1, m2 = m2), "data-clean/models_main.rds")