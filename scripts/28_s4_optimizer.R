# 28_s4_optimizer
# Purpose: refit the S4 model with Nelder-Mead and compare it against the
#          BOBYQA fit, supporting the convergence sentence in Methods.

library(lme4); library(dplyr)
dat <- readRDS("data-clean/model_data.rds")
d4  <- dat |> filter(!ADM1NAME %in% c("Mandera","Wajir","Garissa","Tana River","Isiolo")) |> droplevels()

f_adj <- vaccinated ~ exposure + child_age + female + birth_ord +
  mat_educ + mat_age + wealth + urban +
  log_travel + log_popden + log_lights + malaria_prev + (1 | county)

a <- glmer(f_adj, d4, binomial, control = glmerControl("bobyqa",      optCtrl=list(maxfun=2e5)))
b <- glmer(f_adj, d4, binomial, control = glmerControl("Nelder_Mead", optCtrl=list(maxfun=2e5)))

cat("logLik  bobyqa:", round(logLik(a),4), "  NM:", round(logLik(b),4),
    "  diff:", signif(logLik(a)-logLik(b),3), "\n")
cat("max abs coef diff:", signif(max(abs(fixef(a)-fixef(b))),3), "\n")
cat("county SD  bobyqa:", round(attr(VarCorr(a)$county,"stddev"),5),
    "  NM:", round(attr(VarCorr(b)$county,"stddev"),5), "\n")
cat("warnings  bobyqa:", length(a@optinfo$conv$lme4$messages),
    "  NM:", length(b@optinfo$conv$lme4$messages), "\n")