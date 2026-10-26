# 27_spec_checks
# Purpose: S2 (VCT inclusion) and S3 (four-constituency influence check).

library(dplyr)
library(lme4)

dat  <- readRDS("data-clean/model_data.rds")
ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

f_adj <- vaccinated ~ exposure + child_age + female + birth_ord +
  mat_educ + mat_age + wealth + urban +
  log_travel + log_popden + log_lights + malaria_prev + (1 | county)

report <- function(m, d, lab) {
  s <- summary(m)$coefficients
  i <- grep("^exposure", rownames(s))
  comp <- d |> filter(exposure == "Government only")
  cat(sprintf("%-22s OR %.2f  CI %.2f-%.2f  p=%.3f | n=%d, comp %d children in %d constit (%d unvac) | msgs=%d\n",
              lab, exp(s[i,1]), exp(s[i,1]-1.96*s[i,2]), exp(s[i,1]+1.96*s[i,2]),
              s[i,4], nrow(d), nrow(comp), n_distinct(comp$constit),
              sum(!comp$vaccinated), length(m@optinfo$conv$lme4$messages)))
}

# ---- S2: VCT inclusion moves Bomet East and Belgut to exposed ----
# Bomet East and Belgut are the two that move; 15_exposure.R reports the
# recomputed exposure with VCT and armed-forces facilities included.
d2 <- dat |>
  mutate(exposure = ifelse(adm2_name %in% c("Bomet East", "Belgut"),
                           "Government + NGO/FBO", as.character(exposure)),
         exposure = factor(exposure,
                           levels = c("Government only", "Government + NGO/FBO"))) |>
  droplevels()

cat("=== S2: VCT included ===\n")
print(table(d2$exposure))
m2s <- glmer(f_adj, data = d2, family = binomial, control = ctrl)
report(m2s, d2, "S2 VCT included")

# ---- S3: drop the four constituencies least explained by low facility counts ----
drop4 <- c("Matungulu", "Belgut", "Matayos", "Kuresoi South")
d3 <- dat |> filter(!adm2_name %in% drop4) |> droplevels()

cat("\n=== S3: four constituencies dropped ===\n")
print(table(d3$exposure))
m3s <- glmer(f_adj, data = d3, family = binomial, control = ctrl)
report(m3s, d3, "S3 drop 4 constit")

saveRDS(list(s2 = m2s, s3 = m3s), "data-clean/models_spec.rds")