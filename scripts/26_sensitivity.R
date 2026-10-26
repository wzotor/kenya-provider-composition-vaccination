# 26_sensitivity
# Purpose: sensitivity specification S4, the five-county exclusion,
#          determined before the adjusted results were examined.

library(dplyr)
library(lme4)

dat  <- readRDS("data-clean/model_data.rds")
ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

f_adj <- vaccinated ~ exposure + child_age + female + birth_ord +
  mat_educ + mat_age + wealth + urban +
  log_travel + log_popden + log_lights + malaria_prev + (1 | county)

or_row <- function(m, lab, n_ctrl, n_conly, vac, unvac) {
  s <- summary(m)$coefficients
  i <- grep("^exposure", rownames(s))
  cat(sprintf("%-28s OR %.2f  CI %.2f-%.2f  p=%.3f   n=%d (comp %d: %d vac / %d unvac)\n",
              lab, exp(s[i,1]), exp(s[i,1]-1.96*s[i,2]),
              exp(s[i,1]+1.96*s[i,2]), s[i,4], n_ctrl, n_conly, vac, unvac))
}

# ---- S4: exclude the five counties contributing most of the comparison group ----
drop5 <- c("Mandera","Wajir","Garissa","Tana River","Isiolo")
d4 <- dat |> filter(!ADM1NAME %in% drop5) |> droplevels()

cat("=== S4: five-county exclusion ===\n")
cat("Total children:", nrow(d4), "\n")
print(table(d4$exposure))
cat("Constituencies in comparison group:",
    n_distinct(d4$constit[d4$exposure == "Government only"]), "\n")
print(table(d4$exposure, d4$vaccinated))

m4 <- glmer(f_adj, data = d4, family = binomial, control = ctrl)
cat("\nconvergence messages:", length(m4@optinfo$conv$lme4$messages), "\n")

comp <- d4 |> filter(exposure == "Government only")
or_row(m4, "S4 excl. 5 counties", nrow(d4),
       n_distinct(comp$constit), sum(comp$vaccinated), sum(!comp$vaccinated))

saveRDS(m4, "data-clean/model_s4.rds")

ctrl2 <- glmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun = 2e5))
m4b <- glmer(f_adj, data = d4, family = binomial, control = ctrl2)
s <- summary(m4b)$coefficients
i <- grep("^exposure", rownames(s))
cat(sprintf("Nelder_Mead: OR %.2f  CI %.2f-%.2f  p=%.3f  messages=%d\n",
            exp(s[i,1]), exp(s[i,1]-1.96*s[i,2]), exp(s[i,1]+1.96*s[i,2]),
            s[i,4], length(m4b@optinfo$conv$lme4$messages)))
