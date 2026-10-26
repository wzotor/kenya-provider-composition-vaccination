# 29_survey_benchmark
# Purpose: survey-weighted and unweighted county-indicator models, as a
#          benchmark against the random-intercept specifications.

library(survey); library(dplyr); library(haven)

# Build the design on the full eligible sample, then subset, so that
# variance estimation uses the complete cluster and stratum structure.
elig <- readRDS("data-clean/children_eligible.rds")
mod  <- readRDS("data-clean/model_data.rds")

full <- elig |> mutate(wt = v005/1e6)

# Flag analytic children by the composite child id, since caseid alone
# identifies the mother rather than the child.
mod_ids <- paste(mod$caseid, mod$bidx)
full$in_analysis <- paste(full$caseid, full$bidx) %in% mod_ids
cat("Flagged in analysis:", sum(full$in_analysis), " (model data:", nrow(mod), ")\n")

# attach model variables to the full frame
keep <- c("caseid","bidx","exposure","child_age","female","birth_ord",
          "mat_educ","mat_age","wealth","urban","log_travel","log_popden",
          "log_lights","malaria_prev","county","vaccinated")
full <- full |>
  select(-any_of(setdiff(keep, c("caseid","bidx")))) |>
  left_join(mod |> select(all_of(keep)), by = c("caseid","bidx"))

des_all <- svydesign(ids = ~v021, strata = ~v022, weights = ~wt,
                     data = full, nest = TRUE)
options(survey.lonely.psu = "adjust")

des <- subset(des_all, in_analysis)

cat("\n=== SURVEY-WEIGHTED, COUNTY INDICATORS ===\n")
sw <- svyglm(vaccinated ~ exposure + child_age + female + birth_ord +
               mat_educ + mat_age + wealth + urban +
               log_travel + log_popden + log_lights + malaria_prev + county,
             design = des, family = quasibinomial())

s <- summary(sw)$coefficients
i <- grep("^exposure", rownames(s))
cat(sprintf("Weighted, county FE:   OR %.2f  CI %.2f-%.2f  p=%.3f\n",
            exp(s[i,1]), exp(s[i,1]-1.96*s[i,2]), exp(s[i,1]+1.96*s[i,2]), s[i,4]))

cat("\n=== SAME FORMULA, UNWEIGHTED (isolates weighting) ===\n")
uw <- glm(vaccinated ~ exposure + child_age + female + birth_ord +
            mat_educ + mat_age + wealth + urban +
            log_travel + log_popden + log_lights + malaria_prev + county,
          data = mod, family = binomial)
s2 <- summary(uw)$coefficients
i2 <- grep("^exposure", rownames(s2))
cat(sprintf("Unweighted, county FE: OR %.2f  CI %.2f-%.2f  p=%.3f\n",
            exp(s2[i2,1]), exp(s2[i2,1]-1.96*s2[i2,2]),
            exp(s2[i2,1]+1.96*s2[i2,2]), s2[i2,4]))

cat("\n=== URBAN COEFFICIENT ===\n")
cat(sprintf("Weighted:   OR %.2f  CI %.2f-%.2f\n",
            exp(s["urban",1]), exp(s["urban",1]-1.96*s["urban",2]),
            exp(s["urban",1]+1.96*s["urban",2])))

saveRDS(sw, "data-clean/model_survey.rds")
saveRDS(uw, "data-clean/model_uw.rds")