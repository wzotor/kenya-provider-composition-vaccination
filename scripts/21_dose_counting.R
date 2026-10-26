# 21_dose_counting
# Purpose: apply DHS dose-counting to DPT and OPV, then recheck the
#          diagnostics and the composite.

library(haven); library(dplyr); library(survey)

elig <- readRDS("data-clean/children_eligible.rds") |> mutate(wt = v005/1e6)
got  <- function(x) as.numeric(x) %in% c(1,2,3)

elig <- elig |>
  mutate(
    # count recorded doses, then assign 1st/2nd/3rd dose coverage
    dpt_n  = got(h3) + got(h5) + got(h7),
    opv_n  = got(h4) + got(h6) + got(h8),
    
    dpt1_c = dpt_n >= 1,  dpt2_c = dpt_n >= 2,  dpt3_c = dpt_n >= 3,
    opv1_c = opv_n >= 1,  opv2_c = opv_n >= 2,  opv3_c = opv_n >= 3,
    
    # polio component: 3 doses from OPV or IPV, birth dose excluded
    polio_n  = opv_n + got(h60),
    polio_ok2 = polio_n >= 3,
    
    mr_any = got(h9) | got(h9a),
    
    basic_final = got(h2) & dpt3_c & polio_ok2 & mr_any
  )

des <- svydesign(ids=~v021, strata=~v022, weights=~wt, data=elig, nest=TRUE)
options(survey.lonely.psu="adjust")

p <- function(f) round(100*coef(svymean(as.formula(paste0("~", f)), des))[2], 1)

cat("=== DIAGNOSTICS: dose-counted vs published ===\n")
cat(sprintf("DPT1   %5.1f   published 97.1\n", p("dpt1_c")))
cat(sprintf("DPT2   %5.1f   published 93.9\n", p("dpt2_c")))
cat(sprintf("DPT3   %5.1f   published 89.2\n", p("dpt3_c")))
cat(sprintf("OPV1   %5.1f   published 96.5\n", p("opv1_c")))
cat(sprintf("OPV2   %5.1f   published 94.2\n", p("opv2_c")))
cat(sprintf("OPV3   %5.1f   published 78.2\n", p("opv3_c")))
cat(sprintf("BCG    %5.1f   published 96.9\n", p("v_bcg")))
cat(sprintf("IPV    %5.1f   published 87.4\n", p("v_ipv")))
cat(sprintf("MR1    %5.1f   published 89.0\n", p("mr_any")))

cat("\n=== COMPOSITE ===\n")
cat(sprintf("Dose-counted: %5.2f   published 80.1\n",
            100*coef(svymean(~basic_final, des))[2]))

cat("\n=== CLASSIFICATION CHANGE: PREVIOUS vs DOSE-COUNTED ===\n")
print(table(previous = elig$basic_v2, dose_counted = elig$basic_final))

saveRDS(elig, "data-clean/children_eligible.rds")
