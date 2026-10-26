# 20_outcome_polio_rule
# Purpose: implement Kenya's documented polio rule (three doses of OPV,
#          IPV, or a combination, excluding the birth dose), re-run the
#          national replication, and test an alternative measles definition.

library(dplyr); library(haven); library(survey); library(labelled)

elig <- readRDS("data-clean/children_eligible.rds") |>
  mutate(wt = v005/1e6)

got <- function(x) as.numeric(x) %in% c(1, 2, 3)

elig <- elig |>
  mutate(
    v_ipv = got(h60),
    # count eligible polio doses: OPV1, OPV2, OPV3, IPV (birth dose excluded)
    polio_doses = v_polio1 + v_polio2 + v_polio3 + v_ipv,
    polio_ok    = polio_doses >= 3,
    
    basic_kenya = v_bcg & v_dpt1 & v_dpt2 & v_dpt3 & polio_ok & v_measles
  )

des <- svydesign(ids=~v021, strata=~v022, weights=~wt, data=elig, nest=TRUE)
options(survey.lonely.psu="adjust")

cat("=== POLIO DOSE DISTRIBUTION ===\n")
print(table(elig$polio_doses))

cat("\n=== POLIO COMPONENT (weighted) ===\n")
cat("OPV3 only rule: ", round(100*coef(svymean(~v_polio3, des))[2], 1), "%\n")
cat("Kenya rule:     ", round(100*coef(svymean(~polio_ok,  des))[2], 1), "%\n")

cat("\n=== BASIC ANTIGENS ===\n")
cat("Strict OPV3 rule:", round(100*coef(svymean(~basic_all,   des))[2], 1), "%\n")
cat("Kenya polio rule:", round(100*coef(svymean(~basic_kenya, des))[2], 1), "%\n")
cat("Published:        80.1%\n")

cat("\n=== IPV CHECK against published 87.4 ===\n")
cat("IPV:", round(100*coef(svymean(~v_ipv, des))[2], 1), "%\n")

cat("\n=== ANTIGENS MISSED, AMONG THOSE MISSING EXACTLY ONE ===\n")
elig <- elig |>
  mutate(m2 = (!v_bcg) + (!v_dpt1) + (!v_dpt2) + (!v_dpt3) + (!polio_ok) + (!v_measles))
one <- elig |> filter(m2 == 1)
cat("Missing exactly one, by antigen:\n")
cat(" bcg:", sum(!one$v_bcg), " dpt1:", sum(!one$v_dpt1), " dpt2:", sum(!one$v_dpt2),
    " dpt3:", sum(!one$v_dpt3), " polio:", sum(!one$polio_ok),
    " measles:", sum(!one$v_measles), "\n")

saveRDS(elig, "data-clean/children_eligible.rds")

elig <- readRDS("data-clean/children_eligible.rds")
elig <- elig |> mutate(wt = v005/1e6)
des <- svydesign(ids=~v021, strata=~v022, weights=~wt, data=elig, nest=TRUE)
options(survey.lonely.psu="adjust")
got <- function(x) as.numeric(x) %in% c(1,2,3)

cat("=== MR1 vs MR2 CROSS-TAB ===\n")
print(table(mr1 = got(elig$h9), mr2 = got(elig$h9a)))

cat("\n=== ANY MEASLES-CONTAINING DOSE ===\n")
elig <- elig |> mutate(v_mr_any = got(h9) | got(h9a))
des <- svydesign(ids=~v021, strata=~v022, weights=~wt, data=elig, nest=TRUE)
cat("h9 only:        ", round(100*coef(svymean(~v_measles, des))[2],1), "%\n")
cat("h9 or h9a:      ", round(100*coef(svymean(~v_mr_any,  des))[2],1), "%\n")
cat("Published MR1:   89.0%\n")

cat("\n=== COMPOSITE WITH 'ANY MEASLES DOSE' ===\n")
elig <- elig |> mutate(basic_v2 = v_bcg & v_dpt1 & v_dpt2 & v_dpt3 & polio_ok & v_mr_any)
des <- svydesign(ids=~v021, strata=~v022, weights=~wt, data=elig, nest=TRUE)
cat("Composite:", round(100*coef(svymean(~basic_v2, des))[2],2), "%\n")
cat("Published: 80.1%\n")

saveRDS(elig, "data-clean/children_eligible.rds")
