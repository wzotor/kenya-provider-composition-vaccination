# 19_outcome_build
# Purpose: check how DPT/pentavalent are coded, construct basic-antigen full
#          immunisation for children 12-23 months, and examine antigen-level
#          missingness, coverage and alternative definitions.


library(haven)
library(dplyr)
library(survey)

kr <- read_dta("data-raw/kenya_dhs_2022/children_recode/KEKR8CFL.DTA")

# ---- Coding of the vaccine variables ----
cat("=== CODING OF h2 (BCG) ===\n")
print(table(as_factor(kr$h2), useNA = "ifany"))

cat("\n=== DPT3 (h7) vs PENTA3 (h53) ===\n")
print(table(dpt3 = as_factor(kr$h7), penta3 = as_factor(kr$h53), useNA = "ifany"))

cat("\n=== MEASLES 1 (h9) ===\n")
print(table(as_factor(kr$h9), useNA = "ifany"))

# ---- Build the eligible sample ----
elig <- kr |>
  filter(b5 == 1, b19 >= 12, b19 <= 23)

cat("\nEligible children (alive, 12-23 months):", nrow(elig), "\n")

# ---- Basic antigen indicator ----
# "Received" = codes 1 (vaccination date on card), 2 (reported by mother),
# 3 (vaccination marked on card). 0 = no, 8 = don't know, NA = missing.
got <- function(x) as.numeric(x) %in% c(1, 2, 3)

elig <- elig |>
  mutate(
    v_bcg     = got(h2),
    v_dpt1    = got(h3),
    v_dpt2    = got(h5),
    v_dpt3    = got(h7),
    v_polio1  = got(h4),
    v_polio2  = got(h6),
    v_polio3  = got(h8),
    v_measles = got(h9),
    basic_all = v_bcg & v_dpt1 & v_dpt2 & v_dpt3 &
      v_polio1 & v_polio2 & v_polio3 & v_measles
  )

cat("\n=== UNWEIGHTED BASIC-ANTIGEN COVERAGE ===\n")
print(table(elig$basic_all))
cat("Percent:", round(100 * mean(elig$basic_all), 1), "%\n")

# ---- Weighted estimate, matching DHS convention ----
elig <- elig |> mutate(wt = v005 / 1e6)

des <- svydesign(ids = ~v021, strata = ~v022, weights = ~wt,
                 data = elig, nest = TRUE)
options(survey.lonely.psu = "adjust")

cat("\n=== WEIGHTED BASIC-ANTIGEN COVERAGE ===\n")
est <- svymean(~basic_all, des)
print(est)
cat("\nPercent:", round(100 * coef(est)[1], 1), "%\n")
cat("Published KDHS 2022 figure to compare against: 80.1%\n")

saveRDS(elig, "data-clean/children_eligible.rds")

elig <- readRDS("data-clean/children_eligible.rds")

cat("=== MISSINGNESS IN THE 12-23 MONTH SAMPLE ===\n")
for (v in c("h2","h3","h5","h7","h4","h6","h8","h9")) {
  x <- as.numeric(elig[[v]])
  cat(sprintf("%-4s  no:%4d  card/report:%4d  dk:%3d  NA:%4d\n",
              v, sum(x == 0, na.rm=TRUE), sum(x %in% 1:3, na.rm=TRUE),
              sum(x == 8, na.rm=TRUE), sum(is.na(x))))
}

cat("\n=== COVERAGE OF EACH ANTIGEN (weighted) ===\n")
elig <- elig |> mutate(wt = v005/1e6)
des <- svydesign(ids=~v021, strata=~v022, weights=~wt, data=elig, nest=TRUE)
options(survey.lonely.psu="adjust")
for (v in c("v_bcg","v_dpt1","v_dpt2","v_dpt3","v_polio1","v_polio2","v_polio3","v_measles")) {
  m <- svymean(as.formula(paste0("~", v)), des)
  cat(sprintf("%-10s %5.1f%%\n", v, 100*coef(m)[2]))
}

cat("\n=== h1: has health card ===\n")
print(table(as_factor(elig$h1), useNA="ifany"))

elig <- readRDS("data-clean/children_eligible.rds")

cat("=== ALL MEASLES-LIKE VARIABLES ===\n")
for (v in c("h9","h9a","h58","h59","h60","h61","h62","h63","h64","h65","h66","h67")) {
  if (v %in% names(elig)) {
    lab <- attr(elig[[v]], "label")
    x <- as.numeric(elig[[v]])
    cat(sprintf("%-5s %-45s got:%4d  no:%4d  NA:%4d\n",
                v, substr(lab,1,45), sum(x %in% 1:3, na.rm=TRUE),
                sum(x==0, na.rm=TRUE), sum(is.na(x))))
  }
}

cat("\n=== POLIO: BIRTH DOSE (h0) BY POLIO3 (h8) ===\n")
x0 <- as.numeric(elig$h0); x8 <- as.numeric(elig$h8)
print(table(polio0 = x0 %in% 1:3, polio3 = x8 %in% 1:3))

cat("\n=== IPV VARIABLES ===\n")
for (v in c("h68","h69","h70a","h71")) {
  if (v %in% names(elig)) cat(v, ": ", attr(elig[[v]], "label"), "\n", sep="")
}

library(dplyr); library(haven); library(survey)
elig <- readRDS("data-clean/children_eligible.rds")
elig <- elig |> mutate(wt = v005/1e6)
des <- svydesign(ids=~v021, strata=~v022, weights=~wt, data=elig, nest=TRUE)
options(survey.lonely.psu="adjust")

cat("=== WEIGHTED N ===\n")
cat("Sum of weights:", round(sum(elig$wt)), " Unweighted:", nrow(elig), "\n")

cat("\n=== HOW MANY ANTIGENS MISSED, AMONG THOSE NOT FULLY VACCINATED ===\n")
elig <- elig |>
  mutate(n_missed = (!v_bcg) + (!v_dpt1) + (!v_dpt2) + (!v_dpt3) +
           (!v_polio1) + (!v_polio2) + (!v_polio3) + (!v_measles))
print(table(elig$n_missed))

cat("\n=== ANTIGEN MISSED, AMONG THOSE MISSING EXACTLY ONE ===\n")
one <- elig |> filter(n_missed == 1)
cat("bcg:", sum(!one$v_bcg), " dpt1:", sum(!one$v_dpt1), " dpt2:", sum(!one$v_dpt2),
    " dpt3:", sum(!one$v_dpt3), "\n")
cat("polio1:", sum(!one$v_polio1), " polio2:", sum(!one$v_polio2),
    " polio3:", sum(!one$v_polio3), " measles:", sum(!one$v_measles), "\n")

cat("\n=== BASIC ANTIGENS USING MR (h9) vs excluding polio3 ===\n")
elig <- elig |> mutate(no_p3 = v_bcg & v_dpt1 & v_dpt2 & v_dpt3 &
                         v_polio1 & v_polio2 & v_measles)
des2 <- svydesign(ids=~v021, strata=~v022, weights=~wt, data=elig, nest=TRUE)
cat("With polio3:   ", round(100*coef(svymean(~basic_all, des2))[2],1), "%\n")
cat("Without polio3:", round(100*coef(svymean(~no_p3, des2))[2],1), "%\n")
