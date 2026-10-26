# 18_outcome_explore
# Purpose: locate the vaccination variables in the Children's Recode
#          and check eligibility fields before constructing the outcome.

library(haven)
library(dplyr)
library(labelled)

kr <- read_dta("data-raw/kenya_dhs_2022/children_recode/KEKR8CFL.DTA")

cat("Rows:", nrow(kr), " Columns:", ncol(kr), "\n\n")

# ---- Eligibility fields ----
cat("=== b5 (child alive) ===\n")
print(table(as_factor(kr$b5), useNA = "ifany"))

cat("\n=== b19 (age in months) summary ===\n")
print(summary(kr$b19))

cat("\n=== children aged 12-23 months and alive ===\n")
cat(sum(kr$b5 == 1 & kr$b19 >= 12 & kr$b19 <= 23, na.rm = TRUE), "\n")

# ---- Vaccination variables in the recode ----
cat("\n=== VACCINATION VARIABLES (h-series) ===\n")
vac <- names(kr)[grepl("^h[0-9]", names(kr))]
print(vac)

cat("\n=== LABELS FOR KEY VACCINE VARIABLES ===\n")
key <- intersect(c("h1","h0","h2","h3","h4","h5","h6","h7","h8","h9","h9a",
                   "h50","h51","h52","h53","h54","h55","h56","h57"), names(kr))
for (v in key) {
  cat(v, ": ", var_label(kr[[v]]), "\n", sep = "")
}

# ---- Survey design variables ----
cat("\n=== DESIGN VARIABLES ===\n")
for (v in c("v001","v005","v021","v022","v023","v024","v025")) {
  if (v %in% names(kr)) cat(v, ": ", var_label(kr[[v]]), "\n", sep = "")
}
