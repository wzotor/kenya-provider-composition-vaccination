# 36_table_descriptives
# Purpose: descriptive comparison of the two exposure groups, and a check
#          of DHS-recorded against polygon-assigned county.

library(dplyr); library(haven); library(survey)

dat <- readRDS("data-clean/model_data.rds") |> mutate(wt = v005/1e6)

des <- svydesign(ids = ~v021, strata = ~v022, weights = ~wt,
                 data = dat, nest = TRUE)
options(survey.lonely.psu = "adjust")

cat("=== GROUP SIZES ===\n")
print(dat |> count(exposure, name = "children"))
print(dat |> group_by(exposure) |>
        summarise(constituencies = n_distinct(constit),
                  clusters = n_distinct(v001),
                  counties = n_distinct(county), .groups = "drop"))

cat("\n=== VACCINATION COVERAGE (weighted) ===\n")
print(svyby(~vaccinated, ~exposure, des, svymean))

cat("\n=== WEALTH QUINTILE (weighted row %) ===\n")
print(round(100*prop.table(svytable(~exposure + wealth, des), 1), 1))

cat("\n=== MATERNAL EDUCATION (weighted row %) ===\n")
print(round(100*prop.table(svytable(~exposure + mat_educ, des), 1), 1))

cat("\n=== URBAN (weighted row %) ===\n")
print(round(100*prop.table(svytable(~exposure + urban, des), 1), 1))

cat("\n=== CONTINUOUS COVARIATES (weighted means) ===\n")
for (v in c("child_age","mat_age","birth_ord","travel_time",
            "pop_density","nightlights","malaria_prev")) {
  t <- svyby(as.formula(paste0("~", v)), ~exposure, des, svymean, na.rm = TRUE)
  cat(sprintf("%-14s gov-only %8.2f   both %8.2f\n", v, t[1,2], t[2,2]))
}

cat("\n=== GEOGRAPHIC CONCENTRATION (unweighted counts) ===\n")
print(as.data.frame(
  dat |> filter(exposure == "Government only") |>
    count(county, name = "children") |> arrange(desc(children))
))

cat("=== COUNTY VARIABLE COMPARISON, gov-only group ===\n")
g <- dat |> filter(exposure == "Government only")

cat("\nDHS-recorded county (used in models):\n")
print(as.data.frame(count(g, county, name = "children") |> arrange(desc(children))))

cat("\nPolygon-assigned county:\n")
print(as.data.frame(count(g, adm1_name, name = "children") |> arrange(desc(children))))

cat("\nDisagreeing children:\n")
print(as.data.frame(
  dat |> filter(as.character(county) != as.character(adm1_name)) |>
    count(county, adm1_name, exposure, name = "children")))

cat("\n=== WEIGHTED MEANS AND SDs ===\n")
vars <- c("child_age","mat_age","birth_ord","travel_time",
          "pop_density","nightlights","malaria_prev")

for (v in vars) {
  m <- svyby(as.formula(paste0("~", v)), ~exposure, des, svymean, na.rm = TRUE)
  s <- svyby(as.formula(paste0("~", v)), ~exposure, des, svyvar,  na.rm = TRUE)
  cat(sprintf("%-14s gov-only %9.2f (%8.2f)   both %9.2f (%8.2f)\n",
              v, m[1,2], sqrt(s[1,2]), m[2,2], sqrt(s[2,2])))
}
