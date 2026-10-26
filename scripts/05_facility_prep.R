
# 05_facility_prep
# Purpose: inspect facility register fields before screening.

library(dplyr)
library(readxl)

kmhfr <- read_excel("data-raw/kmhfr/kenya_health_facilities_2017.xlsx")

cat("\n=== OWNER (full list) ===\n")
print(kmhfr |> count(Owner, sort = TRUE), n = 100)

cat("\n=== FACILITY TYPE ===\n")
print(kmhfr |> count(`Facility type`, sort = TRUE), n = 100)

cat("\n=== KEPH LEVEL ===\n")
print(kmhfr |> count(`Keph level`, sort = TRUE), n = 50)

cat("\n=== OPERATION STATUS ===\n")
print(kmhfr |> count(`Operation status`, sort = TRUE))

cat("\n=== CLOSED / APPROVED / PUBLIC VISIBLE ===\n")
print(kmhfr |> count(Closed, Approved, `Public visible`, sort = TRUE))

cat("\n=== DUPLICATE FACILITY CODES ===\n")
cat("Total rows:", nrow(kmhfr), "\n")
cat("Unique codes:", n_distinct(kmhfr$Code), "\n")
cat("Duplicated:", sum(duplicated(kmhfr$Code)), "\n")

cat("\n=== OWNER x FACILITY TYPE (counts) ===\n")
print(
  kmhfr |>
    count(Owner, `Facility type`) |>
    tidyr::pivot_wider(names_from = `Facility type`, values_from = n, values_fill = 0),
  n = 30, width = Inf
)
