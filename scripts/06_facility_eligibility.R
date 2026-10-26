# 06_facility_eligibility
# Purpose: apply ownership classification and eligibility screens to KMHFR,
#          keeping function / access / unresolved flags separate.

library(dplyr)
library(readxl)
library(stringr)

kmhfr <- read_excel("data-raw/kmhfr/kenya_health_facilities_2017.xlsx")

# ---- VCT distribution by owners ----
cat("\n=== VCT BY OWNER (complete) ===\n")
print(
  kmhfr |>
    filter(`Facility type` == "VCT") |>
    count(Owner, sort = TRUE),
  n = 50
)
cat("Total VCT:", sum(kmhfr$`Facility type` == "VCT"), "\n")

# ---- Ownership classification ----
gov_owners <- c("Ministry of Health", "Public Institution", "Parastatal",
                "State Coorporation", "Local Authority", "Prisons", "Armed Forces")

ngofbo_owners <- c("Non-Governmental Organizations",
                   "Kenya Episcopal Conference-Catholic Secretariat",
                   "Christian Health Association of Kenya",
                   "Other Faith Based",
                   "Supreme Council for Kenya Muslims")

private_owners <- c("Private Enterprise (Institution)", "Private Company",
                    "Company Medical Service",
                    "Private Practice - Nurse / Midwifery",
                    "Private Practice - General Practitioner",
                    "Private Practice - Clinical Officer",
                    "Private Practice - Medical Specialist",
                    "Private Practice - Unspecified")

unresolved_owners <- c("Community", "Academic", "Laboratory Technician/Technologist")

fac <- kmhfr |>
  mutate(
    owner_group = case_when(
      Owner %in% gov_owners        ~ "government",
      Owner %in% ngofbo_owners     ~ "ngo_fbo",
      Owner %in% private_owners    ~ "private",
      Owner %in% unresolved_owners ~ "unresolved",
      TRUE                         ~ "UNCLASSIFIED"
    ),
    # Flag 1: non-clinical function
    excl_function = `Facility type` %in% c(
      "Laboratory", "Administrative Offices", "Radiology Clinic", "Pharmacy",
      "Regional Blood Transfusion Centre", "Blood Bank", "Farewell Home"),
    # Flag 2: standalone VCT
    excl_vct = `Facility type` == "VCT",
    # Flag 3: restricted access
    excl_access = Owner %in% c("Armed Forces", "Prisons")
  )

cat("\n=== UNCLASSIFIED OWNERS (should be none) ===\n")
print(fac |> filter(owner_group == "UNCLASSIFIED") |> count(Owner))

cat("\n=== OWNER GROUPS ===\n")
print(fac |> count(owner_group, sort = TRUE))

cat("\n=== EXCLUSION FLAGS (overlapping) ===\n")
cat("Non-clinical function:", sum(fac$excl_function), "\n")
cat("Standalone VCT:       ", sum(fac$excl_vct), "\n")
cat("Restricted access:    ", sum(fac$excl_access), "\n")
cat("Any flag (unique):    ", sum(fac$excl_function | fac$excl_vct | fac$excl_access), "\n")

cat("\n=== OVERLAP CHECK ===\n")
print(fac |> count(excl_function, excl_vct, excl_access))

# ---- Records needing name review ----
cat("\n=== COMMUNITY RECORDS ===\n")
print(fac |> filter(Owner == "Community") |>
        select(Name, `Facility type`, County, `Sub county`))

cat("\n=== ACADEMIC RECORDS ===\n")
print(fac |> filter(Owner == "Academic") |>
        select(Name, `Facility type`, County) , n = 40)

saveRDS(fac, "data-clean/facilities_flagged.rds")

cat("\n=== UNRESOLVED RECORDS: SURROUNDING PROVIDER MIX ===\n")

# Sub-counties containing the two Community records and Baraton MCH
watch <- fac |>
  filter(Owner == "Community" | Name == "Baraton MCH") |>
  distinct(County, `Sub county`)
print(watch)

# Eligible facilities already present in those sub-counties
eligible <- fac |>
  filter(!excl_function, !excl_vct, !excl_access,
         !(Owner %in% c("Academic", "Laboratory Technician/Technologist")))

eligible |>
  semi_join(watch, by = c("County", "Sub county")) |>
  count(County, `Sub county`, owner_group) |>
  print(n = 30)
