# 07_crosswalk_inspect
# Purpose: inspect the three administrative naming vocabularies before matching.

library(dplyr)
library(sf)
library(readxl)
library(stringr)

gadm2 <- st_read("data-raw/gadm_ken_shp/gadm41_KEN_2.shp", quiet = TRUE)
fac   <- readRDS("data-clean/facilities_flagged.rds")

# ---- GADM name structure ----
gadm_names <- gadm2 |>
  st_drop_geometry() |>
  select(county = NAME_1, subcounty = NAME_2)

cat("=== GADM ===\n")
cat("Units:", nrow(gadm_names), "\n")
cat("Unique county names:", n_distinct(gadm_names$county), "\n")
cat("Subcounty names appearing in more than one county:\n")
print(gadm_names |> count(subcounty) |> filter(n > 1))
cat("\nFirst 20:\n")
print(head(gadm_names, 20))

# ---- KMHFR: two candidate keys ----
kmhfr_sub <- fac |>
  distinct(county = County, subcounty = `Sub county`, constituency = Constituency)

cat("\n=== KMHFR ===\n")
cat("Distinct county x subcounty pairs:", n_distinct(paste(fac$County, fac$`Sub county`)), "\n")
cat("Distinct county x constituency pairs:", n_distinct(paste(fac$County, fac$Constituency)), "\n")
cat("\nSubcounty values containing 'sub county' suffix:",
    sum(str_detect(tolower(kmhfr_sub$subcounty), "sub county")), "\n")
cat("\nFirst 20 pairs:\n")
print(head(kmhfr_sub, 20))

# ---- County name comparison ----
cat("\n=== COUNTY NAME COMPARISON ===\n")
g_cty <- sort(unique(toupper(gadm_names$county)))
k_cty <- sort(unique(toupper(fac$County)))
cat("GADM counties:", length(g_cty), " KMHFR counties:", length(k_cty), "\n")
cat("\nIn GADM not KMHFR:\n"); print(setdiff(g_cty, k_cty))
cat("\nIn KMHFR not GADM:\n"); print(setdiff(k_cty, g_cty))
