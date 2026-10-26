# 11_hdx_dagoretti
# Purpose: resolve whether HDX's Kilimani and Dagoretti polygons correspond
#          to Dagoretti North and Dagoretti South.

library(sf)
library(dplyr)

hdx2 <- st_read("data-raw/hdx_boundaries/ken_admin2.shp", quiet = TRUE)
fac  <- readRDS("data-clean/facilities_flagged.rds")

# ---- HDX Nairobi units ---
cat("=== HDX NAIROBI UNITS ===\n")
print(as.data.frame(
  hdx2 |> st_drop_geometry() |>
    filter(adm1_name == "Nairobi") |>
    select(adm2_name, adm2_pcode, area_sqkm) |>
    arrange(adm2_name)
))

# ---- KMHFR Nairobi constituencies ----
cat("\n=== KMHFR NAIROBI CONSTITUENCIES ===\n")
print(as.data.frame(
  fac |> filter(County == "NAIROBI") |>
    count(Constituency) |> arrange(Constituency)
))

# ---- Wards recorded for Dagoretti North and South in KMHFR ----
cat("\n=== KMHFR WARDS: DAGORETTI ===\n")
print(as.data.frame(
  fac |> filter(County == "NAIROBI",
                Constituency %in% c("DAGORETTI NORTH","DAGORETTI SOUTH")) |>
    distinct(Constituency, Ward) |> arrange(Constituency, Ward)
))

