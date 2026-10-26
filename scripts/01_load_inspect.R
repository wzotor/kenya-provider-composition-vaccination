
# 01_load_inspect
# Purpose: load all data sources, confirm structure before any merging

library(haven)
library(sf)
library(dplyr)
library(readxl)
library(janitor)

# ---- File paths ----
p_kr    <- "data-raw/kenya_dhs_2022/children_recode"
p_gps   <- "data-raw/kenya_dhs_2022/geographic_data"
p_gadm  <- "data-raw/gadm_ken_shp"
p_kmhfr <- "data-raw/kmhfr/kenya_health_facilities_2017.xlsx"
p_knbs  <- "data-raw/knbs/kenya_pop_density_2019.xlsx"


cat("\n--- Children's Recode folder ---\n")
print(list.files(p_kr))

cat("\n--- Geographic data folder ---\n")
print(list.files(p_gps))

cat("\n--- GADM folder ---\n")
print(list.files(p_gadm))


# ---- Load data sources ----

# 1. Children's Recode
kr <- read_dta(file.path(p_kr, "KEKR8CFL.DTA"))

# 2. DHS GPS clusters
gps <- st_read(file.path(p_gps, "KEGE8AFL.shp"))

# 3. GADM ADM2 boundaries (comparison only)
gadm2 <- st_read(file.path(p_gadm, "gadm41_KEN_2.shp"))

# 4. KMHFR facilities
kmhfr <- read_excel(p_kmhfr)

# 5. KNBS population
knbs_raw <- read_excel(p_knbs, sheet = 1, col_names = FALSE)


# ---- Report basic dimensions ----
cat("\n=== ROW AND COLUMN COUNTS ===\n")
cat("Children's Recode:", nrow(kr), "rows,", ncol(kr), "columns\n")
cat("GPS clusters:     ", nrow(gps), "rows\n")
cat("GADM sub-counties:", nrow(gadm2), "rows\n")
cat("KMHFR facilities: ", nrow(kmhfr), "rows,", ncol(kmhfr), "columns\n")
cat("KNBS raw sheet:   ", nrow(knbs_raw), "rows,", ncol(knbs_raw), "columns\n")

# ---- Coordinate reference systems ----
cat("\n=== COORDINATE SYSTEMS ===\n")
cat("GPS CRS: ", st_crs(gps)$input, "\n")
cat("GADM CRS:", st_crs(gadm2)$input, "\n")
cat("Match:   ", st_crs(gps) == st_crs(gadm2), "\n")

# ---- GPS structure ----
cat("\n=== GPS COLUMNS ===\n")
print(names(gps))
