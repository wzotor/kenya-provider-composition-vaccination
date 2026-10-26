# 02_clean_gps
# Purpose: inspect the facility and population files, and run integrity
#          checks on the GPS cluster file before exclusions.

library(sf)
library(dplyr)
library(readxl)

p_gps   <- "data-raw/kenya_dhs_2022/geographic_data"
p_kmhfr <- "data-raw/kmhfr/kenya_health_facilities_2017.xlsx"
p_knbs  <- "data-raw/knbs/kenya_pop_density_2019.xlsx"

gps      <- st_read(file.path(p_gps, "KEGE8AFL.shp"), quiet = TRUE)
kmhfr    <- read_excel(p_kmhfr)
knbs_raw <- read_excel(p_knbs, sheet = 1, col_names = FALSE)

# ---- Facility file structure ----
cat("\n=== KMHFR STRUCTURE ===\n")
dplyr::glimpse(kmhfr)

# ---- Population sheet layout, before header handling ----
cat("\n=== KNBS RAW, FIRST 20 ROWS ===\n")
print(head(knbs_raw, 20), width = Inf)

# ---- GPS integrity checks ----
cat("\n=== GPS SOURCE CODES ===\n")
print(table(gps$SOURCE, useNA = "ifany"))

cat("\n=== GPS INTEGRITY ===\n")
print(c(
  missing_cluster_ids   = sum(is.na(gps$DHSCLUST)),
  duplicate_cluster_ids = sum(duplicated(gps$DHSCLUST)),
  missing_coordinates   = sum(is.na(gps$LATNUM) | is.na(gps$LONGNUM)),
  zero_zero_coordinates = sum(gps$LATNUM == 0 & gps$LONGNUM == 0, na.rm = TRUE)
))

# ---- Cluster 778 ----
# Displaced without the admin2 restriction; excluded in 03_apply_exclusions.R.
cat("\n=== CLUSTER 778 ===\n")
print(
  gps |>
    st_drop_geometry() |>
    filter(DHSCLUST == 778) |>
    select(DHSCLUST, SOURCE, URBAN_RURA, LATNUM, LONGNUM, ADM1NAME)
)

# ---- Urban / rural ----
cat("\n=== URBAN / RURAL ===\n")
print(table(gps$URBAN_RURA, useNA = "ifany"))
