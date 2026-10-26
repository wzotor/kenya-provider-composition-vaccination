# 14b_boundary_screen_hdx
# Purpose: recompute the cluster-to-boundary distance screen on the HDX
#          constituency layer, replacing the earlier screen computed on GADM.

library(sf)
library(dplyr)

# ---- Inputs ----
clusters <- readRDS("data-clean/clusters_hdx_final.rds")
hdx2 <- st_read("data-raw/hdx_boundaries/ken_admin2.shp", quiet = TRUE)
sf_use_s2(FALSE); hdx2 <- st_make_valid(hdx2); sf_use_s2(TRUE)

# ---- Distance to nearest constituency boundary ----
cl_m   <- st_transform(st_as_sf(clusters), 21037)
hdx_m  <- st_transform(hdx2, 21037)
hdx_ln <- st_cast(st_geometry(hdx_m), "MULTILINESTRING")

dist_m  <- st_distance(cl_m, hdx_ln)
nearest <- apply(dist_m, 1, min)

# ---- Screen flags ----
# Radii follow the DHS displacement rule: 2 km urban, 5 km rural, and 10 km
# for the 1 percent of rural clusters displaced further.
screen <- cl_m |>
  st_drop_geometry() |>
  mutate(dist_boundary_km = as.numeric(nearest) / 1000,
         urban       = URBAN_RURA == "U",
         radius_std  = ifelse(urban, 2, 5),
         radius_cons = ifelse(urban, 2, 10),
         flag_std    = dist_boundary_km < radius_std,
         flag_cons   = dist_boundary_km < radius_cons)

# ---- Summary ----
cat("clusters:", nrow(screen), "\n")
cat("flagged at 2 km urban / 5 km rural:  ",
    round(100 * mean(screen$flag_std), 1), "%\n", sep = "")
cat("flagged at 2 km urban / 10 km rural: ",
    round(100 * mean(screen$flag_cons), 1), "%\n", sep = "")
cat("median distance to boundary (km): ",
    round(median(screen$dist_boundary_km), 1), "\n", sep = "")

saveRDS(screen, "data-clean/cluster_boundary_screen_hdx.rds")