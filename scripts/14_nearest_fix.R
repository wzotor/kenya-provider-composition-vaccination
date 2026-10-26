# 14_nearest_fix
# Purpose: resolve clusters falling outside all HDX polygons by
#          nearest-polygon assignment, with distances reported.

library(sf)
library(dplyr)

hdx2   <- st_read("data-raw/hdx_boundaries/ken_admin2.shp", quiet = TRUE)
sf_use_s2(FALSE); hdx2 <- st_make_valid(hdx2); sf_use_s2(TRUE)

joined <- readRDS("data-clean/clusters_hdx.rds")

miss <- joined |> filter(is.na(adm2_pcode))
cat("Unmatched clusters:", nrow(miss), "\n\n")

# Project to metres for distance measurement
hdx_m  <- st_transform(hdx2, 21037)
miss_m <- st_transform(miss, 21037)

idx  <- st_nearest_feature(miss_m, hdx_m)
dist <- as.numeric(st_distance(miss_m, hdx_m[idx, ], by_element = TRUE))

out <- miss |> st_drop_geometry() |>
  select(DHSCLUST, URBAN_RURA, ADM1NAME) |>
  mutate(nearest_unit   = hdx2$adm2_name[idx],
         nearest_county = hdx2$adm1_name[idx],
         distance_m     = round(dist),
         county_agrees  = toupper(ADM1NAME) == toupper(nearest_county))

cat("=== NEAREST-POLYGON ASSIGNMENT ===\n")
print(as.data.frame(out))

# ---- Apply the assignment ----
joined_fixed <- joined
i <- which(is.na(joined_fixed$adm2_pcode))

joined_fixed$adm2_pcode[i] <- hdx2$adm2_pcode[idx]
joined_fixed$adm2_name[i]  <- hdx2$adm2_name[idx]
joined_fixed$adm1_pcode[i] <- hdx2$adm1_pcode[idx]
joined_fixed$adm1_name[i]  <- hdx2$adm1_name[idx]
joined_fixed$assign_method <- "point-in-polygon"
joined_fixed$assign_method[i] <- "nearest polygon (<600m)"

cat("\n=== FINAL ASSIGNMENT ===\n")
cat("Clusters:", nrow(joined_fixed), "\n")
cat("Unassigned:", sum(is.na(joined_fixed$adm2_pcode)), "\n")
print(table(joined_fixed$assign_method))

cat("\nConstituencies containing at least one cluster:",
    n_distinct(joined_fixed$adm2_pcode), "of 290\n")

saveRDS(joined_fixed, "data-clean/clusters_hdx_final.rds")
