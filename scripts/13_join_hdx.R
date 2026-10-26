# 13_join_hdxssssss
# Purpose: assign DHS clusters to HDX constituencies; repeat the join
#          integrity and county agreement checks on the new layer.

library(sf)
library(dplyr)

hdx2      <- st_read("data-raw/hdx_boundaries/ken_admin2.shp", quiet = TRUE)
gps_clean <- readRDS("data-clean/gps_clean.rds")

cat("CRS match:", st_crs(gps_clean) == st_crs(hdx2), "\n")

# ---- Repair invalid geometries ----
cat("Invalid before repair:", sum(!st_is_valid(hdx2)), "\n")
sf_use_s2(FALSE)
hdx2 <- st_make_valid(hdx2)
sf_use_s2(TRUE)
cat("Invalid after repair: ", sum(!st_is_valid(hdx2)), "\n\n")

# ---- Spatial join ----
joined <- st_join(gps_clean,
                  hdx2[, c("adm2_pcode", "adm2_name", "adm1_pcode", "adm1_name")],
                  join = st_within, left = TRUE)

cat("=== JOIN INTEGRITY ===\n")
cat("Clusters in:            ", nrow(gps_clean), "\n")
cat("Rows after join:        ", nrow(joined), "\n")
cat("Unmatched (no polygon): ", sum(is.na(joined$adm2_pcode)), "\n")

multi <- joined |> st_drop_geometry() |> count(DHSCLUST) |> filter(n > 1)
cat("Clusters matching >1 polygon:", nrow(multi), "\n")
if (nrow(multi) > 0) print(as.data.frame(multi))

if (sum(is.na(joined$adm2_pcode)) > 0) {
  cat("\n=== UNMATCHED CLUSTER DETAIL ===\n")
  print(as.data.frame(
    joined |> st_drop_geometry() |> filter(is.na(adm2_pcode)) |>
      select(DHSCLUST, URBAN_RURA, LATNUM, LONGNUM, ADM1NAME)
  ))
}

# ---- County agreement: HDX-assigned vs DHS-recorded ----
cat("\n=== COUNTY AGREEMENT ===\n")
cty <- joined |> st_drop_geometry() |>
  mutate(dhs_county = toupper(trimws(ADM1NAME)),
         hdx_county = toupper(trimws(adm1_name)),
         agree      = dhs_county == hdx_county)

cat("Agree:   ", sum(cty$agree, na.rm = TRUE), "\n")
cat("Disagree:", sum(!cty$agree, na.rm = TRUE), "\n")
cat("NA:      ", sum(is.na(cty$agree)), "\n")

cat("\nDisagreements:\n")
print(as.data.frame(
  cty |> filter(!agree) |> count(dhs_county, hdx_county, sort = TRUE)
))

saveRDS(joined, "data-clean/clusters_hdx.rds")
cat("\nSaved", nrow(joined), "clusters to data-clean/clusters_hdx.rds\n")
