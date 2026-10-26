# 04_boundary_checks
# Purpose: initial inspection of candidate boundary layers, and the
#          cluster-to-polygon join against GADM ADM2.
#
# NOTE ON PROVENANCE. This script documents the first attempt at assigning
# clusters to second-level administrative units, using GADM. GADM's ADM2
# layer for Kenya does not correspond to the 290 constituencies, which is
# why it was rejected in favour of the OCHA Common Operational Dataset from
# script 11 onward. Nothing produced here feeds the reported analysis.

library(sf)
library(dplyr)

p_gadm <- "data-raw/gadm_ken_shp"
p_dhsb <- "data-raw/dhs_boundaries/shps"

gps_clean <- readRDS("data-clean/gps_clean.rds")
gadm1 <- st_read(file.path(p_gadm, "gadm41_KEN_1.shp"), quiet = TRUE)
gadm2 <- st_read(file.path(p_gadm, "gadm41_KEN_2.shp"), quiet = TRUE)
dhsb  <- st_read(file.path(p_dhsb, "sdr_subnational_boundaries.shp"), quiet = TRUE)

# ---- DHS boundary layer ----
cat("\n=== DHS BOUNDARY LAYER ===\n")
cat("Features:", nrow(dhsb), "\n")
cat("CRS:", st_crs(dhsb)$input, "\n")
print(names(dhsb))
cat("\nFirst few region names:\n")
print(head(st_drop_geometry(dhsb)[, grepl("NAME|REG", names(dhsb), ignore.case = TRUE), drop = FALSE], 10))

# ---- Metadata lineage search ----
cat("\n=== METADATA LINEAGE SEARCH ===\n")
xml_path <- "data-raw/dhs_boundaries/dhs_spatial_data_subnational_metadata.xml"
if (file.exists(xml_path)) {
  xml_lines <- readLines(xml_path, warn = FALSE)
  hits <- grep("GADM|SALB|lineage|source|Bureau|Statistics|census",
               xml_lines, ignore.case = TRUE, value = TRUE)
  if (length(hits) > 0) print(head(hits, 30)) else cat("No lineage keywords found.\n")
  
  txt   <- paste(xml_lines, collapse = " ")
  start <- regexpr("Administrative0 Boundaries source", txt)
  if (start > 0) cat(substr(txt, start, start + 2000), "\n") else cat("Pattern not found\n")
} else {
  cat("XML not found at expected path.\n")
}

# ---- Spatial join: clusters to GADM ADM2 (initial attempt, later rejected) ----
clusters_sf <- gps_clean  # already an sf object with POINT geometry

joined <- st_join(clusters_sf, gadm2[, c("GID_1","NAME_1","GID_2","NAME_2")],
                  join = st_within, left = TRUE)

cat("\n=== JOIN INTEGRITY (GADM) ===\n")
cat("Clusters in:            ", nrow(clusters_sf), "\n")
cat("Rows after join:        ", nrow(joined), "\n")
cat("(inflation = multi-match)\n")
cat("Unmatched (no polygon): ", sum(is.na(joined$GID_2)), "\n")

multi <- joined |>
  st_drop_geometry() |>
  count(DHSCLUST) |>
  filter(n > 1)
cat("Clusters matching >1 polygon:", nrow(multi), "\n")
if (nrow(multi) > 0) print(multi)

# ---- County agreement: GADM-assigned vs DHS-recorded ----
# Superseded by the equivalent check against HDX in 13_join_hdx.R, which is
# the figure reported in the manuscript.
cat("\n=== COUNTY AGREEMENT (GADM, superseded) ===\n")
cty <- joined |>
  st_drop_geometry() |>
  mutate(
    dhs_county  = toupper(trimws(ADM1NAME)),
    gadm_county = toupper(trimws(NAME_1)),
    agree = dhs_county == gadm_county
  )

cat("Agree:    ", sum(cty$agree, na.rm = TRUE), "\n")
cat("Disagree: ", sum(!cty$agree, na.rm = TRUE), "\n")
cat("NA:       ", sum(is.na(cty$agree)), "\n")

cat("\nDisagreements:\n")
print(
  cty |>
    filter(!agree) |>
    count(dhs_county, gadm_county, sort = TRUE) |>
    head(30)
)

cat("\nGADM's ADM2 layer does not correspond to Kenya's 290 constituencies.\n")
cat("The analysis uses the HDX constituency layer from script 11 onward.\n")