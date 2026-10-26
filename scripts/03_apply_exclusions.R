# 03_apply_exclusions
# Purpose: apply documented GPS exclusions, record counts for the sample flow.

library(sf)
library(dplyr)

p_gps <- "data-raw/kenya_dhs_2022/geographic_data"
gps   <- st_read(file.path(p_gps, "KEGE8AFL.shp"), quiet = TRUE)

flow <- tibble(stage = character(), n = integer(), removed = integer())
add_stage <- function(flow, label, n_now) {
  prev <- if (nrow(flow) == 0) n_now else tail(flow$n, 1)
  bind_rows(flow, tibble(stage = label, n = n_now, removed = prev - n_now))
}

flow <- add_stage(flow, "GPS clusters as released", nrow(gps))

# Guard: unverified coordinates (removes 0 in this survey)
gps <- gps |> filter(SOURCE != "MIS")
flow <- add_stage(flow, "After removing SOURCE == MIS", nrow(gps))

# Guard: null island (removes 0 in this survey)
gps <- gps |> filter(!(LATNUM == 0 & LONGNUM == 0))
flow <- add_stage(flow, "After removing 0,0 coordinates", nrow(gps))

# Exclusion: cluster 778, displaced without admin2 restriction
gps <- gps |> filter(DHSCLUST != 778)
flow <- add_stage(flow, "After removing cluster 778", nrow(gps))

cat("\n=== GPS SAMPLE FLOW ===\n")
print(as.data.frame(flow))

gps_clean <- gps |>
  select(DHSCLUST, SOURCE, URBAN_RURA, LATNUM, LONGNUM, ALT_DEM,
         ADM1DHS, ADM1NAME, DHSREGCO, DHSREGNA)

dir.create("data-clean", showWarnings = FALSE)
saveRDS(gps_clean, "data-clean/gps_clean.rds")

cat("\nSaved", nrow(gps_clean), "clusters to data-clean/gps_clean.rds\n")
