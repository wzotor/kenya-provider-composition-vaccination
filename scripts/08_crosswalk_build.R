# 08_crosswalk_build
# Purpose: match KMHFR administrative units to GADM ADM2 units within county,
#          and check which clusters fall in unnamed or duplicated units.

library(dplyr)
library(sf)
library(stringr)

gadm2 <- st_read("data-raw/gadm_ken_shp/gadm41_KEN_2.shp", quiet = TRUE)
fac   <- readRDS("data-clean/facilities_flagged.rds")

# Normalisation: lowercase, strip 'sub county' suffix, punctuation, extra space
norm <- function(x) {
  x |>
    tolower() |>
    str_replace_all("\\bsub[- ]?county\\b", " ") |>
    str_replace_all("[^a-z0-9]+", " ") |>
    str_squish()
}

gadm_key <- gadm2 |>
  st_drop_geometry() |>
  transmute(
    gid2 = GID_2,
    county_g = toupper(NAME_1),
    sub_g_raw = NAME_2,
    county_n = norm(NAME_1),
    sub_n    = norm(NAME_2)
  )

kmhfr_key <- fac |>
  distinct(county_k = County, sub_k_raw = `Sub county`, const_k_raw = Constituency) |>
  mutate(
    # fix the known county typo
    county_k = ifelse(county_k == "ELEGEYO-MARAKWET", "ELGEYO-MARAKWET", county_k),
    county_n = norm(county_k),
    sub_n    = norm(sub_k_raw),
    const_n  = norm(const_k_raw)
  )

# ---- Pass 1: exact match on county + subcounty ----
m1 <- kmhfr_key |>
  inner_join(gadm_key, by = c("county_n", "sub_n"))

cat("=== PASS 1: exact on subcounty ===\n")
cat("KMHFR units:", nrow(kmhfr_key), "\n")
cat("Matched:    ", nrow(m1), "\n")
cat("Unmatched:  ", nrow(kmhfr_key) - nrow(m1), "\n")

# ---- Pass 2: exact match on county + constituency, for the remainder ----
rest <- kmhfr_key |> anti_join(m1, by = c("county_n", "sub_n"))

m2 <- rest |>
  inner_join(gadm_key, by = c("county_n", "const_n" = "sub_n"))

cat("\n=== PASS 2: exact on constituency ===\n")
cat("Additional matched:", nrow(m2), "\n")

# ---- Remaining unmatched KMHFR units ----
still <- rest |> anti_join(m2, by = c("county_n", "const_n"))

cat("\n=== STILL UNMATCHED (", nrow(still), ") ===\n")
print(still |> select(county_k, sub_k_raw, const_k_raw), n = 60)

# ---- GADM units with no KMHFR match ----
matched_gids <- c(m1$gid2, m2$gid2)
orphan_gadm <- gadm_key |> filter(!gid2 %in% matched_gids)

cat("\n=== GADM UNITS WITH NO KMHFR MATCH (", nrow(orphan_gadm), ") ===\n")

print(as.data.frame(orphan_gadm |> select(county_g, sub_g_raw)))

# ---- Clusters in unnamed or duplicated GADM units ----
joined <- st_join(readRDS("data-clean/gps_clean.rds"),
                  gadm2[, c("GID_2","NAME_1","NAME_2")], join = st_within)

cat("\n=== CLUSTERS IN UNNAMED GADM UNITS ===\n")
print(
  joined |>
    st_drop_geometry() |>
    filter(NAME_2 == "805" | str_detect(NAME_2, "^unknown")) |>
    count(NAME_1, NAME_2, URBAN_RURA)
)

cat("\n=== CLUSTERS IN LIKUYANI / LUGARI ===\n")
print(
  joined |>
    st_drop_geometry() |>
    filter(NAME_2 %in% c("Likuyani","Lugari")) |>
    count(NAME_1, NAME_2, URBAN_RURA)
)
