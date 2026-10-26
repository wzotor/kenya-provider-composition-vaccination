# 15_exposure
# Purpose: aggregate eligible facilities to HDX constituencies via the
#          crosswalk, build the four-category provider exposure, report its
#          distribution, and check the units with no eligible facilities.

library(sf)
library(dplyr)
library(tidyr)
library(stringr)

fac      <- readRDS("data-clean/facilities_flagged.rds")
xw       <- readRDS("data-clean/crosswalk_hdx.rds")
clusters <- readRDS("data-clean/clusters_hdx_final.rds")
hdx2     <- st_read("data-raw/hdx_boundaries/ken_admin2.shp", quiet = TRUE)

norm <- function(x) {
  x |> tolower() |>
    str_replace_all("\\bsub[- ]?county\\b", " ") |>
    str_replace_all("[^a-z0-9]+", " ") |> str_squish()
}

# ---- Attach pcode to every facility via the crosswalk ----
xw_lookup <- xw |>
  select(cty_n, sub_n, con_n, pcode_final, hdx_final) |>
  distinct()

fac2 <- fac |>
  mutate(County = ifelse(County == "ELEGEYO-MARAKWET", "ELGEYO-MARAKWET", County),
         cty_n = norm(County), sub_n = norm(`Sub county`), con_n = norm(Constituency)) |>
  left_join(xw_lookup, by = c("cty_n", "sub_n", "con_n"))

cat("=== CROSSWALK COVERAGE OF FACILITIES ===\n")
cat("Facilities total:      ", nrow(fac2), "\n")
cat("With pcode assigned:   ", sum(!is.na(fac2$pcode_final)), "\n")
cat("Without pcode:         ", sum(is.na(fac2$pcode_final)), "\n")

# ---- Apply eligibility screens ----
elig <- fac2 |>
  filter(!excl_function, !excl_vct, !excl_access,
         !(Owner %in% c("Academic", "Laboratory Technician/Technologist")))

cat("\n=== ELIGIBILITY FLOW ===\n")
cat("All facilities:        ", nrow(fac2), "\n")
cat("After exclusions:      ", nrow(elig), "\n")
print(elig |> count(owner_group))

# ---- Aggregate to constituency ----
counts <- elig |>
  filter(!is.na(pcode_final), owner_group %in% c("government","ngo_fbo","private")) |>
  count(pcode_final, owner_group) |>
  pivot_wider(names_from = owner_group, values_from = n, values_fill = 0)

# Every HDX unit is in the crosswalk, so any unit absent here has a
# verified zero (the crosswalk matched, there were simply no eligible
# facilities), not a failed match.

expo <- hdx2 |> st_drop_geometry() |>
  select(pcode = adm2_pcode, unit = adm2_name, county = adm1_name) |>
  left_join(counts, by = c("pcode" = "pcode_final")) |>
  mutate(across(c(government, ngo_fbo, private), ~replace_na(.x, 0)),
         provider_mix = case_when(
           government >  0 & ngo_fbo >  0 ~ "both",
           government >  0 & ngo_fbo == 0 ~ "government only",
           government == 0 & ngo_fbo >  0 ~ "NGO/FBO only",
           TRUE                           ~ "neither"
         ))

cat("\n=== EXPOSURE: CONSTITUENCIES ===\n")
print(as.data.frame(expo |> count(provider_mix, name = "constituencies")))

# ---- Clusters per category ----
cl <- clusters |> st_drop_geometry() |>
  left_join(expo |> select(pcode, provider_mix, government, ngo_fbo, private),
            by = c("adm2_pcode" = "pcode"))

cat("\n=== EXPOSURE: CLUSTERS ===\n")
print(as.data.frame(cl |> count(provider_mix, name = "clusters")))

cat("\n=== BY URBAN/RURAL ===\n")
print(table(cl$provider_mix, cl$URBAN_RURA))

# ---- Within-county variation (decides FE vs RE) ----
cat("\n=== WITHIN-COUNTY VARIATION ===\n")
county_var <- expo |>
  group_by(county) |>
  summarise(n_units = n(), n_categories = n_distinct(provider_mix), .groups = "drop")

print(as.data.frame(county_var |> count(n_categories, name = "counties")))

cat("\nCounties with NO within-county variation:\n")
print(as.data.frame(
  county_var |> filter(n_categories == 1) |>
    left_join(expo |> distinct(county, provider_mix), by = "county")
))

cat("\n=== NEITHER / NGO-ONLY UNITS ===\n")
print(as.data.frame(
  expo |> filter(provider_mix %in% c("neither","NGO/FBO only")) |>
    select(county, unit, government, ngo_fbo, private, provider_mix)
))

saveRDS(expo, "data-clean/exposure_constituency.rds")
saveRDS(cl,   "data-clean/clusters_with_exposure.rds")

# ---- Units with no eligible government or NGO/FBO facilities ----
cat("=== GANZE AND SIAKAGO: ALL FACILITIES, PRE-EXCLUSION ===\n")
print(as.data.frame(
  fac2 |> filter(hdx_final %in% c("Ganze","Siakago")) |>
    count(hdx_final, Owner, `Facility type`, excl_function, excl_vct, excl_access)
))

cat("\n=== KMHFR ROWS MAPPING TO THESE UNITS ===\n")
print(as.data.frame(
  xw |> filter(hdx_final %in% c("Ganze","Siakago")) |>
    select(County, `Sub county`, Constituency, hdx_final, route)
))

cat("\n=== EXPOSURE IF VCT AND ARMED FORCES WERE INCLUDED ===\n")
alt <- fac2 |>
  filter(!excl_function, !(Owner %in% c("Academic","Laboratory Technician/Technologist"))) |>
  filter(owner_group %in% c("government","ngo_fbo")) |>
  count(pcode_final, owner_group) |>
  tidyr::pivot_wider(names_from = owner_group, values_from = n, values_fill = 0)

alt_expo <- hdx2 |> st_drop_geometry() |>
  select(pcode = adm2_pcode) |>
  left_join(alt, by = c("pcode" = "pcode_final")) |>
  mutate(across(c(government, ngo_fbo), ~tidyr::replace_na(.x, 0)),
         mix = case_when(government > 0 & ngo_fbo > 0 ~ "both",
                         government > 0 ~ "government only",
                         ngo_fbo > 0 ~ "NGO/FBO only",
                         TRUE ~ "neither"))
print(as.data.frame(alt_expo |> count(mix)))
