# 16_check_govonly
# Purpose: assess whether the 27 government-only constituencies are
#          plausible or reflect omissions in the 2017 extract.

library(dplyr)

expo <- readRDS("data-clean/exposure_constituency.rds")
fac  <- readRDS("data-clean/facilities_flagged.rds")

govonly <- expo |> filter(provider_mix == "government only")

cat("=== THE 27 GOVERNMENT-ONLY CONSTITUENCIES ===\n")
print(as.data.frame(
  govonly |> select(county, unit, government, ngo_fbo, private) |>
    arrange(county, unit)
))

cat("\n=== TOTAL FACILITY COUNTS: government-only vs both ===\n")
print(
  expo |> filter(provider_mix %in% c("both","government only")) |>
    group_by(provider_mix) |>
    summarise(units = n(),
              median_gov = median(government),
              median_priv = median(private),
              median_total = median(government + ngo_fbo + private),
              min_total = min(government + ngo_fbo + private),
              .groups = "drop")
)

cat("\n=== FACILITY COUNTS IN GOVERNMENT-ONLY UNITS ===\n")
cat("Government-only units with fewer than 10 total facilities:",
    sum(govonly$government + govonly$private < 10), "of 27\n")
cat("Government-only units with 20+ government facilities:",
    sum(govonly$government >= 20), "of 27\n")

cat("\n=== COUNTY CONTEXT: NGO/FBO PRESENCE IN THE SAME COUNTIES ===\n")
print(as.data.frame(
  expo |> filter(county %in% govonly$county) |>
    group_by(county) |>
    summarise(units = n(),
              gov_only_units = sum(provider_mix == "government only"),
              units_with_ngo = sum(ngo_fbo > 0),
              total_ngo_in_county = sum(ngo_fbo),
              .groups = "drop") |>
    arrange(desc(gov_only_units))
))

expo <- readRDS("data-clean/exposure_constituency.rds")

ne <- c("Mandera", "Wajir", "Garissa")

check12 <- expo |>
  filter(provider_mix == "government only", !county %in% ne) |>
  select(county, unit, government, ngo_fbo, private) |>
  arrange(county, unit)

cat("=== THE 12 TO VERIFY ===\n")
print(as.data.frame(check12))

# Cluster contribution of the 12
cl <- readRDS("data-clean/clusters_with_exposure.rds")
cat("\n=== CLUSTERS IN EACH ===\n")
print(as.data.frame(
  cl |> filter(adm2_name %in% check12$unit) |>
    count(adm1_name, adm2_name, name = "clusters") |>
    arrange(desc(clusters))
))

cat("\nTotal clusters in the 12:", sum(cl$adm2_name %in% check12$unit), "\n")
cat("Total clusters in all 27 gov-only:", sum(cl$provider_mix == "government only"), "\n")

fac2 <- readRDS("data-clean/facilities_flagged.rds") |>
  mutate(County = ifelse(County == "ELEGEYO-MARAKWET", "ELGEYO-MARAKWET", County))
xw <- readRDS("data-clean/crosswalk_hdx.rds")

# reattach pcode/name
norm <- function(x) x |> tolower() |>
  gsub("\\bsub[- ]?county\\b", " ", x = _) |>
  gsub("[^a-z0-9]+", " ", x = _) |> trimws()

fac2 <- fac2 |>
  mutate(cty_n = norm(County), sub_n = norm(`Sub county`), con_n = norm(Constituency)) |>
  left_join(xw |> select(cty_n, sub_n, con_n, hdx_final) |> distinct(),
            by = c("cty_n","sub_n","con_n"))

units12 <- c("Bomet East","Konoin","Matayos","Isiolo South","Belgut","Lamu East",
             "Matungulu","Kuresoi South","Kipipiri","Wundanyi","Bura","Endebess")

# Check 1: NGO/FBO facilities in the 12 units that were excluded
cat("=== EXCLUDED NGO/FBO FACILITIES IN THE 12 ===\n")
hit <- fac2 |>
  filter(hdx_final %in% units12, owner_group == "ngo_fbo")
if (nrow(hit) == 0) {
  cat("No NGO/FBO record of any kind in these 12 units.\n")
} else {
  print(as.data.frame(
    hit |> select(hdx_final, Name, Owner, `Facility type`,
                  excl_function, excl_vct, excl_access)
  ))
}

# Check 2: government facility names suggesting mission origin
# Mission hospitals sometimes get recorded as Ministry of Health.
cat("\n=== FACILITY NAMES SUGGESTING MISSION/CHURCH ORIGIN IN THE 12 ===\n")
patt <- "mission|catholic|church|ack|a\\.c\\.k|pcea|sda|adventist|salvation|christian|islamic|muslim|st\\.|st |saint"
print(as.data.frame(
  fac2 |> filter(hdx_final %in% units12,
                 grepl(patt, Name, ignore.case = TRUE)) |>
    select(hdx_final, Name, Owner, owner_group)
))
