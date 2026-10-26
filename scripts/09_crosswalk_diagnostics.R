# 09_crosswalk_diagnostics
# Purpose: check how GADM classifies its ADM2 units, identify cases where
#          sub-county and constituency matching disagree, and inspect the
#          KNBS workbook structure.

library(dplyr)
library(sf)
library(stringr)
library(readxl)

gadm2 <- st_read("data-raw/gadm_ken_shp/gadm41_KEN_2.shp", quiet = TRUE)
fac   <- readRDS("data-clean/facilities_flagged.rds")

# ---- GADM ADM2 unit types ----
cat("=== GADM ADM2 UNIT TYPES ===\n")
print(gadm2 |> st_drop_geometry() |> count(TYPE_2, ENGTYPE_2, sort = TRUE))

cat("\n=== TYPE for the disputed units ===\n")
print(
  gadm2 |> st_drop_geometry() |>
    filter(NAME_2 %in% c("Likuyani","Lugari","Othaya","Samburu West","Lungalunga","Ol Jorok")) |>
    select(NAME_1, NAME_2, TYPE_2, ENGTYPE_2)
)

# ---- Rebuild keys ----
norm <- function(x) {
  x |> tolower() |>
    str_replace_all("\\bsub[- ]?county\\b", " ") |>
    str_replace_all("[^a-z0-9]+", " ") |> str_squish()
}

gadm_key <- gadm2 |> st_drop_geometry() |>
  transmute(gid2 = GID_2, county_n = norm(NAME_1), sub_n = norm(NAME_2),
            sub_g_raw = NAME_2)

kmhfr_key <- fac |>
  distinct(county_k = County, sub_k_raw = `Sub county`, const_k_raw = Constituency) |>
  mutate(county_k = ifelse(county_k == "ELEGEYO-MARAKWET", "ELGEYO-MARAKWET", county_k),
         county_n = norm(county_k), sub_n = norm(sub_k_raw), const_n = norm(const_k_raw))

# ---- Both match routes, run independently ----
via_sub <- kmhfr_key |>
  left_join(gadm_key |> select(county_n, sub_n, gid_sub = gid2, name_sub = sub_g_raw),
            by = c("county_n", "sub_n"))

both <- via_sub |>
  left_join(gadm_key |> select(county_n, sub_n, gid_con = gid2, name_con = sub_g_raw),
            by = c("county_n", "const_n" = "sub_n"))

cat("\n=== DUAL-MATCH OUTCOMES ===\n")
cat("Both routes matched:      ", sum(!is.na(both$gid_sub) & !is.na(both$gid_con)), "\n")
cat("  of which they AGREE:    ", sum(both$gid_sub == both$gid_con, na.rm = TRUE), "\n")
cat("  of which they CONFLICT: ", sum(both$gid_sub != both$gid_con, na.rm = TRUE), "\n")
cat("Only sub-county matched:  ", sum(!is.na(both$gid_sub) & is.na(both$gid_con)), "\n")
cat("Only constituency matched:", sum(is.na(both$gid_sub) & !is.na(both$gid_con)), "\n")
cat("Neither matched:          ", sum(is.na(both$gid_sub) & is.na(both$gid_con)), "\n")

cat("\n=== CONFLICTS ===\n")
print(as.data.frame(
  both |> filter(!is.na(gid_sub), !is.na(gid_con), gid_sub != gid_con) |>
    select(county_k, sub_k_raw, name_sub, const_k_raw, name_con)
))

# ---- Ward fields for the two proposed correspondences ----
cat("\n=== WARDS: Samburu Central / Nyeri South ===\n")
print(as.data.frame(
  fac |> filter((County == "SAMBURU" & str_detect(tolower(`Sub county`), "samburu central")) |
                  (County == "NYERI"   & str_detect(tolower(`Sub county`), "nyeri south"))) |>
    distinct(County, `Sub county`, Constituency, Ward) |> arrange(County, Ward)
))

# ---- Facilities in the five conflicting combinations ----
conflicts <- tribble(
  ~County,    ~sub,               ~const,
  "NAIROBI",  "dagoretti north",  "DAGORETTI SOUTH",
  "NAIROBI",  "langata",          "KIBRA",
  "KITUI",    "kitui rural",      "KITUI CENTRAL",
  "MURANG'A", "mathioya",         "KANGEMA",
  "LAIKIPIA", "laikipia east",    "LAIKIPIA NORTH"
)

cat("=== FACILITIES IN CONFLICTING COMBINATIONS ===\n")
print(as.data.frame(
  fac |>
    inner_join(conflicts, by = c("County", "Sub county" = "sub", "Constituency" = "const")) |>
    count(County, `Sub county`, Constituency, Ward)
))

#---- Wards shared across more than one constituency ----
cat("\n=== WARDS SHARED ACROSS >1 CONSTITUENCY (should be rare) ===\n")
print(as.data.frame(
  fac |>
    distinct(County, Constituency, Ward) |>
    count(County, Ward) |>
    filter(n > 1) |>
    arrange(desc(n))
))

# ---- KNBS workbook structure ----
knbs <- read_excel("data-raw/knbs/kenya_pop_density_2019.xlsx", sheet = 1, col_names = FALSE)
cat("\n=== KNBS SHEET NAMES ===\n")
print(excel_sheets("data-raw/knbs/kenya_pop_density_2019.xlsx"))
