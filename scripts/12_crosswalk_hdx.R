# 12_crosswalk_hdx
# Purpose: build the KMHFR-to-HDX crosswalk, with documented manual
#          correspondences, and verify it is one-to-one.

library(sf)
library(dplyr)
library(stringr)

hdx2 <- st_read("data-raw/hdx_boundaries/ken_admin2.shp", quiet = TRUE)
fac  <- readRDS("data-clean/facilities_flagged.rds")

norm <- function(x) {
  x |> tolower() |>
    str_replace_all("\\bsub[- ]?county\\b", " ") |>
    str_replace_all("[^a-z0-9]+", " ") |> str_squish()
}

hdx_key <- hdx2 |> st_drop_geometry() |>
  transmute(pcode = adm2_pcode, hdx_name = adm2_name,
            cty_n = norm(adm1_name), unit_n = norm(adm2_name))

kmhfr_key <- fac |>
  distinct(County, `Sub county`, Constituency) |>
  mutate(County = ifelse(County == "ELEGEYO-MARAKWET", "ELGEYO-MARAKWET", County),
         cty_n = norm(County), sub_n = norm(`Sub county`), con_n = norm(Constituency))

# ---- Documented manual correspondences ----
# Each maps a KMHFR constituency name to the HDX unit name, within a county.
manual <- tribble(
  ~cty_n,          ~con_n,                 ~hdx_target,        ~basis,
  "nairobi",       "dagoretti north",      "kilimani",         "ward evidence: Kilimani is a Dagoretti North ward",
  "nairobi",       "dagoretti south",      "dagoretti",        "by elimination once North is assigned",
  "kericho",       "bureti",               "buret",            "spelling variant",
  "meru",          "central imenti",       "cental imenti",    "HDX typo",
  "nyeri",         "mukurweini",           "mukurweni",        "spelling variant",
  "nyandarua",     "oljorook",            "ol jorok",         "spelling variant",
  "nyeri",         "nyeri south",          "othaya",           "ward lists match (Chinga, Iria-ini, Karima, Mahiga)",
  "samburu",       "samburu central",      "samburu west",     "ward lists match (Lodokejek, Loosuk, Maralal, Porro, Suguta Marmar)",
  "garissa",       "garissa township",     "dujis",            "documented constituency renaming",
  "embu",          "mbeere south",         "gachoka",          "documented constituency renaming",
  "embu",          "mbeere north",         "siakago",          "documented constituency renaming",
  "tharaka nithi", "chuka igambang ombe",  "nithi",            "documented constituency renaming",
  "kitui",         "mwingi central",       "mwingi east",      "documented constituency renaming",
  "homa bay",      "mbita",                "suba north",       "documented constituency renaming",
  "homa bay",      "suba",                 "suba south",       "documented constituency renaming",
  "homa bay",      "homa bay town",        "homa bay",         "spelling/short form",
  "bungoma",       "webute west",          "webuye west",      "KMHFR typo for Webuye West",
  "garissa",       "daadab",               "dadaab",           "KMHFR typo for Dadaab"
)

# ---- Match: constituency name first, then manual table ----
step1 <- kmhfr_key |>
  left_join(hdx_key |> select(cty_n, unit_n, pcode, hdx_name),
            by = c("cty_n", "con_n" = "unit_n"))

step2 <- step1 |>
  left_join(manual, by = c("cty_n", "con_n")) |>
  left_join(hdx_key |> select(cty_n, unit_n, pcode_m = pcode, hdx_name_m = hdx_name),
            by = c("cty_n", "hdx_target" = "unit_n")) |>
  mutate(
    pcode_final = coalesce(pcode, pcode_m),
    hdx_final   = coalesce(hdx_name, hdx_name_m),
    route       = case_when(!is.na(pcode) ~ "constituency name",
                            !is.na(pcode_m) ~ "manual correspondence",
                            TRUE ~ "UNMATCHED")
  )

cat("=== MATCH SUMMARY ===\n")
print(step2 |> count(route))

cat("\n=== STILL UNMATCHED ===\n")
print(as.data.frame(step2 |> filter(route == "UNMATCHED") |>
                      select(County, `Sub county`, Constituency)))

cat("\n=== HDX UNITS WITH NO KMHFR MATCH ===\n")
orph <- hdx_key |> filter(!pcode %in% step2$pcode_final)
print(as.data.frame(orph |> select(cty_n, hdx_name)))

cat("\n=== ONE-TO-ONE CHECK ===\n")
cat("KMHFR rows matched:", sum(step2$route != "UNMATCHED"), "\n")
cat("Distinct HDX pcodes used:", n_distinct(step2$pcode_final, na.rm = TRUE), "\n")
dup <- step2 |> filter(!is.na(pcode_final)) |> count(pcode_final) |> filter(n > 1)
cat("HDX units receiving >1 KMHFR row:", nrow(dup), "\n")

saveRDS(step2, "data-clean/crosswalk_hdx.rds")
