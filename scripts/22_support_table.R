# 22_support_table
# Purpose: link children to constituencies, apply the unresolved-constituency
#          exclusion to form the analytic sample, and report exposure support.

library(haven); library(dplyr); library(survey); library(sf)

elig     <- readRDS("data-clean/children_eligible.rds")
clusters <- readRDS("data-clean/clusters_with_exposure.rds")
expo     <- readRDS("data-clean/exposure_constituency.rds")

# ---- Link children to constituency via cluster ----
link <- clusters |>
  select(DHSCLUST, adm2_pcode, adm2_name, adm1_name,
         URBAN_RURA, ADM1NAME, provider_mix, government, ngo_fbo, private)

ch <- elig |>
  left_join(link, by = c("v001" = "DHSCLUST"))

cat("=== LINKAGE ===\n")
cat("Eligible children:        ", nrow(ch), "\n")
cat("Linked to a constituency: ", sum(!is.na(ch$adm2_pcode)), "\n")
cat("Unlinked:                 ", sum(is.na(ch$adm2_pcode)), "\n")

# ---- Apply exclusions: unresolved constituencies (Ganze, Siakago) ----
unresolved <- c("Ganze", "Siakago")
ch <- ch |> mutate(excluded = adm2_name %in% unresolved)

cat("\nChildren in unresolved constituencies:", sum(ch$excluded), "\n")

ch_an <- ch |> filter(!excluded, !is.na(adm2_pcode)) |>
  mutate(exposure = ifelse(provider_mix == "both",
                           "Government + NGO/FBO", "Government only"),
         wt = v005/1e6)

cat("Analytic sample:", nrow(ch_an), "\n")

# ---- Core support table ----
cat("\n=== SUPPORT BY EXPOSURE GROUP ===\n")
tab <- ch_an |>
  group_by(exposure) |>
  summarise(
    children_unwt   = n(),
    constituencies  = n_distinct(adm2_pcode),
    clusters        = n_distinct(v001),
    counties        = n_distinct(adm1_name),
    vaccinated      = sum(basic_final),
    not_vaccinated  = sum(!basic_final),
    .groups = "drop")
print(as.data.frame(tab))

# ---- Weighted coverage by group ----
des <- svydesign(ids=~v021, strata=~v022, weights=~wt, data=ch_an, nest=TRUE)
options(survey.lonely.psu="adjust")

cat("\n=== WEIGHTED COVERAGE BY EXPOSURE (unadjusted) ===\n")
print(svyby(~basic_final, ~exposure, des, svymean))

# ---- Concentration of the comparison group ----
cat("\n=== COMPARISON GROUP BY COUNTY ===\n")
print(as.data.frame(
  ch_an |> filter(exposure == "Government only") |>
    group_by(adm1_name) |>
    summarise(children = n(), constituencies = n_distinct(adm2_pcode),
              .groups="drop") |>
    arrange(desc(children))
))

# ---- Comparability on key characteristics ----
cat("\n=== URBAN / RURAL ===\n")
print(round(100*prop.table(table(ch_an$exposure, ch_an$URBAN_RURA), 1), 1))

cat("\n=== WEALTH QUINTILE (v190) ===\n")
print(round(100*prop.table(table(ch_an$exposure, as_factor(ch_an$v190)), 1), 1))

cat("\n=== MOTHER'S EDUCATION (v106) ===\n")
print(round(100*prop.table(table(ch_an$exposure, as_factor(ch_an$v106)), 1), 1))

saveRDS(ch_an, "data-clean/analysis_children.rds")
