# 17_govonly_context.R
# Purpose: assess whether the 12 non-north-eastern government-only
#          constituencies are simply facility-sparse, or are genuinely
#          places with provision but no NGO/FBO presence.

library(dplyr)

expo <- readRDS("data-clean/exposure_constituency.rds")

ne <- c("Mandera", "Wajir", "Garissa")

expo2 <- expo |>
  mutate(total_fac = government + ngo_fbo + private,
         group = case_when(
           provider_mix == "government only" &  county %in% ne ~ "gov-only, north-east",
           provider_mix == "government only" & !county %in% ne ~ "gov-only, elsewhere",
           provider_mix == "both" ~ "both",
           TRUE ~ "other"))

cat("=== FACILITY TOTALS BY GROUP ===\n")
print(as.data.frame(
  expo2 |> filter(group != "other") |>
    group_by(group) |>
    summarise(units = n(),
              med_total = median(total_fac),
              med_gov   = median(government),
              med_priv  = median(private),
              q1_total  = quantile(total_fac, .25),
              q3_total  = quantile(total_fac, .75),
              .groups = "drop")
))

# Each of the 12 against the other units in its own county
cat("\n=== THE 12 IN THEIR COUNTY CONTEXT ===\n")
twelve <- expo2 |> filter(group == "gov-only, elsewhere")

ctx <- twelve |>
  rowwise() |>
  mutate(
    county_other_med_total = median(expo2$total_fac[expo2$county == county &
                                                      expo2$unit != unit]),
    county_other_med_ngo   = median(expo2$ngo_fbo[expo2$county == county &
                                                    expo2$unit != unit]),
    county_units           = sum(expo2$county == county)
  ) |>
  ungroup() |>
  select(county, unit, government, private, total_fac,
         county_other_med_total, county_other_med_ngo, county_units) |>
  arrange(total_fac)

print(as.data.frame(ctx))

cat("\n=== SUMMARY COUNTS ===\n")
cat("Units with total facilities below their county's median:",
    sum(ctx$total_fac < ctx$county_other_med_total), "of", nrow(ctx), "\n")
cat("Units where county neighbours typically have 0 NGO/FBO too:",
    sum(ctx$county_other_med_ngo == 0), "of", nrow(ctx), "\n")
