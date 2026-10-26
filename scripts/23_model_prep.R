# 23_model_prep
# Purpose: attach cluster-level geospatial covariates and prepare
#          the modelling dataset.

library(dplyr); library(haven); library(readr)

ch <- readRDS("data-clean/analysis_children.rds")

# ---- Geospatial covariates ----
gc_path <- list.files("data-raw/kenya_dhs_2022/geospatial_covariates",
                      pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
cat("Covariate file:", gc_path, "\n")
gc <- read_csv(gc_path[1], show_col_types = FALSE)

cat("\nColumns available:\n")
print(names(gc))

# ---- Merge covariates ----
gc_sel <- gc |>
  select(DHSCLUST,
         travel_time  = Travel_Times,
         nightlights  = Nightlights_Composite,
         pop_density  = UN_Population_Density_2020,
         malaria_prev = Malaria_Prevalence_2020)

dat <- ch |>
  left_join(gc_sel, by = c("v001" = "DHSCLUST"))

cat("=== COVARIATE MERGE ===\n")
cat("Rows:", nrow(dat), "\n")
for (v in c("travel_time","nightlights","pop_density","malaria_prev")) {
  cat(sprintf("%-13s missing: %d\n", v, sum(is.na(dat[[v]]))))
}

# ---- Build model variables ----
dat <- dat |>
  mutate(
    vaccinated = as.integer(basic_final),
    exposure   = factor(exposure,
                        levels = c("Government only", "Government + NGO/FBO")),
    
    # child
    child_age  = b19,
    female     = as.integer(b4 == 2),
    birth_ord  = pmin(bord, 6), 
    
    # mother / household
    mat_educ   = factor(as_factor(v106)),
    mat_age    = v012,
    wealth     = factor(as_factor(v190),
                        levels = c("poorest","poorer","middle","richer","richest")),
    urban      = as.integer(URBAN_RURA == "U"),
    
    # cluster context, logged where skewed
    log_travel = log1p(travel_time),
    log_popden = log1p(pop_density),
    log_lights = log1p(nightlights),
    
    county     = factor(ADM1NAME),
    constit    = factor(adm2_pcode)
  )

cat("\n=== MODEL VARIABLE MISSINGNESS ===\n")
mv <- c("vaccinated","exposure","child_age","female","birth_ord",
        "mat_educ","mat_age","wealth","urban",
        "log_travel","log_popden","log_lights","malaria_prev","county","constit")
for (v in mv) cat(sprintf("%-13s %d\n", v, sum(is.na(dat[[v]]))))

cat("\n=== COMPLETE CASES ===\n")
cc <- complete.cases(dat[, mv])
cat("Complete:", sum(cc), "of", nrow(dat), "\n")
print(table(dat$exposure[cc]))

saveRDS(dat[cc, ], "data-clean/model_data.rds")
