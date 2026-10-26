# 37_flow
# Purpose: verify the sample flow counts for the flow diagram.

library(dplyr); library(haven); library(sf)

kr   <- read_dta("data-raw/kenya_dhs_2022/children_recode/KEKR8CFL.DTA")
gps  <- st_read("data-raw/kenya_dhs_2022/geographic_data/KEGE8AFL.shp", quiet = TRUE)
elig <- readRDS("data-clean/children_eligible.rds")
dat  <- readRDS("data-clean/model_data.rds")

n_778 <- sum(gps$DHSCLUST == 778)

cat("=== CLUSTER FLOW ===\n")
cat("Clusters released:                  ", nrow(gps), "\n")
cat("  removed, SOURCE == MIS:           ", sum(gps$SOURCE == "MIS"), "\n")
cat("  removed, coordinates 0,0:         ",
    sum(gps$LATNUM == 0 & gps$LONGNUM == 0), "\n")
cat("  removed, cluster 778:             ", n_778, "\n")
cat("Clusters retained:                  ", nrow(gps) - n_778, "\n")

cat("\n=== CHILD FLOW ===\n")
cat("Children in Children's Recode:      ", nrow(kr), "\n")
cat("  not alive (b5 != 1):              ", sum(kr$b5 != 1, na.rm = TRUE), "\n")
alive <- kr |> filter(b5 == 1)
cat("Living children:                    ", nrow(alive), "\n")
cat("  outside 12-23 months:             ",
    sum(!(alive$b19 >= 12 & alive$b19 <= 23), na.rm = TRUE), "\n")
cat("Eligible (alive, 12-23 months):     ", nrow(elig), "\n")

ch_778  <- sum(elig$v001 == 778)
n_unres <- nrow(elig) - ch_778 - nrow(dat)

cat("  in dropped cluster 778:           ", ch_778, "\n")
cat("  in unresolved constituencies:     ", n_unres, "\n")
cat("Analytic sample:                    ", nrow(dat), "\n")

cat("\n=== CHECK ===\n")
cat(nrow(elig), "-", ch_778, "-", n_unres, "=",
    nrow(elig) - ch_778 - n_unres, "\n")

saveRDS(list(recode         = nrow(kr),
             alive          = nrow(alive),
             eligible       = nrow(elig),
             analytic       = nrow(dat),
             not_alive      = nrow(kr) - nrow(alive),
             outside_window = nrow(alive) - nrow(elig),
             cluster778     = ch_778,
             unresolved     = n_unres),
        "data-clean/flow_counts.rds")