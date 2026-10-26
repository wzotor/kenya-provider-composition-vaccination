# 33_map.R
# Purpose: map of Kenya's 290 constituencies by provider category.

library(sf); library(dplyr); library(ggplot2)

hdx2 <- st_read("data-raw/hdx_boundaries/ken_admin2.shp", quiet = TRUE)
hdx1 <- st_read("data-raw/hdx_boundaries/ken_admin1.shp", quiet = TRUE)
sf_use_s2(FALSE)
hdx2 <- st_make_valid(hdx2)
hdx1 <- st_make_valid(hdx1)
sf_use_s2(TRUE)

expo <- readRDS("data-clean/exposure_constituency.rds")

mapdat <- hdx2 |>
  left_join(dplyr::select(expo, pcode, provider_mix),
            by = c("adm2_pcode" = "pcode")) |>
  mutate(
    category = case_when(
      adm2_name %in% c("Ganze", "Siakago") ~ "Unresolved",
      provider_mix == "both"               ~ "Government and NGO/FBO",
      provider_mix == "government only"    ~ "Government only",
      TRUE                                 ~ "Unresolved"))

n <- table(mapdat$category)

lab_both <- paste0("Government and NGO/FBO (", n[["Government and NGO/FBO"]], ")")
lab_gov  <- paste0("Government only (", n[["Government only"]], ")")
lab_unr  <- paste0("Unresolved (", n[["Unresolved"]], ")")

mapdat <- mapdat |>
  mutate(category = factor(
    recode(category,
           "Government and NGO/FBO" = lab_both,
           "Government only"        = lab_gov,
           "Unresolved"             = lab_unr),
    levels = c(lab_both, lab_gov, lab_unr)))

# County labels for the five counties in the geographic sensitivity analysis
five <- c("Mandera", "Wajir", "Garissa", "Tana River", "Isiolo")
lab_pts <- hdx1 |>
  filter(adm1_name %in% five) |>
  st_point_on_surface() |>
  mutate(lon = st_coordinates(geometry)[, 1],
         lat = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry()

fills <- c("#4C72B0", "#DD8452", "#CCCCCC")
names(fills) <- c(lab_both, lab_gov, lab_unr)

p <- ggplot(mapdat) +
  geom_sf(aes(fill = category), colour = "white", linewidth = 0.10) +
  geom_sf(data = hdx1, fill = NA, colour = "grey25", linewidth = 0.30) +
  geom_text(data = lab_pts, aes(lon, lat, label = adm1_name),
            size = 2.9, fontface = "bold", colour = "grey15") +
  scale_fill_manual(values = fills, name = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_void(base_size = 10) +
  theme(
    legend.position  = "bottom",
    legend.text      = element_text(size = 8.5),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin      = margin(8, 8, 8, 8))

dir.create("output", showWarnings = FALSE)
ggsave("output/fig_provider_map.png", p, width = 6.5, height = 7.0,
       dpi = 400, bg = "white")
ggsave("output/fig_provider_map.pdf", p, width = 6.5, height = 7.0, bg = "white")

cat("Saved to output/fig_provider_map.png and .pdf\n")
