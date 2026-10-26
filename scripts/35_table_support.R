# 35_table_support
# Purpose: provider-category support table.

library(dplyr)
library(knitr)

expo <- readRDS("data-clean/exposure_constituency.rds")
cl   <- readRDS("data-clean/clusters_with_exposure.rds")
dat  <- readRDS("data-clean/model_data.rds")

unres <- c("Ganze", "Siakago")

expo2 <- expo |>
  mutate(cat = case_when(
    unit %in% unres              ~ "Unresolved classification",
    provider_mix == "both"       ~ "Government and NGO/FBO",
    provider_mix == "government only" ~ "Government only",
    provider_mix == "NGO/FBO only"    ~ "NGO/FBO only",
    TRUE ~ "Unresolved classification"))

# constituencies contributing eligible children, by category
contrib <- dat |>
  count(constit, name = "children") |>
  left_join(expo2 |> dplyr::select(pcode, cat), by = c("constit" = "pcode"))

tab <- expo2 |>
  count(cat, name = "constituencies_mapped") |>
  left_join(
    cl |> count(provider_mix, name = "clusters") |>
      mutate(cat = case_when(
        provider_mix == "both" ~ "Government and NGO/FBO",
        provider_mix == "government only" ~ "Government only",
        TRUE ~ "Unresolved classification")) |>
      dplyr::select(cat, clusters),
    by = "cat") |>
  left_join(
    contrib |> group_by(cat) |>
      summarise(constituencies_analysed = n(),
                children = sum(children), .groups = "drop"),
    by = "cat")

# add the empty NGO/FBO-only row explicitly
if (!"NGO/FBO only" %in% tab$cat) {
  tab <- bind_rows(tab,
                   data.frame(cat = "NGO/FBO only", constituencies_mapped = 0,
                              clusters = 0, constituencies_analysed = 0, children = 0))
}

tab <- tab |>
  mutate(across(everything(), ~tidyr::replace_na(.x, 0))) |>
  arrange(factor(cat, levels = c("Government and NGO/FBO", "Government only",
                                 "NGO/FBO only", "Unresolved classification")))

cat("=== PROVIDER-CATEGORY SUPPORT TABLE ===\n\n")
print(as.data.frame(tab), row.names = FALSE)

cat("\n=== DENOMINATOR RECONCILIATION ===\n")
cat("Constituencies mapped:              ", sum(tab$constituencies_mapped), "\n")
cat("With resolved classification:       ",
    sum(tab$constituencies_mapped[tab$cat != "Unresolved classification"]), "\n")
cat("Contributing eligible children:     ", sum(tab$constituencies_analysed), "\n")
cat("Children in the analytic sample:    ", sum(tab$children), "\n")

write.csv(tab, "output/tab_support.csv", row.names = FALSE)
cat("\nSaved to output/tab_support.csv\n")

tab_out <- tab |>
  mutate(across(where(is.numeric), ~format(.x, big.mark = ",", trim = TRUE))) |>
  rename(`Provider category` = cat,
         `Constituencies mapped` = constituencies_mapped,
         `DHS clusters` = clusters,
         `Constituencies contributing eligible children` = constituencies_analysed,
         `Children analysed` = children)

cat("\n\n=== MARKDOWN ===\n\n")
print(kable(tab_out, format = "pipe", align = c("l","r","r","r","r")))

cat("\n\n=== PLAIN TEXT ===\n\n")
print(kable(tab_out, format = "simple", align = c("l","r","r","r","r")))
