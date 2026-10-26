# 40_tables_html
# Purpose: Table 1, provider categories and analytic support.

library(dplyr)
library(knitr)
library(kableExtra)

expo <- readRDS("data-clean/exposure_constituency.rds")
cl   <- readRDS("data-clean/clusters_with_exposure.rds")
dat  <- readRDS("data-clean/model_data.rds")

unres <- c("Ganze", "Siakago")

expo2 <- expo |>
  mutate(cat = case_when(
    unit %in% unres                   ~ "Unresolved classification",
    provider_mix == "both"            ~ "Government and NGO/FBO",
    provider_mix == "government only" ~ "Government only",
    TRUE                              ~ "Unresolved classification"))

cl2 <- cl |>
  mutate(cat = case_when(
    adm2_name %in% unres              ~ "Unresolved classification",
    provider_mix == "both"            ~ "Government and NGO/FBO",
    provider_mix == "government only" ~ "Government only",
    TRUE                              ~ "Unresolved classification"))

analysed <- dat |>
  count(constit, name = "children") |>
  left_join(expo2 |> dplyr::select(pcode, cat), by = c("constit" = "pcode")) |>
  group_by(cat) |>
  summarise(constit_n = n(), children = sum(children), .groups = "drop")

t1 <- data.frame(cat = c("Government and NGO/FBO", "Government only",
                         "NGO/FBO only", "Neither, verified absence",
                         "Unresolved classification")) |>
  left_join(count(expo2, cat, name = "mapped"),   by = "cat") |>
  left_join(count(cl2,   cat, name = "clusters"), by = "cat") |>
  left_join(analysed, by = "cat") |>
  mutate(across(where(is.numeric), ~tidyr::replace_na(.x, 0)))

t1 <- rbind(t1, data.frame(cat = "Total",
                           mapped    = sum(t1$mapped),
                           clusters  = sum(t1$clusters),
                           constit_n = sum(t1$constit_n),
                           children  = sum(t1$children)))

names(t1) <- c("Provider category", "Constituencies mapped",
               "DHS clusters mapped", "Constituencies in analytic sample",
               "Children analysed")

t1[] <- lapply(t1, function(x)
  if (is.numeric(x)) format(x, big.mark = ",", trim = TRUE) else x)

kable(t1, format = "html", align = c("l", "r", "r", "r", "r"),
      caption = "Table 1. Provider categories and analytic support, Kenyan constituencies") |>
  kable_classic(full_width = FALSE, html_font = "Times New Roman",
                font_size = 12) |>
  row_spec(nrow(t1), bold = TRUE) |>
  footnote(general = paste(
    "Provider categories are based on eligible facilities recorded in the 2017",
    "Kenya Master Health Facility List extract, mapped to 290 constituencies.",
    "Categories are defined by the presence of government and NGO or",
    "faith-based facilities only; private facilities may be present in any",
    "category and do not affect the classification. Three constituencies with",
    "resolved classifications contributed no eligible children. The two",
    "unresolved constituencies (Ganze in Kilifi County and Siakago in Embu",
    "County) recorded no eligible government or NGO/FBO facility; their",
    "classification could not be determined and 12 otherwise eligible children",
    "in them were excluded."),
    general_title = "Note. ", footnote_as_chunk = TRUE, threeparttable = TRUE) |>
  save_kable("output/tab1.html")

cat("Saved to output/tab1.html\n")