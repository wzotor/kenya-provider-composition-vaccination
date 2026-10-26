# 39_table_full_model.R
# Purpose: S2 Table, regression results for the two main adjusted models.

library(lme4)
library(knitr)
library(kableExtra)

mods <- readRDS("data-clean/models_main.rds")
m1 <- mods$m1
m2 <- mods$m2

ord <- c("exposureGovernment + NGO/FBO", "child_age", "female", "birth_ord",
         "mat_educprimary", "mat_educsecondary", "mat_educhigher", "mat_age",
         "wealthpoorer", "wealthmiddle", "wealthricher", "wealthrichest",
         "urban", "log_travel", "log_popden", "log_lights", "malaria_prev",
         "(Intercept)")

labs <- c("Government and NGO/FBO (ref: government only)",
          "Child age, months", "Female (ref: male)", "Birth order",
          "Mother: primary education (ref: none)",
          "Mother: secondary education", "Mother: higher education",
          "Mother's age, years",
          "Wealth: poorer (ref: poorest)", "Wealth: middle",
          "Wealth: richer", "Wealth: richest",
          "Urban residence (ref: rural)",
          "Travel time to urban centre (log)",
          "Population density (log)", "Night-time lights (log)",
          "Malaria prevalence", "Intercept (baseline odds)")

fmt <- function(m) {
  s  <- summary(m)$coefficients
  b  <- as.numeric(s[ord, 1])
  se <- as.numeric(s[ord, 2])
  p  <- as.numeric(s[ord, 4])
  data.frame(
    or = sprintf("%.2f (%.2f, %.2f)", exp(b),
                 exp(b - 1.96 * se), exp(b + 1.96 * se)),
    p  = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
    stringsAsFactors = FALSE)
}

a <- fmt(m1)
b <- fmt(m2)

vc1 <- as.data.frame(VarCorr(m1))
vc2 <- as.data.frame(VarCorr(m2))
icc <- vc1$vcov[1] / (vc1$vcov[1] + pi^2 / 3)

tab <- data.frame(Term = labs,
                  or1 = a$or, p1 = a$p,
                  or2 = b$or, p2 = b$p,
                  stringsAsFactors = FALSE)

# VarCorr for (1 | county/constit) returns constit:county first, then county,
# so [1] is the constituency variance and [2] the county variance.
fitrows <- data.frame(
  Term = c("Observations", "Counties", "County variance",
           "Constituency variance", "County ICC (latent scale)",
           "Log-likelihood", "AIC"),
  or1 = c(format(nobs(m1), big.mark = ","), as.character(ngrps(m1)),
          sprintf("%.3f", vc1$vcov[1]), "--", sprintf("%.3f", icc),
          sprintf("%.1f", logLik(m1)), sprintf("%.1f", AIC(m1))),
  p1  = rep("", 7),
  or2 = c(format(nobs(m2), big.mark = ","),
          as.character(ngrps(m2)[["county"]]),
          sprintf("%.3f", vc2$vcov[2]), sprintf("%.3f", vc2$vcov[1]),
          "--", sprintf("%.1f", logLik(m2)), sprintf("%.1f", AIC(m2))),
  p2  = rep("", 7),
  stringsAsFactors = FALSE)

tab <- rbind(tab, fitrows)
names(tab) <- c("Term", "OR (95% CI)", "p", "OR (95% CI)", "p")

kable(tab, format = "html", align = c("l", "r", "r", "r", "r"),
      caption = "S2 Table. Multilevel logistic regression of full basic-antigen vaccination") |>
  kable_classic(full_width = FALSE, html_font = "Times New Roman",
                font_size = 12) |>
  add_header_above(c(" " = 1, "M1: county RE" = 2,
                     "M2: county + constituency RE" = 2)) |>
  row_spec(nrow(tab) - 6, extra_css = "border-top: 1px solid #999;") |>
  footnote(general = paste(
    "Odds ratios with 95% confidence intervals. Estimates are unweighted.",
    "The intercept row reports the exponentiated intercept (baseline odds)",
    "rather than an odds ratio between groups.",
    "Travel time, population density and night-time lights were log-transformed",
    "using log(1 + x). Malaria prevalence entered as a proportion (0-1).",
    "The intraclass correlation is for counties, calculated on the latent",
    "logistic scale using a residual variance of pi-squared over 3.",
    "Complete results for all eight specifications appear in S3 Table."),
    general_title = "Note. ", footnote_as_chunk = TRUE, threeparttable = TRUE) |>
  save_kable("output/tabS2.html")

cat("Saved to output/tabS2.html\n")
