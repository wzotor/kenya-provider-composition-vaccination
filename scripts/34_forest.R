# 34_forest
# Purpose: forest plot of the exposure odds ratio across specifications.

library(dplyr); library(ggplot2); library(lme4); library(survey); library(patchwork)

mods <- readRDS("data-clean/models_main.rds")
spec <- readRDS("data-clean/models_spec.rds")
m4   <- readRDS("data-clean/model_s4.rds")
sw   <- readRDS("data-clean/model_survey.rds")
uw   <- readRDS("data-clean/model_uw.rds")

get_or <- function(m) {
  b  <- if (inherits(m, "merMod")) fixef(m) else coef(m)
  se <- sqrt(diag(as.matrix(vcov(m))))
  i  <- grep("^exposure", names(b))
  bb <- as.numeric(b[i])
  ss <- as.numeric(se[i])
  c(exp(bb), exp(bb - 1.96*ss), exp(bb + 1.96*ss))
}

vals <- rbind(
  get_or(mods$m0), get_or(mods$m1), get_or(mods$m2),
  get_or(spec$s2), get_or(spec$s3), get_or(m4),
  get_or(uw),      get_or(sw)
)

rows <- data.frame(
  label = c("Exposure-only, county random intercept",
            "Adjusted, county random intercept",
            "Adjusted, county + constituency random intercept",
            "S2: VCT facilities included",
            "S3: four constituencies excluded",
            "S4: five counties excluded",
            "County indicators, unweighted",
            "County indicators, survey-weighted"),
  or = vals[, 1], lo = vals[, 2], hi = vals[, 3],
  stringsAsFactors = FALSE
)

rows$txt   <- sprintf("%.2f (%.2f, %.2f)", rows$or, rows$lo, rows$hi)
rows$label <- factor(rows$label, levels = rev(rows$label))
rows$grp   <- factor(c(rep("Main models", 3),
                       rep("Sensitivity analyses", 3),
                       rep("County-indicator benchmarks", 2)),
                     levels = c("Main models", "Sensitivity analyses",
                                "County-indicator benchmarks"))

base <- theme_minimal(base_size = 10) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        strip.text.y.left  = element_text(angle = 0, face = "bold",
                                          size = 8.5, hjust = 0),
        strip.placement    = "outside",
        plot.background    = element_rect(fill = "white", colour = NA))

p_left <- ggplot(rows, aes(x = or, y = label)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey45") +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0,
                linewidth = 0.5, orientation = "y") +
  geom_point(size = 2.2, colour = "#2B4C7E") +
  facet_grid(grp ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(trans = "log", breaks = c(0.5, 0.75, 1, 1.5, 2),
                     limits = c(0.42, 2.1),
                     name = "Odds ratio (log scale)") +
  labs(y = NULL) +
  base +
  theme(axis.text.y  = element_text(hjust = 0, size = 8.5),
        axis.title.x = element_text(size = 9))

p_right <- ggplot(rows, aes(x = 0, y = label, label = txt)) +
  geom_text(hjust = 0, size = 3, colour = "grey20") +
  facet_grid(grp ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(0, 3)) +
  labs(x = "OR (95% CI)", y = NULL) +
  base +
  theme(axis.text    = element_blank(),
        axis.title.x = element_text(size = 9, hjust = 0, colour = "grey30"),
        panel.grid   = element_blank(),
        strip.text   = element_blank())

p <- (p_left | p_right) +
  plot_layout(widths = c(3, 2.3)) +
  plot_annotation(
    theme = theme(plot.background = element_rect(fill = "white", colour = NA)))

dir.create("output", showWarnings = FALSE)
ggsave("output/fig_forest.png", p, width = 7.5, height = 4.4, dpi = 400, bg = "white")
ggsave("output/fig_forest.pdf", p, width = 7.5, height = 4.4, bg = "white")

print(rows[, c("label", "txt")])
cat("\nSaved to output/fig_forest.png and .pdf\n")