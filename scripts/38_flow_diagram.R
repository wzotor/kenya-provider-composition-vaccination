# 38_flow_diagram.R
# Purpose: Fig 2, the sample flow diagram. Counts come from 37_flow.R.

library(ggplot2)

fc  <- readRDS("data-clean/flow_counts.rds")
fmt <- function(x) format(x, big.mark = ",")

box <- function(y, label, n) {
  data.frame(y = y, label = paste0(label, "\nn = ", format(n, big.mark = ",")))
}

main <- rbind(
  box(7, "Children in DHS Children's Recode", fc$recode),
  box(5, "Living children", fc$alive),
  box(3, "Living children aged 12-23 months", fc$eligible),
  box(1, "Analytic sample", fc$analytic)
)

side <- data.frame(
  y = c(6, 4, 2),
  label = c(
    paste0("Excluded (n = ", fmt(fc$not_alive), ")\nNot alive at interview"),
    paste0("Excluded (n = ", fmt(fc$outside_window),
           ")\nOutside the 12-23 month window"),
    paste0("Excluded (n = ", fmt(fc$cluster778 + fc$unresolved),
           ")\nIn excluded cluster 778: ", fc$cluster778,
           "\nIn unresolved constituencies: ", fc$unresolved)
  )
)

arrows <- data.frame(y = c(6.6, 4.6, 2.6), yend = c(5.4, 3.4, 1.4))

p <- ggplot() +
  geom_label(data = main, aes(x = 1, y = y, label = label),
             size = 3.1, label.padding = unit(0.5, "lines"),
             label.r = unit(0.15, "lines"), fill = "grey97",
             colour = "grey25", lineheight = 1.15) +
  geom_label(data = side, aes(x = 2.3, y = y, label = label),
             size = 2.9, hjust = 0, label.padding = unit(0.4, "lines"),
             label.r = unit(0.1, "lines"), fill = "white",
             colour = "grey30", lineheight = 1.15) +
  geom_segment(data = arrows, aes(x = 1, xend = 1, y = y, yend = yend),
               arrow = arrow(length = unit(0.16, "cm"), type = "closed"),
               colour = "grey40", linewidth = 0.35) +
  geom_segment(data = data.frame(y = c(6, 4, 2)),
               aes(x = 1.05, xend = 2.25, y = y, yend = y),
               colour = "grey45", linewidth = 0.3) +
  scale_x_continuous(limits = c(0.35, 3.5)) +
  scale_y_continuous(limits = c(0.5, 7.5)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(10, 10, 10, 10))

dir.create("output", showWarnings = FALSE)
ggsave("output/fig_flow.png", p, width = 5.9, height = 5.05, dpi = 400, bg = "white")
ggsave("output/fig_flow.pdf", p, width = 5.9, height = 5.05, bg = "white")
cat("Saved\n")