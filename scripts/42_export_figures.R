# 42_export_figures
# Exports the three main figures at PLOS submission specifications.
# Figure numbering follows the final manuscript:
#   Fig 1 = constituency map of co-presence
#   Fig 2 = sample flow diagram
#   Fig 3 = forest plot of adjusted odds ratios
# Each sourced script leaves its plot in an object named p, which is saved
# immediately below it.

# ---- Fig 1: constituency map ----
source("scripts/33_map.R")
ggsave("output/Fig1.tif", p,
       width = 6.5, height = 7.0, dpi = 400, bg = "white",
       device = "tiff", compression = "lzw")

# ---- Fig 2: sample flow ----
source("scripts/38_flow_diagram.R")
ggsave("output/Fig2.tif", p,
       width = 5.9, height = 5.05, dpi = 400, bg = "white",
       device = "tiff", compression = "lzw")

# ---- Fig 3: forest plot ----
source("scripts/34_forest.R")
ggsave("output/Fig3.tif", p,
       width = 7.5, height = 4.4, dpi = 400, bg = "white",
       device = "tiff", compression = "lzw")