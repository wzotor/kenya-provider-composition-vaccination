# 00_run_all
# Purpose: run the full pipeline from raw inputs to final outputs.
#          Run from the project root, in a clean R session.
#          This script overwrites the contents of data-clean/ and output/.

stopifnot(dir.exists("scripts"), dir.exists("data-raw"))

# The loop lives inside a function so that its counters cannot be
# overwritten by variables the sourced scripts create. The scripts
# themselves still evaluate in the global environment (local = FALSE).
run_pipeline <- function(run_order) {
  
  t_start <- Sys.time()
  
  out <- data.frame(script = run_order,
                    seconds = NA_real_,
                    status = NA_character_,
                    stringsAsFactors = FALSE)
  
  for (k in seq_along(run_order)) {
    this_file <- run_order[k]
    cat(sprintf("[%2d/%2d] %s ... ", k, length(run_order), this_file))
    t0 <- Sys.time()
    res <- tryCatch({
      source(file.path("scripts", this_file), echo = FALSE, local = FALSE)
      "ok"
    }, error = function(e) {
      paste("ERROR:", conditionMessage(e))
    })
    out$seconds[k] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    out$status[k]  <- res
    cat(sprintf("%s (%.1fs)\n", res, out$seconds[k]))
    if (startsWith(res, "ERROR")) {
      cat("\nStopped at", this_file, "\n")
      break
    }
  }
  
  cat("\nTotal:", round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), "minutes\n")
  out
}

all_scripts <- sort(list.files("scripts", pattern = "\\.R$", full.names = FALSE))

# Excluded: this script itself, and the three figure scripts, which are
# sourced by 42_export_figures.R and would otherwise run twice.
skip <- c("00_run_all.R",
          "33_map.R",
          "34_forest.R",
          "38_flow_diagram.R")

run_order <- setdiff(all_scripts, skip)

cat("Scripts to run:", length(run_order), "\n\n")

run_log <- run_pipeline(run_order)

dir.create("output/logs", showWarnings = FALSE, recursive = TRUE)
write.csv(run_log, "output/logs/run_all_log.csv", row.names = FALSE)
writeLines(capture.output(sessionInfo()), "output/logs/sessionInfo.txt")