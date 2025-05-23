#' Author: Laura Espinosa
#' Date created: 15 December 2024
#' Date updated: 15 December 2024


# Packages ----------------------
## install/load "pacman" to help installing and loading other packages
message("Installing and loading packages")

while (require("pacman") == FALSE) {
  install.packages("pacman")
  library("pacman")
}

## load packages
p_load(tidyverse, vroom, jsonlite, caret, rlang, grid, ggpubr, ggbreak,
       shiny, DT, DT, shinyBS, yardstick, gridExtra, scales, ggh4x)

# Launch LLM benchmarking tool -----------
runApp("LLM_tool.R")
