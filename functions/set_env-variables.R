# ~~~~~~~~~~~~
# Author: Julia King
# Purpose: defines commonly used environment variables like paths
# Notes: notes
# ~~~~~~~~~~~~

# ~~~~~~~~~~~~
# loading packages ----
# ~~~~~~~~~~~~

library(here)

# ~~~~~~~~~~~~
# defining file paths ----
# ~~~~~~~~~~~~

PATH_DATA <- here("data")
PATH_RAW_SOEPCORE <- list("40" = here(PATH_DATA, "raw", "soep-core.40.1_r_en"), "41" = here(PATH_DATA, "raw", "soep-core.41_r_en"))
PATH_RAW_SOEPIS <- list("23" = here(PATH_DATA, "raw", "soep-is.2023_stata_en"), "24" = here(PATH_DATA, "raw", "soep-is.2024_stata_en"))
PATH_CLEANED <- here(PATH_DATA, "cleaned")
PATH_RECONSTRUCTED <- here(PATH_DATA, "reconstructed")
PATH_SPLIT <- here(PATH_DATA, "split")
PATH_IMPUTED <- here(PATH_DATA, "imputed")
PATH_PREDICTED <- here(PATH_DATA, "predicted")

PATH_MODEL <- here("models")
PATH_RECIPE <- here(PATH_MODEL, "recipes")
PATH_WF_SET <- here(PATH_MODEL, "wf_sets")
PATH_SELECTED <- here(PATH_MODEL, "selected")
PATH_FIT <- here(PATH_MODEL, "fitted")

PATH_EXPORT <- here("out")
PATH_IMG <- here(PATH_EXPORT, "img")
PATH_TABLE <- here(PATH_EXPORT, "tables")

PATH_FUNCTIONS <- here("functions")

# ~~~~~~~~~~~~
# global constants ----
# ~~~~~~~~~~~~

MIN_SYEAR <- 2008
VERSION <- "core"
RELEASE <- "41"
WAVES_EXCLUDE <- c(
  17, # refugees
  18, # refugees
  19, # refugees
  24, # refugees
  29, # first measures in 23, useless
  30 # refugee refresh sample
)
