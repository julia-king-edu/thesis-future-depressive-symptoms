# ~~~~~~~~~~~~
# Author: Julia King
# Purpose: abstraction for loading and writing files so i never accidentally write in places i dont want to.
# Notes:
# ~~~~~~~~~~~~

# ~~~~~~~~~~~~
# loading packages ----
# ~~~~~~~~~~~~

library(tidyverse)
library(here)
library(labelled)

# ~~~~~~~~~~~~
# Data df ----
# ~~~~~~~~~~~~

load_index_df <- function(version = c("core", "is"),
                          release = "40",
                          drop_value_labels = TRUE,
                          replace_with_label = c()) {
  #' Load index data frame
  #'
  #' loads data frame used to stitch others together.
  #' drops observations before MINYEAR
  #' also updates VERSION to specified version to ensure correct import later.
  #'
  #' @param version : "core" or "is", defines file set. defaults to core.
  #' @param release : str containing 40, 41 for core or 23, 24 for IS, corresponds to release order.
  #' @param drop_value_labels : logical; applies remove_value_labels
  #'
  #' @return index_df with hid, pid, syear, weight & basic info

  version <- match.arg(version)
  VERSION <<- version

  if ((is.null(release)) || !is_character(release)) {
    stop("Invalid Release Specification")
  }
  if (!(release %in% c("40", "41") && version == "core") && !(release %in% c("23", "24") && version == "is")) {
    stop("Version and Release dont match.")
  }
  RELEASE <<- release

  if (version == "core") {
    raw <- read_rds(file.path(PATH_RAW_SOEPCORE[release], "ppathl.rds"))
  } else {
    raw <- read_rds(file.path(PATH_RAW_SOEPIS[release], "ppathl.rds"))
  }

  index_df <- raw %>%
    filter(syear >= MIN_SYEAR) %>%
    # remove recent refugees (see thesis for reasoning)
    filter(!psample %in% WAVES_EXCLUDE)

  # drop value labels
  if (drop_value_labels) {
    # check if any of the replace_with_label variables are not in df
    if (length(replace_with_label) != 0 &&
      !all(replace_with_label %in% colnames(index_df))) {
      stop(
        "Not all variables to be replaced with labels are included in dataframe.
           Missing variables: ",
        paste(replace_with_label[!replace_with_label %in% colnames(index_df)], collapse = ", ")
      )
    }
    index_df <- index_df %>%
      # first mutate all columns where values should be replaced with labels
      mutate(
        across(all_of(replace_with_label), ~ {
          temp <- as_factor(.x) # replace value with "[value] label"
          temp <- as.character(temp) # sub needs character
          temp <- sub("^\\[.*?\\] ", "", temp) # strip [value] prefix
          factor(temp) # convert back to factor
        }),
        # now strip all other labels
        across(-all_of(replace_with_label), ~ remove_labels(.x, keep_var_label = TRUE))
      )
  }

  return(index_df)
}

load_raw_df <- function(filename, drop_value_labels = TRUE) {
  #' Load index data frame
  #'
  #' loads data frame of specified dataset.
  #' drops observations before MINYEAR
  #'
  #' @param version : "core" or "is", defines file set. defaults to core.
  #'
  #' @return index_df with hid, pid, syear, weight & basic info

  if (!endsWith(filename, ".rds")) {
    filename <- paste0(filename, ".rds")
  }

  # load
  if (VERSION == "core") {
    raw <- read_rds(file.path(PATH_RAW_SOEPCORE[RELEASE], filename))
  } else {
    raw <- read_rds(file.path(PATH_RAW_SOEPIS[RELEASE], filename))
  }

  # filter
  dataset_df <- raw %>%
    filter(syear >= MIN_SYEAR)

  # drop value labels
  if (drop_value_labels) {
    dataset_df <- dataset_df %>% remove_labels(keep_var_label = TRUE)
  }

  return(dataset_df)
}

save_clean_df <- function(data, filename, ignore_no_raw_equivalent = FALSE) {
  #' Save cleaned DF
  #'
  #' writes a cleaned df into PATH_CLEANED
  #'
  #' @param data : df to write
  #' @param filename : name for the rds file, can be with or without extension
  #' @param ignore_no_raw_equivalent : logical, whether to allow files without a corresponding raw file.

  if (!endsWith(filename, ".rds")) {
    filename <- paste0(filename, ".rds")
  }

  if (!ignore_no_raw_equivalent &
    !file.exists(file.path(
      if_else(VERSION == "core", PATH_RAW_SOEPCORE[RELEASE], PATH_RAW_SOEPIS[RELEASE]),
      filename
    ))) {
    stop(
      "name not present in raw folder.
      Use name of raw file if possible.
      Stop can be disabled using ignore_no_raw_equivalent argument."
    )
  }

  write_rds(data, file.path(PATH_CLEANED, filename))
}

load_clean_df <- function(filename) {
  #' Load cleaned df
  #'
  #' Loads cleaned df from PATH_CLEANED
  #'
  #' @param filename : name for the rds file, can be with or without extension
  #'
  #' @return df loaded from PATH_CLEANED

  if (!endsWith(filename, ".rds")) {
    filename <- paste0(filename, ".rds")
  }

  interim_df <- read_rds(file.path(PATH_CLEANED, filename))

  return(interim_df)
}

save_reconstructed_df <- function(data) {
  #' Save Reconstructed DF
  #'
  #' writes reconstructed df into out folder.
  #' no filename, as there should only ever be 1 output to out
  #'
  #' @param data : df to write

  write_rds(data, file.path(PATH_RECONSTRUCTED, "reconstructed.rds"))
}

load_reconstructed_df <- function() {
  #' Load Reconstructed DF
  #'
  #' Load the reconstructed from PATH_RECONSTRUCTED
  #'
  #' @return df loaded from PATH_RECONSTRUCTED

  reconstructed_df <- read_rds(file.path(PATH_RECONSTRUCTED, "reconstructed.rds"))

  return(reconstructed_df)
}

save_split_df <- function(data, type = c("train", "test")) {
  #' Save split DF
  #'
  #' writes a split df into PATH_SPLIT
  #'
  #' @param data : df to write
  #' @param type : determines which part of the split is supplied, either "train" or "test"

  type <- match.arg(type)

  filename <- paste0(type, ".rds")

  write_rds(data, file.path(PATH_SPLIT, filename))
}

load_split_df <- function(type = c("train", "test")) {
  #' Load split DF
  #'
  #' Load the split df from PATH_SPLIT
  #'
  #' @param type : determines which part of the split to load, either "train" or "test"
  #'
  #' @return df loaded from PATH_SPLIT

  type <- match.arg(type)

  filename <- paste0(type, ".rds")

  split_df <- read_rds(file.path(PATH_SPLIT, filename))

  return(split_df)
}

save_imputed_df <- function(data, imp) {
  #' Save imputed DF
  #'
  #' writes an imputed df into PATH_IMPUTED
  #'
  #' @param data : df to write
  #' @param imp : imputation technique used, used as filename: "imp.rds"

  filename <- paste0(imp, ".rds")

  write_rds(data, file.path(PATH_IMPUTED, filename))
}

load_imputed_df <- function(imp) {
  #' Load imputed DF
  #'
  #' Load an imputed df from PATH_IMPUTED
  #'
  #' @param imp : imputation technique used, used as filename: "imp.rds"
  #'
  #' @return df loaded from PATH_IMPUTED

  filename <- paste0(imp, ".rds")

  imputed_df <- read_rds(file.path(PATH_IMPUTED, filename))

  return(imputed_df)
}

# ~~~~~~~~~~~~
# metadata ----
# ~~~~~~~~~~~~

save_roles_df <- function(data) {
  #' Save Roles Dataframe
  #'
  #' writes a df containing variable roles into PATH_DATA
  #'
  #' @param data : df to write

  write_csv(data, file.path(PATH_DATA, "roles.csv"))
}

load_roles_df <- function() {
  #' Load Roles Dataframe
  #'
  #' Load a df containing variable roles from PATH_DATA and optionally change the dataset.
  #'
  #' @return df loaded from PATH_DATA

  roles_df <- read_csv(file.path(PATH_DATA, "roles.csv"), show_col_types = FALSE)

  return(roles_df)
}

# ~~~~~~~~~~~~
# recipes ----
# ~~~~~~~~~~~~

save_recipe <- function(r, imp) {
  #' Save Tidymodels Recipe
  #'
  #' writes a recipe into PATH_RECIPE
  #'
  #' @param r : recipe to write
  #' @param imp : imputation technique, used as filename: "imp.rds"

  filename <- paste0(imp, ".rds")

  write_rds(r, file.path(PATH_RECIPE, filename))
}

load_recipe <- function(imp) {
  #' Load Tidymodels Recipe
  #'
  #' Load a recipe from PATH_RECIPE
  #'
  #' @param imp : imputation technique, used as filename: "imp.rds"
  #'
  #' @return df loaded from PATH_RECIPE

  filename <- paste0(imp, ".rds")

  r <- read_rds(file.path(PATH_RECIPE, filename))

  return(r)
}

# ~~~~~~~~~~~~
# tuned sets ----
# ~~~~~~~~~~~~

save_wf_set <- function(model, imp = c("avg", "model", "bag")) {
  #' Save Tidymodels Model Set / Workflow Set
  #'
  #' writes a Model Set / Workflow Set into PATH_WF_SET
  #'
  #' @param model : set to write
  #' @param imp : imputation technique of train df, used in file name: "imp.rds"

  # error if specs missing or not valid
  if (missing(imp)) {
    stop("Imputation Strategy of the training dataset must be explicitly provided.")
  }

  imp <- match.arg(imp)

  filename <- paste0(imp, ".rds")

  write_rds(model, file.path(PATH_WF_SET, filename))
}

load_wf_set <- function(imp = c("avg", "model", "bag")) {
  #' Load Tidymodels Model Set / Workflow Set
  #'
  #' Load a Model Set / Workflow Set from PATH_WF_SET.
  #'
  #' @param imp : imputation technique of train df, used in file name: "imp.rds"
  #'
  #' @return set loaded from PATH_WF_SET

  # error if specs missing or not valid
  if (missing(imp)) {
    stop("Imputation Strategy of the training dataset must be explicitly provided.")
  }

  imp <- match.arg(imp)

  filename <- paste0(imp, ".rds")

  model <- read_rds(file.path(PATH_WF_SET, filename))

  return(model)
}

# ~~~~~~~~~~~~
# model specifications ----
# ~~~~~~~~~~~~

save_selected_config <- function(config, threshold) {
  #' Save Tidymodels config
  #'
  #' writes a config into PATH_SELECTED
  #' Should only be used on selected config.
  #'
  #' @param config : config to write
  #' @param threshold : logical; whether config to be written contains a valid threshold value.

  if (!is.logical(threshold) || is.null(threshold)) {
    stop("Presence/Absence of valid Threshold needs to be specified with logical threshold argument.")
  }

  if (threshold) {
    filename <- paste0("full.rds")
  } else {
    filename <- paste0("no-threshold.rds")
  }

  write_rds(config, file.path(PATH_SELECTED, filename))
}

load_selected_config <- function(threshold) {
  #' Load Tidymodels config
  #'
  #' Load a config from PATH_SELECTED.
  #'
  #' @param threshold : logical; whether config to be loaded is version with or without a valid threshold value.
  #'
  #' @return config loaded from PATH_SELECTED

  if (!is.logical(threshold) || is.null(threshold)) {
    stop("Presence/Absence of valid Threshold needs to be specified with logical threshold argument.")
  }

  if (threshold) {
    filename <- paste0("full.rds")
  } else {
    filename <- paste0("no-threshold.rds")
  }

  config <- read_rds(file.path(PATH_SELECTED, filename))

  return(config)
}

save_selected_wf <- function(wf) {
  #' Save Tidymodels workflow
  #'
  #' writes a wf into PATH_SELECTED
  #' Should only be used on selected workflow.
  #'
  #' @param wf : wf to write

  write_rds(wf, file.path(PATH_SELECTED, "wf.rds"))
}

load_selected_wf <- function() {
  #' Load Tidymodels workflow
  #'
  #' Load a workflow from PATH_SELECTED.
  #'
  #' @return wf loaded from PATH_SELECTED

  wf <- read_rds(file.path(PATH_SELECTED, "wf.rds"))

  return(wf)
}

# ~~~~~~~~~~~~
# fitted models ----
# ~~~~~~~~~~~~

save_model <- function(model, type = c("resample", "full")) {
  #' Save Tidymodels Model
  #'
  #' writes a model into PATH_FIT
  #' Should only be used on best model.
  #'
  #' @param model : model to write
  #' @param type : name for the rds file, reflects data used for fit(_resamples): "type.rds"

  # error if specs missing or not valid
  if (missing(type)) {
    stop("type must be explicitly provided.")
  }

  type <- match.arg(type)

  filename <- paste0(type, ".rds")

  write_rds(model, file.path(PATH_FIT, filename))
}

load_model <- function(type = c("resample", "full")) {
  #' Load Tidymodels Model
  #'
  #' Load a model from PATH_FIT
  #'
  #' @param type : name for the rds file, reflects imputation technique: "type.rds"
  #'
  #' @return model loaded from PATH_FIT

  # error if specs missing or not valid
  if (missing(type)) {
    stop("type must be explicitly provided.")
  }

  type <- match.arg(type)

  filename <- paste0(type, ".rds")

  model <- read_rds(file.path(PATH_FIT, filename))

  return(model)
}

# ~~~~~~~~~~~~
# predictions ----
# ~~~~~~~~~~~~

save_predicted_df <- function(data, type = c("threshold", "resample", "test")) {
  #' Save predicted DF
  #'
  #' writes a df augmented with predictions into PATH_PREDICTED
  #'
  #' @param data : df to write
  #' @param type : basis of predictions, appended to filename: "predicted_type.rds"

  # error if specs missing or not valid
  if (missing(type)) {
    stop("type must be explicitly provided.")
  }

  type <- match.arg(type)

  filename <- paste0(type, ".rds")

  write_rds(data, file.path(PATH_PREDICTED, filename))
}

load_predicted_df <- function(type = c("threshold", "resample", "test")) {
  #' Load predicted DF
  #'
  #' Load a df augmented with predictions from PATH_PREDICTED
  #'
  #' @param type : basis of predictions, appended to filename: "predicted_type.rds"
  #'
  #' @return df loaded from PATH_PREDICTED

  # error if specs missing or not valid
  if (missing(type)) {
    stop("type must be explicitly provided.")
  }

  type <- match.arg(type)

  filename <- paste0(type, ".rds")

  predicted_df <- read_rds(file.path(PATH_PREDICTED, filename))

  return(predicted_df)
}
