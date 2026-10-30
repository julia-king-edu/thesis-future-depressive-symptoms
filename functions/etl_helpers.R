# ~~~~~~~~~~~~
# Author: Julia King
# Purpose: helper functions to use while cleaning raw variables
# Notes: notes
# ~~~~~~~~~~~~


# ~~~~~~~~~~~~
# loading packages ----
# ~~~~~~~~~~~~

library(tidyverse)

# ~~~~~~~~~~~~
# missingness ----
# ~~~~~~~~~~~~

missing_to_na <- function(col, missingness_indicators = -9:-1) {
  #' Missing to NA
  #'
  #' turns values associated with missingness (-9--1) in soep to NA.
  #'
  #'
  #' @param col : column to mutate
  #' @param missingness_indicators : values that can indicate missingness. defaults to -9:-1
  #'
  #' @return column with missingness replaced with NA

  # remap all other missingness_indicators to NA
  col <- ifelse(col %in% missingness_indicators, NA, col)

  return(col)
}

# ~~~~~~~~~~~~
# remap values ----
# ~~~~~~~~~~~~

# this function needed a lot of refactoring bc it became too big & convoluted, user function is at bottom

.extract_remap_rules <- function(...) {
  #' Extract remap rules
  #'
  #' Takes a dynamic list of formulas and splits them into remap_from vector and remap_to vector.
  #' Preserves order and types of the formula evaluations.
  #'
  #' @param ... : formulas of the form old_value ~ new_value
  #'
  #' @return list with 2 elements: from (vector of left-hand sides) and to (vector of right-hand sides)

  rules <- list(...)

  # check if any argument passed is not a formula
  if (!all(vapply(rules, inherits, logical(1), what = "formula"))) {
    stop(
      "All arguments supplied to ... must be formulas of the form old_value ~ new_value.",
      call. = FALSE
    )
  }

  # extract old values
  remap_from <- sapply(rules, function(x) {
    eval(x[[2]], parent.frame())
  })

  # extract new values
  remap_to <- lapply(rules, function(x) {
    eval(x[[3]], parent.frame())
  }) %>%
    unlist(use.names = FALSE)

  return(list(from = remap_from, to = remap_to))
}

.validate_remap_rules <- function(col,
                                  remap_from,
                                  remap_to,
                                  target_type,
                                  missingness_indicators = as.numeric(-9:-1),
                                  require_full_remap = TRUE,
                                  warn_on_partial_remap = TRUE) {
  #' Validate Remapping Rules
  #'
  #' Takes a column and the intended old and new values and checks for validity of remapping rules.
  #' Raises error or warning given severity of issues.
  #'
  #' @param col : column intended to be remapped
  #' @param remap_from : vector of old values. order needs to match remap_to
  #' @param remap_to : list of new values. order needs to match remap_from
  #' @param require_full_remap : Logical; if TRUE, the function stops with an error if any non-missing values in col are not covered by the remap rules. Defaults to TRUE.
  #' @param warn_on_partial_remap : Logical; if TRUE and require_full_remap is FALSE, a warning is issued if any non-missing values in col are not covered by the remap rules. Defaults to TRUE.
  #'
  # add missingness indicators to remap_from as those will be remapped.
  # dont include any missingness indicators already covered.
  missingness_indicators <- missingness_indicators[!missingness_indicators %in% remap_from]
  remap_from <- c(remap_from, missingness_indicators)

  # check uniqueness of left-hand side
  if (any(duplicated(remap_from))) {
    stop(
      "Old values must be unique. Duplicates found: ",
      paste(remap_from[duplicated(remap_from)])
    )
  }

  # check for any uncovered values in from
  unmatched <- !(col %in% remap_from | is.na(col))
  if (any(unmatched)) {
    # check if casting is lossless -> otherwise always stop
    # casting is lossless if goal is factor or if old and new are same.
    lossless_cast <- target_type == class(col)[1] ||
      target_type == "factor"

    # case 1: must stop
    if (!lossless_cast || require_full_remap) {
      stop(paste(
        "Remap does not cover all values! Unmapped values:",
        paste(sort(unique(col[unmatched])), collapse = ", ")
      ))
    } else if (warn_on_partial_remap) {
      warning(paste(
        "Remap does not cover all values! Unmapped values:",
        paste(sort(unique(col[unmatched])), collapse = ", ")
      ))
    }
  }
}

.apply_remap <- function(col, remap_from, remap_to) {
  #' apply remapping of column
  #'
  #' given a column, a vector of old values and a list of new values, returns a list with all specified values replaced
  #' mostly a helper for remap_values
  #'
  #' @param col : column with values to be replaced
  #' @param remap_from : vector of old values. order needs to match remap_to
  #' @param remap_to : list of new values. order needs to match remap_from
  #'
  #' @return column with specified values replaced
  col_new <- col
  matches_formula_nr <- match(col, remap_from)
  has_replacement <- !is.na(matches_formula_nr)
  col_new[has_replacement] <- remap_to[matches_formula_nr[has_replacement]]

  return(col_new)
}

remap_values <- function(col,
                         ...,
                         missing_to_na_flag = TRUE,
                         missingness_indicators = as.numeric(-9:-1),
                         factor_ordered = FALSE,
                         require_full_remap = TRUE,
                         warn_on_partial_remap = TRUE) {
  #' Remap values in column
  #'
  #' takes a dataframe column and exchanges all values specified in formulas with their new results.
  #' can also turn missingness indicators to NA.
  #' output will be of type logical, numerical or factor
  #'
  #' @param col : column to mutate
  #' @param ... : formulas of form old_value ~ new_value. contents of new_value determine output type
  #' @param missing_to_na_flag : Logical; if TRUE, turn all values in missingness_indicators to NA at the end. defaults to TRUE
  #' @param missingness_indicators : values that can indicate missingness. defaults to -9:-1
  #' @param factor_ordered : Logical; if TRUE and the output is a factor, the levels are ordered according to the order of formulas. Values not specified in the right hand side of any formula are appended in the order of first appearance in col. Defaults to FALSE.
  #' @param require_full_remap : Logical; if TRUE, the function stops with an error if any non-missing values in col are not covered by the remap rules. Defaults to TRUE.
  #' @param warn_on_partial_remap : Logical; if TRUE and require_full_remap is FALSE, a warning is issued if any non-missing values in col are not covered by the remap rules. Defaults to TRUE.
  #'
  #' @details
  #' - Missingness is handled after remapping, so remap rules take precedence over missingness indicators.
  #' - Output type is inferred from the right-hand side values of the remap formulas:
  #'   - All logical -> logical
  #'   - All numeric -> numeric
  #'   - Else -> factor
  #'
  #' @return column with remapped values

  # create vector with old values and list with new values
  rules <- .extract_remap_rules(...)
  remap_from <- rules$from
  remap_to <- rules$to

  # Determine output type
  if (is.logical(remap_to) &&
    all(remap_to %in% c(TRUE, FALSE, NA))) {
    target_type <- "logical"
  } else if (is.numeric(remap_to)) {
    target_type <- "numeric"
  } else {
    target_type <- "factor"
    col <- as.character(col)
  }

  # if input col is integer, convert to numeric. happens at the end anyway and stops errors
  if (is.integer(col)) {
    col <- as.numeric(col)
  }

  .validate_remap_rules(
    col,
    remap_from,
    remap_to,
    target_type,
    missingness_indicators,
    require_full_remap,
    warn_on_partial_remap
  )

  # remap specified values
  col_new <- .apply_remap(col, remap_from, remap_to)

  # recast missingness (occurs after remap so specified values can override missingness)
  if (!is.null(missingness_indicators) && missing_to_na_flag) {
    col_new <- missing_to_na(col_new, missingness_indicators)
  }

  # finalize type
  if (target_type == "factor") {
    all_levels <- c(unique(remap_to), unique(col_new[!col_new %in% remap_to]))
    all_levels <- all_levels[!is.na(all_levels)]
    col_new <- factor(col_new,
      levels = rev(all_levels),
      ordered = factor_ordered
    )
  } else if (target_type == "logical") {
    col_new <- as.logical(col_new)
  } else if (target_type == "numeric") {
    col_new <- as.numeric(col_new)
  }

  return(col_new)
}

# helper for some scales that range from 1 to x instead of 0 to x-1
# i know this is kind of redundant cause the function is so barebones but it felt more readable to me this way
decrement_by <- function(col, value) {
  # decrements all elements in a given vector by the value specified
  return(col - value)
}

# ~~~~~~~~~~~~
# adding reliable leads and lags ----
# ~~~~~~~~~~~~

add_temporal_shift_columns <- function(df,
                                       shift_by,
                                       colname_pattern,
                                       key = c("pid", "syear"),
                                       time_var = "syear",
                                       suffix = "_in_{shift_by}y") {
  #' Add Temporally Shifted Versions of Existing Columns
  #'
  #' For a specified set of columns detected based on a regex string, add temporally shifted versions of those columns to the dataset.
  #' It does this by creating a new dataframe with the key and those columns.
  #' It shifts the time_var by the given shift and appends the suffix to the non-key columns.
  #' It then joins the original df with the shifted one, thus adding the shifted versions to the original dataset.
  #'
  #' @param df A df/tibble containing panel data.
  #' @param shift_by Number of time units (e.g., years) to look forward to get the new values. EX: 3 will add the 2019 value to the 2016 row.
  #' @param colname_pattern Regex pattern to select columns for shifting.
  #' @param key Columns that uniquely identify each row. defaults to c("pid", "syear").
  #' @param time_var Name of the column representing the temporal index. defaults to "syear".
  #' @param suffix Suffix to append to shifted columns. Can include {shift_by} which will be replaced by the shift value. defaults to "_in_{shift_by}y"
  #'
  #' @return original df with temporally shifted columns appended.

  # input validation
  if (!is.numeric(shift_by) || any(shift_by != as.integer(shift_by))) {
    stop("shift_by needs to be an integer or a vector of integers.")
  }
  if (!is.character(colname_pattern) ||
    length(colname_pattern) != 1 || colname_pattern == "") {
    stop("colname_pattern needs to be a single non-empty string.")
  }
  if (!is.character(suffix) ||
    length(suffix) != 1 || suffix == "") {
    stop("suffix needs to be a single non-empty string.")
  }

  combined_df <- df

  for (shift in shift_by) {
    # replace placeholder in suffix if present
    suffix_current <- str_replace(suffix, "\\{shift_by\\}", as.character(shift))

    # create df to join
    backdated_df <- df %>%
      # keep only primary key and relevant columns
      select(all_of(key), matches(colname_pattern)) %>%
      mutate(!!time_var := !!sym(time_var) - shift) %>%
      rename_with(~ str_c(., suffix_current), matches(colname_pattern))

    combined_df <- left_join(combined_df, backdated_df, by = key, relationship = "one-to-one")
  }

  return(combined_df)
}
