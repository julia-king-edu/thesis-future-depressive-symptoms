# ~~~~~~~~~~~~
# Author: Julia King
# Purpose: functions to aid in reconstruction workflow
# Notes: notes
# ~~~~~~~~~~~~

# ~~~~~~~~~~~~
# loading packages ----
# ~~~~~~~~~~~~

library(tidyverse)
library(here)
library(zoo)
library(rlang)

# ~~~~~~~~~~~~
# control functions ----
# ~~~~~~~~~~~~

missingness_reduction <- function(df,
                                  reference_df,
                                  tolerance = 1e-6,
                                  include_overall = TRUE,
                                  compact = TRUE,
                                  strict_row_match = TRUE,
                                  strict_column_match = TRUE) {
  #' Missingness Reduction
  #'
  #' Report changes in NA percentages & counts between two dataframes.
  #' By default, both datasets need to contain the same number of rows and the same columns.
  #' In case of different column orders, order of df is used.
  #'
  #' @param df : df to evaluate (new or updated version)
  #' @param reference_df : df to compare against (baseline)
  #' @param tolerance : minimum absolute % change to report
  #' @param include_overall : whether to include a row for "all affected variables" at the top.
  #' @param compact : logical; prints a version with less columns for easier readability, excluding relative change and absolute nr changes. defaults to TRUE.
  #' @param strict_row_match : logical; whether to stop if nrows differ. defaults to TRUE
  #' @param strict_column_match : logical; allow for the df to contain additional rows compared to the reference_df. reference_df still needs to be a subset.
  #'
  #' @return a tibble with variable-level NA comparison

  # input validation
  if (strict_column_match & !setequal(colnames(df), colnames(reference_df))) {
    # check is active and columns are not the same in both df's (can be in different order)
    stop("Data frames must have identical columns. Change strict_column_match to FALSE to relax this requirement.")
  } else if (!all(colnames(reference_df) %in% colnames(df))) {
    # reference_df is not a subset of df
    stop("reference_df cannot contain columns not present in  df.")
  } else {
    # constraints are fulfilled. expand reference_df if necessary and reorder reference_df.
    reference_df[setdiff(colnames(df), colnames(reference_df))] <- NA

    reference_df <- reference_df %>%
      relocate(all_of(colnames(df)))
  }


  if (strict_row_match && nrow(df) != nrow(reference_df)) {
    warning(
      paste(
        "Number of rows differs: df has",
        nrow(df),
        "rows, reference_df has",
        nrow(reference_df),
        "rows. Change strict_row_match to FALSE to relax this requirement."
      )
    )
  }

  # compute na counts and the derivative vals
  df_na_count <- colSums(is.na(df))
  ref_na_count <- colSums(is.na(reference_df))

  df_na_percent <- df_na_count / nrow(df)
  ref_na_percent <- ref_na_count / nrow(reference_df)

  differences_df <- tibble(
    variable = colnames(df),
    rel_decrease = ifelse(
      ref_na_percent == 0,
      NA,
      (ref_na_percent - df_na_percent) / ref_na_percent
    ),
    na_percent = df_na_percent,
    na_percent_ref = ref_na_percent,
    count_change = df_na_count - ref_na_count,
    NA_count = df_na_count,
    NA_count_ref = ref_na_count
  ) %>%
    filter(abs(rel_decrease) > tolerance) %>%
    mutate(across(where(is.numeric), ~ round(., 3)))

  # add row for all cells in afffected variables
  if (include_overall) {
    overall_row <- differences_df %>%
      summarise(
        variable = paste0("all_affected_variables (n=", n(), ")"),
        rel_decrease = (sum(NA_count_ref) - sum(NA_count)) / sum(NA_count_ref),
        na_percent = sum(NA_count) / (nrow(df) * n()),
        na_percent_ref = sum(NA_count_ref) / (nrow(reference_df) * n()),
        count_change = sum(count_change),
        NA_count = sum(NA_count),
        NA_count_ref = sum(NA_count_ref)
      ) %>%
      mutate(across(where(is.numeric), ~ round(., 3)))

    differences_df <- bind_rows(overall_row, differences_df)
  }

  # format relative cols to %
  differences_df <- differences_df %>%
    mutate(
      rel_decrease = scales::percent(rel_decrease, accuracy = 0.1),
      na_percent = scales::percent(na_percent, accuracy = 0.1),
      na_percent_ref = scales::percent(na_percent_ref, accuracy = 0.1)
    )
  
  # remove less interesting rows if i speciy it
  if (compact) {
    differences_df <- differences_df %>%
      select(
        variable,
        rel_decrease,
        na_percent,
        na_percent_ref,
        NA_count,
        NA_count_ref
      )
  }

  differences_df <- differences_df %>%
    rename(
      "Variable" = variable,
      "Reduction(%)" = rel_decrease,
      "NA(%)" = na_percent,
      "Before(%)" = na_percent_ref,
      "NA(n)" = NA_count,
      "Before(n)" = NA_count_ref
    )

  return(differences_df)
}


print_missingness_reduction <- function(df,
                                        reference_df,
                                        tolerance = 1e-6,
                                        compact = TRUE,
                                        strict_row_match = TRUE,
                                        strict_column_match = TRUE) {
  #' Print Missingness Reduction
  #'
  #' Print changes in NA percentages & counts between two dataframes.
  #' Wrapper for missingness_reduction
  #'
  #' @param df : df to evaluate (new or updated version)
  #' @param reference_df : df to compare against (baseline)
  #' @param tolerance : minimum absolute % change to report
  #' @param compact : logical; prints a version with less columns for easier readability, excluding relative change and absolute nr changes. defaults to TRUE.
  #' @param strict_row_match : logical; whether to stop if nrows differ. defaults to TRUE
  #' @param strict_column_match : logical; allow for the df to contain additional rows compared to the reference_df. reference_df still needs to be a subset.
  #'
  #' @return None, prints a tibble with variable-level NA comparison

  missingness_reduction(
    df = df,
    reference_df = reference_df,
    tolerance = tolerance,
    compact = compact,
    strict_row_match = strict_row_match,
    strict_column_match = strict_column_match
  ) %>%
    print(., n = nrow(.))

  invisible(NULL)
}

# ~~~~~~~~~~~~
# irregularity diagnostics ----
# ~~~~~~~~~~~~

mode <- function(col, na.rm = FALSE) {
  #' Mode
  #'
  #' calculates most frequent value in column
  #'
  #' @param col : column to calculate mode for
  #' @param na.rm : logical, whether to allow NA as the mode
  #'
  #' @return mode of column

  if (na.rm) {
    col <- col[!is.na(col)]
  }

  unique <- unique(col)
  mode <- unique[which.max(tabulate(match(col, unique)))]
  return(mode)
}

mode_table <- function(df, cols, grouping = c("pid")) {
  #' Create contingency table of unit modes
  #'
  #' Given a df and 1 or 2 columns,
  #' calculates the mode of each column for each group
  #' and creates frequency/ conditional frequency table of those modes.
  #' Prints the results to the console
  #'
  #' @param df : data frame
  #' @param cols : string vector of length 1 or 2 specifying the columns to use
  #' @param grouping : string vector containing the variables to group by when creating the modes
  #'

  modes_df <- df %>%
    group_by(across(all_of(grouping))) %>%
    summarise(across(all_of(cols), ~ mode(.x), .names = "{.col}"))

  # create contingency table
  if (length(cols) == 1) {
    print(table(modes_df[[cols[1]]]))
  } else if (length(cols) == 2) {
    print(table(modes_df[[cols[1]]], modes_df[[cols[2]]]))
  } else {
    stop("Cols needs to be vector of length 1 or 2")
  }
}

intra_unit_changes <- function(df,
                               value_col,
                               key = c("pid", "syear"),
                               time_col = "syear",
                               na.rm = TRUE) {
  #' Report Intra-Unit Changes to Variable
  #'
  #' Reports statistics about transitions of a given variable within a unit over consecutive waves of the data.
  #' Dataset is grouped by key - time_col.
  #'
  #' @param df : df to check for transitions
  #' @param value_col : string of value name to report transitions in
  #' @param key : primary key of the panel dataset. defaults to c("pid", "syear")
  #' @param time_col : variable (usually part of the key) indicating the wave/timepoint of the panel. defaults to "syear"
  #' @param na.rm : logical; disregards NA's for transitions if TRUE, meaning x -> NA -> y reports x -> y instead. defaults to TRUE.
  #'
  #' @return df with the transitions

  # convert inputs & optimize for runtime
  group_cols <- syms(setdiff(key, time_col))
  value_col <- sym(value_col)
  time_col <- sym(time_col)

  df <- df %>%
    select(!!!group_cols, !!time_col, !!value_col) %>%
    arrange(!!!group_cols, !!time_col)

  # remove na if specified
  if (na.rm) {
    df <- df %>%
      filter(!is.na(!!value_col))
  }

  # get total nr of cases/people where the value changes
  change_count <- df %>%
    group_by(!!!group_cols) %>%
    filter(n_distinct(!!value_col, na.rm = TRUE) >= 2) %>%
    n_groups()

  total_count <- df %>%
    group_by(!!!group_cols) %>%
    n_groups()

  print(
    paste(
      "Nr. of units with changes: ",
      change_count,
      " (",
      round(change_count / total_count * 100, 2),
      "% of ",
      total_count,
      ").",
      sep = ""
    )
  )

  # get transition frequencies within person
  transition_df <- df %>%
    group_by(!!!group_cols) %>%
    mutate(lag = lag(!!value_col)) %>%
    slice(-1) %>% # i remove first row per group as that by definition does not have a lag. its value is perserved in lag of row 2
    # i can ungroup here because i will only compare lag and value_col,
    # meaning i never compare between rows, so i can never compare values of different people
    ungroup() %>%
    filter(!is.na(lag)) %>%
    count(lag, !!value_col) %>%
    mutate(percent = n / sum(n) * 100) %>%
    arrange(lag != !!value_col, desc(percent))

  return(transition_df)
}

print_intra_unit_changes <- function(df,
                                     value_col,
                                     key = c("pid", "syear"),
                                     time_col = "syear",
                                     na.rm = TRUE) {
  #' Report Intra-Unit Changes to Variable
  #'
  #' Reports statistics about transitions of a given variable within a unit over consecutive waves of the data.
  #' Dataset is grouped by key - time_col.
  #'
  #' @param df : df to check for transitions
  #' @param value_col : string of value name to report transitions in
  #' @param key : primary key of the panel dataset. defaults to c("pid", "syear")
  #' @param time_col : variable (usually part of the key) indicating the wave/timepoint of the panel. defaults to "syear"
  #' @param na.rm : logical; disregards NA's for transitions if TRUE, meaning x -> NA -> y reports x -> y instead. defaults to TRUE.
  #'
  #' @return Nothing, prints table of intra unit changes

  intra_unit_changes(
    df = df,
    value_col = value_col,
    key = key,
    time_col = time_col,
    na.rm = na.rm
  ) %>%
    print(., n = nrow(.))

  invisible(NULL)
}

# ~~~~~~~~~~~~
# specific fill ----
# ~~~~~~~~~~~~

fill_if_constant <- function(data,
                             ...,
                             .fill_edge = FALSE,
                             .direction = c("down", "up", "downup", "updown")) {
  #' Fill if Constant
  #'
  #' Fill NA's in a column. behaves like fill with one exception:
  #' If an NA (or group of NA's) is inbetween 2 values, it only fills if
  #' the two values are identical.
  #' Trailing & leading NA's are left alone by default, unless .fill_edge = TRUE. EVEN THEN:
  #' Trailing NA's will only be filled if direction contains down,
  #' Leading NA's will only be filled if direction contains up.
  #'
  #' @param data : df
  #' @param ... : columns
  #' @param .fill_edge : logical; whether to fill leading / trailing NA's. defaults to FALSE
  #' @param .direction : direction to fill in. see fill()
  #'
  #' @return df with columns filled
  .direction <- match.arg(.direction)

  cols <- map_chr(enquos(...), quo_name)
  dir_list <- str_extract_all(.direction, "(up|down)") %>% unlist()
  result_df <- data

  for (colname in cols) {
    # get a fill up and a fill down.
    # i can grab the "bounding" values with this, cause they will be
    # the closest non-na value before and after that point
    # (for non-na values, they will always be the cell itself, but i dont want to change those anyway)

    colname <- sym(colname)

    result_df <- result_df %>%
      mutate(
        fillconstant_prev = !!colname,
        fillconstant_next = !!colname
      ) %>%
      fill(fillconstant_prev, .direction = "down") %>%
      fill(fillconstant_next, .direction = "up")

    # apply copying
    for (current_dir in dir_list) {
      # determine which positions to copy.
      # there are a few cases, which should be gone down in order.
      # this means, for example, that from 2 onwards, you always assume current is NA
      #
      # | Case | Direction | fill_edge | Other conditions (condensed)        | Action type | Copy source |
      # | ---- | --------- | --------- | ----------------------------------- | ----------- | ----------- |
      # | 1    |           |           | current value in column is not NA   | keep        |             |
      # | 2    |           |           | previous ≠ next AND both are not NA | keep        |             |
      # | 3    | down      | TRUE      | leading NA                          | keep        |             |
      # | 4    | down      | FALSE     | leading NA                          | keep        |             |
      # | 5    | down      | TRUE      | trailing NA                         | copy        | prev_values |
      # | 6    | down      | FALSE     | trailing NA                         | keep        |             |
      # | 7    | up        | TRUE      | leading NA                          | copy        | next_values |
      # | 8    | up        | FALSE     | leading NA                          | keep        |             |
      # | 9    | up        | TRUE      | trailing NA                         | keep        |             |
      # | 10   | up        | FALSE     | trailing NA                         | keep        |             |
      # | 11   | down      |           | previous = next AND both are not NA | copy        | prev_values |
      # | 12   | up        |           | previous = next AND both are not NA | copy        | next_values |
      #
      # an NA in fillconstant_prev means position is a leading NA (as no value could be carried down)
      # an NA in fillconstant_next means position is a trailing NA (as no value could be carried up)
      # thus, assuming we sort out which column to copy from later, the condition for when to copy is

      .constant_condition <- function(col_current,
                                      col_prev,
                                      col_next,
                                      current_dir,
                                      fill_edge) {
        condition <- is.na(col_current) &
          (
            # case 5 OR case 7 OR case 11&12
            (current_dir == "down" &
              fill_edge &
              is.na(col_next)) | # case 5
              (current_dir == "up" &
                fill_edge &
                is.na(col_prev)) | # case 7
              (
                col_prev == col_next &
                  !is.na(col_prev) &
                  !is.na(col_next)
              ) # cases 11&12
          )
        return(condition)
      }

      result_df <- result_df %>%
        # create helper variable containing TRUE/FALSE for if we should copy
        mutate(
          fillconstant_copycondition = .constant_condition(
            col_current = !!colname,
            col_prev = fillconstant_prev,
            col_next = fillconstant_next,
            current_dir = current_dir,
            fill_edge = .fill_edge
          )
        ) %>%
        # if copycondition = TRUE, then copy based on direction
        mutate(
          !!colname := case_when(
            fillconstant_copycondition &
              current_dir == "down" ~ fillconstant_prev,
            fillconstant_copycondition &
              current_dir == "up" ~ fillconstant_next,
            TRUE ~ !!colname # only happens if copy_condition not true, so those to keep
          )
        )
    }

    # before moving on to new variable, delete all helper columns. theoretically redundant,
    # but ensures nothing is carried over between variables.
    result_df <- result_df %>%
      select(-c(
        fillconstant_prev,
        fillconstant_next,
        fillconstant_copycondition
      ))
  }

  return(result_df)
}

fill_n <- function(data,
                   ...,
                   .n = 1,
                   .time = NULL,
                   .direction = c("down", "up", "downup", "updown")) {
  #' Fill N Rows
  #'
  #' Fill NA's in a column, but only for .n consecutive NA's.
  #' Note: downup and updown fills will fill n consecutive NA's from down and up / up and down!
  #' EX: 1 NA NA NA 2 NA NA NA NA NA 3 with .n = 2 and "downup" results in
  #'     1 1 1 2 2 2 2 NA 3 3 3
  #'
  #' @param data : df
  #' @param ... : columns
  #' @param .n : integer, amount of NA's to fill from any non-na value
  #' @param .time : colname string of col that contains your time variable. defaults to row number. has to be integer col.
  #' @param .direction : direction to fill in. see fill()
  #'
  #' @return df with columns filled

  # convert function arguments
  .direction <- match.arg(.direction)

  n <- .n

  use_time <- !is.null(.time)

  cols <- map_chr(enquos(...), quo_name)
  dir_list <- str_extract_all(.direction, "(up|down)") %>% unlist()
  groups_input <- group_vars(data) # needed to regroup inbetween & at end

  # main logic
  result_df <- data %>%
    # add time variable
    mutate(filln_time_current = if (use_time) {
      .data[[.time]]
    } else {
      row_number()
    }) %>%
    # add variable that contains the original order of the df, so i can reorder at the end.
    ungroup() %>%
    mutate(filln_original_order = row_number()) %>%
    group_by(across(all_of(groups_input)))

  for (colname in cols) {
    colname <- sym(colname)
    for (dir in dir_list) {
      dir <- as.character(dir)

      result_df <- result_df %>%
        # sort by the group variables and the time variable. sort with desc(time variable) in case of dir = "up".
        arrange(
          across(all_of(groups_input)),
          if (dir == "up") {
            desc(filln_time_current)
          } else {
            filln_time_current
          }
        ) %>%
        # add indicator for missing values and add a grouping variable.
        # the grouping variable increases each time a non-na value is present,
        # meaning a row and all following NA rows have the same group.
        mutate(
          filln_isna = is.na(!!colname),
          filln_grouping = cumsum(!filln_isna)
        ) %>%
        # within each group, add a variable indicating the time point of the non-na value in the group.
        group_by(filln_grouping, .add = TRUE) %>%
        mutate(filln_time_last_non_na = if_else(!filln_isna, filln_time_current, NA)) %>% # this only copies the time var of non-na (always first row in group)
        fill(filln_time_last_non_na, .direction = dir) %>% # fill the non-na time downwards

        # create variable indicating "time distance" to last valid case
        mutate(filln_distance = abs(filln_time_current - filln_time_last_non_na)) %>%
        # restore original grouping
        ungroup() %>%
        group_by(across(all_of(groups_input))) %>%
        # establish column containing result of unconditional fill
        mutate(filln_fill = !!colname) %>%
        fill(filln_fill, .direction = dir) %>%
        # remove values from fill col if distance > n
        mutate(filln_fill = if_else(filln_distance <= n, filln_fill, NA)) %>%
        # copy if .n <= filln_distance (EX 1 NA 2 2 3 3)
        mutate(!!colname := filln_fill) %>%
        # drop temp columns
        select(
          -c(
            filln_isna,
            filln_grouping,
            filln_time_last_non_na,
            filln_distance,
            filln_fill
          )
        ) %>%
        ungroup() %>%
        arrange(filln_original_order) %>%
        group_by(across(all_of(groups_input)))
    }
  }

  result_df <- result_df %>%
    select(-c(filln_time_current, filln_original_order))


  return(result_df)
}


interpolate_likert <- function(col,
                               time_col = index(col),
                               na.rm = FALSE) {
  #' Interpolate Likert Scale Values
  #'
  #' In panel data, take a col with likert scores (integers of a given range),
  #' ideally containing missing data.
  #' Then interpolates missing values stuck between 2 actual values,
  #' rounds to preserve likert scale, and returns col.
  #' Will never extend likert scale range, as its always bounded between 2 valid values.
  #' EX NA 1 NA NA 3 NA turns into NA 1 2 2 3 NA
  #'
  #' @param col : likert column with to interpolate
  #' @param time_col : time column for accurate interpolation with missing data. defaults to index of col
  #' @param na.rm : whether to remove leading and trailing NA's from the return string. defaults to FALSE
  #'
  #' @return col with interpolated values

  if (length(col) != length(time_col)) {
    stop("Length of time_col is not the same as length of col.")
  }

  result <- col %>%
    na.approx(x = time_col, na.rm = na.rm) %>%
    {
      . + 0.5
    } %>%
    floor() # round() does bankers rounding (at .5 always chooses the round number instead of the ceiling)

  return(result)
}
