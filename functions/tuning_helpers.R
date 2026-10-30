# ~~~~~~~~~~~~
# Author: Julia King
# Purpose: functions to aid in modelling workflow
# Notes: notes
# ~~~~~~~~~~~~

# ~~~~~~~~~~~~
# selection helpers ----
# ~~~~~~~~~~~~

select_formula_cols <- function(data, roles_df = NULL, keep_weights = TRUE) {
  #' Select Formula Columns
  #'
  #' Given a df, return shrunk version of df where all variables except the ones needed for formula are removed.
  #' also relocates outcome col to front
  #'
  #' @param data : df
  #' @param roles_df : df defining roles, with cols variable and role.
  #' @param keep_weights : logical, declares if columns with "case_weights" role should be kept. defaults to true.
  #'
  #' @return shrunk df

  # load roles df if not provided
  if (is.null(roles_df)) {
    roles_df <- load_roles_df()
  }

  # reduce roles_df if necessary (if variables were dropped)
  roles_df <- roles_df %>%
    filter(variable %in% colnames(data))

  # get columns
  predictor_cols <- roles_df %>%
    filter(role == "predictor") %>%
    pull(variable)

  outcome_col <- roles_df %>%
    filter(role == "outcome") %>%
    pull(variable)

  weight_col <- c()

  if (keep_weights) {
    weight_col <- roles_df %>%
      filter(role == "case_weights") %>%
      pull(variable)
  }

  # select relevant columns
  data <- data %>%
    select(all_of(c(predictor_cols, outcome_col, weight_col))) %>%
    relocate(all_of(c(outcome_col, weight_col)))

  return(data)
}

# ~~~~~~~~~~~~
# fold generators ----
# ~~~~~~~~~~~~

split_vfold <- function(data, v = 5, strata = NULL, seed = 94) {
  #' Split data into Folds
  #'
  #' Split given dataset using vfold_cv.
  #'
  #' @param data : df to apply vfold_cv to
  #' @param v : number of partitions
  #' @param strata : strata for vfold_cv
  #' @param seed : seed set before split
  #'
  #' @return return_desc

  if (is.null(data)) {
    return(NULL)
  }

  set.seed(seed)
  train_cv <- vfold_cv(data = data, v = v, strata = all_of(strata))

  return(train_cv)
}


# this function is not used, bc im too unsure about the implementation and vfold works too
weighted_mc_cv <- function(data,
                           weight_col,
                           times = 10,
                           train_prop = 0.8,
                           compress_weight = FALSE,
                           seed = 94) {
  #' Weighted Monte Carlo Crossvalidation
  #'
  #' Monte Carlo Crossvalidation fold generator that can incorporate sampling probabilties.
  #' Higher weights are drawn more often, thus enabling a dataset with sampling probabilities to act as a
  #' "pseudo-population". The resulting folds can then be interpreted as random samples of the population without further weights.
  #'
  #' @param data : df to sample from
  #' @param weight_col : colname of col with weights / sampling probabilities. will be scaled internally
  #' @param times : nr of folds to draw
  #' @param train_prop : size of the training set compared to the original data. defaults to 0.8
  #' @param compress_weight : whether or not to use sqrt on weights before sampling. preserves rank but distorts pop distribution. useful if non-compressed weights lead to folds dominated by a few observations.
  #' @param seed : seed set before bootstrap draws.
  #'
  #' @return return_desc

  # filter out 0 weights
  data <- data %>%
    mutate(across(all_of(weight_col), as.double))
  data <- data %>%
    filter(.data[[weight_col]] > 0) %>%
    mutate(.row_id = row_number()) # fix row numbers, was causing issues.

  # smooth weights
  weights <- data[[weight_col]]
  if (compress_weight) {
    weights <- sqrt(weights)
  }
  weights <- weights / sum(weights)

  mc_cv_single <- function(data, weights, train_prop) {
    # get size of both sets
    n <- nrow(data)
    train_n <- floor(n * train_prop)
    validation_n <- n - train_n

    # get training sample
    train_ids <- sample(
      x = seq_len(n),
      size = train_n,
      replace = TRUE,
      prob = weights
    )

    # get validation sample
    validation_ids <- sample(
      x = seq_len(n),
      size = validation_n,
      replace = TRUE,
      prob = weights
    )

    # create split
    split <- make_splits(list(analysis = train_ids, assessment = validation_ids),
      data = data
    )

    return(split)
  }

  set.seed(seed)
  # get splits
  splits <- map(
    seq_len(times),
    ~ mc_cv_single(
      data = data,
      weights = weights,
      train_prop = train_prop
    )
  )

  manual_rset(splits = splits, ids = paste0("Resample", seq_len(times)))
}
