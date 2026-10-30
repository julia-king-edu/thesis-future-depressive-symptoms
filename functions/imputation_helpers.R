# ~~~~~~~~~~~~
# Author: Julia King
# Purpose: functions to aid in imputation workflow
# Notes: notes
# ~~~~~~~~~~~~

# ~~~~~~~~~~~~
# loading packages ----
# ~~~~~~~~~~~~

library(tidyverse)
library(here)
library(vctrs)
library(nnet)

# ~~~~~~~~~~~~
# filter functions ----
# ~~~~~~~~~~~~

filter_relevant <- function(df) {
  #' Filter df to relevant cases and years
  #'
  #' Takes the train or test df and limits it to the pid-syear combos with depression in 1y variable present.
  #' Basically a wrapper function for filter(!is.na(depression_phq4_score_in_1y))
  #'
  #' @param df : train or test df
  #'
  #' @return df with irrelevant rows and cols removed.

  df <- df %>%
    filter(!is.na(depression_phq4_binary_in_1y))

  return(df)
}

# ~~~~~~~~~~~~
# custom step impute multinom ----
# ~~~~~~~~~~~~
# copied pretty much in its entirety from the implementations of step_impute_linear, just swapped out the actual model.

step_impute_multinom <- function(recipe,
                                 ...,
                                 role = NA,
                                 trained = FALSE,
                                 impute_with = all_predictors(),
                                 models = NULL,
                                 skip = FALSE,
                                 id = rand_id("impute_multinom")) {
  add_step(
    recipe,
    step_impute_multinom_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      impute_with = enquos(impute_with),
      models = models,
      skip = skip,
      id = id,
      case_weights = NULL
    )
  )
}

step_impute_multinom_new <- function(terms,
                                     role,
                                     trained,
                                     models,
                                     impute_with,
                                     skip,
                                     id,
                                     case_weights) {
  step(
    subclass = "impute_multinom",
    terms = terms,
    role = role,
    trained = trained,
    impute_with = impute_with,
    models = models,
    skip = skip,
    id = id,
    case_weights = case_weights
  )
}

multinom_wrap <- function(vars,
                          dat,
                          wts = NULL,
                          call = caller_env(2)) {
  dat <- as.data.frame(dat[, c(vars$y, vars$x), drop = FALSE])

  complete <- vec_detect_complete(dat)
  dat <- dat[complete, , drop = FALSE]

  if (!is.null(wts)) {
    wts <- wts[complete]
  }

  if (nrow(dat) == 0) {
    cli::cli_abort(
      "The data did not have any rows where the imputation values were all complete. It is thus unable to fit the multinomial regression model.",
      call = call
    )
  }

  if (!is.factor(dat[[vars$y]]) || is.ordered(dat[[vars$y]])) {
    cli::cli_abort(
      "Variable {.var {vars$y}} chosen for multinomial regression imputation must be an unordered factor.",
      call = call
    )
  }

  dat[[vars$y]] <- droplevels(dat[[vars$y]])

  if (nlevels(dat[[vars$y]]) < 2) {
    cli::cli_abort("Variable {.var {vars$y}} must have at least two observed levels.",
      call = call
    )
  }

  if (is.null(wts)) {
    wts <- rep(1, nrow(dat))
  } else {
    wts <- as.double(wts)
  }

  # the line im actually changing, isnt this exciting?
  out <- nnet::multinom(
    as.formula(paste0(vars$y, "~", ".")),
    data = dat,
    weights = wts,
    trace = FALSE,
    model = FALSE
  )

  out$..imp_vars <- vars$x
  out
}

prep.step_impute_multinom <- function(x, training, info = NULL, ...) {
  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)

  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  var_lists <- recipes:::impute_var_lists(
    to_impute = x$terms,
    impute_using = x$impute_with,
    training = training,
    info = info
  )

  x$models <- lapply(var_lists, multinom_wrap, dat = training, wts = wts)

  names(x$models) <- vapply(var_lists, function(x) {
    x$y
  }, c(""))

  step_impute_multinom_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    models = x$models,
    impute_with = x$impute_with,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

bake.step_impute_multinom <- function(object, new_data, ...) {
  col_names <- names(object$models)

  check_new_data(col_names, object, new_data)

  missing_rows <- !vec_detect_complete(new_data)

  if (!any(missing_rows)) {
    return(new_data)
  }

  old_data <- new_data

  for (col_name in col_names) {
    missing_rows <- is.na(new_data[[col_name]])

    if (!any(missing_rows)) {
      next
    }

    preds <- object$models[[col_name]]$..imp_vars
    pred_data <- old_data[missing_rows, preds, drop = FALSE]

    if (anyNA(pred_data)) {
      cli::cli_warn(
        "There were missing values in the predictor(s) used to impute; imputation did not occur."
      )
    } else {
      pred_vals <- predict(object$models[[col_name]], newdata = pred_data, type = "class")
      pred_vals <- recipes:::cast(pred_vals, new_data[[col_name]])
      new_data[[col_name]] <- vec_cast(new_data[[col_name]], pred_vals)
      new_data[missing_rows, col_name] <- pred_vals
    }
  }

  new_data
}

print.step_impute_multinom <- function(x, width = max(20, options()$width - 31), ...) {
  title <- "Multinomial regression imputation for "
  print_step(names(x$models),
    x$terms,
    x$trained,
    title,
    width,
    case_weights = x$case_weights
  )
  invisible(x)
}

tidy.step_impute_multinom <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$models), model = unname(x$models))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, model = list(NULL))
  }
  res$id <- x$id
  res
}
