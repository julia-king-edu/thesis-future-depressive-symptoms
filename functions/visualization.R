# ~~~~~~~~~~~~
# Author: Julia King
# Purpose: functions providing visualizations utilized in multiple files
# Notes: notes
# ~~~~~~~~~~~~

# ~~~~~~~~~~~~
# loading packages ----
# ~~~~~~~~~~~~

library(tidyverse)
library(here)
library(paletteer)
library(ggtext)

# ~~~~~~~~~~~~
# common palettes ----
# ~~~~~~~~~~~~
# Okabe–Ito-esque

palette_qualitative <- c(
  "#DD3540FF",
  "#0072B2FF",
  "#009E73FF",
  "#E69F00FF",
  "#CC79A7FF",
  "#56B4E9FF",
  "#F0E442FF",
  "#000000FF",
  "#D55E00FF",
  "#999999FF"
)

palette_diverging <- list(
  low = "#0072B2FF",
  mid = "#F5F5F5FF",
  high = "#DD3540FF"
)

palette_sequential <- list(
  low = "#F5F5F5FF",
  high = "#DD3540FF"
)

fill_qualitative <- function(limits = NULL, drop = TRUE, na.value = "grey50", ...) {
  scale_fill_manual(
    values = palette_qualitative,
    limits = limits,
    drop = drop,
    na.value = na.value,
    ...
  )
}

color_qualitative <- function(limits = NULL, drop = TRUE, na.value = "grey50", ...) {
  scale_color_manual(
    values = palette_qualitative,
    limits = limits,
    drop = drop,
    na.value = na.value,
    ...
  )
}

fill_diverging <- function(limits = NULL, midpoint = 0, na.value = "grey50", oob = scales::squish, ...) {
  scale_fill_gradient2(
    low = palette_diverging$low,
    mid = palette_diverging$mid,
    high = palette_diverging$high,
    midpoint = midpoint,
    limits = limits,
    na.value = na.value,
    oob = oob,
    ...
  )
}

color_diverging <- function(limits = NULL, midpoint = 0, na.value = "grey50", oob = scales::squish, ...) {
  scale_color_gradient2(
    low = palette_diverging$low,
    mid = palette_diverging$mid,
    high = palette_diverging$high,
    midpoint = midpoint,
    limits = limits,
    na.value = na.value,
    oob = oob,
    ...
  )
}

fill_sequential <- function(limits = NULL, na.value = "grey50", oob = scales::squish, ...) {
  scale_fill_gradient(
    low = palette_sequential$low,
    high = palette_sequential$high,
    limits = limits,
    na.value = na.value,
    oob = oob,
    ...
  )
}

color_sequential <- function(limits = NULL, na.value = "grey50", oob = scales::squish, ...) {
  scale_color_gradient(
    low = palette_sequential$low,
    high = palette_sequential$high,
    limits = limits,
    na.value = na.value,
    oob = oob,
    ...
  )
}

# ~~~~~~~~~~~~
# missingness ----
# ~~~~~~~~~~~~

# create table with % missing per year-variable combination
na_heatmap_variables <- function(df, group_var = "syear", highlight_groups = c(2015, 2018, 2020, 2022), highlight_borders = TRUE,
                                 bold_variables = c()) {
  #' NA Heatmap Generator
  #'
  #' Given a df, generates missingness heatmap per variable per group_var
  #'
  #' @param df : df to visualize missingness of
  #' @param group_var : single string, indicates variables to group by beforehand, will then be displayed in x axis
  #' @param highlight_groups : vector of values present in group_var to highlight
  #' @param highlight_borders : logical; whether to put rectangular borders around the highlighted groups or not.
  #' @param bold_variables : vector of strings. variable names in df matching any of the strings will be formatted as bold.
  #'
  #' @return ggplot object of heatmap

  # find years to highlight later
  if (highlight_borders) {
    highlight_df <- tibble(
      xmin = match(highlight_groups, sort(unique(df[[group_var]]))) - 0.5,
      xmax = match(highlight_groups, sort(unique(df[[group_var]]))) + 0.5,
      ymin = -Inf,
      ymax = Inf
    )
  } else {
    highlight_df <- tibble(
      xmin = numeric(),
      xmax = numeric(),
      ymin = numeric(),
      ymax = numeric()
    )
  }

  # create df with missing % per variable & group
  missing_percentage_df <- df %>%
    group_by(!!sym(group_var)) %>%
    summarise(across(where(is.atomic), ~ mean(is.na(.x)) * 100)) %>%
    pivot_longer(-all_of(group_var), names_to = "variable", values_to = "missing_percent") %>%
    mutate(
      variable = factor(variable, levels = colnames(df %>%
        select(
          -all_of(group_var)
        ))),
      missing_percent = floor(missing_percent * 100) / 100
    ) %>%
    mutate(
      fill_value = if_else(
        !!sym(group_var) %in% highlight_groups,
        -missing_percent,
        missing_percent
      )
    ) %>%
    mutate(
      variable = fct_rev(variable),
      x_groups = factor(!!sym(group_var))
    ) %>%
    select(
      x_groups,
      variable,
      missing_percent,
      fill_value
    )

  # create string that contains "plain" for all non-bold variables and "bold" for the bold ones.
  format_vector <- c()
  for (level in levels(missing_percentage_df$variable)) {
    if (level %in% bold_variables) {
      format_vector <- c(format_vector, "bold")
    } else {
      format_vector <- c(format_vector, "plain")
    }
  }

  # plot
  missing_percentage_heatmap <- missing_percentage_df %>%
    ggplot(aes(
      y = variable,
      x = x_groups,
      fill = fill_value
    )) +
    geom_tile() +
    geom_rect(
      data = highlight_df,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      inherit.aes = FALSE,
      color = palette_qualitative[8],
      alpha = 0,
      linewidth = 1
    ) +
    # scale_fill_paletteer_c("ggthemes::Red-Green-Gold Diverging", direction = -1, limits = c(0, 100)) +
    fill_diverging(
      limits = c(-100, 100),
      breaks = c(-100, -50, 0, 50, 100),
      labels = c(100, 50, 0, 50, 100)
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      ),
      axis.text.y = element_text(face = format_vector)
    ) +
    geom_text(aes(label = missing_percent), size = 3) +
    labs(
      title = "Missingness Heatmap of all Variables",
      subtitle = paste("Percentage of NA's in each variable per", group_var, "(range 0-100)"),
      x = group_var,
      y = "Variable Name",
      fill = "Missing (%)"
    )

  return(missing_percentage_heatmap)
}


na_heatmap_groups <- function(df, y, x = "syear", ignore_complete = TRUE) {
  #' NA Heatmap Generator
  #'
  #' Given a df, generates missingness heatmap per interaction of the grouping variables x and y
  #'
  #' @param df : df to visualize missingness of
  #' @param y : single string, indicates variable to group by & present on y axis. Named before x cause this is the one you have to name.
  #' @param x : single string, indicates variable to group by & present on x axis. should in almost all cases remain as default. defaults to "syear"
  #' @param ignore_complete : logical; ignores columns with no NA at all. This usually only affects metadata variables.
  #'
  #' @return ggplot object of heatmap

  x_str <- x
  y_str <- y

  # drop complete if needed
  if (ignore_complete) {
    df <- df %>%
      select(all_of(x), all_of(y), where(~ any(is.na(.))))
  }

  missing_percentage_df <- df %>%
    # rename vars for less annoying syntax
    rename(
      x = !!sym(x),
      y = !!sym(y)
    ) %>%
    group_by(x, y) %>%
    # missingness per xy per variable
    summarise(across(where(is.atomic), ~ mean(is.na(.x)) * 100),
      .groups = "keep"
    ) %>%
    # aggregate missingness
    pivot_longer(-c(x, y), names_to = "variable", values_to = "missing_percent") %>%
    summarise(
      missing_percent = mean(missing_percent),
      .groups = "drop"
    ) %>%
    mutate(
      # rounding but always use floor to distinguish between close to 100 or true completeness
      missing_percent = floor(missing_percent * 100) / 100
    )

  missing_percentage_heatmap <- missing_percentage_df %>%
    ggplot(aes(
      y = fct_rev(factor(y)),
      x = factor(x),
      fill = missing_percent
    )) +
    geom_tile() +
    # scale_fill_paletteer_c("ggthemes::Red-Green-Gold Diverging", direction = -1, limits = c(0, 100)) +
    fill_sequential(limits = c(0, 100)) +
    theme_minimal() +
    theme(axis.text.x = element_text(
      angle = 25,
      vjust = 0.5,
      hjust = 1
    )) +
    geom_text(aes(label = missing_percent), size = 3) +
    labs(
      title = paste("Heatmap of Missingness grouped by", x, "&", y),
      subtitle = paste("Percentage of NA's each ", x, "-", y, " combination (range: 0-100)", sep = ""),
      x = x_str,
      y = y_str
    )
  return(missing_percentage_heatmap)
}
