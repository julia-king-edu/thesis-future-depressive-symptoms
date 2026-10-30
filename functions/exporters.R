# ~~~~~~~~~~~~
# Author: Julia King
# Purpose: functions to export tables and figures every time i regenerate them, while also still displaying them inline
# Notes: notes
# ~~~~~~~~~~~~

library(tidyverse)
library(xtable)


# ~~~~~~~~~~~~
# figures ----
# ~~~~~~~~~~~~

# pipe helper
# ggplots are evaluated at the end of a %>%, thus you cant directly pipe them to export.
# instead do
# - instead of %>%, then
# partial(export_return, filename = "s", width = 10, height = 10) for images
# idk why but sometimes -gg wasnt enough so i added some variants with the exact same call

`-.gg` <- function(e1, e2) {
  e2(e1)
}

`-.ggplot` <- function(e1, e2) {
  e2(e1)
}

`-.patchwork` <- function(e1, e2) {
  e2(e1)
}

.export_figure <- function(figure,
                           filename,
                           width = 10,
                           height = 10,
                           dpi = 300,
                           ...) {
  if (!endsWith(filename, ".png")) {
    filename <- paste0(filename, ".png")
  }

  ggsave(
    filename = file.path(PATH_IMG, filename),
    plot = figure,
    width = width,
    height = height,
    dpi = dpi,
    ...
  )
}


# ~~~~~~~~~~~~
# tables ----
# ~~~~~~~~~~~~

# this function was awful to write and was changed super often bc i always needed some new feature, 
# so its also kind of unorganized.
.export_table <- function(data,
                          filename,
                          caption = NULL,
                          label = NULL,
                          align = NULL,
                          digits = 3,
                          vline.after = NULL,
                          floating = TRUE,
                          table.placement = "!ht",
                          tabular.environment = "tabularx",
                          hline.after = NULL,
                          NA.string = "\\textcolor{gray}{\\textit{NA}}",
                          add.to.row = NULL,
                          sanitize.text.function = NULL,
                          transform.colnames.function = function(x) {
                            paste0(
                              "\\textbf{",
                              x,
                              "}"
                            )
                          },
                          width = NULL,
                          break.chars = c(),
                          ...) {
  # input validation & fixing

  ## check if label and caption are present
  if (is.null(caption)) {
    stop("Arguments caption needs to be provided for tables.")
  }
  if (is.null(label)) {
    warning("No label provided, using filename as label")
    label <- filename
  }
  if (grepl("\\s", label)) {
    stop("Label cannot have whitespaces.")
  }

  ## add extension to filename
  if (!endsWith(filename, ".tex")) {
    filename <- paste0(filename, ".tex")
  }

  ## add table: to label if not done
  if (!startsWith(label, "table:")) {
    label <- paste0("table:", label)
  }


  ## placement of vertical lines (between cols):
  ## if NULL specified, make it empty vector
  ## if "all" or "outer" make it match the parameter
  if (is.null(vline.after)) {
    vline.after <- c()
  } else if (is.character(vline.after) &&
    length(vline.after) == 1) {
    if (vline.after == "all") {
      vline.after <- 0:ncol(data)
    } else if (vline.after == "outer") {
      vline.after <- c(0, ncol(data))
    }
  }
  ## at the end, add 1 to all vline positions. xtable assumes i want a column with the row indexes.
  ## i am removing that index but i still need to pass what format the index would be, thus all cols are moved by 1
  vline.after <- vline.after + 1


  ## column text aligning:
  # if not given, use default
  if (is.null(align) || length(align) == 0) {
    if (tabular.environment == "tabularx") {
      align <- rep("X", ncol(data))
    } else {
      align <- rep("l", ncol(data))
    }
  }
  # if length 1 or < ncol(data), extend align vector to be of length ncol(data)
  if (length(align) == 1) {
    align <- rep(align, ncol(data))
  } else if (length(align) < ncol(data)) {
    warning(
      "Align formatting length does not match number of columns. Last element will be carried forward."
    )
    align <- c(
      align,
      rep(tail(align, 1), ncol(data) - length(align))
    )
  # if length too long, kill export
  } else if (length(align) > ncol(data)) {
    stop("Number of align specifications cannot exceed number of columns.")
  }

  ## add "r" in beginning for the hypothetical rownames col that xtable assumes i want.
  ## this could be any coltype, as its dropped internally in xtable
  align <- c("r", align)


  ## combine vline locations with align: insert a | at every location,
  ## starting from the back, as starting from the front would shift positions.
  for (i in sort(vline.after, decreasing = TRUE)) {
    align[i] <- paste0(align[i], "|")
  }
  # collapse align from vector to single string
  align <- paste0(align, collapse = "")


  ## placement of horizontal lines (between rows)
  ## if NULL specified, make it empty vector
  ## if "all" or "outer" make it match the parameter.
  ## this is passed to xtable, and their indexing is "after which row should i insert it",
  ## and they count the colnames as col 0, meaning top is -1 and below all is nrow(data)
  if (is.null(hline.after)) {
    hline.after <- c(-1, 0, nrow(data))
  } else if (is.character(hline.after) &&
    length(hline.after) == 1) {
    if (hline.after == "all") {
      hline.after <- -1:nrow(data)
    } else if (hline.after == "outer") {
      hline.after <- c(-1, nrow(data))
    }
  }

  ## if not given, get default sanitize function
  if (is.null(sanitize.text.function)) {
    sanitize.text.function <- function(x) {
      sanitize(x, type = "latex")
    }
  }

  # create function that inserts allowbreak after the allowbreak characters
  allowbreak_after <- function(x, break.chars = character()) {
    for (char in break.chars) {
      latex_char <- sanitize(char, type = "latex") # characters like _ i need to scan for \_

      x <- str_replace_all(
        x,
        fixed(latex_char),
        paste0(latex_char, "\\allowbreak ")
      )
    }
    x
  }

  ## in any case, wrapper is needed so it doesnt replace the na string
  sanitize_text_wrapper <- function(x, na_string, sanitize.text.function, break.chars) {
    ids <- x != na_string & !is.na(x)

    x[ids] <- sanitize.text.function(x[ids]) %>%
      allowbreak_after(break.chars)

    return(x)
  }

  ## same for titles, except it also does textbf and italics
  sanitize_colnames_wrapper <- function(x, na_string, sanitize.text.function, break.chars) {
    ids <- x != na_string & !is.na(x)

    x[ids] <- sanitize.text.function(x[ids]) %>%
      allowbreak_after(break.chars) %>%
      transform.colnames.function()

    return(x)
  }

  ## if width not provided, use textwidth for tabularx. can stay NULL for tabular.
  if (tabular.environment == "tabularx" &&
    is.null(width)) {
    width <- "\\textwidth"
  }

  ## rename columns if necessary: removes leading ., collapse multiple whitespace to 1
  data <- data %>%
    rename_with(~ .x %>%
      str_remove("^\\.") %>%
      str_replace_all("\\s\\s+", " "))

  ## if environment is longtable, disable floating and position
  if (tabular.environment == "longtable") {
    floating <- FALSE
    table.placement <- NULL

    # sanitize manually included header
    header <- sanitize_colnames_wrapper(
      colnames(data),
      na_string = NA.string,
      sanitize.text.function = sanitize.text.function,
      break.chars = break.chars
    )

    add.to.row <- list(
      pos = list(-1),
      command = paste0(
        "\\endfirsthead\n",
        "\\hline\n",
        paste(header, collapse = " & "), " \\\\\n",
        "\\hline\n",
        "\\endhead\n",
        "\\hline\n",
        "\\multicolumn{", ncol(data), "}{r}{\\textit{Continued on next page}} \\\\\n",
        "\\hline\n",
        "\\endfoot\n",
        "\\hline\n",
        "\\endlastfoot\n"
      )
    )
  }

  # create the actual table.
  # only the arguments for the table itself go here,
  # formatting for surrounding stuff goes to print
  ltx <- xtable(
    data,
    type = "latex",
    caption = caption,
    label = label,
    align = align,
    digits = digits,
    ...
  )

  # for some reason xtable is supposed to save with print
  print(
    ltx,
    file = file.path(PATH_TABLE, filename),
    floating = floating,
    table.placement = table.placement,
    tabular.environment = tabular.environment,
    hline.after = hline.after,
    NA.string = NA.string,
    include.rownames = FALSE,
    add.to.row = add.to.row,
    sanitize.text.function = function(x) {
      sanitize_text_wrapper(x, NA.string, sanitize.text.function, break.chars)
    },
    sanitize.colnames.function = function(x) {
      sanitize_colnames_wrapper(x, NA.string, sanitize.text.function, break.chars)
    },
    width = width,
    timestamp = NULL # remove timestamp from files, so itll only actually update in the git repo if table changes.
  )
}


# ~~~~~~~~~~~~
# common exposed export function ----
# ~~~~~~~~~~~~

export_return <- function(x, filename, ...) {
  # determine x type
  if ("gg" %in% class(x)) {
    .export_figure(figure = x, filename = filename, ...)
  } else if (is.data.frame(x)) {
    .export_table(data = x, filename = filename, ...)
  } else {
    stop("object is neither df nor ggplot object.")
  }

  return(x)
}
