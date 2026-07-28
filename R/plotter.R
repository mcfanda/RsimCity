#' Plot aggregated simulation results by method
#'
#' Produces a `ggplot2` line/point plot of one outcome variable against one
#' predictor variable, separately for each method. The outcome is aggregated
#' across all other variables in the data.
#'
#' @param data A `data.frame` containing the simulation results.
#' @param xvar Character string. Name of the variable to use on the x-axis.
#' @param yvar Character string. Name of the numeric variable to plot on the
#'   y-axis.
#' @param method_var Character string. Name of the variable identifying the
#'   method. Default is `"method"`.
#' @param fun Function used to aggregate `yvar` over the remaining variables.
#'   Default is `mean`.
#' @param na.rm Logical. Should missing values be removed before aggregation?
#'   Default is `TRUE`.
#' @param xlabel Optional character string. Label for the x-axis. If `NULL`,
#'   `xvar` is used.
#' @param ylabel Optional character string. Label for the y-axis. If `NULL`,
#'   `yvar` is used.
#' @param title Optional character string. Plot title.
#' @param line Logical. If `TRUE`, lines are drawn. Default is `TRUE`.
#' @param points Logical. If `TRUE`, points are drawn. Default is `TRUE`.
#'
#' @details
#' The function first keeps only `xvar`, `yvar`, and `method_var`. It then
#' aggregates `yvar` by each combination of `xvar` and `method_var`. Therefore,
#' all other columns in `data` are averaged over implicitly.
#'
#' This is useful for simulation summaries where results are computed over
#' several design factors, but the plot should show the average behaviour of
#' each method as a function of a single variable.
#'
#' @return
#' A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' plot_by_method(simdata, xvar = "N", yvar = "rmse")
#'
#' plot_by_method(
#'   data = simdata,
#'   xvar = "rho",
#'   yvar = "PPV",
#'   ylabel = "Positive predictive value"
#' )
#'
#' plot_by_method(
#'   data = simdata,
#'   xvar = "N",
#'   yvar = "bias",
#'   title = "Bias by sample size"
#' )
#' }
#'
#' @export
plot_by_method <- function(data,
                           xvar,
                           yvar,
                           method_var = "method",
                           fun = mean,
                           na.rm = TRUE,
                           xlabel = NULL,
                           ylabel = NULL,
                           title = NULL,
                           line = TRUE,
                           points = TRUE) {

  if (!is.data.frame(data))
    stop("data must be a data.frame")

  needed <- c(xvar, yvar, method_var)
  missing_vars <- setdiff(needed, names(data))

  if (length(missing_vars) > 0)
    stop("Missing variables in data: ", paste(missing_vars, collapse = ", "))

  if (!is.numeric(data[[yvar]]))
    stop("yvar must be numeric")

  plotdata <- data[, c(xvar, method_var, yvar), drop = FALSE]

  aggdata <- stats::aggregate(
    plotdata[[yvar]],
    by = list(
      x = plotdata[[xvar]],
      method = plotdata[[method_var]]
    ),
    FUN = function(z) fun(z, na.rm = na.rm)
  )

  names(aggdata)[3] <- "value"

  aggdata$method <- factor(
    aggdata$method,
    levels = unique(data[[method_var]])
  )

  p <- ggplot2::ggplot(
    aggdata,
    ggplot2::aes(x = x, y = value, color = method, group = method)
  )

  if (line) {
    p <- p + ggplot2::geom_line(linewidth = 1)
  }

  if (points) {
    p <- p + ggplot2::geom_point(size = 2)
  }

  p <- p +
    ggplot2::labs(
      x = if (is.null(xlabel)) xvar else xlabel,
      y = if (is.null(ylabel)) yvar else ylabel,
      color = method_var,
      title = title
    ) +
    ggplot2::theme_classic()

  return(p)
}


#' Plot columns matching a name pattern against an x variable
#'
#' Plots several numeric columns whose names match a given pattern, such as
#' `".SE"` or `".p"`, against a selected x variable. Values are aggregated by
#' the x variable before plotting.
#'
#' @param data A `data.frame`.
#' @param xvar Character string. Name of the variable to use on the x-axis.
#' @param pattern Character string. Pattern used to select columns. Passed to
#'   `grep()`. For example, `".SE"` selects columns such as `"po.SE"`,
#'   `"ca.SE"`, and `"fu.SE"`.
#' @param fun Function used to aggregate values over rows with the same value of
#'   `xvar`. Default is `mean`.
#' @param na.rm Logical. Should missing values be removed before aggregation?
#'   Default is `TRUE`.
#' @param fixed Logical. Passed to `grep()`. If `TRUE`, `pattern` is matched as
#'   plain text rather than as a regular expression. Default is `FALSE`.
#' @param xlabel Optional character string. Label for the x-axis. If `NULL`,
#'   `xvar` is used.
#' @param ylabel Optional character string. Label for the y-axis. If `NULL`,
#'   `pattern` is used.
#' @param title Optional character string. Plot title.
#' @param line Logical. If `TRUE`, lines are drawn. Default is `TRUE`.
#' @param points Logical. If `TRUE`, points are drawn. Default is `TRUE`.
#'
#' @param varsname Optional character vector of column names to plot (used if `pattern` is `NULL`).
#' @param legend Logical. Whether to show a legend (default `TRUE`).
#' @param legend_labels Optional character vector of labels for the legend, same length as matched columns.
#' @param colors Optional character vector of colors for the plotted lines/points.
#' @param linetypes Optional character vector of linetypes for the matched columns.
#'
#' @details
#' The function identifies all columns whose names match `pattern`, reshapes
#' them internally to long format, aggregates the values by `xvar` and column
#' name, and then plots one line per matched column.
#'
#' This is useful for comparing quantities such as different standard errors,
#' p-values, or test statistics stored in separate columns.
#'
#' @return
#' A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' plot_by_columns(results_ubc, xvar = "N", pattern = ".SE")
#'
#' plot_by_columns(results_ubc, xvar = "rho", pattern = ".p")
#'
#' plot_by_columns(
#'   data = results_ubc,
#'   xvar = "N",
#'   pattern = ".SE",
#'   ylabel = "Standard error",
#'   title = "Estimated SE by sample size"
#' )
#' }
#'
#' @export
plot_by_columns <- function(data,
                                 xvar,
                                 pattern=NULL,
                                 varsname=NULL,
                                 fun = mean,
                                 na.rm = TRUE,
                                 fixed = FALSE,
                                 xlabel = NULL,
                                 ylabel = NULL,
                                 legend = TRUE,
                                 legend_labels=NULL,
                                 title = NULL,
                                 line = TRUE,
                                 points = TRUE,
                                 colors = NULL,
                                 linetypes = NULL) {

  if (!is.data.frame(data))
    stop("data must be a data.frame")

  if (!is.character(xvar) || length(xvar) != 1L || is.na(xvar) || !nzchar(xvar)) {
    stop("xvar must be a single, non-missing character string", call. = FALSE)
  }

  if (!xvar %in% names(data)) {
    stop("xvar not found in data: ", xvar, call. = FALSE)
  }

  if (!is.null(pattern)) {
    if (!is.character(pattern) || length(pattern) != 1L || is.na(pattern)) {
      stop("pattern must be a single, non-missing character string", call. = FALSE)
    }

    matched <- grep(pattern, names(data), value = TRUE, fixed = fixed)
  } else {
    if (is.null(varsname) || !is.character(varsname) || length(varsname) == 0L) {
      stop("Specify one or more columns with pattern or varsname", call. = FALSE)
    }

    matched <- varsname
  }

  if (length(matched) == 0L) {
    stop("No columns matched pattern: ", pattern, call. = FALSE)
  }

  missing_vars <- setdiff(matched, names(data))
  if (length(missing_vars) > 0L) {
    stop(
      "The following columns were not found in data: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  non_numeric <- matched[!vapply(data[matched], is.numeric, logical(1))]
  if (length(non_numeric) > 0L) {
    stop(
      "The following selected columns are not numeric: ",
      paste(non_numeric, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.function(fun)) {
    stop("fun must be a function", call. = FALSE)
  }

  if (!is.logical(legend) || length(legend) != 1L || is.na(legend)) {
    stop("legend must be a single logical value", call. = FALSE)
  }

  longdata <- data.frame(
    x = rep(data[[xvar]], times = length(matched)),
    variable = rep(matched, each = nrow(data)),
    value = unlist(data[matched], use.names = FALSE),
    stringsAsFactors = FALSE
  )

  aggdata <- stats::aggregate(
    longdata$value,
    by = list(
      x = longdata$x,
      variable = longdata$variable
    ),
    FUN = function(z) fun(z, na.rm = na.rm)
  )

  names(aggdata)[3] <- "value"

  aggdata$variable <- factor(aggdata$variable, levels = matched)

  if (is.null(legend_labels)) {
    legend_labels <- matched
  } else {
    if (!is.character(legend_labels) || length(legend_labels) < length(matched)) {
      stop(
        "legend_labels must be a character vector with one label per selected column",
        call. = FALSE
      )
    }

    legend_labels <- legend_labels[seq_along(matched)]
  }
  names(legend_labels) <- matched

  p <- ggplot2::ggplot(
    aggdata,
    ggplot2::aes(
      x = x,
      y = value,
      color = variable,
      group = variable
    )
  )

  if (line) {
    if (is.null(linetypes)) {
      p <- p + ggplot2::geom_line(linewidth = 1, linetype = "solid")
    } else {
      if (length(linetypes) < length(matched)) {
        stop(
          "linetypes has length ", length(linetypes),
          " but there are ", length(matched), " matched columns"
        )
      }

      linetypes <- linetypes[seq_along(matched)]
      names(linetypes) <- matched

      p <- p +
        ggplot2::geom_line(
          ggplot2::aes(linetype = variable),
          linewidth = 1
        ) +
        ggplot2::scale_linetype_manual(values = linetypes, labels = legend_labels)
    }
  }

  if (points) {
    p <- p + ggplot2::geom_point(size = 2)
  }

  if (!is.null(colors)) {
    if (length(colors) < length(matched)) {
      stop(
        "colors has length ", length(colors),
        " but there are ", length(matched), " matched columns"
      )
    }

    colors <- colors[seq_along(matched)]
    names(colors) <- matched

    p <- p + ggplot2::scale_color_manual(values = colors, labels = legend_labels)
  }

  p <- p +
    ggplot2::labs(
      x = if (is.null(xlabel)) xvar else xlabel,
      y = if (is.null(ylabel)) {
        if (is.null(pattern)) "Value" else pattern
      } else {
        ylabel
      },
      color = "Variable",
      linetype = "Variable",
      title = title
    )

  if (is.null(colors)) {
    p <- p + ggplot2::scale_color_discrete(labels = legend_labels)
  }

  p <- p + ggplot2::theme_classic()

  if (!legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)
}

#' Plot one outcome against one predictor with optional grouping and splits
#'
#' Produces a `ggplot2` line/point plot of one numeric outcome against one
#' predictor variable. An optional `zvar` draws separate lines, while
#' `splits` replicate the plot across panels.
#'
#' @param data A `data.frame` containing the simulation results.
#' @param xvar Character string. Name of the variable to use on the x-axis.
#' @param yvar Character string or character vector. Name of the numeric
#'   variable to plot on the y-axis, or several numeric variables to plot as
#'   separate lines.
#' @param zvar Optional character string. Name of the variable used to draw one
#'   line per value. Default is `NULL`.
#' @param splits Optional character vector, or a list of character strings,
#'   naming variables used to split the plot. The first split variable is laid
#'   out in columns, the second in rows, and each additional split variable
#'   generates a separate plot for every combination of its values.
#' @param fun Function used to aggregate `yvar` over duplicate combinations of
#'   `xvar`, `zvar`, and `splits`. Default is `mean`.
#' @param na.rm Logical. Should missing values be removed before aggregation?
#'   Default is `TRUE`.
#' @param xlabel Optional character string. Label for the x-axis. If `NULL`,
#'   `xvar` is used.
#' @param ylabel Optional character string. Label for the y-axis. If `NULL`,
#'   `yvar` is used for a single response and `"Value"` for multiple responses.
#' @param color_label Optional character string. Label for the color legend. If
#'   `NULL`, `zvar` is used for single-response plots and `"Variable"` for
#'   multi-response plots.
#' @param axis_labels Optional named list or named character vector with labels
#'   for plot guides. Supported names are `"x"`, `"y"`, `"color"`, and
#'   `"linetype"`. These are used only when the corresponding explicit label
#'   argument is `NULL`.
#' @param stack_labels Optional character vector, or list of character strings,
#'   giving display labels for split variables. If unnamed, it must have one
#'   label per split variable and is matched by order. If named, names must be
#'   drawn from `splits`, and unmatched split variables keep their original
#'   names.
#' @param title Optional character string. Plot title.
#' @param line Logical. If `TRUE`, lines are drawn. Default is `TRUE`.
#' @param points Logical. If `TRUE`, points are drawn. Default is `FALSE`.
#'
#' @details
#' The function keeps only `xvar`, `yvar`, `zvar`, and the requested
#' `splits`, then aggregates `yvar` by every unique combination of those
#' variables. This makes it suitable for simulation results where several
#' replications or design factors must be averaged before plotting.
#'
#' If `yvar` contains more than one name, the function reshapes those columns to
#' long format internally and draws one line per response variable. If `zvar`
#' is also supplied, one line is drawn for each `yvar` by `zvar` combination.
#'
#' If `splits` has length 0, the function returns a single plot. If it has
#' length 1 or 2, it returns one faceted `ggplot` object. If it has length
#' greater than 2, it returns a named list of `ggplot` objects, one for each
#' combination of split variables from the third onward.
#'
#' @return
#' A `ggplot` object when `length(splits) <= 2`; otherwise a named list of
#' `ggplot` objects.
#'
#' @examples
#' \dontrun{
#' plot_by_splits(simdata, xvar = "N", yvar = "rmse")
#'
#' plot_by_splits(
#'   data = simdata,
#'   xvar = "N",
#'   yvar = c("bias", "rmse")
#' )
#'
#' plot_by_splits(
#'   data = simdata,
#'   xvar = "N",
#'   yvar = "rmse",
#'   zvar = "method"
#' )
#'
#' plot_by_splits(
#'   data = simdata,
#'   xvar = "N",
#'   yvar = "rmse",
#'   zvar = "method",
#'   splits = c("rho", "design"),
#'   axis_labels = list(x = "Sample size", y = "RMSE"),
#'   stack_labels = c(rho = "Correlation", design = "Design")
#' )
#' }
#'
#' @export
plot_by_splits <- function(data,
                           xvar,
                           yvar,
                           zvar = NULL,
                           splits = NULL,
                           fun = mean,
                           na.rm = TRUE,
                           xlabel = NULL,
                           ylabel = NULL,
                           color_label = NULL,
                           axis_labels = NULL,
                           stack_labels = NULL,
                           title = NULL,
                           line = TRUE,
                           points = FALSE) {

  validate_name <- function(name, arg) {
    if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
      stop(arg, " must be a single, non-missing character string", call. = FALSE)
    }
  }

  validate_names <- function(names, arg) {
    if (!is.character(names) || length(names) == 0L) {
      stop(arg, " must be one or more non-missing character strings", call. = FALSE)
    }

    if (anyNA(names) || any(!nzchar(names))) {
      stop(arg, " must contain only non-missing, non-empty names", call. = FALSE)
    }

    if (anyDuplicated(names)) {
      stop(arg, " contains duplicated names", call. = FALSE)
    }

    names
  }

  normalize_axis_labels <- function(x) {
    if (is.null(x)) {
      return(list())
    }

    if (!is.list(x)) {
      if (!is.character(x)) {
        stop(
          "axis_labels must be NULL, a named character vector, or a named list",
          call. = FALSE
        )
      }

      x <- as.list(x)
    }

    if (length(x) == 0L) {
      return(list())
    }

    if (is.null(names(x)) || anyNA(names(x)) || any(!nzchar(names(x)))) {
      stop("axis_labels must be named", call. = FALSE)
    }

    if (anyDuplicated(names(x))) {
      stop("axis_labels contains duplicated names", call. = FALSE)
    }

    allowed <- c("x", "y", "color", "linetype")
    extra <- setdiff(names(x), allowed)

    if (length(extra) > 0L) {
      stop(
        "axis_labels names must be drawn from: ",
        paste(allowed, collapse = ", "),
        call. = FALSE
      )
    }

    out <- list()
    for (nm in names(x)) {
      validate_name(x[[nm]], paste0("axis_labels$", nm))
      out[[nm]] <- x[[nm]]
    }

    out
  }

  normalize_splits <- function(x) {
    if (is.null(x)) {
      return(character(0))
    }

    if (is.list(x)) {
      x <- unlist(x, recursive = TRUE, use.names = FALSE)
    }

    if (!is.character(x)) {
      stop(
        "splits must be NULL, a character vector, or a list of character strings",
        call. = FALSE
      )
    }

    if (length(x) == 0L) {
      return(character(0))
    }

    if (anyNA(x) || any(!nzchar(x))) {
      stop("splits must contain only non-missing, non-empty names", call. = FALSE)
    }

    if (anyDuplicated(x)) {
      stop("splits contains duplicated names", call. = FALSE)
    }

    x
  }

  resolve_display_labels <- function(vars, labels, arg) {
    if (length(vars) == 0L) {
      return(stats::setNames(character(0), character(0)))
    }

    out <- stats::setNames(vars, vars)

    if (is.null(labels)) {
      return(out)
    }

    if (is.list(labels)) {
      labels <- unlist(labels, recursive = TRUE, use.names = TRUE)
    }

    if (!is.character(labels)) {
      stop(
        arg,
        " must be NULL, a character vector, or a list of character strings",
        call. = FALSE
      )
    }

    if (length(labels) == 0L) {
      return(out)
    }

    if (anyNA(labels) || any(!nzchar(labels))) {
      stop(arg, " must contain only non-missing, non-empty labels", call. = FALSE)
    }

    label_names <- names(labels)

    if (is.null(label_names)) {
      if (length(labels) != length(vars)) {
        stop(
          arg,
          " must have one label per split variable when unnamed",
          call. = FALSE
        )
      }

      out[] <- unname(labels)
      return(out)
    }

    if (anyNA(label_names) || any(!nzchar(label_names))) {
      stop(arg, " must have non-empty names when named", call. = FALSE)
    }

    if (anyDuplicated(label_names)) {
      stop(arg, " contains duplicated names", call. = FALSE)
    }

    extra <- setdiff(label_names, vars)
    if (length(extra) > 0L) {
      stop(
        arg,
        " contains unknown split names: ",
        paste(extra, collapse = ", "),
        call. = FALSE
      )
    }

    out[label_names] <- unname(labels)
    out
  }

  if (!is.data.frame(data)) {
    stop("data must be a data.frame", call. = FALSE)
  }

  validate_name(xvar, "xvar")
  yvar <- validate_names(yvar, "yvar")

  if (!is.null(zvar)) {
    validate_name(zvar, "zvar")
  }

  axis_labels <- normalize_axis_labels(axis_labels)
  splits <- normalize_splits(splits)
  split_display_labels <- resolve_display_labels(splits, stack_labels, "stack_labels")

  needed <- c(xvar, yvar, zvar, splits)
  missing_vars <- setdiff(needed, names(data))

  if (length(missing_vars) > 0L) {
    stop("Missing variables in data: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  non_numeric <- yvar[!vapply(data[yvar], is.numeric, logical(1))]
  if (length(non_numeric) > 0L) {
    stop(
      "The following yvar columns are not numeric: ",
      paste(non_numeric, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.function(fun)) {
    stop("fun must be a function", call. = FALSE)
  }

  multi_y <- length(yvar) > 1L

  x_axis_label <- if (!is.null(xlabel)) {
    xlabel
  } else if (!is.null(axis_labels[["x"]])) {
    axis_labels[["x"]]
  } else {
    xvar
  }

  y_axis_label <- if (!is.null(ylabel)) {
    ylabel
  } else if (!is.null(axis_labels[["y"]])) {
    axis_labels[["y"]]
  } else if (multi_y) {
    "Value"
  } else {
    yvar[[1L]]
  }

  color_axis_label <- if (!is.null(color_label)) {
    color_label
  } else if (!is.null(axis_labels[["color"]])) {
    axis_labels[["color"]]
  } else if (multi_y) {
    "Variable"
  } else {
    zvar
  }

  linetype_axis_label <- if (multi_y && !is.null(zvar)) {
    if (!is.null(axis_labels[["linetype"]])) {
      axis_labels[["linetype"]]
    } else {
      zvar
    }
  } else {
    NULL
  }

  if (multi_y) {
    plotdata <- data.frame(
      x = rep(data[[xvar]], times = length(yvar)),
      variable = rep(yvar, each = nrow(data)),
      value = unlist(data[yvar], use.names = FALSE),
      stringsAsFactors = FALSE
    )
  } else {
    plotdata <- data.frame(
      x = data[[xvar]],
      value = data[[yvar[[1L]]]],
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(zvar)) {
    plotdata$z <- rep(data[[zvar]], times = if (multi_y) length(yvar) else 1L)
  }

  if (length(splits) > 0L) {
    split_times <- if (multi_y) length(yvar) else 1L

    for (i in seq_along(splits)) {
      plotdata[[paste0("split", i)]] <- rep(data[[splits[[i]]]], times = split_times)
    }
  }

  group_cols <- setdiff(names(plotdata), "value")

  aggdata <- stats::aggregate(
    plotdata$value,
    by = plotdata[, group_cols, drop = FALSE],
    FUN = function(z) fun(z, na.rm = na.rm)
  )

  names(aggdata)[ncol(aggdata)] <- "value"

  if (is.factor(data[[xvar]]) || is.character(data[[xvar]])) {
    aggdata$x <- factor(aggdata$x, levels = unique(data[[xvar]]))
  }

  if (multi_y) {
    aggdata$variable <- factor(aggdata$variable, levels = yvar)
  }

  if (!is.null(zvar)) {
    aggdata$z <- factor(aggdata$z, levels = unique(data[[zvar]]))
  }

  if (length(splits) > 0L) {
    for (i in seq_along(splits)) {
      split_col <- paste0("split", i)
      split_values <- data[[splits[[i]]]]

      if (is.factor(split_values)) {
        aggdata[[split_col]] <- factor(aggdata[[split_col]], levels = levels(split_values))
      } else {
        aggdata[[split_col]] <- factor(aggdata[[split_col]], levels = unique(split_values))
      }
    }
  }

  row_theme <- ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "white"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  facet_labeller <- NULL
  if (length(splits) > 0L) {
    labeller_args <- list(
      split1 = function(x) paste0(split_display_labels[[splits[[1L]]]], ": ", x)
    )

    if (length(splits) > 1L) {
      labeller_args$split2 <- function(x) paste0(split_display_labels[[splits[[2L]]]], ": ", x)
    }

    facet_labeller <- do.call(ggplot2::labeller, labeller_args)
  }

  build_plot <- function(plot_df, plot_title) {
    if (multi_y && !is.null(zvar)) {
      p <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = x, y = value, color = variable, linetype = z, group = interaction(variable, z))
      )
    } else if (multi_y) {
      p <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = x, y = value, color = variable, group = variable)
      )
    } else if (is.null(zvar)) {
      p <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = x, y = value, group = 1)
      )
    } else {
      p <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = x, y = value, color = z, group = z)
      )
    }

    if (line) {
      p <- p + ggplot2::geom_line(linewidth = 1)
    }

    if (points) {
      p <- p + ggplot2::geom_point(size = 2)
    }

    if ("split1" %in% names(plot_df)) {
      if ("split2" %in% names(plot_df)) {
        p <- p + ggplot2::facet_grid(
          stats::as.formula("split2 ~ split1"),
          labeller = facet_labeller
        )
      } else {
        p <- p + ggplot2::facet_grid(
          stats::as.formula(". ~ split1"),
          labeller = facet_labeller
        )
      }
    }

    p <- p +
      ggplot2::labs(
        x = x_axis_label,
        y = y_axis_label,
        color = color_axis_label,
        linetype = linetype_axis_label,
        title = plot_title
      ) +
      row_theme

    p
  }

  if (length(splits) <= 2L) {
    return(build_plot(aggdata, title))
  }

  extra_cols <- paste0("split", seq.int(3L, length(splits)))
  extra_values <- unique(aggdata[, extra_cols, drop = FALSE])
  plots <- vector("list", nrow(extra_values))
  plot_names <- character(nrow(extra_values))

  for (i in seq_len(nrow(extra_values))) {
    keep <- rep(TRUE, nrow(aggdata))
    name_parts <- character(length(extra_cols))

    for (j in seq_along(extra_cols)) {
      split_col <- extra_cols[[j]]
      split_value <- extra_values[[split_col]][i]

      if (is.na(split_value)) {
        keep <- keep & is.na(aggdata[[split_col]])
      } else {
        keep <- keep & as.character(aggdata[[split_col]]) == as.character(split_value)
      }

      name_parts[[j]] <- paste0(
        split_display_labels[[splits[[j + 2L]]]],
        "=",
        as.character(split_value)
      )
    }

    plot_names[[i]] <- paste(name_parts, collapse = ", ")

    plot_title <- title
    if (is.null(plot_title)) {
      plot_title <- plot_names[[i]]
    } else {
      plot_title <- paste0(plot_title, " (", plot_names[[i]], ")")
    }

    plots[[i]] <- build_plot(aggdata[keep, , drop = FALSE], plot_title)
  }

  names(plots) <- plot_names
  plots
}
