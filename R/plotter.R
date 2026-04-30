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
#'   plain text rather than as a regular expression. Default is `TRUE`.
#' @param xlabel Optional character string. Label for the x-axis. If `NULL`,
#'   `xvar` is used.
#' @param ylabel Optional character string. Label for the y-axis. If `NULL`,
#'   `pattern` is used.
#' @param title Optional character string. Plot title.
#' @param line Logical. If `TRUE`, lines are drawn. Default is `TRUE`.
#' @param points Logical. If `TRUE`, points are drawn. Default is `TRUE`.
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
                                 legend_labels=NULL,
                                 title = NULL,
                                 line = TRUE,
                                 points = TRUE,
                                 colors = NULL,
                                 linetypes = NULL) {

  if (!is.data.frame(data))
    stop("data must be a data.frame")

  if (!xvar %in% names(data))
    stop("xvar not found in data: ", xvar)
  if (!is.null(pattern)) {
      matched <- grep(pattern, names(data), value = TRUE, fixed = fixed)

      if (length(matched) == 0)
        stop("No columns matched pattern: ", pattern)

        non_numeric <- matched[!vapply(data[matched], is.numeric, logical(1))]

        if (length(non_numeric) > 0) {
          stop(
            "The following matched columns are not numeric: ",
            paste(non_numeric, collapse = ", ")
          )
        }
  } else {
    if (is.null(varsname)) stop("Please specify the columns to plot either with `pattern` or with `varsname`")
    matched <- varsname
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
        ggplot2::scale_linetype_manual(values = linetypes)
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

    p <- p + ggplot2::scale_color_manual(values = colors)
  }

  p <- p +
    ggplot2::labs(
      x = if (is.null(xlabel)) xvar else xlabel,
      y = if (is.null(ylabel)) pattern else ylabel,
      color = "Variable",
      linetype = "Variable",
      title = title
    )

  if (!is.null(legend_labels)) {
    if (length(legend_labels) < length(matched)) {
      stop(
        "legend_labels has length ", length(legend_labels),
        " but there are ", length(matched), " matched columns"
      )
    }

    legend_labels <- legend_labels[seq_along(matched)]
    names(legend_labels) <- matched
  }
  p <- p +  ggplot2::theme_classic()

  return(p)
}
