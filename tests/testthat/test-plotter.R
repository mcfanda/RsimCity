testthat::test_that("plot_by_splits aggregates a single line", {
  data <- data.frame(
    x = c(1, 1, 2, 2),
    y = c(2, 4, 6, 8)
  )

  p <- Rsimcity::plot_by_splits(data, xvar = "x", yvar = "y")

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_equal(p$data$x, c(1, 2))
  testthat::expect_equal(p$data$value, c(3, 7))
  testthat::expect_equal(p$theme$legend.position, "bottom")
  testthat::expect_true(inherits(p$theme$panel.grid.minor, "element_blank"))
})


extract_strip_labels <- function(plot, prefix) {
  grob <- ggplot2::ggplotGrob(plot)
  idx <- grep(prefix, grob$layout$name)

  vapply(
    idx,
    function(i) grob$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$label,
    character(1)
  )
}


testthat::test_that("plot_by_splits shows the split variable name in strip labels", {
  data <- expand.grid(
    x = c(1, 2),
    a = c("A1", "A2"),
    rep = 1:2,
    stringsAsFactors = FALSE
  )

  data$y <- with(data, x + ifelse(a == "A2", 10, 0) + rep)

  p <- Rsimcity::plot_by_splits(
    data,
    xvar = "x",
    yvar = "y",
    splits = "a"
  )

  labels <- extract_strip_labels(p, "^strip-t")

  testthat::expect_equal(labels, c("a: A1", "a: A2"))
})



testthat::test_that("plot_by_splits accepts custom axis and stack labels", {
  data <- expand.grid(
    x = c(1, 2),
    a = c("A1", "A2"),
    rep = 1:2,
    stringsAsFactors = FALSE
  )

  data$y <- with(data, x + ifelse(a == "A2", 10, 0) + rep)

  p <- Rsimcity::plot_by_splits(
    data,
    xvar = "x",
    yvar = "y",
    splits = "a",
    axis_labels = list(x = "Sample size", y = "Outcome"),
    stack_labels = c(a = "Scenario")
  )

  testthat::expect_equal(p$labels$x, "Sample size")
  testthat::expect_equal(p$labels$y, "Outcome")
  testthat::expect_equal(extract_strip_labels(p, "^strip-t"), c("Scenario: A1", "Scenario: A2"))
})


testthat::test_that("plot_by_splits plots one line per y variable", {
  data <- data.frame(
    x = c(1, 1, 2, 2),
    y1 = c(2, 4, 6, 8),
    y2 = c(10, 14, 18, 22)
  )

  p <- Rsimcity::plot_by_splits(data, xvar = "x", yvar = c("y1", "y2"))
  lookup <- stats::setNames(
    p$data$value,
    paste(as.character(p$data$variable), as.character(p$data$x), sep = ":")
  )

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_equal(levels(p$data$variable), c("y1", "y2"))
  testthat::expect_equal(
    unname(lookup[c("y1:1", "y1:2", "y2:1", "y2:2")]),
    c(3, 7, 12, 20)
  )
})


testthat::test_that("plot_by_splits combines y variables and z groups", {
  data <- expand.grid(
    x = c(1, 2),
    z = c("m1", "m2"),
    rep = 1:2,
    stringsAsFactors = FALSE
  )

  data$y1 <- with(data, x + ifelse(z == "m2", 10, 0) + rep)
  data$y2 <- with(data, 2 * x + ifelse(z == "m2", 20, 0) + rep)

  p <- Rsimcity::plot_by_splits(
    data,
    xvar = "x",
    yvar = c("y1", "y2"),
    zvar = "z"
  )

  built <- ggplot2::ggplot_build(p)

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_equal(levels(p$data$variable), c("y1", "y2"))
  testthat::expect_equal(levels(p$data$z), c("m1", "m2"))
  testthat::expect_equal(length(unique(built$data[[1]]$group)), 4L)
})


testthat::test_that("plot_by_splits facets on the first two split variables", {
  data <- expand.grid(
    x = c(1, 2),
    z = c("m1", "m2"),
    a = c("A1", "A2"),
    b = c("B1", "B2"),
    rep = 1:2,
    stringsAsFactors = FALSE
  )

  data$y <- with(
    data,
    x + ifelse(z == "m2", 10, 0) + ifelse(a == "A2", 1, 0) + ifelse(b == "B2", 2, 0) + rep
  )

  p <- Rsimcity::plot_by_splits(
    data,
    xvar = "x",
    yvar = "y",
    zvar = "z",
    splits = c("a", "b")
  )

  layout <- ggplot2::ggplot_build(p)$layout$layout

  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_true(inherits(p$facet, "FacetGrid"))
  testthat::expect_equal(nrow(layout), 4L)
  testthat::expect_equal(levels(p$data$split1), c("A1", "A2"))
  testthat::expect_equal(levels(p$data$split2), c("B1", "B2"))
  testthat::expect_equal(levels(p$data$z), c("m1", "m2"))
  testthat::expect_equal(p$theme$strip.background$fill, "white")
  testthat::expect_equal(extract_strip_labels(p, "^strip-t")[1], "a: A1")
})


testthat::test_that("plot_by_splits returns one plot per extra split combination", {
  data <- expand.grid(
    x = c(1, 2),
    z = c("m1", "m2"),
    a = c("A1", "A2"),
    b = "B1",
    c = c("C1", "C2"),
    rep = 1:2,
    stringsAsFactors = FALSE
  )

  data$y <- seq_len(nrow(data))

  plots <- Rsimcity::plot_by_splits(
    data,
    xvar = "x",
    yvar = "y",
    zvar = "z",
    splits = list("a", "b", "c")
  )

  testthat::expect_type(plots, "list")
  testthat::expect_length(plots, 2L)
  testthat::expect_named(plots, c("c=C1", "c=C2"))
  testthat::expect_true(all(vapply(plots, inherits, logical(1), what = "ggplot")))
  testthat::expect_equal(unique(as.character(plots[[1]]$data$split3)), "C1")
  testthat::expect_equal(unique(as.character(plots[[2]]$data$split3)), "C2")
})


testthat::test_that("plot_by_splits uses custom stack labels in plot names", {
  data <- expand.grid(
    x = c(1, 2),
    z = c("m1", "m2"),
    a = c("A1", "A2"),
    b = "B1",
    c = c("C1", "C2"),
    rep = 1:2,
    stringsAsFactors = FALSE
  )

  data$y <- seq_len(nrow(data))

  plots <- Rsimcity::plot_by_splits(
    data,
    xvar = "x",
    yvar = "y",
    zvar = "z",
    splits = list("a", "b", "c"),
    stack_labels = c(c = "Scenario")
  )

  testthat::expect_named(plots, c("Scenario=C1", "Scenario=C2"))
})
