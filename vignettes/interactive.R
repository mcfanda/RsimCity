## ----callback-signatures-intro, eval=FALSE------------------------------------
# plot_fun(data, step)
# agg_plot_fun(agg_x, step)

## ----setup, include=FALSE-----------------------------------------------------
interactive_deps <- all(vapply(
  c("later", "shiny"),
  requireNamespace,
  logical(1),
  quietly = TRUE
))

knitr::opts_chunk$set(
  eval = interactive_deps,
  echo = TRUE,
  warning = FALSE,
  message = FALSE
)

## ----define-runner------------------------------------------------------------
runner <- Rsimcity::Runner$new("interactive-demo")
runner$params <- list(N = 80)
runner$design <- list(mu = c(-1, 0, 1))

runner$step <- function(N, mu) {
  data.frame(value = stats::rnorm(N, mean = mu))
}

runner$aggregate <- function(data, mu) {
  data.frame(mu = mu, mean_value = mean(data$value))
}

## ----define-plots-------------------------------------------------------------
live_plot <- function(data, step) {
  ggplot2::ggplot(data, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = 25, fill = "#2C7FB8", colour = "white") +
    ggplot2::labs(
      title = sprintf("Current condition, update %d", step),
      x = "Simulated value",
      y = "Count"
    ) +
    ggplot2::theme_minimal()
}

aggregate_plot <- function(data, step) {
  ggplot2::ggplot(data, ggplot2::aes(x = mu, y = mean_value)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70") +
    ggplot2::geom_line(colour = "#D95F0E") +
    ggplot2::geom_point(size = 2, colour = "#D95F0E") +
    ggplot2::labs(
      title = "Completed conditions",
      subtitle = sprintf("Latest update: %d", step),
      x = "Population mean",
      y = "Observed mean"
    ) +
    ggplot2::theme_minimal()
}

## ----create-monitor-----------------------------------------------------------
monitor <- Rsimcity::Interactive$new(
  plot_fun = live_plot,
  agg_plot_fun = aggregate_plot,
  title = "Interactive simulation demo",
  plot_refresh_ms = 250,
  plot_height = "600px",
  max_rows = 2000
)

monitor$set_start_fun(function(obj) {
  runner$experiment_interactive(
    interactive = obj,
    Rep = 20,
    delay = 0.05,
    reset = FALSE
  )
})

## ----run-monitor, eval=FALSE--------------------------------------------------
# monitor$run(viewer = "pane")

## ----callback-signatures, eval=FALSE------------------------------------------
# live_plot(data, step)
# aggregate_plot(agg_x, step)

## ----live-only, eval=FALSE----------------------------------------------------
# live_monitor <- Rsimcity::Interactive$new(plot_fun = live_plot)
# live_monitor$set_start_fun(function(obj) {
#   runner$experiment_interactive(interactive = obj, Rep = 20)
# })
# live_monitor$run(viewer = "pane")

