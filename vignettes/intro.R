## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(eval = TRUE)
library(Rsimcity)

## ----example------------------------------------------------------------------
runner <- Rsimcity::Runner$new("example")
runner$params <- list(N = 100)
runner$design <- list(mu = c(0, 1, 2))

runner$step <- function(N, mu) {
  data.frame(x = stats::rnorm(N, mean = mu))
}

# Aggregate function: compute mean of x for the condition
runner$aggregate <- function(data, mu) {
  data.frame(mu = mu, mean_x = mean(data$x))
}

# Run the experiment (Rep = number of replications per condition)
res <- runner$experiment(Rep = 10, progress = FALSE)

head(res)


## ----plot-by-splits, fig.width=10, fig.height=6, fig.retina=2, out.width='100%', fig.align='center'----
plot_data <- expand.grid(
  x = c(1, 2, 3),
  model = c("A", "B", "C"),
  scenario = c("baseline", "alternative"),
  method = c("actual", "adjusted"),
  rep = 1:2,
  stringsAsFactors = FALSE
)

plot_data$estimate <- with(
  plot_data,
  x / 3 +
    ifelse(model == "B", 0.05, ifelse(model == "C", 0.10, 0)) +
    ifelse(scenario == "alternative", 0.03, 0) +
    ifelse(method == "adjusted", 0.02, 0) +
    rep / 100
)
plot_data$target <- with(plot_data, x / 3 + rep / 100)

figure <- plot_by_splits(
  data = plot_data,
  xvar = "x",
  yvar = c("estimate", "target"),
  zvar = "method",
  splits = c("model", "scenario"),
  title = c("Baseline", "Alternative"),
  titles = "top",
  xlabel = "Simulation size",
  ylabel = "Value",
  color_labels = c(estimate = "Estimate", target = "Target"),
  linetype_labels = c(actual = "Actual", adjusted = "Adjusted")
)

figure

## ----plot-by-splits-theme, fig.width=10, fig.height=6, fig.retina=2, out.width='100%', fig.align='center'----
figure +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12),
    panel.spacing.y = grid::unit(10, "pt")
  )

