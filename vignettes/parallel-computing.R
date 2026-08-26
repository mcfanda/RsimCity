## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(eval = TRUE)
library(Rsimcity)

## ----sequential---------------------------------------------------------------
my_step <- function(N, mu) {
  data.frame(x = rnorm(N, mean = mu))
}

runner <- Rsimcity::Runner$new("sequential example")
runner$params <- list(N = 200)
runner$design <- list(mu = c(0, 1, 2))
runner$step <- my_step
runner$aggregate <- function(data, mu) data.frame(mu = mu, mean_x = mean(data$x))

res <- runner$experiment(Rep = 5, progress = FALSE)
res

## ----parallel-----------------------------------------------------------------
runner$parallel <- TRUE
runner$par_method <- "multisession"   # or "multicore" on Linux/macOS

res <- runner$experiment(Rep = 5, progress = FALSE)
res

## ----nested-------------------------------------------------------------------
future::plan(future::multisession)

## A step that internally runs 200 trials in parallel and returns their
## average, instead of relying on Runner's own Rep for replication.
one_step <- function(mu) {
  trials <- future.apply::future_lapply(1:200, function(i) {
    mean(rnorm(50, mean = mu))
  }, future.seed = TRUE)
  data.frame(mu = mu, avg = mean(unlist(trials)))
}

runner2 <- Rsimcity::Runner$new("nested parallel example")
runner2$parallel <- FALSE          # Runner itself stays sequential across cells...
runner2$design <- list(mu = c(0, 1, 2))
runner2$step <- one_step
runner2$aggregate <- function(data, mu) data.frame(mu = mu, avg = data$avg)

res2 <- runner2$experiment(Rep = 1, progress = FALSE)   # ...one_step does its own parallel work
res2

future::plan(future::sequential)   # good practice: shut the workers down when done

