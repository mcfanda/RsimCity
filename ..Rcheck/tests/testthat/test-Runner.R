testthat::test_that("experiment_interactive() stores error in info$error and pauses when step args don't match params", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("later")

  runner <- Runner$new("error-test")
  runner$params <- list(N = 1)

  # No formal argument matches params$N, so .one_step() throws
  # "no argument can be passed to fun()" before its inner tryCatch.
  # That error escapes up to experiment_interactive()'s outer tryCatch,
  # which writes to interactive$info$error and calls interactive$pause().
  runner$step <- function(no_match) stats::rnorm(no_match)

  mon <- Interactive$new(plot_fun = \(data, step) NULL)

  runner$experiment_interactive(interactive = mon, Rep = 1, delay = 0, reset = FALSE)
  later::run_now()

  testthat::expect_match(mon$info$error, "no argument")
  testthat::expect_true(mon$is_paused())
  testthat::expect_false(mon$is_running())
})
