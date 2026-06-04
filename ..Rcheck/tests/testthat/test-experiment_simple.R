testthat::test_that("Simple non-interactive experiment computes means across design", {
  # Reproducible
  set.seed(123)

  # Runner with 10 design levels for mu and 10 replications per cell
  runner <- Rsimcity::Runner$new("simple-test")
  runner$params <- list(N = 100)
  runner$design <- list(mu = seq(0, 9))

  runner$step <- function(N, mu) {
    data.frame(x = stats::rnorm(N, mean = mu))
  }

  runner$aggregate <- function(data, mu) {
    data.frame(mu = mu, mean_x = mean(data$x))
  }

  res <- runner$experiment(Rep = 10, progress = FALSE)

  testthat::expect_equal(nrow(res), 10)
  testthat::expect_true(all(c("mu", "mean_x") %in% names(res)))

  # With N=100 and Rep=10 the combined sample per cell is 1000, so mean should be
  # very close to the target mu. Check within 0.1
  testthat::expect_true(all(abs(res$mean_x - res$mu) < 0.1))
})
