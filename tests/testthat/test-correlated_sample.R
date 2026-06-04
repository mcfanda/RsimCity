testthat::test_that("correlated_sample returns correct dimensions and statistics", {
  R <- matrix(c(1, 0.5,
                0.5, 1), nrow = 2, byrow = TRUE)
  N <- 1000

  dat <- Rsimcity::correlated_sample(N = N, R = R, mu = c(0, 2), sd = c(1, 3), seed = 123)

  testthat::expect_equal(nrow(dat), N)
  testthat::expect_equal(ncol(dat), 2)
  testthat::expect_named(dat, c("x1", "x2"))

  # Check means and standard deviations are close to requested values
  testthat::expect_equal(mean(dat$x1), 0, tolerance = 0.1)
  testthat::expect_equal(mean(dat$x2), 2, tolerance = 0.1)

  testthat::expect_equal(sd(dat$x1), 1, tolerance = 0.1)
  testthat::expect_equal(sd(dat$x2), 3, tolerance = 0.1)

  # Check correlation approximately matches the target
  testthat::expect_equal(cor(dat$x1, dat$x2), 0.5, tolerance = 0.1)
})
