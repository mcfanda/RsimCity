testthat::test_that("simulate_sample_from_corr returns correct dimensions and statistics", {
  R <- matrix(c(1, 0.5,
                0.5, 1), nrow = 2, byrow = TRUE)
  N <- 1000

  dat <- Rsimcity::simulate_sample_from_corr(N = N, R = R, mu = c(0, 2), sd = c(1, 3), seed = 123)

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

# only the first predictor has a true effect; the rest are rho-correlated nuisance predictors
focal_beta <- function(model, k, yl = 3) {
  if (model == "multinomial") {
    B <- matrix(0, nrow = k, ncol = yl - 1)
    B[1, ] <- 1
    B
  } else {
    b <- rep(0, k)
    b[1] <- 1
    b
  }
}
equicorr <- function(k, rho) { S <- matrix(rho, k, k); diag(S) <- 1; S }

testthat::test_that("sample_by_eta2 returns the requested calibrated eta-squared", {
  dat <- Rsimcity::sample_by_eta2(
    n = 100,
    eta2 = 0.1,
    model = "logistic",
    k = 3,
    beta = focal_beta("logistic", 3),
    x_cov = equicorr(3, 0.30),
    order = 5,
    seed = 123
  )

  testthat::expect_equal(attr(dat, "quadrature_eta2"), 0.1, tolerance = 1e-5)
})


testthat::test_that("categorical models support arbitrary outcome levels and predictor counts", {
  for (model in c("multinomial", "ordinal")) {
    for (k in c(2, 4)) {
      dat <- Rsimcity::sample_by_eta2(
        eta2 = 0.1,
        n = 250,
        model = model,
        k = k,
        beta = focal_beta(model, k, yl = 4),
        x_cov = equicorr(k, 0.30),
        order = 5,
        yl = 4,
        seed = 100 + k
      )

      testthat::expect_equal(ncol(dat), k + 1L)
      testthat::expect_equal(sort(unique(as.integer(dat$y))), seq_len(4))
    }
  }
})

testthat::test_that("categorical levels can be inferred from p_target", {
  dat <- Rsimcity::sample_by_eta2(
    eta2 = 0.1,
    n = 250,
    model = "ordinal",
    k = 2,
    beta = focal_beta("ordinal", 2),
    x_cov = equicorr(2, 0.30),
    order = 5,
    p_target = c(0.1, 0.2, 0.3, 0.4),
    seed = 42
  )

  testthat::expect_equal(sort(unique(as.integer(dat$y))), seq_len(4))
  testthat::expect_error(
    Rsimcity::sample_by_eta2(
      eta2 = 0.1, n = 20, model = "multinomial", yl = 4,
      p_target = rep(1/3, 3)
    ),
    "p_target must contain yl"
  )
})
