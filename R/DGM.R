#' Generate a correlated sample
#'
#' @description
#' Generate a data frame containing `N` observations from a multivariate normal
#' distribution with a specified correlation matrix, means, and standard
#' deviations.
#'
#' @param R Numeric matrix. A positive definite correlation matrix. The number of
#'   rows and columns defines the number of generated variables.
#' @param N Integer. Number of observations to generate.'
#' @param mu Numeric vector. Means of the generated variables. If a single value
#'   is supplied, it is recycled across variables. Default is `0`.
#' @param sd Numeric vector. Standard deviations of the generated variables. If a
#'   single value is supplied, it is recycled across variables. Default is `1`.
#' @param seed Optional integer. Random seed used for reproducible sampling.
#'   Default is `NULL`.
#'
#' @return A data frame with `N` rows and `K` columns, where `K = nrow(R)`.
#'   Variables are named `x1`, `x2`, ..., `xK`.
#'
#' @details
#' The function first generates independent standard normal variables and then
#' applies the Cholesky decomposition of `R` to induce the requested correlation
#' structure. The variables are then rescaled using `sd` and shifted using `mu`.
#'
#' `R` must be a square, symmetric, positive definite matrix.
#'
#' @examples
#' R <- matrix(
#'   c(1.0, 0.5,
#'     0.5, 1.0),
#'   nrow = 2,
#'   byrow = TRUE
#' )
#'
#' dat <- simulate_sample_from_corr( R = R,N = 100, mu = c(0, 2), sd = c(1, 3))
#' head(dat)
#' cor(dat)
#'
#' @importFrom stats rnorm
#' @export
simulate_sample_from_corr <- function(R, N, mu = 0, sd = 1, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)
  K <- nrow(R)
  Sigma <- R

  if (!is.matrix(Sigma)) stop("R must be either a scalar or a correlation matrix")
  if (nrow(Sigma) != ncol(Sigma)) stop("R matrix must be K x K")
  if (!isTRUE(all.equal(Sigma, t(Sigma)))) stop("R must be symmetric")
  if (any(eigen(Sigma, symmetric = TRUE)$values <= 0)) stop("R must be positive definite")

  mu <- rep(mu, length.out = K)
  sd <- rep(sd, length.out = K)

  Z <- matrix(stats::rnorm(N * K), nrow = N, ncol = K)
  X <- Z %*% chol(Sigma)

  X <- sweep(X, 2, sd, FUN = "*")
  X <- sweep(X, 2, mu, FUN = "+")

  colnames(X) <- paste0("x", seq_len(K))
  as.data.frame(X)
}



## Generalized linear models given eta-squared ###

#' Simulate a sample with a known population eta-squared for a GLM
#'
#' @description
#' Draws a sample of `N` cases from a population of `k` correlated normal
#' predictors in which the first predictor has an exactly known population
#' eta-squared, for a logistic, probit, multinomial, ordinal (proportional
#' odds), or gaussian (classical linear) model.
#'
#' Unlike simulating a very large population and searching for coefficients
#' that approximately match a target effect size, the target population
#' eta-squared here is computed exactly -- via closed-form covariance algebra
#' for the gaussian model, and via Gauss-Hermite quadrature for the others --
#' and the model coefficient that produces it is found analytically or by
#' root-finding, before any random sample is drawn.
#'
#' @param target_eta2 Numeric. Desired population eta-squared of the first
#'   predictor, strictly between 0 and 1.
#' @param N Integer. Number of cases to draw.
#' @param model Character. One of `"logistic"`, `"probit"`, `"multinomial"`,
#'   `"ordinal"`, or `"gaussian"`.
#' @param k Integer. Number of correlated predictors to generate; only the
#'   first one carries an effect on the outcome. Default is `3`.
#' @param rho Numeric. Common pairwise correlation among the `k` predictors.
#'   Default is `0.30`.
#' @param n_nodes Integer. Number of Gauss-Hermite quadrature nodes per
#'   dimension used to compute the exact population values. Ignored for
#'   `"gaussian"`, which uses a closed-form calibration instead. Default is
#'   `20`.
#' @param p_target Optional numeric vector. For `"multinomial"` and
#'   `"ordinal"` models, the target marginal distribution of the outcome.
#'   Its length must equal `n_levels` and its positive elements must sum to one.
#'   When `NULL`, all levels are equally likely. If `n_levels` is omitted and
#'   `p_target` is supplied, the number of levels is inferred from its length.
#' @param seed Optional integer. Random seed used for reproducible sampling.
#'   Default is `NULL`.
#' @param n_levels Integer. Number of outcome levels for `"multinomial"` and
#'   `"ordinal"` models. Must be at least 2. Default is `3`. Ignored for the
#'   other models.
#'
#' @return A data frame with `N` rows and columns `x1`, ..., `xk` (the
#'   predictors) and `y` (the outcome). `y` is coded `0`/`1` for `"logistic"`
#'   and `"probit"`, integers from `1` through `n_levels` for `"multinomial"`
#'   and `"ordinal"`, and is continuous for `"gaussian"`.
#'
#' @details
#' Because every model considered here reaches the outcome only through one
#' or two linear predictors, and the predictors are assumed jointly normal,
#' the population quantities needed to compute eta-squared (average predicted
#' probability, average model entropy) reduce to low-dimensional integrals
#' against normal densities, regardless of `k`. For `"logistic"`, `"probit"`,
#' `"multinomial"`, and `"ordinal"` these are evaluated by Gauss-Hermite
#' quadrature. For `"gaussian"`, the same quantities have an exact closed
#' form in terms of the predictor covariance matrix, requiring no numerical
#' integration at all; this model is included mainly as a classical-GLM
#' benchmark against which the quadrature-based models can be compared.
#'
#' One subtlety is handled explicitly for every model: even though only the
#' first predictor has a true effect, the remaining `k - 1` predictors are
#' correlated with it, so the model that omits the first predictor does not
#' have a population-optimal coefficient of zero on them -- it partly
#' recovers the omitted predictor's effect through the correlation. For
#' `"logistic"`, `"probit"`, `"multinomial"`, and `"ordinal"`, the calibration
#' fits this reduced model's actual population-optimal coefficients (by
#' minimizing its expected deviance over the assumed predictor distribution)
#' rather than assuming they coincide with the null model. For `"gaussian"`,
#' the same correction is available in closed form, via the population
#' regression of the first predictor on the others.
#'
#' @examples
#' # a sample of 200 cases from a logistic model in which the first predictor
#' # accounts for 10% of the population variance (eta-squared = .10)
#' dat <- simulate_sample_from_eta(target_eta2 = 0.10, N = 200, model = "logistic")
#' head(dat)
#'
#' # the same population effect size, for an ordinal (proportional-odds)
#' # outcome with three equally likely categories
#' dat_ord <- simulate_sample_from_eta(target_eta2 = 0.10, N = 200, model = "ordinal")
#' head(dat_ord)
#'
#' # a four-level multinomial outcome
#' dat_multi4 <- simulate_sample_from_eta(
#'   target_eta2 = 0.10, N = 200, model = "multinomial", n_levels = 4
#' )
#' table(dat_multi4$y)
#'
#' # the classical linear model benchmark
#' dat_gauss <- simulate_sample_from_eta(target_eta2 = 0.10, N = 200, model = "gaussian")
#' head(dat_gauss)
#'
#' @importFrom stats rnorm plogis rbinom binomial glm uniroot optim fitted
#' @export
simulate_sample_from_eta <- function(target_eta2, N,
                                 model = c("logistic", "probit", "multinomial", "ordinal", "gaussian"),
                                 k = 3, rho = 0, n_nodes = 20,
                                 p_target = NULL,
                                 seed = NULL,
                                 n_levels = 3) {

  model <- match.arg(model)
  if (!is.null(seed)) set.seed(seed)
  if (target_eta2 <= 0 || target_eta2 >= 1)
    stop("target_eta2 must be strictly between 0 and 1")

  categorical <- model %in% c("multinomial", "ordinal")
  if (categorical) {
    if (missing(n_levels) && !is.null(p_target)) {
      n_levels <- length(p_target)
    }
    if (length(n_levels) != 1L || !is.numeric(n_levels) ||
        !is.finite(n_levels) || n_levels < 2 || n_levels != as.integer(n_levels)) {
      stop("n_levels must be a single integer greater than or equal to 2")
    }
    n_levels <- as.integer(n_levels)

    if (is.null(p_target)) {
      p_target <- rep(1 / n_levels, n_levels)
    } else if (!is.numeric(p_target) || length(p_target) != n_levels ||
               any(!is.finite(p_target)) || any(p_target <= 0) ||
               abs(sum(p_target) - 1) > sqrt(.Machine$double.eps)) {
      stop("p_target must contain n_levels positive probabilities that sum to 1")
    }
  }

  Sigma <- matrix(rho, k, k); diag(Sigma) <- 1
  U     <- chol(Sigma)
  X     <- matrix(stats::rnorm(N * k), N, k) %*% U

  if (model == "gaussian") {
    cal <- .glm_calibrate_gaussian(target_eta2, k, rho)
    Y <- cal$b1 * X[, 1] + stats::rnorm(N, sd = cal$sigma)

  } else if (model %in% c("logistic", "probit")) {
    grid <- .glm_build_grid(k, n_nodes, Sigma)
    fam <- switch(model, logistic = stats::binomial(), probit = stats::binomial(link = "probit"))
    t_star <- .glm_calibrate_t(target_eta2, grid, fam)
    Y <- stats::rbinom(N, 1, fam$linkinv(X[, 1] * t_star))

  } else if (model == "multinomial") {
    grid <- .glm_build_grid(k, n_nodes, Sigma)
    t_star <- stats::uniroot(function(t) .glm_eta2_multinom(t, grid, p_target) - target_eta2,
                              c(1e-4, 10), tol = 1e-6)$root
    alpha <- .glm_solve_intercepts(t_star, grid, p_target)
    eta <- matrix(t_star * X[, 1], nrow = N, ncol = n_levels - 1)
    eta <- sweep(eta, 2, alpha, FUN = "+")
    P <- .glm_multinom_probs(eta)
    Y <- apply(P, 1, function(p) sample.int(n_levels, 1, prob = p))

  } else { # ordinal
    grid <- .glm_build_grid(k, n_nodes, Sigma)
    cum_target <- cumsum(p_target)[seq_len(n_levels - 1)]
    t_star <- stats::uniroot(function(t) .glm_eta2_ordinal(t, grid, cum_target) - target_eta2,
                              c(1e-4, 10), tol = 1e-6)$root
    thresholds <- vapply(
      cum_target,
      .glm_solve_threshold,
      numeric(1),
      eta = t_star * grid$X[, 1],
      w = grid$w
    )
    P <- .glm_ordinal_probs(t_star * X[, 1], thresholds)
    Y <- apply(P, 1, function(p) sample.int(n_levels, 1, prob = p))
  }

  colnames(X) <- paste0("x", seq_len(k))
  data.frame(X, y = Y)
}

#' Simulate a sample with a known population R-squared for a GLM
#'
#' @description
#' Draws a sample of `N` cases from a population of `k` correlated normal
#' predictors in which the first predictor has an exactly known effect,
#' chosen so that the *full* model (all `k` predictors) has the requested
#' population R-squared, for a logistic, probit, multinomial, ordinal
#' (proportional odds), or gaussian (classical linear) model.
#'
#' @param target_r2 Numeric. Desired population R-squared of the full model,
#'   strictly between 0 and 1.
#' @inheritParams simulate_sample_from_eta
#'
#' @return A data frame with `N` rows and columns `x1`, ..., `xk` (the
#'   predictors) and `y` (the outcome), exactly as in
#'   [simulate_sample_from_eta()].
#'
#' @details
#' As in [simulate_sample_from_eta()], only the first predictor carries a
#' true effect; the remaining `k - 1` predictors are correlated nuisance
#' predictors with a true coefficient of zero. The two functions differ in
#' what population quantity is targeted and, as a consequence, in how the
#' calibration is computed.
#'
#' [simulate_sample_from_eta()] targets the eta-squared of the first
#' predictor within the full model, which requires comparing the full model
#' against a *reduced* model omitting the first predictor; because the
#' omitted predictor is correlated with the ones left in, that reduced
#' model's population-optimal fit must itself be found by numerical
#' optimization, and it depends on `k` and `rho`.
#'
#' Here, the R-squared of the *full* model is targeted directly. Since the
#' full model is correctly specified (it includes every predictor that
#' actually affects the outcome), its population-optimal fit places a
#' coefficient of exactly zero on the nuisance predictors regardless of `k`
#' or `rho`, and the first predictor is always marginally standard normal.
#' The full-model deviance therefore depends on the coefficient of the first
#' predictor alone and no reduced-model fit is needed: the calibration
#' reduces to a one-dimensional Gauss-Hermite quadrature over the first
#' predictor's marginal distribution, exact for any `k` (including `k = 1`)
#' and any `rho`.
#'
#' @examples
#' dat <- simulate_sample_from_r2(target_r2 = 0.10, N = 200, model = "logistic")
#' head(dat)
#'
#' dat_ord <- simulate_sample_from_r2(target_r2 = 0.10, N = 200, k = 1, model = "ordinal")
#' head(dat_ord)
#'
#' @importFrom stats rnorm plogis rbinom binomial glm uniroot optim fitted
#' @export
simulate_sample_from_r2 <- function(target_r2, N,
                                model = c("logistic", "probit", "multinomial", "ordinal", "gaussian"),
                                k = 3, rho = 0, n_nodes = 20,
                                p_target = NULL,
                                seed = NULL,
                                n_levels = 3) {

  model <- match.arg(model)
  if (!is.null(seed)) set.seed(seed)
  if (target_r2 <= 0 || target_r2 >= 1)
    stop("target_r2 must be strictly between 0 and 1")

  categorical <- model %in% c("multinomial", "ordinal")
  if (categorical) {
    if (missing(n_levels) && !is.null(p_target)) {
      n_levels <- length(p_target)
    }
    if (length(n_levels) != 1L || !is.numeric(n_levels) ||
        !is.finite(n_levels) || n_levels < 2 || n_levels != as.integer(n_levels)) {
      stop("n_levels must be a single integer greater than or equal to 2")
    }
    n_levels <- as.integer(n_levels)

    if (is.null(p_target)) {
      p_target <- rep(1 / n_levels, n_levels)
    } else if (!is.numeric(p_target) || length(p_target) != n_levels ||
               any(!is.finite(p_target)) || any(p_target <= 0) ||
               abs(sum(p_target) - 1) > sqrt(.Machine$double.eps)) {
      stop("p_target must contain n_levels positive probabilities that sum to 1")
    }
  }

  Sigma <- matrix(rho, k, k); diag(Sigma) <- 1
  U     <- chol(Sigma)
  X     <- matrix(stats::rnorm(N * k), N, k) %*% U

  if (model == "gaussian") {
    cal <- .glm_calibrate_gaussian_r2(target_r2)
    Y <- cal$b1 * X[, 1] + stats::rnorm(N, sd = cal$sigma)

  } else if (model %in% c("logistic", "probit")) {
    grid1d <- .glm_build_grid_1d(n_nodes)
    fam <- switch(model, logistic = stats::binomial(), probit = stats::binomial(link = "probit"))
    t_star <- .glm_calibrate_r2_t(target_r2, grid1d, fam)
    Y <- stats::rbinom(N, 1, fam$linkinv(X[, 1] * t_star))

  } else if (model == "multinomial") {
    grid1d <- .glm_build_grid_1d(n_nodes)
    t_star <- stats::uniroot(function(t) .glm_R2_multinom_of_t(t, grid1d, p_target) - target_r2,
                              c(1e-4, 10), tol = 1e-6)$root
    alpha <- .glm_solve_intercepts(t_star, grid1d, p_target)
    eta <- matrix(t_star * X[, 1], nrow = N, ncol = n_levels - 1)
    eta <- sweep(eta, 2, alpha, FUN = "+")
    P <- .glm_multinom_probs(eta)
    Y <- apply(P, 1, function(p) sample.int(n_levels, 1, prob = p))

  } else { # ordinal
    grid1d <- .glm_build_grid_1d(n_nodes)
    cum_target <- cumsum(p_target)[seq_len(n_levels - 1)]
    t_star <- stats::uniroot(function(t) .glm_R2_ordinal_of_t(t, grid1d, cum_target) - target_r2,
                              c(1e-4, 10), tol = 1e-6)$root
    thresholds <- vapply(
      cum_target,
      .glm_solve_threshold,
      numeric(1),
      eta = t_star * grid1d$X[, 1],
      w = grid1d$w
    )
    P <- .glm_ordinal_probs(t_star * X[, 1], thresholds)
    Y <- apply(P, 1, function(p) sample.int(n_levels, 1, prob = p))
  }

  colnames(X) <- paste0("x", seq_len(k))
  data.frame(X, y = Y)
}

### end of exported function; internal calibration machinery below ###

# Gauss-Hermite nodes and weights for computing E[f(Z)] for a standard normal Z.
# Returns a list with `nodes` (the z_i) and `weights` (the w_i, summing to 1), so that
# E[f(Z)] = sum(weights * f(nodes)).
.glm_gauss_hermite_normal <- function(n) {
  i   <- seq_len(n - 1)
  off <- sqrt(i / 2)
  J <- matrix(0, n, n)
  J[cbind(1:(n - 1), 2:n)] <- off
  J[cbind(2:n, 1:(n - 1))] <- off
  ee <- eigen(J, symmetric = TRUE)
  x_phys <- ee$values
  w_phys <- (ee$vectors[1, ])^2 * sqrt(pi)
  ord <- order(x_phys)
  list(nodes   = sqrt(2) * x_phys[ord],
       weights = w_phys[ord] / sqrt(pi))
}

# Builds a quadrature grid for k correlated normal predictors with covariance Sigma, by
# taking the tensor product of k independent 1-D Gauss-Hermite grids and transforming it
# through the Cholesky factor of Sigma. Returns a list with `X` (grid of predictor values)
# and `w` (matching weights, summing to 1).
.glm_build_grid <- function(k, n_nodes, Sigma) {
  gh  <- .glm_gauss_hermite_normal(n_nodes)
  idx <- as.matrix(do.call(expand.grid, rep(list(seq_len(n_nodes)), k)))
  Z   <- matrix(gh$nodes[idx], ncol = k)
  W   <- apply(matrix(gh$weights[idx], ncol = k), 1, prod)
  U   <- chol(Sigma)                # U'U = Sigma
  list(X = Z %*% U, w = W)
}

# A one-dimensional quadrature grid for the first predictor's own marginal distribution
# (standard normal, regardless of k or rho), in the same list(X, w) shape as
# .glm_build_grid. Used for R-squared calibration, where only the first predictor's
# marginal ever enters the calculation, so the full k-dimensional tensor grid -- and its
# n_nodes^k cost -- is unnecessary.
.glm_build_grid_1d <- function(n_nodes) {
  gh <- .glm_gauss_hermite_normal(n_nodes)
  list(X = matrix(gh$nodes, ncol = 1), w = gh$weights)
}

# Closed-form calibration for the gaussian (classical linear) model: finds the
# coefficient on the first predictor (with residual SD fixed to 1) that produces
# the target population eta2, accounting in closed form for the fact that the
# other k-1 predictors, though truly unrelated to y, are correlated with the
# first predictor and so pick up part of its effect when it is dropped from the
# model (the population regression of the first predictor on the others).
# Returns a list with `b1` (focal coefficient) and `sigma` (residual SD).
.glm_calibrate_gaussian <- function(target_eta2, k, rho) {
  if (k == 1) {
    C <- 1
  } else {
    Sigma_Z   <- matrix(rho, k - 1, k - 1); diag(Sigma_Z) <- 1
    Sigma_Zx1 <- rep(rho, k - 1)
    C <- 1 - as.numeric(t(Sigma_Zx1) %*% solve(Sigma_Z) %*% Sigma_Zx1)
  }
  R2_full <- target_eta2 / C
  if (R2_full <= 0 || R2_full >= 1)
    stop("target_eta2 is not achievable with this combination of k and rho")
  list(b1 = sqrt(R2_full / (1 - R2_full)), sigma = 1)
}

# Closed-form calibration for the gaussian model's FULL-model R-squared: since only the
# first predictor has a true effect and is marginally standard normal, the population R2
# of the full k-predictor model is simply b1^2/(b1^2+sigma^2), independent of k and rho --
# unlike eta2, no correlation-based adjustment is needed because R2 is a property of the
# full (correctly specified) model, not of a reduced one.
.glm_calibrate_gaussian_r2 <- function(target_r2) {
  list(b1 = sqrt(target_r2 / (1 - target_r2)), sigma = 1)
}

# Entropy of a binary outcome with success probability p.
.glm_H_bin <- function(p) ifelse(p <= 0 | p >= 1, 0, -(p * log(p) + (1 - p) * log(1 - p)))

# Cross-entropy between a true binary probability and a model's fitted probability.
.glm_CE_bin <- function(p_true, p_model) {
  ok  <- p_model > 0 & p_model < 1
  out <- numeric(length(p_true))
  out[ok] <- -(p_true[ok] * log(p_model[ok]) + (1 - p_true[ok]) * log(1 - p_model[ok]))
  out
}

# Population eta2 of the first predictor (binomial family), for a candidate coefficient t:
# computes the full model's probability at the grid points, fits the reduced model's true
# population-optimal coefficients by weighted regression on the grid (using the population
# probability as a fractional response), and returns (Dmx - Dm) / D0.
.glm_eta2_of_t <- function(t, grid, fam) {
  p_true <- fam$linkinv(grid$X[, 1] * t)
  D0 <- 2 * .glm_H_bin(sum(grid$w * p_true))
  Dm <- 2 * sum(grid$w * .glm_H_bin(p_true))

  others <- as.data.frame(grid$X[, -1, drop = FALSE])
  others$p_true <- p_true
  fit_reduced <- suppressWarnings(               # reduced model, fitted to the population
    stats::glm(p_true ~ ., data = others, family = fam, weights = grid$w * 1e8)
  )
  Dmx <- 2 * sum(grid$w * .glm_CE_bin(p_true, stats::fitted(fit_reduced)))

  (Dmx - Dm) / D0
}

# Finds the coefficient t on the first predictor (binomial family) that produces the
# target population eta2, by root-finding on .glm_eta2_of_t.
.glm_calibrate_t <- function(target, grid, fam) {
  stats::uniroot(function(t) .glm_eta2_of_t(t, grid, fam) - target, c(1e-6, 30), tol = 1e-8)$root
}

# Population R2 of the FULL model (binomial family) for a candidate coefficient t: because
# only the first predictor has a true effect, the full-model deviance Dm depends on t alone,
# so -- unlike .glm_eta2_of_t -- no reduced-model fit is required.
.glm_R2_of_t <- function(t, grid1d, fam) {
  p_true <- fam$linkinv(grid1d$X[, 1] * t)
  D0 <- 2 * .glm_H_bin(sum(grid1d$w * p_true))
  Dm <- 2 * sum(grid1d$w * .glm_H_bin(p_true))
  1 - Dm / D0
}

# Finds the coefficient t on the first predictor (binomial family) that produces the
# target population R2, by root-finding on .glm_R2_of_t.
.glm_calibrate_r2_t <- function(target, grid1d, fam) {
  stats::uniroot(function(t) .glm_R2_of_t(t, grid1d, fam) - target, c(1e-6, 30), tol = 1e-8)$root
}

# Entropy of a categorical outcome, one row per case (P: matrix of category probabilities).
.glm_H_cat <- function(P) {
  L <- ifelse(P > 0, log(P), 0)
  -rowSums(P * L)
}

# Cross-entropy between a true and a model category-probability matrix, one row per case.
.glm_CE_cat <- function(p_true, p_model) {
  p_model <- pmax(p_model, 1e-12)
  -rowSums(p_true * log(p_model))
}

# Category probabilities for a baseline-category multinomial logit model.
# `eta` has one column for every non-reference outcome category.
.glm_multinom_probs <- function(eta) {
  eta <- as.matrix(eta)
  logits <- cbind(eta, 0)
  row_max <- apply(logits, 1, max)
  exp_logits <- exp(logits - row_max)
  exp_logits / rowSums(exp_logits)
}

# Construct the full-model linear predictors for all non-reference categories.
.glm_multinom_full_eta <- function(linear_predictor, intercepts) {
  eta <- matrix(
    linear_predictor,
    nrow = length(linear_predictor),
    ncol = length(intercepts)
  )
  sweep(eta, 2, intercepts, FUN = "+")
}

# Find intercepts that reproduce the requested marginal category probabilities.
# Because all non-reference logits share the same slope, their relative
# probabilities are constant and only the total non-reference odds need solving.
.glm_solve_intercepts <- function(t, grid, p_target) {
  n_categories <- length(p_target)
  nonref_target <- sum(p_target[-n_categories])
  shares <- p_target[-n_categories] / nonref_target

  log_total_odds <- stats::uniroot(
    function(a) {
      sum(grid$w * stats::plogis(a + t * grid$X[, 1])) - nonref_target
    },
    c(-40, 40), extendInt = "yes", tol = 1e-12
  )$root

  log_total_odds + log(shares)
}

# Fit the reduced multinomial model. The common-slope multinomial model
# decomposes into a binary model for reference versus non-reference outcomes
# plus fixed probabilities within the non-reference group.
.glm_fit_reduced_multinom <- function(p_true, grid, p_target) {
  n_categories <- ncol(p_true)
  nonref_true <- rowSums(p_true[, seq_len(n_categories - 1L), drop = FALSE])
  reduced_data <- as.data.frame(grid$X[, -1, drop = FALSE])
  reduced_data$nonref_true <- nonref_true

  fit <- suppressWarnings(
    stats::glm(
      nonref_true ~ ., data = reduced_data, family = stats::binomial(),
      weights = grid$w * 1e8
    )
  )
  nonref_fitted <- stats::fitted(fit)
  shares <- p_target[-n_categories] / sum(p_target[-n_categories])
  cbind(outer(nonref_fitted, shares), 1 - nonref_fitted)
}

# Population eta-squared of the first predictor for a multinomial model.
.glm_eta2_multinom <- function(t, grid, p_target) {
  alpha <- .glm_solve_intercepts(t, grid, p_target)
  eta <- .glm_multinom_full_eta(t * grid$X[, 1], alpha)
  p_true <- .glm_multinom_probs(eta)
  Dm <- 2 * sum(grid$w * .glm_H_cat(p_true))
  D0 <- -2 * sum(p_target * log(p_target))

  p_red <- .glm_fit_reduced_multinom(p_true, grid, p_target)
  Dmx <- 2 * sum(grid$w * .glm_CE_cat(p_true, p_red))

  (Dmx - Dm) / D0
}

# Population R2 of the FULL multinomial model for a candidate coefficient t: same logic as
# .glm_R2_of_t -- no reduced-model fit is needed, since only the first predictor is truly
# effective.
.glm_R2_multinom_of_t <- function(t, grid1d, p_target) {
  alpha <- .glm_solve_intercepts(t, grid1d, p_target)
  eta <- .glm_multinom_full_eta(t * grid1d$X[, 1], alpha)
  p_true <- .glm_multinom_probs(eta)
  Dm <- 2 * sum(grid1d$w * .glm_H_cat(p_true))
  D0 <- -2 * sum(p_target * log(p_target))
  1 - Dm / D0
}

# Category probabilities for an ordinal proportional-odds model.
.glm_ordinal_probs <- function(eta, thresholds) {
  thresholds <- as.numeric(thresholds)
  cumulative <- vapply(
    thresholds,
    function(tau) stats::plogis(tau - eta),
    numeric(length(eta))
  )
  if (!is.matrix(cumulative)) {
    cumulative <- matrix(cumulative, ncol = 1L)
  }

  n_thresholds <- length(thresholds)
  first <- cumulative[, 1, drop = FALSE]
  middle <- if (n_thresholds > 1L) {
    cumulative[, 2:n_thresholds, drop = FALSE] -
      cumulative[, seq_len(n_thresholds - 1L), drop = FALSE]
  } else {
    matrix(numeric(0), nrow = length(eta), ncol = 0L)
  }
  last <- matrix(1 - cumulative[, n_thresholds], ncol = 1L)
  cbind(first, middle, last)
}

# Find a threshold giving a requested marginal cumulative probability.
.glm_solve_threshold <- function(cum_p, eta, w) {
  stats::uniroot(
    function(tau) sum(w * stats::plogis(tau - eta)) - cum_p,
    c(-20, 20), tol = 1e-12
  )$root
}

# Parameterize ordered thresholds for unconstrained optimization.
.glm_pack_thresholds <- function(thresholds) {
  if (length(thresholds) == 1L) {
    return(thresholds)
  }
  c(thresholds[1], log(diff(thresholds)))
}

.glm_unpack_thresholds <- function(par, n_thresholds) {
  if (n_thresholds == 1L) {
    return(par[1])
  }
  c(par[1], par[1] + cumsum(exp(par[2:n_thresholds])))
}

# Fit the reduced ordinal model to the population quadrature grid.
.glm_fit_reduced_ordinal <- function(p_true, grid, thresholds) {
  X_reduced <- grid$X[, -1, drop = FALSE]
  n_thresholds <- ncol(p_true) - 1L
  n_predictors <- ncol(X_reduced)
  start <- c(.glm_pack_thresholds(thresholds), rep(0, n_predictors))

  obj <- function(par) {
    tau <- .glm_unpack_thresholds(par, n_thresholds)
    beta <- par[n_thresholds + seq_len(n_predictors)]
    eta_r <- drop(X_reduced %*% beta)
    P <- .glm_ordinal_probs(eta_r, tau)
    sum(grid$w * .glm_CE_cat(p_true, P))
  }

  fit <- stats::optim(
    start, obj, method = "BFGS",
    control = list(reltol = 1e-14, maxit = 1000)
  )
  if (fit$convergence != 0) {
    stop("Could not calibrate the reduced ordinal model")
  }

  list(
    thresholds = .glm_unpack_thresholds(fit$par, n_thresholds),
    beta = fit$par[n_thresholds + seq_len(n_predictors)]
  )
}

# Population eta-squared of the first predictor for an ordinal model.
.glm_eta2_ordinal <- function(t, grid, cum_target) {
  eta <- t * grid$X[, 1]
  thresholds <- vapply(
    cum_target, .glm_solve_threshold, numeric(1), eta = eta, w = grid$w
  )
  p_true <- .glm_ordinal_probs(eta, thresholds)
  Dm <- 2 * sum(grid$w * .glm_H_cat(p_true))
  p_full <- diff(c(0, cum_target, 1))
  D0 <- -2 * sum(p_full * log(p_full))

  reduced <- .glm_fit_reduced_ordinal(p_true, grid, thresholds)
  X_reduced <- grid$X[, -1, drop = FALSE]
  eta_r <- drop(X_reduced %*% reduced$beta)
  p_red <- .glm_ordinal_probs(eta_r, reduced$thresholds)
  Dmx <- 2 * sum(grid$w * .glm_CE_cat(p_true, p_red))

  (Dmx - Dm) / D0
}

# Population R2 of the FULL ordinal model for a candidate coefficient t: same logic as
# .glm_R2_of_t -- no reduced-model fit is needed, since only the first predictor is truly
# effective.
.glm_R2_ordinal_of_t <- function(t, grid1d, cum_target) {
  eta <- t * grid1d$X[, 1]
  thresholds <- vapply(
    cum_target, .glm_solve_threshold, numeric(1), eta = eta, w = grid1d$w
  )
  p_true <- .glm_ordinal_probs(eta, thresholds)
  Dm <- 2 * sum(grid1d$w * .glm_H_cat(p_true))
  p_full <- diff(c(0, cum_target, 1))
  D0 <- -2 * sum(p_full * log(p_full))
  1 - Dm / D0
}

### end of Generalized Linear Models #####
