
## Generalized linear models given eta-squared ###

#' Simulate a sample with a known population eta-squared for a GLM
#'
#' @description
#' Draws a sample of `N` cases from a population of `k` correlated normal
#' predictors in which the first predictor has an exactly known population
#' eta-squared, for a logistic, probit, multinomial, or ordinal (proportional
#' odds) model.
#'
#' Unlike simulating a very large population and searching for coefficients
#' that approximately match a target effect size, the target population
#' eta-squared here is computed exactly via Gauss-Hermite quadrature, and the
#' model coefficient that produces it is found by root-finding, before any
#' random sample is drawn.
#'
#' @param target_eta2 Numeric. Desired population eta-squared of the first
#'   predictor, strictly between 0 and 1.
#' @param N Integer. Number of cases to draw.
#' @param model Character. One of `"logistic"`, `"probit"`, `"multinomial"`,
#'   or `"ordinal"`.
#' @param k Integer. Number of correlated predictors to generate; only the
#'   first one carries an effect on the outcome. Default is `3`.
#' @param rho Numeric. Common pairwise correlation among the `k` predictors.
#'   Default is `0.30`.
#' @param n_nodes Integer. Number of Gauss-Hermite quadrature nodes per
#'   dimension used to compute the exact population values. Default is `20`.
#' @param p_target Numeric vector of length 3. For `"multinomial"` and
#'   `"ordinal"` models, the target marginal distribution of the outcome
#'   across its three categories. Default is equal thirds. Ignored for
#'   `"logistic"` and `"probit"`.
#' @param seed Optional integer. Random seed used for reproducible sampling.
#'   Default is `NULL`.
#'
#' @return A data frame with `N` rows and columns `x1`, ..., `xk` (the
#'   predictors) and `y` (the outcome). `y` is coded `0`/`1` for `"logistic"`
#'   and `"probit"`, and `1`/`2`/`3` for `"multinomial"` and `"ordinal"`.
#'
#' @details
#' Because every model considered here reaches the outcome only through one
#' or two linear predictors, and the predictors are assumed jointly normal,
#' the population quantities needed to compute eta-squared (average predicted
#' probability, average model entropy) reduce to low-dimensional integrals
#' against normal densities, regardless of `k`. These are evaluated by
#' Gauss-Hermite quadrature.
#'
#' One subtlety is handled explicitly: even though only the first predictor
#' has a true effect, the remaining `k - 1` predictors are correlated with it,
#' so the model that omits the first predictor does not have a
#' population-optimal coefficient of zero on them -- it partly recovers the
#' omitted predictor's effect through the correlation. The calibration
#' therefore fits this reduced model's actual population-optimal coefficients
#' (by minimizing its expected deviance over the assumed predictor
#' distribution) rather than assuming they coincide with the null model.
#'
#' @examples
#' # a sample of 200 cases from a logistic model in which the first predictor
#' # accounts for 10% of the population variance (eta-squared = .10)
#' dat <- glm_simulate_sample(target_eta2 = 0.10, N = 200, model = "logistic")
#' head(dat)
#'
#' # the same population effect size, for an ordinal (proportional-odds)
#' # outcome with three equally likely categories
#' dat_ord <- glm_simulate_sample(target_eta2 = 0.10, N = 200, model = "ordinal")
#' head(dat_ord)
#'
#' @importFrom stats rnorm qlogis plogis rbinom binomial glm uniroot optim fitted
#' @export
simulate_sample_from_eta <- function(target_eta2, N,
                                 model = c("logistic", "probit", "multinomial", "ordinal"),
                                 k = 3, rho = 0.30, n_nodes = 20,
                                 p_target = c(1/3, 1/3, 1/3),
                                 seed = NULL) {

  model <- match.arg(model)
  if (!is.null(seed)) set.seed(seed)
  if (target_eta2 <= 0 || target_eta2 >= 1)
    stop("target_eta2 must be strictly between 0 and 1")

  Sigma <- matrix(rho, k, k); diag(Sigma) <- 1
  grid  <- .glm_build_grid(k, n_nodes, Sigma)
  U     <- chol(Sigma)
  X     <- matrix(stats::rnorm(N * k), N, k) %*% U

  if (model %in% c("logistic", "probit")) {
    fam <- switch(model, logistic = stats::binomial(), probit = stats::binomial(link = "probit"))
    t_star <- .glm_calibrate_t(target_eta2, grid, fam)
    Y <- stats::rbinom(N, 1, fam$linkinv(X[, 1] * t_star))

  } else if (model == "multinomial") {
    t_star <- stats::uniroot(function(t) .glm_eta2_multinom(t, grid, p_target) - target_eta2,
                              c(1e-4, 10), tol = 1e-6)$root
    alpha <- .glm_solve_intercepts(t_star, grid, p_target)
    P <- .glm_multinom_probs(alpha[1] + t_star * X[, 1], alpha[2] + t_star * X[, 1])
    Y <- apply(P, 1, function(p) sample(1:3, 1, prob = p))

  } else { # ordinal
    cum_target <- cumsum(p_target)[1:2]
    t_star <- stats::uniroot(function(t) .glm_eta2_ordinal(t, grid, cum_target) - target_eta2,
                              c(1e-4, 10), tol = 1e-6)$root
    tau1 <- .glm_solve_threshold(cum_target[1], t_star * grid$X[, 1], grid$w)
    tau2 <- .glm_solve_threshold(cum_target[2], t_star * grid$X[, 1], grid$w)
    P <- .glm_ordinal_probs(t_star * X[, 1], tau1, tau2)
    Y <- apply(P, 1, function(p) sample(1:3, 1, prob = p))
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

# Category probabilities for the multinomial (baseline-category logit) model with three
# categories, given the linear predictors of the first two categories (third is reference).
.glm_multinom_probs <- function(eta1, eta2) {
  denom <- 1 + exp(eta1) + exp(eta2)
  cbind(exp(eta1) / denom, exp(eta2) / denom, 1 / denom)
}

# Finds the two intercepts of the multinomial full model (first predictor only, coefficient
# t) that make the grid-averaged category probabilities match a target marginal distribution.
.glm_solve_intercepts <- function(t, grid, p_target) {
  obj <- function(alpha) {
    P    <- .glm_multinom_probs(alpha[1] + t * grid$X[, 1], alpha[2] + t * grid$X[, 1])
    pbar <- c(sum(grid$w * P[, 1]), sum(grid$w * P[, 2]))
    sum((pbar - p_target[1:2])^2)
  }
  stats::optim(stats::qlogis(p_target[1:2]), obj, method = "BFGS",
               control = list(reltol = 1e-14))$par
}

# Finds the reduced multinomial model's population-optimal intercepts and slopes on the
# two remaining predictors, by minimizing the cross-entropy against the true probabilities
# over the grid (the multi-category analogue of fitting glm() to the grid).
.glm_fit_reduced_multinom <- function(p_true, grid, start) {
  obj <- function(par) {
    P <- .glm_multinom_probs(par[1] + par[2] * grid$X[, 2] + par[3] * grid$X[, 3],
                              par[4] + par[5] * grid$X[, 2] + par[6] * grid$X[, 3])
    sum(grid$w * .glm_CE_cat(p_true, P))
  }
  stats::optim(start, obj, method = "BFGS", control = list(reltol = 1e-14, maxit = 1000))$par
}

# Population eta2 of the first predictor for the multinomial model, at a candidate
# coefficient t: builds the full model, fits the reduced model's population-optimal fit,
# and returns (Dmx - Dm) / D0.
.glm_eta2_multinom <- function(t, grid, p_target) {
  alpha  <- .glm_solve_intercepts(t, grid, p_target)
  p_true <- .glm_multinom_probs(alpha[1] + t * grid$X[, 1], alpha[2] + t * grid$X[, 1])
  Dm <- 2 * sum(grid$w * .glm_H_cat(p_true))
  D0 <- -2 * sum(p_target * log(p_target))

  start   <- c(stats::qlogis(p_target[1]), 0, 0, stats::qlogis(p_target[2]), 0, 0)
  par_red <- .glm_fit_reduced_multinom(p_true, grid, start)
  p_red   <- .glm_multinom_probs(par_red[1] + par_red[2] * grid$X[, 2] + par_red[3] * grid$X[, 3],
                                  par_red[4] + par_red[5] * grid$X[, 2] + par_red[6] * grid$X[, 3])
  Dmx <- 2 * sum(grid$w * .glm_CE_cat(p_true, p_red))

  (Dmx - Dm) / D0
}

# Category probabilities for the ordinal (proportional-odds) model with three categories,
# given a linear predictor eta and two thresholds tau1 < tau2.
.glm_ordinal_probs <- function(eta, tau1, tau2) {
  F1 <- stats::plogis(tau1 - eta); F2 <- stats::plogis(tau2 - eta)
  cbind(F1, F2 - F1, 1 - F2)
}

# Finds the threshold that makes the grid-averaged cumulative probability P(Y<=j) match a
# target cumulative proportion, for a given linear predictor eta (thresholds are independent
# of each other, so each is solved with its own root-find).
.glm_solve_threshold <- function(cum_p, eta, w) {
  stats::uniroot(function(tau) sum(w * stats::plogis(tau - eta)) - cum_p,
                 c(-20, 20), tol = 1e-12)$root
}

# Finds the reduced ordinal model's population-optimal thresholds and slopes on the two
# remaining predictors, by minimizing the cross-entropy against the true probabilities
# over the grid.
.glm_fit_reduced_ordinal <- function(p_true, grid, start) {
  obj <- function(par) {
    eta_r <- par[3] * grid$X[, 2] + par[4] * grid$X[, 3]
    P <- .glm_ordinal_probs(eta_r, par[1], par[2])
    sum(grid$w * .glm_CE_cat(p_true, P))
  }
  stats::optim(start, obj, method = "BFGS", control = list(reltol = 1e-14, maxit = 1000))$par
}

# Population eta2 of the first predictor for the ordinal model, at a candidate coefficient
# t: builds the full model (with thresholds matching a target cumulative distribution),
# fits the reduced model's population-optimal fit, and returns (Dmx - Dm) / D0.
.glm_eta2_ordinal <- function(t, grid, cum_target) {
  eta  <- t * grid$X[, 1]
  tau1 <- .glm_solve_threshold(cum_target[1], eta, grid$w)
  tau2 <- .glm_solve_threshold(cum_target[2], eta, grid$w)
  p_true <- .glm_ordinal_probs(eta, tau1, tau2)
  Dm <- 2 * sum(grid$w * .glm_H_cat(p_true))
  p_full <- c(cum_target[1], cum_target[2] - cum_target[1], 1 - cum_target[2])
  D0 <- -2 * sum(p_full * log(p_full))

  par_red <- .glm_fit_reduced_ordinal(p_true, grid, c(tau1, tau2, 0, 0))
  eta_r <- par_red[3] * grid$X[, 2] + par_red[4] * grid$X[, 3]
  p_red <- .glm_ordinal_probs(eta_r, par_red[1], par_red[2])
  Dmx <- 2 * sum(grid$w * .glm_CE_cat(p_true, p_red))

  (Dmx - Dm) / D0
}

### end of Generalized Linear Models #####
