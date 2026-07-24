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
#' dat <- sample_by_corr( R = R,N = 100, mu = c(0, 2), sd = c(1, 3))
#' head(dat)
#' cor(dat)
#'
#' @importFrom stats rnorm
#' @export
sample_by_corr <- function(R, N, mu = 0, sd = 1, seed = NULL) {

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

#' Generate a correlated sample (legacy name)
#'
#' @description
#' Deprecated alias for [sample_by_corr()], kept for backward compatibility with code
#' published under the old name. New code should call [sample_by_corr()] directly.
#'
#' @inheritParams sample_by_corr
#'
#' @return See [sample_by_corr()].
#'
#' @examples
#' R <- matrix(
#'   c(1.0, 0.5,
#'     0.5, 1.0),
#'   nrow = 2,
#'   byrow = TRUE
#' )
#'
#' dat <- simulate_sample_from_corr(R = R, N = 100, mu = c(0, 2), sd = c(1, 3))
#' head(dat)
#'
#' @export
simulate_sample_from_corr <- function(R, N, mu = 0, sd = 1, seed = NULL) {
  sample_by_corr(R = R, N = N, mu = mu, sd = sd, seed = seed)
}



## Generalized linear models, calibrated by exact (Gauss-Hermite quadrature based)
## population effect sizes ###

#' Simulate a sample with a known population R-squared for a GLM
#'
#' @description
#' Draws a sample of `n` cases from a population of `k` correlated normal
#' predictors whose combined effect on the outcome produces an exactly known
#' population R-squared, for a gaussian (classical linear), logistic,
#' multinomial, or ordinal (proportional odds) model. The population R-squared
#' is computed exactly by Gauss-Hermite quadrature, and the coefficients that
#' produce it are found by root-finding, before any random sample is drawn.
#'
#' @param n Integer. Number of cases to draw.
#' @param beta Optional numeric vector of length `k` (or, for `"multinomial"`,
#'   a `k` by `yl - 1` matrix) giving the *relative* direction of the
#'   predictors' effects. Only the pattern matters -- its overall scale is
#'   discarded and replaced with whatever value produces `R2`. When `NULL`,
#'   every predictor is given an equal, unit effect. Default is `NULL`.
#' @param R2 Numeric. Desired population R-squared, strictly between 0 and 1.
#' @param model Character. One of `"gaussian"`, `"logistic"`, `"multinomial"`,
#'   or `"ordinal"`.
#' @param k Integer. Number of correlated predictors to generate. Default is `1`.
#' @param yl Integer. Number of outcome categories, for `"multinomial"` and
#'   `"ordinal"` models. Must be at least 3. Ignored for `"gaussian"` and
#'   `"logistic"`. Can be omitted if it is inferable from `beta` (a matrix,
#'   for `"multinomial"`), from a vector-valued `intercept` (for `"ordinal"`),
#'   or from `p_target`; otherwise it must be supplied explicitly.
#' @param intercept Numeric. For `"gaussian"` and `"logistic"`, a single
#'   intercept (default `0`). For `"multinomial"`, either one common
#'   intercept (recycled across the `yl - 1` non-reference logits) or a
#'   vector of `yl - 1` category-specific intercepts. For `"ordinal"`, either
#'   a single baseline offset applied to a set of equally spaced cutpoints,
#'   or an explicit vector of `yl - 1` cutpoints. Ignored for `"multinomial"`
#'   and `"ordinal"` when `p_target` is supplied.
#' @param p_target Optional numeric vector. For `"multinomial"` and
#'   `"ordinal"` models, the target marginal distribution of the outcome.
#'   Its length must equal `yl` and its positive elements must sum to one.
#'   When supplied, the intercepts (`"multinomial"`) or cutpoints
#'   (`"ordinal"`) are solved automatically -- for whatever coefficient scale
#'   ends up producing `R2` -- so that the model's population marginal
#'   distribution equals `p_target` exactly, and `intercept` is ignored.
#'   Default is `NULL`, in which case the marginal distribution is whatever
#'   `intercept` produces.
#' @param x_mean Numeric vector of length `k`. Population means of the
#'   predictors. Default is `rep(0, k)`.
#' @param x_cov Numeric `k` by `k` matrix. Population covariance matrix of
#'   the predictors. Default is `diag(k)`.
#' @param error_sd Numeric. Residual standard deviation for the `"gaussian"`
#'   model. Default is `1`.
#' @param order Integer. Number of Gauss-Hermite quadrature nodes per
#'   dimension used to compute the exact population R-squared. Default is `10`.
#' @param seed Optional integer. Random seed used for reproducible sampling.
#'   Default is `NULL`.
#'
#' @return A data frame with `n` rows and columns `x1`, ..., `xk` (the
#'   predictors) and `y` (the outcome). `y` is coded `0`/`1` for
#'   `"logistic"`, integers from `1` through `yl` for `"multinomial"` and
#'   `"ordinal"`, and is continuous for `"gaussian"`.
#'
#' @details
#' Since the full model is correctly specified by construction, its
#' population deviance depends only on the scale of the candidate effect
#' pattern `beta`, so the calibration reduces to a `k`-dimensional
#' Gauss-Hermite quadrature over the joint distribution of the predictors,
#' followed by a one-dimensional root-finding step for the common scale
#' factor that produces `R2`. See [sample_by_eta2()] for the corresponding
#' function that targets the eta-squared of a single focal predictor rather
#' than the R-squared of the whole model.
#'
#' The calibration step is independent of `n` and `seed` and is cached
#' internally, so repeated calls with the same population-level arguments
#' (as is typical across the replications of a simulation study) are not
#' recomputed from scratch.
#'
#' @examples
#' dat <- sample_by_r2(n = 200, R2 = 0.20, model = "logistic")
#' head(dat)
#'
#' dat_multi <- sample_by_r2(n = 200, R2 = 0.20, model = "multinomial", k = 3, yl = 3)
#' table(dat_multi$y)
#'
#' # a 3-category outcome with an exactly known, unequal marginal distribution
#' dat_multi2 <- sample_by_r2(
#'   n = 200, R2 = 0.20, model = "multinomial", k = 3, p_target = c(.2, .3, .5)
#' )
#' table(dat_multi2$y)
#'
#' @importFrom stats rnorm plogis rbinom qlogis uniroot optim glm lm binomial fitted
#' @importFrom statmod gauss.quad.prob
#' @importFrom digest digest
#' @export
sample_by_r2 <- function(n, beta = NULL, R2,
                          model = c("gaussian", "logistic", "multinomial", "ordinal"),
                          k = 1, yl = NULL, intercept = 0, p_target = NULL,
                          x_mean = NULL, x_cov = NULL,
                          error_sd = 1, order = 10, seed = NULL) {

  model <- match.arg(model)

  if (R2 <= 0 || R2 >= 1)
    stop("R2 must be strictly between 0 and 1")

  args <- mget(names(formals()), envir = environment())          # Evaluate all arguments
  class(args) <- c(class(args), model)
  args <- gh_check_input(args)                                   # checks and resolves defaults

  if (length(args$x_mean) != k || !all(dim(args$x_cov) == c(k, k)))
    stop("Incompatible dimensions.")

  cal  <- gh_get_calibration(args, R2, "r2")                      # cached population calibration
  args <- gh_check_input(args, cal)                               # fold in any solved intercept/cutpoints

  if (!is.null(seed)) set.seed(seed)

  Z <- matrix(stats::rnorm(n * k), n, k)                          # Independent normals
  X <- sweep(Z %*% chol(args$x_cov), 2, args$x_mean, "+")         # Correlated covariates
  colnames(X) <- paste0("x", seq_len(k))

  yfun <- gh_yfun(args)                                           # Model-specific y generator
  out  <- data.frame(y = yfun(X, cal$beta), X, check.names = FALSE)

  attr(out, "beta")           <- cal$beta                         # Final coefficients
  attr(out, "beta_direction") <- args$beta_direction               # Candidate pattern
  attr(out, "beta_scale")     <- cal$a                             # Common multiplier
  attr(out, "quadrature_R2")  <- cal$quadrature_value              # Population R-squared
  attr(out, "intercept")      <- if (model == "ordinal") args$cutpoints else args$intercept
  attr(out, "model")          <- model

  out
}

#' Simulate a sample with a known population eta-squared for a GLM
#'
#' @description
#' Draws a sample of `n` cases from a population of `k` correlated normal
#' predictors in which the first predictor has an exactly known population
#' eta-squared -- its own unique contribution, controlling for the other
#' `k - 1` predictors -- for a gaussian (classical linear), logistic,
#' multinomial, or ordinal (proportional odds) model. As in
#' [sample_by_r2()], the population eta-squared is computed exactly by
#' Gauss-Hermite quadrature, and the coefficients that produce it are found
#' by root-finding, before any random sample is drawn.
#'
#' @inheritParams sample_by_r2
#' @param eta2 Numeric. Desired population eta-squared of the first
#'   predictor, strictly between 0 and 1.
#'
#' @return A data frame with `n` rows and columns `x1`, ..., `xk` (the
#'   predictors) and `y` (the outcome), exactly as in [sample_by_r2()].
#'
#' @details
#' [sample_by_r2()] targets the R-squared of the whole model, which -- since
#' the model is correctly specified -- depends only on the scale of `beta`.
#' `sample_by_eta2()` instead targets the eta-squared of the *first*
#' predictor alone: the increase in population deviance explained by adding
#' it to a model that already contains the other `k - 1` predictors. This
#' requires, at every candidate coefficient scale, fitting that *reduced*
#' model's own population-optimal coefficients (by minimizing its expected
#' deviance over the assumed predictor distribution) -- because the other
#' predictors, being correlated with the first, partly recover its effect
#' when it is dropped -- rather than assuming they coincide with `beta`'s
#' pattern on the remaining predictors. When `k = 1` there are no other
#' predictors to control for, and eta-squared reduces to the model's own
#' R-squared.
#'
#' As in [sample_by_r2()], the calibration step is independent of `n` and
#' `seed` and is cached internally.
#'
#' @examples
#' dat <- sample_by_eta2(n = 200, eta2 = 0.10, model = "logistic")
#' head(dat)
#'
#' dat_ord <- sample_by_eta2(n = 200, eta2 = 0.10, model = "ordinal", k = 3, yl = 3)
#' table(dat_ord$y)
#'
#' @importFrom stats rnorm plogis rbinom qlogis uniroot optim glm lm binomial fitted
#' @importFrom statmod gauss.quad.prob
#' @importFrom digest digest
#' @export
sample_by_eta2 <- function(n, beta = NULL, eta2,
                            model = c("gaussian", "logistic", "multinomial", "ordinal"),
                            k = 1, yl = NULL, intercept = 0, p_target = NULL,
                            x_mean = NULL, x_cov = NULL,
                            error_sd = 1, order = 10, seed = NULL) {

  model <- match.arg(model)

  if (eta2 <= 0 || eta2 >= 1)
    stop("eta2 must be strictly between 0 and 1")

  args <- mget(names(formals()), envir = environment())          # Evaluate all arguments
  class(args) <- c(class(args), model)
  args <- gh_check_input(args)                                   # checks and resolves defaults

  if (length(args$x_mean) != k || !all(dim(args$x_cov) == c(k, k)))
    stop("Incompatible dimensions.")

  cal  <- gh_get_calibration(args, eta2, "eta2")                  # cached population calibration
  args <- gh_check_input(args, cal)                               # fold in any solved intercept/cutpoints

  if (!is.null(seed)) set.seed(seed)

  Z <- matrix(stats::rnorm(n * k), n, k)                          # Independent normals
  X <- sweep(Z %*% chol(args$x_cov), 2, args$x_mean, "+")         # Correlated covariates
  colnames(X) <- paste0("x", seq_len(k))

  yfun <- gh_yfun(args)                                           # Model-specific y generator
  out  <- data.frame(y = yfun(X, cal$beta), X, check.names = FALSE)

  attr(out, "beta")            <- cal$beta                        # Final coefficients
  attr(out, "beta_direction")  <- args$beta_direction               # Candidate pattern
  attr(out, "beta_scale")      <- cal$a                             # Common multiplier
  attr(out, "quadrature_eta2") <- cal$quadrature_value              # Population eta-squared
  attr(out, "intercept")       <- if (model == "ordinal") args$cutpoints else args$intercept
  attr(out, "model")           <- model

  out
}

### end of exported functions; shared internal machinery below ###

# Calibration cache shared by sample_by_r2() and sample_by_eta2(). The calibration step
# (building the quadrature grid and root-finding the coefficient scale `a`, plus -- for
# sample_by_eta2() -- fitting a reduced model at every candidate scale) depends only on the
# population-level arguments and the `kind` of target ("r2" or "eta2") -- never on `n`, `seed`,
# or the replication index -- and is free of randomness, so caching it changes nothing about
# reproducibility. This matters because the same small set of designs is typically
# recalibrated on every one of many replications in a simulation loop.
gh_calibration_cache <- new.env(parent = emptyenv())

gh_calibration_key <- function(x, target, kind) {
  raw <- paste(
    kind, sprintf("%.15g", target), x$model, x$k, x$order, sprintf("%.15g", x$error_sd),
    paste(sprintf("%.15g", x$beta_direction), collapse = ","),
    paste(sprintf("%.15g", x$x_mean), collapse = ","),
    paste(sprintf("%.15g", x$x_cov), collapse = ","),
    if (!is.null(x$p_target))
      paste("p_target", paste(sprintf("%.15g", x$p_target), collapse = ","))
    else
      paste(sprintf("%.15g", if (x$model == "ordinal") x$cutpoints else x$intercept), collapse = ","),
    sep = "|"
  )
  # Hashed rather than used verbatim: `raw` grows with k^2 (it embeds the full
  # x_cov matrix), and R caps assign()/exists() variable names at 10000 bytes --
  # a dense x_cov for even a moderate k (~50+) overflows that limit otherwise.
  digest::digest(raw, algo = "xxhash64")
}

gh_get_calibration <- function(x, target, kind) {
  key <- gh_calibration_key(x, target, kind)
  if (exists(key, envir = gh_calibration_cache, inherits = FALSE)) {
    return(get(key, envir = gh_calibration_cache, inherits = FALSE))
  }
  cal <- gh_compute_calibration(x, target, kind)
  assign(key, cal, envir = gh_calibration_cache)
  cal
}

# Builds the k-dimensional quadrature grid, root-finds the beta scale `a` that produces
# `target` (dispatching the model-specific R2(a)/eta2(a) formula via gh_r2fun()/gh_eta2fun()),
# and, when p_target was supplied for a multinomial/ordinal model, solves the matching
# intercept/cutpoints once more at the converged `a`, to be stored for sampling and reported
# as attributes.
gh_compute_calibration <- function(x, target, kind) {
  grid  <- gh_build_grid(x$k, x$order, x$x_cov, x$x_mean)
  x$Xgh <- grid$X
  x$w   <- grid$w

  fun <- if (kind == "r2") gh_r2fun(x) else gh_eta2fun(x)

  upper <- 1                                                      # Initial upper bound
  while (fun(upper) < target && upper < 1e6)
    upper <- upper * 2                                            # Expand interval

  if (fun(upper) < target)
    stop(sprintf("The target %s could not be reached.", if (kind == "r2") "R2" else "eta2"))

  a <- stats::uniroot(function(a) fun(a) - target, c(0, upper))$root

  cal <- list(a = a, beta = a * x$beta_direction, quadrature_value = fun(a))

  if (!is.null(x$p_target)) {
    eta0 <- x$Xgh %*% (a * x$beta_direction)
    if (x$model == "multinomial") cal$intercept <- gh_solve_multinom_alpha(eta0, x$w, x$p_target)
    if (x$model == "ordinal")     cal$cutpoints <- gh_solve_ordinal_cutpoints(drop(eta0), x$w, x$p_target)
  }

  cal
}

# Gauss-Hermite quadrature grid for k correlated normal predictors: the tensor product of k
# independent 1-D Gauss-Hermite rules (statmod::gauss.quad.prob(), whose weights already sum
# to 1 so that E[f(X)] = sum(w * f(X))), transformed through the Cholesky factor of x_cov and
# shifted by x_mean. Returns a list with `X` (grid of predictor values) and `w` (matching
# weights, summing to 1).
gh_build_grid <- function(k, order, x_cov, x_mean) {
  gh <- statmod::gauss.quad.prob(order, dist = "normal")
  g  <- as.matrix(expand.grid(rep(list(seq_len(order)), k)))
  Z  <- matrix(gh$nodes[g], ncol = k)
  w  <- apply(matrix(gh$weights[g], ncol = k), 1, prod)
  X  <- sweep(Z %*% chol(x_cov), 2, x_mean, "+")
  list(X = X, w = w)
}

#### check input and resolve defaults per model (shared by sample_by_r2() and sample_by_eta2())

# `cal` is NULL on the first (pre-calibration) call, which validates arguments and resolves
# defaults. It is supplied with the calibration object on a second, post-calibration call, so
# that -- for "multinomial"/"ordinal" models fit with p_target -- the intercepts/cutpoints
# solved during calibration can replace the placeholder ones from the first call, dispatched
# by model rather than branching on `model` in the calling function.
gh_check_input <- function(x, cal = NULL, ...) UseMethod("gh_check_input")

#' @exportS3Method
gh_check_input.default <- function(x, cal = NULL, ...) {

  if (!is.null(cal)) return(x)                                  # nothing to fold in for this model

  if (is.null(x$x_mean)) x$x_mean <- rep(0, x$k)                # Default means
  if (is.null(x$x_cov))  x$x_cov  <- diag(x$k)                  # Default covariance
  if (is.null(x$beta))   x$beta   <- rep(1, x$k)                # Equal slopes
  if (length(x$beta) != x$k)
    stop("length of candidate beta (", length(x$beta), ") should be equal to k=", x$k)
  if (all(x$beta == 0))
    stop("At least one candidate beta must be nonzero.")

  x$beta_direction <- x$beta
  x$x_cov <- as.matrix(x$x_cov)

  x
}

#' @exportS3Method
gh_check_input.multinomial <- function(x, cal = NULL, ...) {

  if (!is.null(cal)) {                                          # second, post-calibration call
    if (!is.null(x$p_target)) x$intercept <- cal$intercept       # fold in the solved intercepts
    return(x)
  }

  if (is.null(x$yl) && !is.null(x$p_target))
    x$yl <- length(x$p_target)                                  # Infer levels
  if (is.null(x$yl) && !is.null(x$beta))
    x$yl <- ncol(as.matrix(x$beta)) + 1L                        # Infer levels
  if (is.null(x$yl) || length(x$yl) != 1L || x$yl < 3L)
    stop("yl must be at least 3. Supply yl explicitly, pass a beta matrix with yl-1 columns, or pass p_target.")
  x$yl <- as.integer(x$yl)

  if (!is.null(x$p_target)) {
    if (!is.numeric(x$p_target) || length(x$p_target) != x$yl ||
        any(!is.finite(x$p_target)) || any(x$p_target <= 0) ||
        abs(sum(x$p_target) - 1) > sqrt(.Machine$double.eps))
      stop("p_target must contain yl positive probabilities that sum to 1")
  } else {
    if (length(x$intercept) == 1L)
      x$intercept <- rep(x$intercept, x$yl - 1L)                # One intercept per logit
    if (length(x$intercept) != x$yl - 1L)
      stop("intercept must have length yl-1.")
  }

  if (is.null(x$x_mean)) x$x_mean <- rep(0, x$k)                # Default means
  if (is.null(x$x_cov))  x$x_cov  <- diag(x$k)                  # Default covariance
  if (is.null(x$beta))   x$beta   <- matrix(1, nrow = x$k, ncol = x$yl - 1L)  # Equal logit slopes
  x$beta <- as.matrix(x$beta)
  if (nrow(x$beta) != x$k)
    stop("nrow of candidate beta (", nrow(x$beta), ") should be equal to k=", x$k)
  if (ncol(x$beta) != (x$yl - 1L))
    stop("ncol of candidate beta (", ncol(x$beta), ") should be equal to yl-1=", (x$yl - 1L))
  if (all(x$beta == 0))
    stop("At least one candidate beta must be nonzero.")

  x$beta_direction <- x$beta
  x$x_cov <- as.matrix(x$x_cov)

  x
}

#' @exportS3Method
gh_check_input.ordinal <- function(x, cal = NULL, ...) {

  if (!is.null(cal)) {                                          # second, post-calibration call
    if (!is.null(x$p_target)) x$cutpoints <- cal$cutpoints       # fold in the solved cutpoints
    return(x)
  }

  if (is.null(x$yl) && !is.null(x$p_target))
    x$yl <- length(x$p_target)                                  # Infer levels
  if (is.null(x$yl) && length(x$intercept) > 1L)
    x$yl <- length(x$intercept) + 1L                            # Infer levels
  if (is.null(x$yl) || length(x$yl) != 1L || x$yl < 3L)
    stop("yl must be at least 3. Supply yl explicitly, pass intercept as a vector of yl-1 cutpoints, or pass p_target.")
  x$yl <- as.integer(x$yl)

  if (!is.null(x$p_target)) {
    if (!is.numeric(x$p_target) || length(x$p_target) != x$yl ||
        any(!is.finite(x$p_target)) || any(x$p_target <= 0) ||
        abs(sum(x$p_target) - 1) > sqrt(.Machine$double.eps))
      stop("p_target must contain yl positive probabilities that sum to 1")
  } else {
    if (length(x$intercept) == 1L) {
      x$cutpoints <- x$intercept + stats::qlogis(seq_len(x$yl - 1L) / x$yl)  # Equal baseline groups
    } else {
      x$cutpoints <- as.numeric(x$intercept)                    # Explicit cutpoints
    }
    if (length(x$cutpoints) != x$yl - 1L)
      stop("Ordinal intercept must be scalar or have length yl-1.")
    if (any(diff(x$cutpoints) <= 0))
      stop("Ordinal cutpoints must be strictly increasing.")
  }

  if (is.null(x$x_mean)) x$x_mean <- rep(0, x$k)                # Default means
  if (is.null(x$x_cov))  x$x_cov  <- diag(x$k)                  # Default covariance
  if (is.null(x$beta))   x$beta   <- rep(1, x$k)                # Equal slopes
  if (length(x$beta) != x$k)
    stop("length of candidate beta (", length(x$beta), ") should be equal to k=", x$k)
  if (all(x$beta == 0))
    stop("At least one candidate beta must be nonzero.")

  x$beta_direction <- x$beta
  x$x_cov <- as.matrix(x$x_cov)

  x
}

#### shared probability / deviance / entropy helpers

gh_softmax <- function(eta) {
  eta <- cbind(0, eta)                                          # Reference logit (category 1)
  eta <- eta - apply(eta, 1, max)                                # Prevent overflow
  prob <- exp(eta)
  prob / rowSums(prob)                                          # Category probabilities
}

gh_ordinal_prob <- function(eta, cutpoints) {

  zeta <- matrix(cutpoints, nrow = length(eta),
                 ncol = length(cutpoints), byrow = TRUE)

  cumulative <- stats::plogis(zeta - eta)                       # Cumulative probabilities

  prob <- cbind(
    cumulative[, 1],
    cumulative[, -1, drop = FALSE] -
      cumulative[, -ncol(cumulative), drop = FALSE],
    1 - cumulative[, ncol(cumulative)]
  )

  prob <- pmax(prob, .Machine$double.eps)                       # Avoid zero
  prob / rowSums(prob)                                          # Normalize
}

# Entropy of a binary outcome with success probability p.
gh_H_bin <- function(p) ifelse(p <= 0 | p >= 1, 0, -(p * log(p) + (1 - p) * log(1 - p)))

# Cross-entropy between a true binary probability and a model's fitted probability.
gh_CE_bin <- function(p_true, p_model) {
  ok  <- p_model > 0 & p_model < 1
  out <- numeric(length(p_true))
  out[ok] <- -(p_true[ok] * log(p_model[ok]) + (1 - p_true[ok]) * log(1 - p_model[ok]))
  out
}

# Entropy of a categorical outcome, one row per case (P: matrix of category probabilities).
gh_H_cat <- function(P) {
  L <- ifelse(P > 0, log(P), 0)
  -rowSums(P * L)
}

# Cross-entropy between a true and a model category-probability matrix, one row per case.
gh_CE_cat <- function(p_true, p_model) {
  p_model <- pmax(p_model, 1e-12)
  -rowSums(p_true * log(p_model))
}

# Deviance R-squared (D0 - D1)/D0 of a full-model probability matrix (one row per grid node,
# one column per category -- a two-column cbind(1-p, p) for the binary case).
gh_deviance_r2 <- function(prob, w) {

  prob <- pmax(prob, .Machine$double.eps)                       # Avoid log(0)
  prob <- prob / rowSums(prob)                                  # Normalize
  prob0 <- colSums(prob * w)                                    # Null probabilities
  D1 <- -2 * sum(w * rowSums(prob * log(prob)))                 # Model deviance
  D0 <- -2 * sum(prob0 * log(prob0))                            # Null deviance

  1 - D1 / D0                                                   # Deviance R-squared
}

gh_draw_categories <- function(prob, ordered = FALSE) {

  u <- stats::runif(nrow(prob))                                 # Uniform draws
  cumulative <- t(apply(prob, 1, cumsum))                       # Cumulative probabilities
  y <- 1L + rowSums(u > cumulative)                              # Draw categories
  y <- pmin(y, ncol(prob))

  if (ordered)
    ordered(y, levels = seq_len(ncol(prob)))
  else
    factor(y, levels = seq_len(ncol(prob)))
}

#### solving intercepts/cutpoints for a target marginal distribution (p_target)

# Find a threshold giving a requested marginal cumulative probability.
gh_solve_threshold <- function(cum_p, eta, w) {
  stats::uniroot(
    function(tau) sum(w * stats::plogis(tau - eta)) - cum_p,
    c(-20, 20), tol = 1e-12
  )$root
}

# Solves for the (yl - 1) multinomial intercepts that reproduce p_target's marginal category
# probabilities, given `eta0` -- the category-specific linear predictors (n_nodes x (yl - 1))
# before any intercept is added. Because each non-reference category may have its own slope
# pattern (rather than sharing one common slope), the (yl - 1) intercepts are solved jointly
# by minimizing the squared discrepancy between the model's and target's marginals.
gh_solve_multinom_alpha <- function(eta0, w, p_target) {
  obj <- function(alpha) {
    eta <- sweep(eta0, 2, alpha, "+")
    marg <- colSums(w * gh_softmax(eta))
    sum((marg - p_target)^2)
  }
  stats::optim(rep(0, length(p_target) - 1L), obj, method = "BFGS",
               control = list(reltol = 1e-14, maxit = 2000))$par
}

# Solves for the (yl - 1) ordinal cutpoints that reproduce p_target's marginal category
# probabilities, given `eta0` -- the common linear predictor (length n_nodes). Because the
# ordinal model has a single linear predictor regardless of category count, each cutpoint
# solves its own univariate cumulative-probability equation independently.
gh_solve_ordinal_cutpoints <- function(eta0, w, p_target) {
  cum_target <- cumsum(p_target)[seq_len(length(p_target) - 1L)]
  vapply(cum_target, gh_solve_threshold, numeric(1), eta = eta0, w = w)
}

#### reduced-model refitting for sample_by_eta2() (population-weighted regression on the grid)

# Parameterize ordered thresholds for unconstrained optimization.
gh_pack_thresholds <- function(thresholds) {
  if (length(thresholds) == 1L) {
    return(thresholds)
  }
  c(thresholds[1], log(diff(thresholds)))
}

gh_unpack_thresholds <- function(par, n_thresholds) {
  if (n_thresholds == 1L) {
    return(par[1])
  }
  c(par[1], par[1] + cumsum(exp(par[2:n_thresholds])))
}

# Fits the population-optimal reduced multinomial model on X_reduced (the grid's predictors
# excluding the first), i.e. the (yl - 1) intercepts and slopes that best approximate p_true
# (the full model's true category probabilities) in cross-entropy, by general optimization --
# unlike the common-slope decomposition trick, this makes no assumption that beta_direction's
# non-reference categories share a slope pattern.
gh_fit_reduced_multinom <- function(p_true, X_reduced, w) {
  yl     <- ncol(p_true)
  n_pred <- ncol(X_reduced)
  start  <- rep(0, (yl - 1L) * (n_pred + 1L))

  obj <- function(par) {
    par_mat <- matrix(par, nrow = n_pred + 1L, ncol = yl - 1L)
    alpha <- par_mat[1, ]
    B     <- par_mat[-1, , drop = FALSE]
    eta   <- sweep(X_reduced %*% B, 2, alpha, "+")
    sum(w * gh_CE_cat(p_true, gh_softmax(eta)))
  }

  fit <- stats::optim(start, obj, method = "BFGS", control = list(reltol = 1e-14, maxit = 2000))

  par_mat <- matrix(fit$par, nrow = n_pred + 1L, ncol = yl - 1L)
  alpha <- par_mat[1, ]
  B     <- par_mat[-1, , drop = FALSE]
  eta   <- sweep(X_reduced %*% B, 2, alpha, "+")
  gh_softmax(eta)
}

# Fits the population-optimal reduced ordinal model on X_reduced, i.e. the cutpoints and
# slopes that best approximate p_true in cross-entropy, by general optimization (BFGS),
# starting from the full model's own cutpoints.
gh_fit_reduced_ordinal <- function(p_true, X_reduced, w, start_cutpoints) {
  n_thr  <- ncol(p_true) - 1L
  n_pred <- ncol(X_reduced)
  start  <- c(gh_pack_thresholds(start_cutpoints), rep(0, n_pred))

  obj <- function(par) {
    tau   <- gh_unpack_thresholds(par, n_thr)
    beta  <- par[n_thr + seq_len(n_pred)]
    eta_r <- drop(X_reduced %*% beta)
    P     <- gh_ordinal_prob(eta_r, tau)
    sum(w * gh_CE_cat(p_true, P))
  }

  fit <- stats::optim(start, obj, method = "BFGS", control = list(reltol = 1e-14, maxit = 1000))

  tau   <- gh_unpack_thresholds(fit$par, n_thr)
  beta  <- fit$par[n_thr + seq_len(n_pred)]
  eta_r <- drop(X_reduced %*% beta)
  gh_ordinal_prob(eta_r, tau)
}

#### population R-squared as a function of the beta scale `a`, dispatched per model

gh_r2fun <- function(x, ...) UseMethod("gh_r2fun")

#' @exportS3Method
gh_r2fun.gaussian <- function(x, ...) {
  function(a) {
    mu <- x$intercept + drop(x$Xgh %*% (a * x$beta_direction))       # Predicted values
    mu_mean <- sum(x$w * mu)                                          # Population mean
    explained <- sum(x$w * (mu - mu_mean)^2)                          # Explained variance
    explained / (explained + x$error_sd^2)                            # Population R-squared
  }
}

#' @exportS3Method
gh_r2fun.logistic <- function(x, ...) {
  function(a) {
    eta <- x$intercept + drop(x$Xgh %*% (a * x$beta_direction))      # Linear predictor
    prob <- stats::plogis(eta)                                       # Event probability
    gh_deviance_r2(cbind(1 - prob, prob), x$w)                       # Deviance R-squared
  }
}

#' @exportS3Method
gh_r2fun.multinomial <- function(x, ...) {
  if (!is.null(x$p_target)) {
    function(a) {
      eta0 <- x$Xgh %*% (a * x$beta_direction)                            # no intercept yet
      alpha <- gh_solve_multinom_alpha(eta0, x$w, x$p_target)             # solved to match p_target
      eta <- sweep(eta0, 2, alpha, "+")                                   # Category logits
      gh_deviance_r2(gh_softmax(eta), x$w)                                # Deviance R-squared
    }
  } else {
    function(a) {
      eta <- sweep(x$Xgh %*% (a * x$beta_direction), 2, x$intercept, "+")  # Category logits
      gh_deviance_r2(gh_softmax(eta), x$w)                                # Deviance R-squared
    }
  }
}

#' @exportS3Method
gh_r2fun.ordinal <- function(x, ...) {
  if (!is.null(x$p_target)) {
    function(a) {
      eta0 <- drop(x$Xgh %*% (a * x$beta_direction))                 # Common predictor
      cutpoints <- gh_solve_ordinal_cutpoints(eta0, x$w, x$p_target)  # solved to match p_target
      gh_deviance_r2(gh_ordinal_prob(eta0, cutpoints), x$w)          # Deviance R-squared
    }
  } else {
    function(a) {
      eta <- drop(x$Xgh %*% (a * x$beta_direction))                    # Common predictor
      gh_deviance_r2(gh_ordinal_prob(eta, x$cutpoints), x$w)           # Deviance R-squared
    }
  }
}

#### population eta-squared of the first predictor as a function of the beta scale `a`,
#### dispatched per model. Unlike gh_r2fun(), this requires refitting a *reduced* model (the
#### grid's predictors excluding the first) at every candidate `a`, since the other k - 1
#### predictors -- being correlated with the first -- partly recover its effect when it is
#### dropped. When k == 1, there are no other predictors, so the reduced model is trivially
#### the null model and eta-squared equals R-squared.

gh_eta2fun <- function(x, ...) UseMethod("gh_eta2fun")

#' @exportS3Method
gh_eta2fun.gaussian <- function(x, ...) {
  function(a) {
    mu <- x$intercept + drop(x$Xgh %*% (a * x$beta_direction))
    mu_mean <- sum(x$w * mu)
    var_full <- sum(x$w * (mu - mu_mean)^2)
    total_var <- var_full + x$error_sd^2
    R2_full <- var_full / total_var

    if (x$k == 1L) {
      R2_reduced <- 0
    } else {
      others <- as.data.frame(x$Xgh[, -1, drop = FALSE])
      others$mu <- mu
      fit_reduced <- stats::lm(mu ~ ., data = others, weights = x$w)
      resid_reduced <- mu - stats::fitted(fit_reduced)
      var_resid_reduced <- sum(x$w * resid_reduced^2) + x$error_sd^2
      R2_reduced <- 1 - var_resid_reduced / total_var
    }

    R2_full - R2_reduced
  }
}

#' @exportS3Method
gh_eta2fun.logistic <- function(x, ...) {
  function(a) {
    eta <- x$intercept + drop(x$Xgh %*% (a * x$beta_direction))
    p_true <- stats::plogis(eta)
    D0 <- 2 * gh_H_bin(sum(x$w * p_true))
    Dm <- 2 * sum(x$w * gh_H_bin(p_true))

    if (x$k == 1L) {
      Dmx <- D0
    } else {
      others <- as.data.frame(x$Xgh[, -1, drop = FALSE])
      others$p_true <- p_true
      fit_reduced <- suppressWarnings(
        stats::glm(p_true ~ ., data = others, family = stats::binomial(), weights = x$w * 1e8)
      )
      Dmx <- 2 * sum(x$w * gh_CE_bin(p_true, stats::fitted(fit_reduced)))
    }

    (Dmx - Dm) / D0
  }
}

#' @exportS3Method
gh_eta2fun.multinomial <- function(x, ...) {
  function(a) {
    eta0 <- x$Xgh %*% (a * x$beta_direction)
    alpha <- if (!is.null(x$p_target)) gh_solve_multinom_alpha(eta0, x$w, x$p_target) else x$intercept
    eta <- sweep(eta0, 2, alpha, "+")
    p_true <- gh_softmax(eta)
    Dm <- 2 * sum(x$w * gh_H_cat(p_true))
    p0 <- colSums(x$w * p_true)
    D0 <- -2 * sum(p0 * log(p0))

    if (x$k == 1L) {
      Dmx <- D0
    } else {
      X_reduced <- x$Xgh[, -1, drop = FALSE]
      p_red <- gh_fit_reduced_multinom(p_true, X_reduced, x$w)
      Dmx <- 2 * sum(x$w * gh_CE_cat(p_true, p_red))
    }

    (Dmx - Dm) / D0
  }
}

#' @exportS3Method
gh_eta2fun.ordinal <- function(x, ...) {
  function(a) {
    eta0 <- drop(x$Xgh %*% (a * x$beta_direction))
    cutpoints <- if (!is.null(x$p_target)) gh_solve_ordinal_cutpoints(eta0, x$w, x$p_target) else x$cutpoints
    p_true <- gh_ordinal_prob(eta0, cutpoints)
    Dm <- 2 * sum(x$w * gh_H_cat(p_true))
    p0 <- colSums(x$w * p_true)
    D0 <- -2 * sum(p0 * log(p0))

    if (x$k == 1L) {
      Dmx <- D0
    } else {
      X_reduced <- x$Xgh[, -1, drop = FALSE]
      p_red <- gh_fit_reduced_ordinal(p_true, X_reduced, x$w, cutpoints)
      Dmx <- 2 * sum(x$w * gh_CE_cat(p_true, p_red))
    }

    (Dmx - Dm) / D0
  }
}

#### dependent variable generator, dispatched per model (shared by sample_by_r2() and
#### sample_by_eta2(): y-generation only depends on the final beta/intercept/cutpoints and
#### model family, not on how beta was calibrated)

gh_yfun <- function(x, ...) UseMethod("gh_yfun")

#' @exportS3Method
gh_yfun.gaussian <- function(x, ...) {
  function(X, beta) {
    eta <- x$intercept + drop(X %*% beta)                           # Linear predictor
    eta + stats::rnorm(length(eta), 0, x$error_sd)                  # Gaussian outcome
  }
}

#' @exportS3Method
gh_yfun.logistic <- function(x, ...) {
  function(X, beta) {
    eta <- x$intercept + drop(X %*% beta)                           # Linear predictor
    stats::rbinom(length(eta), 1, stats::plogis(eta))                # Binary outcome
  }
}

#' @exportS3Method
gh_yfun.multinomial <- function(x, ...) {
  function(X, beta) {
    eta <- sweep(X %*% beta, 2, x$intercept, "+")                   # Nominal logits
    gh_draw_categories(gh_softmax(eta), ordered = FALSE)             # Nominal outcome
  }
}

#' @exportS3Method
gh_yfun.ordinal <- function(x, ...) {
  function(X, beta) {
    eta <- drop(X %*% beta)                                        # Ordinal predictor
    gh_draw_categories(gh_ordinal_prob(eta, x$cutpoints), ordered = TRUE)  # Ordered outcome
  }
}

### end of Generalized Linear Models #####
