#' Generate a correlated sample
#'
#' @description
#' Generate a data frame containing `N` observations from a multivariate normal
#' distribution with a specified correlation matrix, means, and standard
#' deviations.
#'
#' @param N Integer. Number of observations to generate.
#' @param R Numeric matrix. A positive definite correlation matrix. The number of
#'   rows and columns defines the number of generated variables.
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
#' dat <- correlated_sample(N = 100, R = R, mu = c(0, 2), sd = c(1, 3))
#' head(dat)
#' cor(dat)
#'
#' @importFrom stats rnorm
#' @export
correlated_sample <- function(N, R, mu = 0, sd = 1, seed = NULL) {

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


bind_list_cols <- function(df, x) {
  x <- x[sapply(x, function(z) is.numeric(z) || is.character(z))]
  xdf <- as.data.frame(x, stringsAsFactors = FALSE)
  xdf <- xdf[rep(1, nrow(df)), , drop = FALSE]  # repeat across rows
  rownames(xdf) <- NULL
  cbind(xdf, df)
}

#' Count elements
#'
#' @description
#' Count the frequency of elements of a vector in another vector
#' @export
count_names <- function(V, X) {
  out <- tabulate(match(X, V), nbins = length(V))
  names(out) <- V
  out
}


### makes column means for numeric and use the first entry for character columns
numColMean <- function(x) {
  means <- lapply(names(x), function(name) {
    if (is.numeric(x[[name]])) {
      return(mean(x[[name]], na.rm = TRUE))
    }
    x[[name]][1]
  })

  names(means) <- names(x)
  as.data.frame(means, stringsAsFactors = FALSE, check.names = FALSE)
}


######### this is for parellel to work fine

.collect_user_functions <- function(env = .GlobalEnv) {

  nms <- ls(envir = env, all.names = TRUE)

  if (length(nms) == 0) {
    return(list())
  }

  objs <- mget(nms, envir = env, inherits = FALSE)

  objs[
    vapply(objs, is.function, logical(1))
  ]
}

### this is just aesthetics

.running_in_rscript <- function() {
  exe <- basename(commandArgs()[1])
  grepl("^Rscript", exe)
}
