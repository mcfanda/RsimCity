
bind_list_cols <- function(df, x) {

  if (!is.data.frame(df))
    stop("df must be a data.frame")

  keep <- vapply(x, function(z) {
    (is.numeric(z) || is.character(z) || is.logical(z)) && length(z) == 1
  }, logical(1))

  x <- x[keep]

  # Aggregate functions may return a design variable themselves. Keep the
  # user-returned column and do not bind a duplicate metadata column.
  x <- x[setdiff(names(x), names(df))]

  if (length(x) == 0)
    return(df)

  xdf <- as.data.frame(x, stringsAsFactors = FALSE)

  xdf <- xdf[rep(1, nrow(df)), , drop = FALSE]
  rownames(xdf) <- NULL
  rownames(df) <- NULL
  cbind(xdf, df)
}
#' Count elements
#'
#' @description
#' Count the frequency of elements of a vector in another vector
#' @param V Character vector of values to count
#' @param X Vector in which to count occurrences of `V`
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

print_params <-function(x) {
  lapply(x, function(z) {
    if ((is.numeric(z) || is.character(z) || is.logical(z)) && length(z) == 1)
       return(x)
  else
       return(class(x)[1])
  })
}



######### this is for parellel to work fine

.collect_user_objects <- function(env = .GlobalEnv, objects = NULL) {

  nms <- ls(envir = env, all.names = TRUE)

  if (length(nms) == 0) {
    return(list())
  }

  if (!is.null(objects)) {
    missing <- setdiff(objects, nms)

    if (length(missing) > 0) {
      stop(
        "These requested objects were not found in `env`: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
  }

  objs <- mget(nms, envir = env, inherits = FALSE)

  is_fun <- vapply(objs, is.function, logical(1))

  keep_extra <- names(objs) %in% objects

  objs[is_fun | keep_extra]
}

### this is just aesthetics

.running_in_rscript <- function() {
  exe <- basename(commandArgs()[1])
  grepl("^Rscript", exe)
}


### helpers

check_named_not_reserved <- function(alist, reserved) {

  if (!is.list(alist))
    stop("`alist` must be a list", call. = FALSE)

  nms <- names(alist)

  if (is.null(nms) || !all(nzchar(nms))) {
    stop("`alist` must be a fully named list", call. = FALSE)
  }

  bad <- nms %in% reserved

  if (any(bad)) {
    stop(
      "These names are reserved and cannot be used: ",
      paste(nms[bad], collapse = ", "),
      call. = FALSE
    )
  }

  TRUE
}


merge_params <- function(params, one) {
  params <- params[setdiff(names(params), names(one))]
  c(params, one)
}
