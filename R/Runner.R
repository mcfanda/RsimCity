#' Simulation runner
#'
#' @description
#' `Runner` is an R6 class used to define and run simulation experiments.
#'
#' A runner stores fixed parameters, experimental design conditions, simulation
#' steps, and optional aggregate functions. The main method is `$experiment()`,
#' which runs all design conditions for a given number of replications.
#'
#' @details
#' Simulation steps are functions whose formal arguments are matched by name
#' against the parameters stored in `params`, the current design condition, and
#' the output of the previous step, passed as `data`.
#'
#' If a step function has an argument named `simcity`, the runner object itself
#' is passed to that argument.
#'
#' Aggregate functions are applied after the replications for one condition have
#' been completed. Their formal arguments are matched in the same way.
#'
#' @field name Character. Name of the simulation runner.
#' @field debug Integer. Debugging level. Higher values print more information.
#' @field info List. Optional list for storing user-defined information.
#' @field parallel Logical. If `TRUE`, conditions are evaluated in parallel using
#'   `future::multisession`; otherwise they are evaluated sequentially.
#'
#' @examples
#' runner <- Runner$new("example")
#'
#' runner$params <- list(N = 10)
#' runner$design <- list(mu = c(0, 1))
#'
#' runner$step <- function(N, mu) {
#'   data.frame(x = stats::rnorm(N, mean = mu))
#' }
#'
#' runner$aggregate <- function(data) {
#'   data.frame(mean_x = mean(data$x))
#' }
#'
#' runner$test_one()
#' runner$test_aggregates(Rep = 5)
#'
#' \dontrun{
#' runner$experiment(Rep = 100)
#' }
#'
#' @importFrom stats rnorm
#' @export
Runner <- R6::R6Class(
  "simcity_runner",

  public = list(

    name = NULL,
    debug = 0,
    info = list(),
    parallel = FALSE,

    #' @description
    #' Create a new simulation runner.
    #'
    #' @param name A label for this object
    #'
    #' @return A new `Runner` object.
    initialize = function(name) {
      self$name <- name
    },

    #' @description
    #' Print registered private methods.
    #'
    #' @return Invisibly returns `NULL`.
    methods = function() {
      cat("Methods:\n")
      print(private$.methods)
      invisible(NULL)
    },

    #' @description
    #' Print a short summary of the simulation runner.
    #'
    #' @param ... Additional arguments, currently ignored.
    #'
    #' @return Invisibly returns `self`.
    print = function(...) {
      cat("<Simcity Runner\n")
      cat("  name :", self$name,  "\n")
      invisible(self)
    },

    #' @description
    #' Run the full simulation experiment.
    #'
    #' @param Rep Integer. Number of replications for each design condition.
    #' @param label Optional character label. Currently unused.
    #' @param progress Logical. If `TRUE`, show a progress bar.
    #'
    #' @return A data frame with the simulation results aggregated by design
    #'   condition. The returned object has attributes `"label"` and `"time"`.
    experiment = function(Rep = 10, label = NULL, progress = TRUE) {

      if ((length(private$.params)) == 0)
        stop("There are no parameters to run, please set it with obj$params")

      if ((length(private$.design)) == 0)
        warning("There is no design to run, please set it with obj$design. This run is based on obj$params")

      if (length(private$.steps) == 0)
        stop("There is no function to run, please set it with obj$steps")

      if (!requireNamespace("future", quietly = TRUE)) {
        stop("Package 'future' is required")
      }

      if (progress && !requireNamespace("progress", quietly = TRUE)) {
        stop("Package 'progress' is required when progress = TRUE")
      }

      if (isTRUE(self$parallel)) {
        future::plan(future::multisession)
      } else {
        future::plan(future::sequential)
      }

      params <- private$.params[setdiff(names(private$.params), names(private$.design))]
      params <- c(params, private$.design)

      egrid  <- expand.grid(params,stringsAsFactors = FALSE)
      .names <- names(egrid)

      ncond <- nrow(egrid)
      ntotal_runs <- ncond * Rep

      run_one <- function(i) {

        one <- egrid[i, , drop = FALSE]

        data <- self$one_cycle(
          Rep = Rep,
          design_params = one
        )

        private$.run_aggregates(data, one)
      }

      shorten <- function(x, max_chars = 35) {

        x <- as.character(x)

        if (nchar(x, type = "width") <= max_chars) {
          return(x)
        }

        paste0(substr(x, 1, max_chars - 3), "...")
      }

      make_params_message <- function(i) {

        one <- egrid[i, , drop = FALSE]

        vals <- unlist(one[1, .names, drop = TRUE], use.names = FALSE)

        txt <- paste(.names, vals, sep = "=", collapse = ", ")

        terminal_width <- getOption("width", 80)

        ## leave room for bar, percent, elapsed, eta, etc.
        max_param_width <- max(20, terminal_width - 70)

        shorten(txt, max_param_width)
      }

      pb <- NULL

      if (progress) {

        pb <- progress::progress_bar$new(
          total = ncond,
          clear = FALSE,
          width = min(getOption("width", 80), 100),
          show_after = 0,
          format = paste0(
            "[:bar] :percent | ",
            ":current/:total cond | ",
            "elapsed: :elapsed | ",
            "eta: :eta | ",
            ":params"
          )
        )

        pb$tick(
          0,
          tokens = list(
            params = sprintf(
              "starting | %s reps each | %s planned runs",
              Rep,
              ntotal_runs
            )
          )
        )
      }

      if (isTRUE(self$parallel)) {

        user_fun_globals <- .collect_user_functions(.GlobalEnv)

        future_globals <- c(
          list(
            run_one = run_one,
            egrid = egrid,
            Rep = Rep,
            private = private,
            self = self
          ),
          user_fun_globals
        )

        futures <- lapply(seq_len(ncond), function(i) {
          local({
            ii <- i
            future::future(
              run_one(ii),
              seed = TRUE,
              globals = c(
                future_globals,
                list(ii = ii)
              )
            )
          })
        })

        results <- vector("list", ncond)
        done <- rep(FALSE, ncond)

        while (!all(done)) {

          ready <- which(
            !done &
              vapply(futures, future::resolved, logical(1))
          )

          if (length(ready) > 0) {

            for (i in ready) {

              results[[i]] <- future::value(futures[[i]])
              done[i] <- TRUE

              if (progress) {
                pb$tick(
                  tokens = list(
                    params = make_params_message(i)
                  )
                )
              }
            }
          }

          if (!all(done)) {
            Sys.sleep(0.1)
          }
        }

      } else {

        results <- vector("list", ncond)

        for (i in seq_len(ncond)) {

          results[[i]] <- run_one(i)

          if (progress) {
            pb$tick(
              tokens = list(
                params = make_params_message(i)
              )
            )
          }
        }
      }

      results <- do.call(rbind, results)

      attr(results, "label") <- self$name
      attr(results, "time")  <- Sys.time()

      return(results)
    },

    #' @description
    #' Run one simulation condition.
    #'
    #' @param Rep Integer. Number of replications.
    #' @param design_params Optional named list or one-row data frame containing
    #'   design parameters for this condition.
    #'
    #' @return A data frame with one row per successful replication. Failed
    #'   replications are stored in attribute `"fail"`.
    one_cycle = function(Rep = 10, design_params = NULL) {

      fail <- list()

      if ((length(private$.params)) == 0)
        stop("There are no parameters to run, please set it with obj$params")

      if (length(private$.steps) == 0)
        stop("There is no function to run, please set it with obj$steps")

      one <- c(private$.params, design_params)
      .names <- names(one)

      if (self$debug > 0)
        cat("Exec run for ", Rep, " times", paste(.names, one, sep = "=", collapse = ", "), "\n")

      results <- lapply(seq_len(Rep), function(i) {
        data <- private$.one_run(one)

        if (is.null(data)) {
          fail[[length(fail) + 1]] <<- i
          return(NULL)
        }

        data$RepId <- i
        data
      })

      results <- results[!sapply(results, is.null)]
      results <- as.data.frame(do.call(rbind, results))

      attr(results, "fail") <- fail

      return(results)
    },

    #' @description
    #' Test aggregate functions.
    #'
    #' @param Rep Integer. Number of replications used for the test run.
    #'
    #' @return A data frame returned by the aggregate functions.
    test_aggregates = function(Rep = 10) {
      results <- self$one_cycle(Rep = Rep)
      p <- private$.params
      results <- private$.run_aggregates(results, p)
      return(results)
    },

    #' @description
    #' Run one single simulation.
    #'
    #' @return The result of one full step sequence.
    test_one = function() {

      if ((length(private$.params)) == 0)
        stop("There are no parameters to run, please set it with obj$params")

      if (length(private$.steps) == 0)
        stop("There is no function to run, please set it with obj$steps")

      one <- private$.params
      .names <- names(one)

      if (self$debug > 0)
        cat("Exec test run: ", paste(.names, one, sep = "=", collapse = ", "), "\n")

      data <- private$.one_run(one)

      return(data)
    },
    #' @description
    #' reset step functions.
    #'
    #' @return NULL.
    reset_steps = function() {
      private$.steps<-list()
      cat("SC Runner: steps reset ok\n")

    }
  ),

  private = list(

    .params = list(),
    .design = list(),
    .steps  = list(),
    .aggregates = list(),

    .one_run = function(one) {
      data <- NULL
      j <- 0

      for (step in private$.steps) {
        j <- j + 1
        p <- c(private$.params, one)
        p$data <- data

        if (is.function(step)) {
          if (self$debug > 1)
            cat("Exec step: ", j, "\n")

          p$fun <- step
          data <- do.call(private$.one_step, p)
        }

        if (is.list(step)) {
          alist <- lapply(step, function(x) {
            p$fun <- x

            if (self$debug > 1)
              cat("Exec step: ", j, "\n")

            do.call(private$.one_step, p)
          })

          data <- do.call(cbind, alist)
        }
      }

      return(data)
    },

    .one_step = function(...) {

      arg <- list(...)

      if (is.null(arg$fun))
        stop("Please specify a function for the step")

      fun <- arg$fun
      arg$fun <- NULL

      needed <- names(formals(fun))
      wanted <- intersect(needed, names(arg))
      params <- arg[wanted]

      if ("simcity" %in% names(formals(fun)))
        params <- c(params, simcity = self)

      if (length(params) == 0)
        stop("no argument can be passed to fun()")

      results <- tryCatch(
        withCallingHandlers(
          do.call(fun, params),
          warning = function(w) {
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          warning(paste("one_step", conditionMessage(e)))
          NULL
        }
      )

      if (is.null(results) || NROW(results) == 0) {
        return(NULL)
      }

      results
    },

    .run_aggregates = function(results, args) {

      args <- as.list(args)

      for (fun in private$.aggregates) {
        args$fun <- fun
        args$data <- results
        results <- do.call(private$.one_step, args)
      }

      results <- bind_list_cols(results, args)

      return(results)
    }
  ),

  active = list(

    #' @field params Named list. Fixed simulation parameters passed to simulation steps.
    params = function(alist) {
      if (missing(alist)) {
        private$.params
      } else {
        if (is.null(names(alist)))
          stop("obj$params must be a named list")

        lapply(names(alist), function(x) private$.params[[x]] <- alist[[x]])
      }
    },

    #' @field design Named list. Experimental design parameters. All combinations
    #'   are expanded with `expand.grid()`. In case some parameter is already
    #'   set in obj$params, the obj%design is used and the obj$params parameter is ignored.
    design = function(alist) {
      if (missing(alist)) {
        private$.design
      } else {
        if (is.null(names(alist)))
          stop("obj$design must be a named list"
               )
        private$.params <- private$.params[
          setdiff(names(private$.params), names(alist))
        ]

        lapply(names(alist), function(x) private$.design[[x]] <- alist[[x]])
      }
    },

    #' @field step Function or list of functions. Adds a simulation step, or
    #'   returns the currently defined steps when accessed without assignment.
    #'   obj$step can be called repeatedly to add a sequence of functions to be run.
    #'   Each function receives the argument `data` from the previous step
    #'   If obj$step<-list(a=fun,b=fun) is a list of functions, the functions are
    #'   run in parallel, each with the previous step `data` as an argument.
    #'   If the simcity Runner object is needed by the function, add `simcity=` argument to the function
    #'   and then it can be accessed as `simcity$param`, where `param` is any method or variable
    #'   exposed by the Runner.
    step = function(alist) {
      if (missing(alist)) {
        cat("You defined ", length(private$.steps), " for this sim\n")
        invisible(private$.steps)
      } else {
        private$.steps[[length(private$.steps) + 1]] <- alist
      }
    },

    #' @field aggregate Function. Adds an aggregate function, or returns the
    #'   currently defined aggregate functions when accessed without assignment.
    #'   Aggregating functions are sequentially run over the data obtained by
    #'   the obj$step functions
    aggregate = function(afun) {
      if (missing(afun)) {
        cat("You defined ", length(private$.aggregates), " for this sim\n")
        invisible(private$.aggregates)
      } else {
        private$.aggregates[[length(private$.aggregates) + 1]] <- afun
      }
    }
  )
)
