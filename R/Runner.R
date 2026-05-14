#' Simulation runner
#'
#' @description
#' `Runner` is an R6 class used to define and run simulation experiments.
#'
#' A runner stores fixed parameters, experimental design conditions, simulation
#' steps, and optional aggregate functions. The main entry points are:
#'
#' * `$experiment()` — runs all design conditions for a given number of
#'   replications and returns a data frame of aggregated results.
#' * `$experiment_interactive()` — same pipeline, but feeds results into an
#'   [Interactive] monitor as they are produced, enabling live Shiny plots.
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
#' been completed. Their formal arguments are matched in the same way. The last
#' aggregate function must return a [data.frame]. The default aggregate is
#' `as.data.frame(do.call(rbind, data))`. Use `obj$aggregate <- FALSE` to
#' disable aggregation entirely.
#'
#' In interactive mode, the aggregate pipeline is also run at the end of each
#' condition and the result is pushed to [Interactive]`$update_agg()`, so an
#' optional second plot function can visualise per-condition summaries as they
#' accumulate.
#'
#' Experiments can be run in parallel using the [future] package.
#' `Runner` prepares the parallel sessions and collects the results.
#' Functions defined in `.GlobalEnv` are exported to the parallel sessions,
#' so user-defined functions can be used by the experiment code.
#'
#' @field name Character. Name of the simulation runner.
#' @field debug Integer. Debugging level. Higher values print more information.
#' @field info List. Optional list for storing user-defined information.
#' @field parallel Logical. If `TRUE`, conditions are evaluated in parallel
#'   using `future::multisession` (or `future::multicore` on Linux); otherwise
#'   they are evaluated sequentially. One process is spawned per design cell,
#'   so a single-cell design is not parallelised. To parallelise it, replicate
#'   the cell, e.g. `obj$design <- list(N = c(100, 100, 100))` with
#'   `obj$experiment(Rep = 400)`.
#' @field par_method Character. Parallel backend when `parallel = TRUE`.
#'   `"multisession"` (default) works on Windows and Linux; `"multicore"`
#'   works on Linux only.
#' @field collect List. Names of objects in `.GlobalEnv` to export to parallel
#'   workers. By default all functions in `.GlobalEnv` are exported; non-function
#'   objects (data frames, etc.) must be listed here explicitly. Ignored when
#'   `parallel = FALSE`.
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
#' # Batch experiment
#' runner$experiment(Rep = 100)
#'
#' # Live interactive experiment (requires shiny and later)
#' mon <- Interactive$new(
#'   plot_fun = function(data, step) {
#'     ggplot2::ggplot(data, ggplot2::aes(x = x)) +
#'       ggplot2::geom_histogram(bins = 30) +
#'       ggplot2::theme_minimal()
#'   },
#'   agg_plot_fun = function(data, step) {
#'     ggplot2::ggplot(data, ggplot2::aes(x = mu, y = mean_x)) +
#'       ggplot2::geom_col() +
#'       ggplot2::theme_minimal()
#'   }
#' )
#' mon$set_start_fun(function(obj) {
#'   runner$experiment_interactive(interactive = obj, Rep = 100)
#' })
#' mon$run(viewer = "pane")
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
    par_method="multisession",
    collect = list(),
    #' @description
    #' Create a new simulation runner.
    #'
    #' @param name A label for this object
    #'
    #' @return A new `Runner` object.
    initialize = function(name="SimCityRunner") {
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
    experiment = function(Rep = 1, label = NULL, progress = TRUE) {

      .params <- private$.params[setdiff(names(private$.params), names(private$.design))]
      .params <- c(.params, private$.design)
      keep <- vapply(.params, function(z) {
        (is.numeric(z) || is.character(z) || is.logical(z))
      }, logical(1))
      params<-.params[keep]

      if ((length(params)) == 0)
        stop("There are no parameters to run, please set it with obj$params or obj$design")

      if (length(private$.steps) == 0)
        stop("There is no function to run, please set it with obj$steps")

      if (self$parallel && !requireNamespace("future", quietly = TRUE)) {
        stop("Package 'future' is required when obj$parallel = TRUE")
      }

      if (progress && !requireNamespace("progress", quietly = TRUE)) {
        stop("Package 'progress' is required when progress = TRUE")
      }

      if (isTRUE(self$parallel)) {
        if (self$par_method=="multicore")
              future::plan(future::multicore)
        else
              future::plan(future::multisession)

      } else {
        future::plan(future::sequential)
      }


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
        one$.Rep <- Rep
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

        user_fun_globals <- .collect_user_objects(.GlobalEnv, self$collect)

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
    #' Run the simulation experiment with live Shiny updates.
    #'
    #' Executes the same step and aggregate pipeline as `$experiment()`, but
    #' feeds results into an [Interactive] monitor as each replication completes
    #' rather than collecting everything at the end. The event loop is driven by
    #' [later::later()], so the Shiny app remains responsive while the simulation
    #' runs.
    #'
    #' After all replications for a condition are done, the aggregate pipeline
    #' (`$aggregate`) is applied to the accumulated data and the result is pushed
    #' to `interactive$update_agg()`. If the [Interactive] object has an
    #' `agg_plot_fun` set, this triggers a redraw of the "Aggregated" tab.
    #'
    #' @param interactive An [Interactive] object. Receives live updates via
    #'   `$update()` and per-condition aggregated results via `$update_agg()`.
    #' @param Rep Integer. Number of replications per design condition.
    #' @param delay Numeric. Seconds to wait between replications. Increase for
    #'   smoother animation; use `0` for maximum throughput.
    #' @param reset Logical. If `TRUE` (default), call `interactive$reset()`
    #'   before starting so previous data is cleared.
    #'
    #' @return Invisibly returns the `interactive` object.
    experiment_interactive = function(interactive,
                                      Rep = 1,
                                      delay = 0,
                                      reset = TRUE) {

      if (!requireNamespace("later", quietly = TRUE)) {
        stop("Package 'later' is required for interactive simulations.")
      }

      if (isTRUE(reset)) {
        interactive$reset()
      }

      .params <- private$.params[setdiff(names(private$.params), names(private$.design))]
      .params <- c(.params, private$.design)

      keep <- vapply(.params, function(z) {
        is.numeric(z) || is.character(z) || is.logical(z)
      }, logical(1))

      params <- .params[keep]

      if (length(params) == 0) {
        stop("There are no parameters to run, please set them with obj$params or obj$design")
      }

      if (length(private$.steps) == 0) {
        stop("There is no function to run, please set it with obj$steps")
      }

      egrid <- base::expand.grid(params, stringsAsFactors = FALSE)

      ncond <- base::nrow(egrid)

      cond_i <- 1L
      rep_i <- 0L

      last_cond_i <- NA_integer_

      run_one_step <- function() {

        tryCatch({

          if (interactive$is_paused()) {
            later::later(run_one_step, delay = delay)
            return(invisible(NULL))
          }

          if (interactive$is_finished()) {
            return(invisible(NULL))
          }

          if (cond_i > ncond) {
            return(invisible(NULL))
          }

          rep_i <<- rep_i + 1L

          one <- egrid[cond_i, , drop = FALSE]

          if (!identical(cond_i, last_cond_i)) {
            interactive$set_condition(one)
            last_cond_i <<- cond_i
          }

          one_list <- base::as.list(one)

          data <- private$.one_run(
            c(private$.params, one_list)
          )

          if (!is.null(data) && NROW(data) > 0) {

            if (base::is.data.frame(data)) {
              data$.RepId <- rep_i
              data$.CondId <- cond_i
            } else {
              attr(data, ".RepId") <- rep_i
              attr(data, ".CondId") <- cond_i
            }

            interactive$update(data, append = TRUE)
          }

          if (rep_i >= Rep) {

            if (!is.null(interactive$agg_plot_fun)) {
              one_agg <- one
              one_agg$.Rep <- Rep
              agg_data <- private$.run_aggregates(interactive$x, one_agg)
              interactive$update_agg(agg_data, append = TRUE)
            }

            cond_i <<- cond_i + 1L
            rep_i <<- 0L

            interactive$clear_data(reset_step = TRUE)
          }

          if (cond_i > ncond) {

            interactive$running <- FALSE
            interactive$paused <- FALSE
            interactive$finished <- TRUE
            interactive$dirty_plot <- TRUE
            interactive$redraw_pending <- FALSE

            interactive$invalidate_data()
            interactive$invalidate_plot()

            return(invisible(NULL))
          }

          later::later(run_one_step, delay = delay)

          invisible(NULL)

        }, error = function(e) {

          interactive$info$error <- conditionMessage(e)

          message("Error in experiment_interactive():")
          message(conditionMessage(e))

          interactive$pause()

          invisible(NULL)
        })
      }

      interactive$resume()

      later::later(run_one_step, delay = 0)

      invisible(interactive)
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

      one <- c(private$.params, design_params)
      .names <- names(one)

      if (self$debug > 0) {
            cat("Exec run for ", Rep, " times", paste(.names, print_params(one), sep = "=", collapse = ", "), "\n")
      }

      results <- lapply(seq_len(Rep), function(i) {
        data <- private$.one_run(one)

        if (is.null(data)) {
          fail[[length(fail) + 1]] <<- i
          return(NULL)
        }
        if (is.data.frame(data))
          data$.RepId <- i
        else
          attr(data,".RepId")<-i
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

      if (self$debug > 0) {
        cat("Exec run for one time:", paste(.names, print_params(one), sep = "=", collapse = ", "), "\n")
      }

      data <- private$.one_run(one)

      return(data)
    },

    #' @description Reset step functions.
    #'
    #' @return NULL.
    reset_steps = function() {
      private$.steps <- list()
      cat(self$name, ": steps reset ok\n")
    }
  ),


  private = list(

    .params = list(),
    .design = list(),
    .steps  = list(),
    .aggregates = list(function(data) as.data.frame(do.call(rbind,data))),
    .default_aggregate=TRUE,
    .reserved   = list(".Rep",".RepId"),
    .one_run = function(one) {
      data <- NULL
      j <- 0
      for (step in private$.steps) {
        j <- j + 1
        p <- merge_params(private$.params, one)
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

          data <- alist
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
          warning(deparse(fun))
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
        check_named_not_reserved(alist,private$.reserved)
        ## all good, digest
        lapply(names(alist), function(x) private$.params[[x]] <- alist[[x]])
      }
    },

    #' @field design Named list. Experimental design parameters. All combinations
    #'   are expanded with [base::expand.grid()]. In case some parameter is already
    #'   set in `obj$params`, the `obj$design` is used and the `obj$params` parameter is ignored.
    design = function(alist) {
      if (missing(alist)) {
        private$.design
      } else {

        check_named_not_reserved(alist,private$.reserved)

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
    #'   and then it can be accessed as `simcity$foo`, where `foo` is any method or variable
    #'   exposed by the Runner.
    step = function(alist) {
      if (missing(alist)) {
        cat("You defined ", length(private$.steps), " for this sim\n")
        invisible(private$.steps)
      } else {
        private$.steps[[length(private$.steps) + 1]] <- alist
      }
    },

    #' @field aggregate Function(s). Adds an aggregate function, or returns the
    #'   currently defined aggregate functions when accessed without assignment.
    #'   Can be called repeatedly to chain multiple functions. Each function
    #'   should accept a `data` argument; the last one must return a
    #'   [data.frame]. Aggregating functions are applied sequentially to the
    #'   data collected by `$step` functions. The default is
    #'   `as.data.frame(do.call(rbind, data))`. To disable aggregation, set
    #'   `obj$aggregate <- FALSE`. The same pipeline is also run at the end of
    #'   each condition during `$experiment_interactive()` to feed the
    #'   aggregated-results plot in [Interactive].
    aggregate = function(afun) {
      if (missing(afun)) {
        cat("There are ", length(private$.aggregates), " aggregating functions for this sim\n")
        invisible(private$.aggregates)
      } else {
        if (isFALSE(afun))
          private$.aggregates<-list()
        else {
          if (private$.default_aggregate) private$.aggregates<-list()
          private$.aggregates[[length(private$.aggregates) + 1]] <- afun
          private$.default_aggregate<-FALSE
        }
      }
    }
  )
)
