#' Interactive simulation monitor
#'
#' @description
#' Interactive provides a minimal Shiny-based monitor for visualising
#' simulation output as it is produced. It is intended to be driven by a
#' Runner object (see Rsimcity::Runner): the Runner calls $update() after each
#' replication and $update_agg() after each condition completes. The monitor
#' shows a live plot of the current-condition data and — when an aggregate
#' plotting function is supplied — an accumulating aggregated-results plot.
#'
#' The Interactive class does not run simulations itself; it only receives and
#' displays results produced elsewhere. This separation keeps the monitor
#' lightweight and suitable for embedding in RStudio or browser-based UIs.
#'
#' @details
#' **Basic workflow** (live plot only):
#'
#' mon <- Interactive$new(plot_fun = my_plot)
#'
#' mon$set_start_fun(function(obj) runner$experiment_interactive(interactive = obj, Rep = 100))
#'
#' mon$run(viewer = "pane")
#'
#' **Workflow with an aggregated-results plot:**
#'
#' mon <- Interactive$new(plot_fun = my_plot, agg_plot_fun = my_agg_plot)
#'
#' When `agg_plot_fun` is supplied the Shiny app shows two tabs: **"Live"**
#' displays the current-condition raw data updated every replication;
#' **"Aggregated"** displays the output of the runner's aggregate pipeline,
#' updated once per completed condition and accumulated across conditions.
#'
#' The Shiny app provides Start, Stop, Continue, and Reset buttons.
#'
#' Both plotting functions are called with two positional arguments:
#'
#' plot_fun(data, step)      # `data` = current-condition accumulated output
#' agg_plot_fun(agg_x, step) # `agg_x` = aggregated output, all conditions so far
#'
#' where `step` is the total number of per-replication updates received.
#'
#' The `viewer` argument to `$run()` controls where the app opens:
#' `"pane"` (RStudio Viewer), `"browser"` (system browser),
#' `"dialog"` (RStudio external window), or `"none"`.
#'
#' @field plot_fun Function. Function used to create the live plot. Called as
#'   `plot_fun(data, step)` where `data` is the current-condition accumulated output.
#' @field agg_plot_fun Function or `NULL`. Optional function used to plot aggregated
#'   results. Called as `agg_plot_fun(agg_x, step)` after each condition completes.
#'   If `NULL` (the default), no aggregated plot tab is shown.
#' @field start_fun Function or `NULL`. Function called when the Start button is pressed.
#' @field x Object. Accumulated simulation output for the current condition.
#' @field agg_x Object. Accumulated aggregated output across all completed conditions.
#' @field step Integer. Number of updates received.
#' @field running Logical. Whether the monitor is currently running.
#' @field paused Logical. Whether the monitor is paused.
#' @field finished Logical. Whether the simulation has finished.
#' @field dirty_plot Logical. Whether the plot needs to be redrawn.
#' @field redraw_pending Logical. Whether a plot redraw has already been scheduled.
#' @field run_id Integer. Internal counter used to invalidate old scheduled callbacks.
#' @field info List. General-purpose list for user data. Also used internally
#'   to store the error message (`info$error`) when a step throws an unhandled
#'   error during `$experiment_interactive()`.
#' @field condition Named list or `NULL`. The current design condition being
#'   processed, set automatically by [Runner]`$experiment_interactive()`.
#' @field title Character. Title shown in the Shiny app.
#' @field plot_refresh_ms Numeric. Minimum delay, in milliseconds, between plot redraws.
#' @field plot_height Character. Height of the Shiny plot output.
#' @field max_rows Integer or `NULL`. Maximum number of rows to retain when `x` is a data frame.
#'
#' @examples
#' \dontrun{
#' # Minimal example — Runner drives the simulation and Interactive displays it
#' runner <- Rsimcity::Runner$new("example")
#' runner$params <- list(N = 10)
#' runner$design <- list(mu = c(0, 1))
#' runner$step <- function(N, mu) data.frame(x = stats::rnorm(N, mean = mu))
#'
#' mon <- Rsimcity::Interactive$new(plot_fun = function(data, step) NULL)
#' runner$experiment_interactive(interactive = mon, Rep = 5, delay = 0)
#' }
#'
#' @export
Interactive <- R6::R6Class(
  classname = "simcity_interactive",

  public = list(

    plot_fun = NULL,
    agg_plot_fun = NULL,
    start_fun = NULL,

    x = NULL,
    agg_x = NULL,
    step = 0,

    running = FALSE,
    paused = FALSE,
    finished = FALSE,

    dirty_plot = FALSE,
    redraw_pending = FALSE,

    run_id = 0,
    info = list(),
    condition = NULL,
    title = NULL,
    plot_refresh_ms = NULL,
    plot_height = NULL,
    max_rows = NULL,

    #' @description
    #' Create a new interactive simulation monitor.
    #'
    #' @param plot_fun Function used to create the live plot. It is called as
    #'   `plot_fun(data, step)`, where `data` is the accumulated simulation
    #'   output for the current condition and `step` is the current update number.
    #' @param agg_plot_fun Optional function used to plot aggregated results. It
    #'   is called as `agg_plot_fun(agg_x, step)` after each condition completes,
    #'   where `agg_x` accumulates the aggregated output across all conditions.
    #'   When non-`NULL`, a second "Aggregated" tab is shown in the Shiny app.
    #' @param title Character. Title displayed in the Shiny app.
    #' @param plot_refresh_ms Numeric. Minimum delay, in milliseconds, between
    #'   plot redraws.
    #' @param plot_height Character. Plot height passed to [shiny::plotOutput()].
    #' @param max_rows Integer or `NULL`. If `x` is a data frame, keep only the
    #'   last `max_rows` rows. If `NULL`, keep all rows.
    #'
    #' @return A new `Interactive` object.
    initialize = function(plot_fun,
                          agg_plot_fun = NULL,
                          title = "Live Shiny plot",
                          plot_refresh_ms = 500,
                          plot_height = "500px",
                          max_rows = NULL) {

      stopifnot(is.function(plot_fun))
      if (!is.null(agg_plot_fun)) stopifnot(is.function(agg_plot_fun))

      if (!requireNamespace("shiny", quietly = TRUE)) {
        stop("Package 'shiny' is required.")
      }

      if (!requireNamespace("later", quietly = TRUE)) {
        stop("Package 'later' is required.")
      }

      self$plot_fun <- plot_fun
      self$agg_plot_fun <- agg_plot_fun
      self$start_fun <- NULL

      self$title <- title
      self$plot_refresh_ms <- plot_refresh_ms
      self$plot_height <- plot_height
      self$max_rows <- max_rows

      self$x <- NULL
      self$agg_x <- NULL
      self$step <- 0

      self$running <- FALSE
      self$paused <- FALSE
      self$finished <- FALSE

      self$dirty_plot <- TRUE
      self$redraw_pending <- FALSE

      self$run_id <- 0

      private$data_count <- 0
      private$plot_count <- 0
      private$agg_plot_count <- 0

      private$data_trigger     <- shiny::reactiveVal(0)
      private$plot_trigger     <- shiny::reactiveVal(0)
      private$agg_plot_trigger <- shiny::reactiveVal(0)
    },

    #' @description
    #' Register the function called when the Start button is pressed.
    #'
    #' @param fun Function. The function should accept the interactive object as
    #'   its first argument. For example: `function(obj) runner$experiment_interactive(obj)`.
    #'
    #' @return Invisibly returns `self`.
    set_start_fun = function(fun) {
      stopifnot(is.function(fun))
      self$start_fun <- fun
      invisible(self)
    },

    #' @description
    #' Register the optional aggregated-results plotting function.
    #'
    #' @param fun Function. Called as `fun(agg_x, step)` after each condition
    #'   completes. When set, an "Aggregated" tab appears in the Shiny app.
    #'
    #' @return Invisibly returns `self`.
    set_agg_plot_fun = function(fun) {
      stopifnot(is.function(fun))
      self$agg_plot_fun <- fun
      invisible(self)
    },

    #' @description
    #' Add new aggregated simulation output to the monitor.
    #'
    #' Called by [Runner]`$experiment_interactive()` after each condition
    #' completes. The data are never cleared between conditions; use `$reset()`
    #' to clear.
    #'
    #' @param x Object. Aggregated results for the just-completed condition.
    #' @param append Logical. If `TRUE`, append to the existing aggregated data.
    #'   If `FALSE`, replace it.
    #'
    #' @return Invisibly returns `self`.
    update_agg = function(x, append = TRUE) {

      if (isTRUE(append)) {
        self$agg_x <- private$append_data(self$agg_x, x)
      } else {
        self$agg_x <- x
      }

      self$invalidate_plot()

      invisible(self)
    },

    #' @description
    #' Start the interactive monitor.
    #'
    #' @param reset Logical. If `TRUE`, reset the stored data before starting.
    #'
    #' @return Invisibly returns `self`.
    start = function(reset = TRUE) {

      if (isTRUE(reset)) {
        self$reset()
      }

      self$run_id <- self$run_id + 1

      self$running <- TRUE
      self$paused <- FALSE
      self$finished <- FALSE

      self$dirty_plot <- TRUE
      self$redraw_pending <- FALSE

      self$invalidate_data()
      self$invalidate_plot()

      if (!is.null(self$start_fun)) {
        self$start_fun(self)
      }

      invisible(self)
    },

    #' @description
    #' Add new simulation output to the monitor.
    #'
    #' @param x Object. New simulation output. If both the existing data and the
    #'   new data are data frames, they are combined with [base::rbind()].
    #' @param append Logical. If `TRUE`, append `x` to the existing data. If
    #'   `FALSE`, replace the existing data.
    #'
    #' @return Invisibly returns `self`.
    update = function(x, append = TRUE) {

      if (isTRUE(self$paused) || isTRUE(self$finished)) {
        return(invisible(self))
      }

      if (isTRUE(append)) {
        self$x <- private$append_data(self$x, x)
      } else {
        self$x <- x
      }

      if (
        !is.null(self$max_rows) &&
        base::is.data.frame(self$x) &&
        base::nrow(self$x) > self$max_rows
      ) {
        self$x <- utils::tail(self$x, self$max_rows)
      }

      self$step <- self$step + 1
      self$running <- TRUE
      self$dirty_plot <- TRUE

      self$invalidate_data()
      self$schedule_plot_redraw(immediate = FALSE)

      invisible(self)
    },
    #' @description
    #' Clear the current-condition live data.
    #'
    #' Clears `x` so the live plot starts fresh for the next condition.
    #' Aggregated data (`agg_x`) is **not** cleared; use `$reset()` for that.
    #' Called automatically by [Runner]`$experiment_interactive()` between
    #' conditions.
    #'
    #' @param reset_step Logical. If `TRUE` (default), reset `step` to zero.
    #'
    #' @return Invisibly returns `self`.
    clear_data = function(reset_step = TRUE) {

      self$x <- NULL

      if (isTRUE(reset_step)) {
        self$step <- 0
      }

      self$dirty_plot <- TRUE
      self$redraw_pending <- FALSE

      self$invalidate_data()
      self$invalidate_plot()
      self$invalidate_agg_plot()

      invisible(self)
    },
    #' @description
    #' Reset the monitor.
    #'
    #' @return Invisibly returns `self`.
    reset = function() {

      self$run_id <- self$run_id + 1

      self$x <- NULL
      self$agg_x <- NULL
      self$step <- 0

      self$running <- FALSE
      self$paused <- FALSE
      self$finished <- FALSE

      self$dirty_plot <- TRUE
      self$redraw_pending <- FALSE

      self$invalidate_data()
      self$invalidate_plot()
      self$invalidate_agg_plot()

      invisible(self)
    },

    #' @description
    #' Pause the monitor.
    #'
    #' @return Invisibly returns `self`.
    pause = function() {

      self$paused <- TRUE
      self$running <- FALSE

      self$invalidate_data()

      invisible(self)
    },

    #' @description
    #' Resume the monitor after it has been paused.
    #'
    #' @return Invisibly returns `self`.
    resume = function() {

      if (!isTRUE(self$finished)) {
        self$paused <- FALSE
        self$running <- TRUE
        self$dirty_plot <- TRUE
      }

      self$invalidate_data()
      self$schedule_plot_redraw(immediate = TRUE)

      invisible(self)
    },

    #' @description
    #' Mark the monitor as finished.
    #'
    #' @return Invisibly returns `self`.
    finish = function() {

      self$run_id <- self$run_id + 1

      self$finished <- TRUE
      self$running <- FALSE
      self$paused <- FALSE

      self$dirty_plot <- FALSE
      self$redraw_pending <- FALSE

      self$invalidate_data()
      self$invalidate_plot()
      self$invalidate_agg_plot()

      invisible(self)
    },

    #' @description
    #' Check whether the monitor is paused.
    #'
    #' @return Logical.
    is_paused = function() {
      isTRUE(self$paused)
    },

    #' @description
    #' Check whether the monitor is finished.
    #'
    #' @return Logical.
    is_finished = function() {
      isTRUE(self$finished)
    },

    #' @description
    #' Check whether the monitor is running.
    #'
    #' @return Logical.
    is_running = function() {
      isTRUE(self$running)
    },

    #' @description
    #' Invalidate Shiny outputs that depend on status/data information.
    #'
    #' @return Invisibly returns `NULL`.
    invalidate_data = function() {
      private$data_count <- private$data_count + 1
      private$data_trigger(private$data_count)
      invisible(NULL)
    },
    #' @description
    #' Set the current design condition shown in the Shiny panel.
    #'
    #' @param condition Named list or one-row data frame with the current design
    #'   condition.
    #'
    #' @return Invisibly returns `self`.
    set_condition = function(condition) {

      if (is.data.frame(condition)) {
        condition <- as.list(condition[1, , drop = FALSE])
      }

      self$condition <- condition

      self$invalidate_data()

      invisible(self)
    },
    #' @description
    #' Invalidate Shiny outputs that depend on the plot.
    #'
    #' @return Invisibly returns `NULL`.
    invalidate_plot = function() {
      private$plot_count <- private$plot_count + 1
      private$plot_trigger(private$plot_count)
      invisible(NULL)
    },

    #' @description
    #' Schedule a plot redraw.
    #'
    #' @param immediate Logical. If `TRUE`, schedule the redraw immediately.
    #'   Otherwise, wait `plot_refresh_ms` milliseconds.
    #'
    #' @return Invisibly returns `NULL`.
    schedule_plot_redraw = function(immediate = FALSE) {

      if (!isTRUE(self$dirty_plot)) {
        return(invisible(NULL))
      }

      if (isTRUE(self$redraw_pending) && !isTRUE(immediate)) {
        return(invisible(NULL))
      }

      self$redraw_pending <- TRUE

      this_run_id <- self$run_id

      delay_sec <- if (isTRUE(immediate)) {
        0
      } else {
        self$plot_refresh_ms / 1000
      }

      later::later(
        function() {

          tryCatch({

            if (!identical(this_run_id, self$run_id)) {
              return(invisible(NULL))
            }

            self$redraw_pending <- FALSE

            if (isTRUE(self$finished)) {
              return(invisible(NULL))
            }

            if (!isTRUE(self$dirty_plot)) {
              return(invisible(NULL))
            }

            self$dirty_plot <- FALSE
            self$invalidate_plot()

            invisible(NULL)

          }, error = function(e) {

            self$redraw_pending <- FALSE
            self$running <- FALSE
            self$paused <- TRUE

            message("Error in scheduled plot redraw:")
            message(conditionMessage(e))

            invisible(NULL)
          })
        },
        delay = delay_sec
      )

      invisible(NULL)
    },

    #' @description
    #' Create a dependency on the data/status trigger.
    #'
    #' @return Invisibly returns `NULL`.
    depend_data = function() {
      private$data_trigger()
      invisible(NULL)
    },

    #' @description
    #' Create a dependency on the plot trigger.
    #'
    #' @return Invisibly returns `NULL`.
    depend_plot = function() {
      private$plot_trigger()
      invisible(NULL)
    },

    #' @description
    #' Invalidate Shiny outputs that depend on the aggregated plot.
    #'
    #' @return Invisibly returns `NULL`.
    invalidate_agg_plot = function() {
      private$agg_plot_count <- private$agg_plot_count + 1
      private$agg_plot_trigger(private$agg_plot_count)
      invisible(NULL)
    },

    #' @description
    #' Create a dependency on the aggregated plot trigger.
    #'
    #' @return Invisibly returns `NULL`.
    depend_agg_plot = function() {
      private$agg_plot_trigger()
      invisible(NULL)
    },

    #' @description
    #' Build the Shiny application.
    #'
    #' @return A Shiny application object.
    app = function() {

      obj <- self
      has_agg <- !is.null(obj$agg_plot_fun)

      plot_error_display <- function(e) {
        graphics::plot.new()
        graphics::text(x = 0.5, y = 0.60, labels = "Plot error", cex = 1.4)
        graphics::text(x = 0.5, y = 0.45, labels = conditionMessage(e), cex = 0.8)
        message("Error in plot_fun():")
        message(conditionMessage(e))
      }

      main_content <- if (has_agg) {
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Live",
            shiny::plotOutput("live_plot", height = obj$plot_height)
          ),
          shiny::tabPanel(
            "Aggregated",
            shiny::plotOutput("agg_plot", height = obj$plot_height)
          )
        )
      } else {
        shiny::plotOutput("live_plot", height = obj$plot_height)
      }

      ui <- shiny::fluidPage(

        shiny::titlePanel(obj$title),

        shiny::sidebarLayout(

          shiny::sidebarPanel(
            shiny::actionButton("start", "Start"),
            shiny::actionButton("pause", "Stop"),
            shiny::actionButton("resume", "Continue"),
            shiny::actionButton("reset", "Reset"),
            shiny::tags$hr(),
            shiny::verbatimTextOutput("info")
          ),

          shiny::mainPanel(main_content)
        )
      )

      server <- function(input, output, session) {

        shiny::observeEvent(input$start, {
          obj$start(reset = TRUE)
        })

        shiny::observeEvent(input$pause, {
          obj$pause()
        })

        shiny::observeEvent(input$resume, {
          obj$resume()
        })

        shiny::observeEvent(input$reset, {
          obj$reset()
        })

        output$live_plot <- shiny::renderPlot({

          obj$depend_plot()

          if (is.null(obj$x)) {
            ggplot2::ggplot() +
                ggplot2::labs(title = "Waiting for cycle data") +
                ggplot2::theme_minimal()

          } else
            tryCatch(
              obj$plot_fun(obj$x, obj$step),
              error = plot_error_display
            )
        })

        if (has_agg) {
          output$agg_plot <- shiny::renderPlot({

            obj$depend_agg_plot()

            if (is.null(obj$agg_x)) {
                ggplot2::ggplot() +
                  ggplot2::labs(title = "Waiting for aggregate data") +
                  ggplot2::theme_minimal()

            } else
              tryCatch(
                obj$agg_plot_fun(obj$agg_x, obj$step),
                error = plot_error_display
              )
            })
        }

        output$info <- shiny::renderPrint({

          obj$depend_data()

          cat("Step:", obj$step, "\n")

          cat("\n\nCurrent condition:\n")

          if (is.null(obj$condition) || length(obj$condition) == 0) {
            cat("  none\n")
          } else {
            for (nm in names(obj$condition)) {
              cat(" ", nm, ":", obj$condition[[nm]], "\n")
            }
          }
          cat("\n\n")

          if (is.null(obj$x)) {
            cat("N:", 0, "\n")
          } else if (base::is.data.frame(obj$x)) {
            cat("Rows:", base::nrow(obj$x), "\n")
            cat("Columns:", base::ncol(obj$x), "\n")
          } else {
            cat("N:", length(obj$x), "\n")
          }

          cat("Running:", obj$running, "\n")
          cat("Paused:", obj$paused, "\n")
          cat("Finished:", obj$finished, "\n")
          cat("Dirty plot:", obj$dirty_plot, "\n")
          cat("Redraw pending:", obj$redraw_pending, "\n")
          cat("Run id:", obj$run_id, "\n")

          if (base::is.numeric(obj$x)) {
            cat("Mean:", base::mean(obj$x), "\n")
            cat("SD:", stats::sd(obj$x), "\n")
          }
        })
      }

      shiny::shinyApp(ui = ui, server = server)
    },

    #' @description
    #' Run the Shiny app.
    #'
    #' @param port Integer or `NULL`. Port passed to [shiny::runApp()].
    #' @param viewer Character. Where to display the app:
    #'   \describe{
    #'     \item{`"pane"`}{RStudio Viewer pane (default).}
    #'     \item{`"browser"`}{System web browser.}
    #'     \item{`"dialog"`}{RStudio external window (floating dialog).}
    #'     \item{`"none"`}{Do not open any viewer.}
    #'   }
    #'
    #' @return Runs the Shiny app.
    run = function(port = NULL,
                   viewer = c("pane", "browser", "dialog", "none")) {

      viewer <- match.arg(viewer)

      launch.browser <- switch(
        viewer,
        pane    = shiny::paneViewer(),
        browser = shiny::browserViewer(),
        dialog  = shiny::dialogViewer(self$title),
        none    = FALSE
      )

      shiny::runApp(
        appDir = self$app(),
        port = port,
        host = "127.0.0.1",
        launch.browser = launch.browser
      )
    }
  ),

  private = list(

    data_trigger     = NULL,
    plot_trigger     = NULL,
    agg_plot_trigger = NULL,

    data_count     = 0,
    plot_count     = 0,
    agg_plot_count = 0,

    append_data = function(old, new) {

      if (is.null(new)) {
        return(old)
      }

      if (is.null(old) || length(old) == 0) {
        return(new)
      }

      if (base::is.data.frame(old) && base::is.data.frame(new)) {

        old_names <- names(old)
        new_names <- names(new)

        missing_in_new <- base::setdiff(old_names, new_names)
        missing_in_old <- base::setdiff(new_names, old_names)

        for (nm in missing_in_new) {
          new[[nm]] <- NA
        }

        for (nm in missing_in_old) {
          old[[nm]] <- NA
        }

        new <- new[, names(old), drop = FALSE]

        return(base::rbind(old, new))
      }

      if (base::is.list(old) && !base::is.data.frame(old)) {
        return(c(old, list(new)))
      }

      c(old, new)
    }
  )
)
