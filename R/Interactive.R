Interactive <- R6::R6Class(
  "simcity_interactive",

  public = list(
    runner=NULL

  ), # end of public
  active = list(
    plot = function(obj) {
      if (missing(obj)) return(private$.plots)
      if (is.list(obj)) stop("obj$plot shuld be a function producing a plot")
      private$.plots[[length(private$.plots)+1]]<-obj
    }
  ), ### end of active
  private = list(
    .plots<-list()
  ) # end of private
