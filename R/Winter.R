# To implement: library(Winter)
# To load: roxygen2::roxygenise("C:/Users/jadti/Desktop/R Scripts")

#' @title Assert Debugging Function
#' 
#' @description Function alerts the user of an error and terminates
#' 
#' @param passthru_msg Adds a message to the aleart
#' @param term Sets the function to terminating (TRUE - default) or continuing (FALSE) # nolint: line_length_linter.
#' 
#' @return Void Function
#' 
#' @export
assert = function(passthru_msg, term = TRUE) { # nolint: assignment_linter.
  if (term) {
    stop("[Winter] - Assert: ", passthru_msg, "\n", call. = FALSE)
  } else {
    cat("[Winter] - Assert: ", passthru_msg, "\n")
  }
}

#' @title Algorithm Class Creator
#' 
#' @description Wraps algorithm creation to chain initialization and running the algorithm
#' 
#' @param passthru_df (dataframe) Dataframe to pull info
#' @param passthru_fncs (Function) for nls to optimize
#' @param passthru_start (numeric vector) starting guess for nls
#' 
#' @export 
start_algorithm_nls = function(passthru_df, passthru_fncs, passthru_start) {
  obj = Algorithm$new(passthru_df)
  obj$run_nls(passthru_fncs, passthru_start)
  return(obj)
}

#' @title Algorithm Class
#' 
#' @description Class to do a non-linear minimization algorithm
#' 
#' @import R6
#' @export
Algorithm = R6::R6Class(
  "Algorithm",
  
  public = list(
    # === Functions ===

    #' @description Empty constructor
    #' 
    #' @param passthru_df (dataframe) Dataframe to pull info
    #' 
    #' @export
    initialize = function(passthru_df = NULL) {
      self$set_df(passthru_df)
    },
    
    #' @description Sets the dataframe for the object 
    #' 
    #' @param passthru_df (dataframe) Dataframe to pull info
    #' 
    #' @export
    set_df = function(passthru_df = NULL) {
      private$df = passthru_df
    },

    #' @description Runs the non-least squares model with passed in variables
    #' 
    #' @param passthru_fncs (function) For the nls to optimize
    #' @param passthru_start (numeric vector) starting guess for nls
    #' 
    #' @return self - Allows chaining
    #' 
    #' @export
    run_nls = function(passthru_fncs, passthru_start) {
      private$model = stats::nls(formula=passthru_fncs, data=private$df, start=passthru_start)
      self$params = coef(private$model)
      invisible(self)
    },

    #' @description Runs a linear model regression
    #' 
    #' @param passthru_df (dataframe) Dataframe to run the lm on
    #' 
    #' @return self - Allows chaining
    #' 
    #' @export
    run_lm = function(passthru_df = NA) {
      if (missing(passthru_df)) {
        private$model = stats::lm(private$df$V ~ private$df$S)
      } else {
        private$model = stats::lm(passthru_df)
      }
      self$params = coef(private$model)
      invisible(self)
    },

    #' @description Safley returns the parameters 
    #' 
    #' @return List - Empty if run() has not been called. Otherwise returns params
    #' 
    #' @export 
    solved_params = function() {
      if (is.null(self$params)) {
        assert("No parameters solved for the model", term = FALSE)
        return(list())
      }
      return(self$params)
    },

    #' @description Plots the data
    #' 
    #' @param ... (params) Feeds into styling the plot
    #' 
    #' @export
    plotter = function(...) {
      base::plot(private$df, ...)
      invisible(self)
    },

    #' @description GG2 Plotter
    #' 
    #' @param ... (params) Feeds into styling the plot
    #' @param passthru_df (dataframe) Dataframe to pull info (overides stored df)
    #' 
    #' @import ggplot2
    #' @export
    gg2_plotter = function(..., passthru_df = NA) {
      if (!missing(passthru_df)) {
        ggobj = ggplot2::ggplot(passthru_df, ...)
      } else {
        ggobj = ggplot2::ggplot(private$df, ...)
      }
      ggobj = ggobj + ggplot2::geom_point() + ggplot2::theme_classic() + ggplot2::theme(legend.position = "none")
      return(ggobj)
    }

    ,

    #' @description Function Overlay 
    #' 
    #' @param ... (params) Feeds plot styling
    #' 
    #' @export
    plus_overlay = function(...) {
      overlay_fncs(...)
    }
    ,

    # Public Members
    #' @field params (list) parameters solved for the model
    params = NULL
  )
  ,
  private = list(
    # Pass in members
    df = NULL,
    
    # Pass out members
    model = NULL
  )
)

#' @title Function Overlay
#' 
#' @description Overlayed function on the plot
#' 
#' @param passthru_fncs (function - opt) overlayed function on the plot
#' @param ... (params - opt) passthru styling for the function overlayed  
#' 
#' @export 
overlay_fncs = function(passthru_fncs, ...) {
  curve(passthru_fncs, add=TRUE, ...)
}


#' @title Windows VScode Plot Visualization
#' 
#' @description alters the output of plots created in VS code to display in a serperate window, This function is windows specific
#' 
#' @export 
graphical_enhancment = function() {
  if (Sys.info()[['sysname']] == "Windows") {
    grDevices::windows()
  }
}