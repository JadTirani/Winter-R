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
assert = function(passthru_msg, term = FALSE) { # nolint: assignment_linter.
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
    
    #' @description Gets the dataframe for the object 
    #' 
    #' @param ... (params) Can specify which columns to pull
    #' 
    #' @export
    get_df = function(...) {
      if (length(list(...)) == 0) {
        return(private$df)
      } 
        
      cols = private$df[, as.character(list(...)),drop = FALSE]
      return(cols)
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
      #invisible(self)
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
    
    # #' @description Runs the modified nlm.ls function with Levenberg-Marquardt algorithm
    # #' Designed to take in a residual function along with 
    # #' 
    # #' @param passthru_fncsparams (list) arguments for optimization
    # #' @param passthru_fncsparams (quote) body for optimization function
    # #' @param passthru_start (numeric vector) starting guess for nls
    # #' @param ... (params) Use to specify the columns of the dataframe your using
    # #' 
    # #' @return self - Allows chaining
    # #' 
    # #' @import minpack.lm
    # #' @export
    # run_nlslm = function(passthru_fncsparams, passthru_fncsbody, passthru_start) {
    #   
    #   fncs = runtime_function(passthru_fncsparams, passthru_fncsbody)
    #   resi_params = c(list("p","obs"),passthru_fncsparams)
    #   resi_fncs = runtime_function(resi_params, quote(obs-fncs()))
    #   
    #   minpack.lm::nls.lm(par=passthru_start, fn = passthru_resi, ...)
    # },

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
      ggobj = NA
      if (!missing(passthru_df)) {
        ggobj = ggplot2::ggplot(passthru_df, ...)
      } else {
        ggobj = ggplot2::ggplot(private$df, ...)
      }
      # ggobj = ggobj + 
      #   ggplot2::geom_point() + 
      #   ggplot2::theme_classic() + 
      #   ggplot2::theme(legend.position = "none") +
      #   ggplot2::coord_cartesian(clip = "off")
      # return(ggobj)
    },

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

#' @title Linear Regression
#' 
#' @description Preforms a linear regression and returns the lm object
#' 
#' @param passthru_data A dataframe or y~x style function for the model to analyze
#' @param X The X variable of your regression
#' @param Y The Y variable of your regression
#' 
#' @return (list) 3 elements in order of params, std error, and the model
#' 
#' @export
start_lm = function(passthru_data, X, Y) {
  formula_str <- paste(Y, "~", paste(X, collapse = " + "))
  temp_lm = stats::lm(formula_str, data=passthru_data)
  passout_params = coef(temp_lm)
  passout_stderr = summary(temp_lm)$coefficients[,2]
  return(list(passout_params, passout_stderr, temp_lm))
}

#' @title Non-Least Squares Regression
#' 
#' @description Preforms a nls and returns the nls object
#' 
#' @param passthru_data A dataframe or y~x style function for the model to analyze
#' @param passthru_fncs the function that the template function the fit is trying to replicate
#' @param passthru_start starting parameters for the model to run
#' 
#' @return (list) 3 elements in order of params, std error, and the model
#' 
#' @export
start_nls = function (passthru_data, 
                      passthru_fncs, 
                      passthru_start = NA) {
  temp_nls = stats::nls(formula = passthru_fncs, data = passthru_data, start = passthru_start)
  passout_params = coef(temp_nls)
  passout_stderr = summary(temp_nls)$coefficients[,2]
  return(list(passout_params, passout_stderr, temp_nls))
}

#' @title Non-Linear Regression
#' 
#' @description Preforms a non-linear regression using the e Levenberg-Marquardt algorithm and returns the nls.lm object
#' 
#' @param passthru_resifncs the residual function constructed between observed points and template points to minimize
#' @param passthru_start starting parameters for the model to run
#' @param ... (params) MUST match the inputs of the templte function used besides optimization parameter (often in the format obs=data, var1=data1, var2=data2...)
#' 
#' @return (list) 3 elements in order of params, std error, and the model
#' 
#' @import minpack.lm
#' @export
start_nls_lm = function(passthru_resifncs, 
                        passthru_start, 
                        ...) {
  temp_nls_lm = minpack.lm::nls.lm(par = passthru_start,
                                   fn=passthru_resifncs,
                                   ...)
  passout_params = coef(temp_nls_lm)
  passout_stderr = summary(temp_nls_lm)$coefficients[,2]
  return(list(passout_params, passout_stderr, temp_nls_lm))
}

#' @title Function Overlay
#' 
#' @description Overlayed function on the plot
#' 
#' @export
ggdefault = function() {
  return(list(ggplot2::geom_point(), 
              ggplot2::theme_classic(),
              ggplot2::theme(legend.position = "none"), 
              ggplot2::coord_cartesian(clip = "off")))
}

#' @title ggplot2 Function Overlay
#' 
#' @description Overlayed function on the plot
#' 
#' @param passthru_plot physical ggplot object
#' @param passthru_fncs (function/list(function)) overlayed function on the plot
#' @param passthru_colour (colour/ list(colour))
#' @param ... (params - opt) passthru styling for the function overlayed  
#' 
#' @export 
overlay_curve = function(passthru_plot, passthru_fncs, passthru_colour, ...) {
  return_curve = list()
  if (typeof(passthru_fncs) == "list") {
    for (i in 1:length(passthru_fncs)) {
      return_curve[[i]] = ggplot2::stat_function(fun = passthru_fncs[[i]],
                                                 color = passthru_colour[[i]],
                                                 ...)
    }
  } else {
    return_curve[[1]] = ggplot2::stat_function(fun = passthru_fncs,
                                               color = passthru_colour,
                                               ...)
  }

  return(passthru_plot + return_curve)
}


#' #' @title Function Overlay
#' #' 
#' #' @description Overlayed function on the plot
#' #' 
#' #' @param passthru_fncs (function - opt) overlayed function on the plot
#' #' @param ... (params - opt) passthru styling for the function overlayed  
#' #' 
#' #' @export 
#' overlay_fncs = function(passthru_fncs, ...) {
#'   curve(passthru_fncs, add=TRUE, ...)
#' }


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

#' @title Creates a function at run time
#' 
#' @description Pass through the function parameters, and the body
#' 
#' @param var_names (list) passthrough the parameters for the new function
#' @param expression (quote) pass through the function body
#' 
#' @export 
runtime_function <- function(var_names, expression) {
  formals_list <- lapply(var_names, function(name) alist(a=)$a)
  names(formals_list) <- var_names
  
  function_body <- quote({
    eval(expression)
  })
  
  # Create the new function
  new_function <- as.function(c(formals_list, function_body))
  return(new_function)
}

#' @title ggplot2 default settings
#' 
#' @description applys the default settings for a graph to a passed in ggplot
#' 
#' @param passthru_gg pass through a ggplot
#'
#' @export
ggthemedefault = function(passthru_gg) {
  passthru_gg = passthru_gg +
    ggplot2::geom_point() +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_cartesian(clip = "off")
  return(passthru_gg)
}