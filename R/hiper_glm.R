#' Fit a Generalized Linear Model Using Different Optimization Methods
#'
#' This function fits a generalized linear model using either a pseudo-inverse 
#' method (linear algebra approach) or BFGS optimization.
#'
#' @param design A numeric matrix representing the design matrix (predictors).
#' @param outcome A numeric vector representing the response variable.
#' @param model A character string specifying the type of model (default: `"linear"`).
#' @param option A list specifying additional options, such as `mle_solver`, which can be `"linalg"` for the linear algebra solver or `"BFGS"` for optimization-based MLE.
#'
#' @return A list containing the fitted model parameters, including:
#' \item{coefficients}{Estimated model coefficients}
#' \item{solver}{Optimization method used}
#' \item{model}{The type of model fitted}
#' 
#' @export
hiper_glm <- function(design, outcome, model = "linear", option = list()) {
  # Extract solver options (default `linalg`)
  solver <- option$mle_solver
  if (is.null(solver)) {
    solver <- "linalg"  # default
  }

  if (model == "linear") {
    out <- switch(
      solver,
      "linalg" = mle_solver_linalg(design, outcome),  # pseudo-inverse approach
      "BFGS"   = mle_solver_bfgs(design, outcome),
      stop("Unsupported solver: ", solver)
    )
  } else {
    stop("model = '", model, "' not implemented yet.")
  }

  hglm_out <- list(
    model     = model,
    solver    = solver,
    coefficients = as.numeric(out$coefficients)
    # Other options ...
  )
  class(hglm_out) <- "hiper_glm" # Make sure it is S3
  return(hglm_out)
}