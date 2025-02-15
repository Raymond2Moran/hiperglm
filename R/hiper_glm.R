#' Fit a Generalized Linear Model Using Different Optimization Methods
#'
#' This function fits a generalized linear model using either a pseudo-inverse 
#' method (linear algebra approach) or BFGS optimization.
#'
#' @param design A numeric matrix representing the design matrix (predictors).
#' @param outcome A numeric vector representing the response variable.
#' @param model A character string specifying the type of model (default: `"linear"`).
#'              Supported models: `"linear"`, `"logistic"`.
#' @param option A list specifying additional options, such as `mle_solver`, which can be:
#'               - `"linalg"` for the linear algebra solver (linear model only)
#'               - `"BFGS"` for optimization-based MLE (for both linear and logistic models)
#' @return A list containing the fitted model parameters, including:
#' \item{coefficients}{Estimated model coefficients}
#' \item{solver}{Optimization method used}
#' \item{model}{The type of model fitted}
#' 
#' @export
hiper_glm <- function(design, outcome, model = "linear", option = list()) {
  # Extract solver options (default: `linalg` for linear models)
  solver <- option$mle_solver
  if (is.null(solver)) {
    solver <- ifelse(model == "linear", "linalg", "BFGS")  # default to BFGS for logistic
  }

  if (model == "linear") {
    out <- switch(
      solver,
      "linalg" = mle_solver_linalg(design, outcome),  # Pseudo-inverse approach
      "BFGS"   = mle_solver_bfgs(design, outcome),
      stop("Unsupported solver: ", solver)
    )
  } else if (model == "logistic") {
    if (solver != "BFGS") {
      stop("Logistic model currently only supports BFGS optimization.")
    }
    out <- mle_solver_bfgs_logit(design, outcome)  # MLE via BFGS for logistic
  } else {
    stop("Unsupported model type: '", model, "'. Supported models: 'linear', 'logistic'.")
  }

  hglm_out <- list(
    model     = model,
    solver    = solver,
    coefficients = as.numeric(out$coefficients),
    convergence  = out$convergence
    # Other options ...
  )
  class(hglm_out) <- "hiper_glm" # Make sure it is S3
  return(hglm_out)
}