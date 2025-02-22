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
#'               - `"Newton"` for Newton's method (logistic model only)
#' @return A list containing the fitted model parameters, including:
#' \item{coefficients}{Estimated model coefficients}
#' \item{solver}{Optimization method used}
#' \item{model}{The type of model fitted}
#' \item{convergence}{Indicates whether optimization converged (logit)}
#' 
#' @export
hiper_glm <- function(design, outcome, model = "linear", option = list()) {
  solver <- option$mle_solver
  if (is.null(solver)) {
    solver <- if (model == "linear") "linalg" else "BFGS"
  }

  if (!model %in% c("linear", "logistic")) {
    stop("Unsupported model type: '", model, "'. Supported models: 'linear', 'logistic'.")
  }

  available_solvers <- c("linalg", "BFGS", "Newton")
  if (!solver %in% available_solvers) {
    stop("Unsupported solver: '", solver, "'. Supported solvers: ",
         paste(available_solvers, collapse = ", "), ".")
  }

  out <- switch(
    solver,
    "linalg" = if (model == "linear") mle_solver_linalg(design, outcome)
               else stop("linalg solver is only available for the linear model."),
    "BFGS"   = mle_solver_bfgs(design, outcome, model),
    "Newton" = if (model == "logistic") mle_solver_newton_logit(design, outcome)
               else stop("Newton solver is only available for the logistic model.")
  )

  hglm_out <- list(
    model        = model,
    solver       = solver,
    coefficients = as.numeric(out$coefficients),
    convergence  = !is.null(out$converged) && out$converged
  )

  class(hglm_out) <- "hiper_glm"  # Assign S3 class
  return(hglm_out)
}