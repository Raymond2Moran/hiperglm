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