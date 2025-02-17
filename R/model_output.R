#' Extract estimated coefficients from a hiper_glm object
#'
#' @param hglm_out A fitted hiper_glm model object
#' @return A numeric vector of estimated coefficients
#' @export
coef.hiper_glm <- function(hglm_out) {
  if (!"hiper_glm" %in% class(hglm_out)) {
    stop("Input is not a valid hiper_glm object.")
  }

  if (!is.null(hglm_out$coefficients)) {
    return(as.numeric(hglm_out$coefficients))
  } else {
    stop("No coefficients found in the hiper_glm object.")
  }
}

#' Extract the variance-covariance matrix from a hiper_glm object
#'
#' This function returns the estimated variance-covariance matrix of the fitted `hiper_glm` model.
#' Currently, this function is not implemented and will issue a warning.
#'
#' @param hglm_out A fitted `hiper_glm` model object.
#' @return A warning indicating that the function is not yet implemented.
#' @export
vcov.hiper_glm <- function(hglm_out) {
  warning("This function is yet to be implemented.")
}

#' Print a summary of a hiper_glm object
#'
#' This function prints a summary of the fitted `hiper_glm` model, displaying the model type and solver used.
#'
#' @param hglm_out A fitted `hiper_glm` model object.
#' @return A placeholder message indicating the model output.
#' @export
print.hiper_glm <- function(hglm_out) {
  cat("Output of hiper_glm.\n")
}
