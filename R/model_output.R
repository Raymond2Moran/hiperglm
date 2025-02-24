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
  if (hglm_out$model != "linear") {
    stop("Variance-covariance matrix is currently only implemented for linear models.")
  }
  
  X <- hglm_out$design
  y <- hglm_out$outcome
  beta_hat <- hglm_out$coefficients

  residuals <- y - X %*% beta_hat

  n <- nrow(X)
  p <- ncol(X)

  sigma2_hat <- sum(residuals^2) / (n - p)

  qr_decomp <- qr(X)
  R <- qr.R(qr_decomp)
  R <- R[, order(qr_decomp$pivot)]

  if (!are_all_close(X, qr.Q(qr_decomp) %*% R)) {
    warning("QR decomposition reconstruction failed. Potential numerical instability.")
  }

  XtX_inv <- chol2inv(R)
  vcov_matrix <- sigma2_hat * XtX_inv

  vcov_matrix
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
