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
#' This function computes the estimated variance-covariance matrix of the 
#' fitted `hiper_glm` model. 
#' For linear models, it computes \(\hat{\sigma}^2 (X^\top X)^{-1}\) using a QR-based approach. 
#' For logistic models, it computes the variance-covariance matrix by inverting the observed Fisher information matrix. 
#' If an alternative Fisher information matrix is provided in the `info_mat` element of the model object, that matrix is used; 
#' otherwise, it is computed using `logit_hessian()`. The function employs QR decomposition and a QR-based solver (`qr.solve()`) 
#' to improve numerical stability.
#'
#' @param hglm_out A fitted `hiper_glm` model object.
#' @return A numeric matrix representing the estimated variance-covariance matrix 
#' of the model coefficients.
#' @export
vcov.hiper_glm <- function(hglm_out) {
  if (!inherits(hglm_out, "hiper_glm")) {
    stop("Input is not a valid `hiper_glm` object.")
  }

  if (hglm_out$model == "linear") {
    X <- hglm_out$design
    y <- hglm_out$outcome
    beta_hat <- hglm_out$coefficients

    n <- nrow(X)
    p <- ncol(X)

    residuals <- y - X %*% beta_hat
    sigma2_hat <- sum(residuals^2) / (n - p)

    qr_decomp <- qr(X)
    R <- extract_R_matrix(qr_decomp)
    XtX_inv <- chol2inv(R)
    vcov_matrix <- sigma2_hat * XtX_inv

  } else if (hglm_out$model == "logistic") {
    X <- hglm_out$design
    beta_hat <- hglm_out$coefficients

    # Calculate weights and weighted design matrix
    eta <- X %*% beta_hat
    p_hat <- expit(eta)
    weights <- p_hat * (1 - p_hat)
    W_sqrt <- as.vector(sqrt(weights))

    weight_matrix <- outer(W_sqrt, rep(1, ncol(X)))
    X_weighted <- weight_matrix * X

    # QR decomposition of weighted design matrix
    qr_decomp <- qr(X_weighted)
    R <- extract_R_matrix(qr_decomp)
    vcov_matrix <- chol2inv(R)

  } else {
    stop("`vcov()` is only implemented for linear and logistic models.")
  }

  return(vcov_matrix)
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
