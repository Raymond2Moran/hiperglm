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
    R <- qr.R(qr_decomp)
    R <- R[, order(qr_decomp$pivot)]  # Correct column ordering

    if (!are_all_close(X, qr.Q(qr_decomp) %*% R)) {
      warning("QR decomposition failed. Potential numerical instability.")
    }

    XtX_inv <- chol2inv(R)
    vcov_matrix <- sigma2_hat * XtX_inv

  } else if (hglm_out$model == "logistic") {

    X <- hglm_out$design
    y <- hglm_out$outcome
    beta_hat <- hglm_out$coefficients

    if (!is.null(hglm_out$info_mat)) {
      H <- hglm_out$info_mat
    } else {
      H <- logit_hessian(beta_hat, X, y)  # Positive Fisher information
    }

    if (!isSymmetric(H, tol = 1e-8)) {
    warning("Fisher information is not symmetric; check logit_hessian or provided info_mat.")
    }

    qr_decomp <- qr(H)
    R <- qr.R(qr_decomp)
    R <- R[, order(qr_decomp$pivot)]
    vcov_matrix <- qr.solve(qr_decomp)

    if (!are_all_close(H, qr.Q(qr_decomp) %*% R)) {
      warning("QR decomposition failed. Potential numerical instability in Fisher information.")
    }

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
