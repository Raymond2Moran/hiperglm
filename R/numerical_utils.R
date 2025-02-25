lm_neg_log <- function(beta, X, y, noise_var = 1) {
  residual <- y - X %*% beta
  0.5 * sum(residual^2) / noise_var
}

lm_neg_log_grad <- function(beta, X, y, noise_var = 1) {
  residual <- y - X %*% beta
  grad_vec <- - crossprod(X, residual) / noise_var
  as.vector(grad_vec)
}

expit <- function(eta) {
  1 / (1 + exp(-eta))
}

logit_neg_log <- function(beta, X, y) {
  eta <- X %*% beta
  log_likelihood <- sum(y * eta - log1p(exp(eta)))
  -log_likelihood
}

logit_neg_log_grad <- function(beta, X, y) {
  eta <- X %*% beta
  p <- expit(eta)
  grad <- t(X) %*% (y - p)
  -as.vector(grad)
}

logit_hessian <- function(beta, X, y) {
  eta <- X %*% beta
  p <- expit(eta)
  W <- diag(as.vector(p * (1 - p)))
  H <- t(X) %*% W %*% X
  return(H)
}

take_one_newton_step <- function(beta, X, y, step_thres = 1e3, solver = "QR") {
  grad <- logit_neg_log_grad(beta, X, y)   # Gradient
  H <- logit_hessian(beta, X, y)           # Hessian

  # Check if Hessian is singular (ill-conditioned)
  if (min(abs(eigen(H)$values)) < 1e-10) {
    warning("Hessian is nearly singular; possible complete separation. Newton's method may not converge.")
    return(list(beta_new = beta, failed = TRUE))
  }

  if (solver == "QR") {
    qr_decomp <- qr(H)
    beta_update <- qr.solve(qr_decomp, grad)
  } else if (solver == "pseudo-inverse") {
    beta_update <- solve(H, grad)
  } else {
    stop("Unsupported solver type. Use 'QR' or 'pseudo-inverse'.")
  }

  # Exit early if necessary
  if (any(is.na(beta_update))) {
    return(list(beta_new = beta, failed = TRUE))
  }

  # Check for large step sizes (potential divergence)
  if (max(abs(beta_update)) > step_thres) {
    warning("Newton's method is diverging: Step size too large. Possible complete separation.")
    return(list(beta_new = beta, failed = TRUE))
  }

  beta_new <- beta - beta_update
  p_hat <- 1 / (1 + exp(-X %*% beta_new))

  if (any(p_hat < 1e-10 | p_hat > 1 - 1e-10)) {
    warning("Perfect separation detected: Coefficients diverging to infinity.")
    return(list(beta_new = beta, failed = TRUE))
  }

  return(list(beta_new = beta_new, failed = FALSE))
}

are_all_close <- function(v, w, abs_tol = 1e-6, rel_tol = 1e-6) {
  abs_diff <- abs(v - w)
  are_all_within_atol <- all(abs_diff < abs_tol)
  are_all_within_rtol <- all(abs_diff < rel_tol * pmax(abs(v), abs(w)))
  return(are_all_within_atol && are_all_within_rtol)
}
