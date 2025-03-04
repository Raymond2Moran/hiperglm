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
  # Calculate working response and weights
  eta <- X %*% beta
  p_hat <- expit(eta)
  weights <- p_hat * (1 - p_hat)
  working_response <- eta + (y - p_hat) / weights
  W_sqrt <- as.vector(sqrt(weights))

  weight_matrix <- outer(W_sqrt, rep(1, ncol(X)))
  X_weighted <- weight_matrix * X
  y_weighted <- working_response * W_sqrt

  # Solve weighted least squares problem
  if (solver == "QR") {
    qr_decomp <- qr(X_weighted)
    beta_new <- qr.solve(qr_decomp, y_weighted)
  } else if (solver == "pseudo-inverse") {
    beta_new <- solve(crossprod(X_weighted), crossprod(X_weighted, y_weighted))
  } else {
    stop("Unsupported solver type. Use 'QR' or 'pseudo-inverse'.")
  }

  # Check for large step sizes (potential divergence)
  if (max(abs(beta_new - beta)) > step_thres) {
    warning("Newton's method is diverging: Step size too large. Possible complete separation.")
    return(list(beta_new = beta, failed = TRUE))
  }

  # Check for perfect separation
  p_hat_new <- expit(X %*% beta_new)
  if (any(p_hat_new < 1e-10 | p_hat_new > 1 - 1e-10)) {
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

extract_R_matrix <- function(qr_decomp) {
  R <- qr.R(qr_decomp)
  R[, order(qr_decomp$pivot)]
}