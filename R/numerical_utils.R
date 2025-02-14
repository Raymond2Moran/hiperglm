lm_neg_log <- function(beta, X, y, noise_var = 1) {
  # Negative log-likelihood ignoring constants w.r.t. beta:
  residual <- y - X %*% beta
  0.5 * sum(residual^2) / noise_var
}

lm_neg_log_grad <- function(beta, X, y, noise_var = 1) {
  residual <- y - X %*% beta
  # gradient is -(X^T * residual) / noise_var
  grad_vec <- - crossprod(X, residual) / noise_var
  as.vector(grad_vec)
}

