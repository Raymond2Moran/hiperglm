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

logit_neg_log <- function(beta, X, y) {
  # Return negative log-likelihood of logistic regression
  eta <- X %*% beta
  log_likelihood <- sum(y * eta - log1p(exp(eta)))
  return(-log_likelihood)
}

logit_neg_log_grad <- function(beta, X, y) {
  # Return negative gradient of logistic regression
  eta <- X %*% beta
  
  # Compute probabilities (sigmoid function)
  p <- 1 / (1 + exp(-eta))
  
  # Gradient
  grad <- t(X) %*% (y - p)

  return(-as.vector(grad))
}