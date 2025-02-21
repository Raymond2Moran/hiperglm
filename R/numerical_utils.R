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
  return(1 / (1 + exp(-eta)))
}

logit_neg_log <- function(beta, X, y) {
  eta <- X %*% beta
  log_likelihood <- sum(y * eta - log1p(exp(eta)))
  return(-log_likelihood)
}

logit_neg_log_grad <- function(beta, X, y) {
  eta <- X %*% beta
  p <- expit(eta)
  grad <- t(X) %*% (y - p)
  return(-as.vector(grad))
}

logit_hessian <- function(beta, X, y) {
  eta <- X %*% beta
  p <- expit(eta)
  W <- diag(as.vector(p * (1 - p)))
  H <- t(X) %*% W %*% X
  return(H)
}