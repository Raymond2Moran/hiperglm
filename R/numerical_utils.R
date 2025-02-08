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

approx_grad <- function(func, x, dx = .Machine$double.eps^(1/3)) {
  numerical_grad <- rep(0, length(x))
  # Fill in
  for (i in seq_along(x)) {
    x_next <- x
    x_back <- x
    x_next[i] <- x_next[i] + dx
    x_back[i] <- x_back[i] - dx
    f_next <- func(x_next)
    f_back <- func(x_back)
    numerical_grad[i] <- (f_next - f_back) / (2 * dx)
  }
  return(numerical_grad)
}