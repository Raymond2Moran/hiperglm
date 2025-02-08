mle_solver_bfgs <- function(X, y) {
  # MLE via `stats::optim()`'s BFGS

  # Negative log-likelihood
  nll <- function(beta) lm_neg_log(beta, X, y, noise_var = 1)

  # Gradient
  grad_nll <- function(beta) lm_neg_log_grad(beta, X, y, noise_var = 1)

  # Define initial starting point
  init_param <- rep(0, ncol(X))

  # Perform optimization
  fit <- optim(
    par     = init_param,
    fn      = nll,
    gr      = grad_nll,
    method  = "BFGS"
    # other params
  )

  return(list(
    coefficients = fit$par,
    value = fit$value,
    convergence = fit$convergence
  ))
}