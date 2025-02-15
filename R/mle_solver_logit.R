mle_solver_bfgs_logit <- function(X, y) {
  # MLE via `stats::optim()`'s BFGS for Logistic Regression

  # Negative log-likelihood for logistic regression
  nll <- function(beta) logit_neg_log(beta, X, y)

  # Gradient of the negative log-likelihood
  grad_nll <- function(beta) logit_neg_log_grad(beta, X, y)

  # Define initial starting point (zero coefficients)
  init_param <- rep(0, ncol(X))

  # Perform optimization using BFGS
  fit <- optim(
    par     = init_param,
    fn      = nll,
    gr      = grad_nll,
    method  = "BFGS"
  )

  # Check if optimization has converged
  if (fit$convergence != 0) {
    warning(
      "BFGS did not converge. Convergence code: ", fit$convergence,
      ". Refer to optim() documentation for more details."
    )
  }

  # Return results
  return(list(
    coefficients = fit$par,
    value = fit$value,
    convergence = fit$convergence
  ))
}