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

mle_solver_newton_logit <- function(X, y, max_iter = 100) {
  # Initialize beta as a zero vector
  beta <- rep(0, ncol(X))
  
  for (iter in 1:max_iter) {
    # Compute gradient and Hessian
    grad <- logit_neg_log_grad(beta, X, y)   # Gradient
    H <- logit_hessian(beta, X, y)           # Hessian

    # Solve for Newton update step
    beta_update <- solve(H, grad)  # Equivalent to H_inv * grad

    # Update beta
    beta <- beta - beta_update

    # Print progress
    message("Iteration ", iter, ": ||update|| = ", sqrt(sum(beta_update^2)))
  }

  return(list(
    coefficients = beta,
    iterations = max_iter
  ))
}