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

mle_solver_newton_logit <- function(X, y, max_iter = 100, tol_abs = 1e-6, tol_rel = 1e-6) {
  # Initialize beta as a zero vector
  beta <- rep(0, ncol(X))

  # Compute initial log-likelihood
  ll_old <- -logit_neg_log(beta, X, y)

  for (iter in 1:max_iter) {
    # Compute gradient and Hessian
    grad <- logit_neg_log_grad(beta, X, y)   # Gradient
    H <- logit_hessian(beta, X, y)           # Hessian

    # Check if Hessian is singular (ill-conditioned)
    if (det(H) < 1e-10) {  # Indicates singular matrix
      warning("Hessian is nearly singular; Newton's method may not converge.")
      return(list(coefficients = beta, iterations = iter, converged = FALSE))
    }

    # Solve for Newton update step
    beta_update <- tryCatch(
      solve(H, grad),
      error = function(e) {
        warning("Newton's method failed: Singular Hessian.")
        return(rep(NA, length(beta)))  # Return NA vector
      }
    )

    # If solving failed, return early
    if (any(is.na(beta_update))) {
      return(list(coefficients = beta, iterations = iter, converged = FALSE))
    }

    # Update beta
    beta_new <- beta - beta_update

    # Compute new log-likelihood
    ll_new <- -logit_neg_log(beta_new, X, y)

    # Compute absolute and relative change in log-likelihood
    ll_abs_change <- abs(ll_new - ll_old)
    ll_rel_change <- ll_abs_change / (abs(ll_old) + 1e-10) # Avoid division by 0

    # Check convergence
    if (ll_abs_change < tol_abs && ll_rel_change < tol_rel) {
      message("Converged in ", iter, " iterations.")
      return(list(coefficients = beta_new, iterations = iter, converged = TRUE))
    }

    # Update beta and log-likelihood for the next iteration
    beta <- beta_new
    ll_old <- ll_new
  }

  warning("Reached max iterations (", max_iter, ") without convergence. Newton's method may have failed.")
  return(list(coefficients = beta, iterations = max_iter, converged = FALSE))
}