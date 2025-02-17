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

mle_solver_newton_logit <- function(X, y, max_iter = 100, tol_abs = 1e-6, tol_rel = 1e-6, step_thres = 1e3) {
  # Initialize beta as a zero vector, compute initial log-likelihood
  beta <- rep(0, ncol(X))
  ll_old <- -logit_neg_log(beta, X, y)

  for (iter in 1:max_iter) {
    grad <- logit_neg_log_grad(beta, X, y)   # Gradient
    H <- logit_hessian(beta, X, y)           # Hessian

    # Check if Hessian is singular (ill-conditioned)
    if (min(abs(eigen(H)$values)) < 1e-10) {  # Eigenvalues check for singularity
      warning("Hessian is nearly singular; possible complete separation. Newton's method may not converge.")
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

    # Check for large step sizes (potential divergence)
    if (max(abs(beta_update)) > step_thres) {
      warning("Newton's method is diverging: Step size too large. Possible complete separation.")
      return(list(coefficients = beta, iterations = iter, converged = FALSE))
    }

    # Update beta, compute new log-likelihood
    beta_new <- beta - beta_update
    ll_new <- -logit_neg_log(beta_new, X, y)

    # Check for complete separation (probabilities too close to 0 or 1)
    p_hat <- 1 / (1 + exp(-X %*% beta_new))
    print(p_hat)
    if (any(p_hat < 1e-10 |p_hat > 1 - 1e-10)) {
      warning("Perfect separation detected: Coefficients diverging to infinity.")
      return(list(coefficients = beta, iterations = iter, converged = FALSE))
    }

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