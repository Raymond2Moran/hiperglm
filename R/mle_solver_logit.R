mle_solver_bfgs_logit <- function(X, y) {
  nll <- function(beta) logit_neg_log(beta, X, y)
  grad_nll <- function(beta) logit_neg_log_grad(beta, X, y)
  init_param <- rep(0, ncol(X))
  fit <- optim(
    par     = init_param,
    fn      = nll,
    gr      = grad_nll,
    method  = "BFGS"
  )
  
  if (fit$convergence != 0) {
    warning(
      "BFGS did not converge. Convergence code: ", fit$convergence,
      ". Refer to optim() documentation for more details."
    )
  }

  return(list(
    coefficients = fit$par,
    value = fit$value,
    convergence = fit$convergence
  ))
}

mle_solver_newton_logit <- function(X, y, max_iter = 100, tol_abs = 1e-6, tol_rel = 1e-6, step_thres = 1e3) {
  beta <- rep(0, ncol(X))
  ll_old <- -logit_neg_log(beta, X, y)

  for (iter in 1:max_iter) {
    grad <- logit_neg_log_grad(beta, X, y)   # Gradient
    H <- logit_hessian(beta, X, y)           # Hessian

    # Check if Hessian is singular (ill-conditioned)
    if (min(abs(eigen(H)$values)) < 1e-10) {
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

    # Return early if necessary
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

    ll_abs_change <- abs(ll_new - ll_old)
    ll_rel_change <- ll_abs_change / abs(ll_old)

    # Check convergence
    if (are_all_close(ll_new, ll_old, abs_tol = tol_abs, rel_tol = tol_rel)) {
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