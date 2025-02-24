mle_solver_newton_logit <- function(X, y, max_iter = 100, tol_abs = 1e-6, tol_rel = 1e-6, step_thres = 1e3) {
  beta <- rep(0, ncol(X))
  ll_old <- -logit_neg_log(beta, X, y)
  converged <- FALSE
  iter <- 1

  while(! converged && iter <= max_iter) {
    grad <- logit_neg_log_grad(beta, X, y)   # Gradient
    H <- logit_hessian(beta, X, y)           # Hessian

    # Check if Hessian is singular (ill-conditioned)
    if (min(abs(eigen(H)$values)) < 1e-10) {
      warning("Hessian is nearly singular; possible complete separation. Newton's method may not converge.")
      break
    }

    # Solve for Newton update step
    beta_update <- tryCatch(
      solve(H, grad),
      error = function(e) {
        warning("Newton's method failed: Singular Hessian.")
        return(rep(NA, length(beta)))  # Return NA vector
      }
    )

    # Exit early if necessary
    if (any(is.na(beta_update))) break

    # Check for large step sizes (potential divergence)
    if (max(abs(beta_update)) > step_thres) {
      warning("Newton's method is diverging: Step size too large. Possible complete separation.")
      break
    }

    beta_new <- beta - beta_update
    ll_new <- -logit_neg_log(beta_new, X, y)

    p_hat <- 1 / (1 + exp(-X %*% beta_new))

    if (any(p_hat < 1e-10 | p_hat > 1 - 1e-10)) {
      warning("Perfect separation detected: Coefficients diverging to infinity.")
      break
    }

    converged <- are_all_close(ll_new, ll_old, abs_tol = tol_abs, rel_tol = tol_rel)

    # Check convergence
    if (are_all_close(ll_new, ll_old, abs_tol = tol_abs, rel_tol = tol_rel)) {
      message("Converged in ", iter, " iterations.")
      return(list(coefficients = beta_new, iterations = iter, converged = TRUE))
    }

    # Update beta and log-likelihood for the next iteration
    beta <- beta_new
    ll_old <- ll_new
    iter <- iter + 1
  }

  warning("Reached max iterations (", max_iter, ") without convergence. Newton's method may have failed.")
  return(list(coefficients = beta, iterations = max_iter, converged = FALSE))
}