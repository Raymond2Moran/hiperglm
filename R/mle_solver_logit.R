mle_solver_newton_logit <- function(X, y, max_iter = 100, tol_abs = 1e-6, tol_rel = 1e-6, step_thres = 1e3) {
  beta <- rep(0, ncol(X))
  ll_old <- -logit_neg_log(beta, X, y)
  converged <- FALSE
  iter <- 1

  while (!converged && iter <= max_iter) {
    newton_step <- take_one_newton_step(beta, X, y, step_thres)

    if (newton_step$failed) {
      warning("Newton's method failed to update at iteration ", iter)
      return(list(coefficients = beta, iterations = iter, converged = FALSE))
    }

    beta_new <- newton_step$beta_new
    ll_new <- -logit_neg_log(beta_new, X, y)

    converged <- are_all_close(ll_new, ll_old, abs_tol = tol_abs, rel_tol = tol_rel)

    if (converged) {
      # message("Converged in ", iter, " iterations.")
      return(list(coefficients = beta_new, iterations = iter, converged = TRUE))
    }

    beta <- beta_new
    ll_old <- ll_new
    iter <- iter + 1
  }

  warning("Reached max iterations (", max_iter, ") without convergence. Newton's method may have failed.")
  return(list(coefficients = beta, iterations = max_iter, converged = FALSE))
}