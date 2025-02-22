mle_solver_bfgs <- function(X, y, model = "linear") {
  if (model == "linear") {
    nll <- function(beta) lm_neg_log(beta, X, y, noise_var = 1)
    grad_nll <- function(beta) lm_neg_log_grad(beta, X, y, noise_var = 1)
  } else if (model == "logistic") {
    nll <- function(beta) logit_neg_log(beta, X, y)
    grad_nll <- function(beta) logit_neg_log_grad(beta, X, y)
  }

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