are_all_close <- function(v, w, abs_tol = 1e-6, rel_tol = 1e-6) {
  abs_diff <- abs(v - w)
  are_all_within_atol <- all(abs_diff < abs_tol)
  are_all_within_rtol <- all(abs_diff < rel_tol * pmax(abs(v), abs(w)))
  return(are_all_within_atol && are_all_within_rtol)
}

simulate_data <- function(
    n_obs, n_pred, model = "linear", intercept = NULL,
    coef_true = NULL, design = NULL, seed = NULL, option = list()
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if ((model != "linear")  && !is.null(option$signal_to_noise)) {
    warning(paste(
      "The `signal_to_noise` option is currently unsupported for",
      "non-linear models and will be ignored."
    ))
  }
  if (is.null(coef_true)) {
    coef_true <- rnorm(n_pred, sd = 1 / sqrt(n_pred))
  } 
  if (is.null(design)) {
    design <- matrix(rnorm(n_obs * n_pred), nrow = n_obs, ncol = n_pred)
  }
  if (!is.null(intercept)) {
    if (!is.numeric(intercept)) {
      stop("The intercept argument must be numeric.")
    }
    coef_true <- c(intercept, coef_true)
    design <- cbind(rep(1, n_obs), design)
  }
  expected_mean <- as.vector(design %*% coef_true)

  if (model == "linear") {
    signal_to_noise <- option$signal_to_noise
    if (is.null(signal_to_noise)) {
      signal_to_noise <- 0.1
    }
    noise_magnitude <- sqrt(var(expected_mean) / signal_to_noise^2)
    noise <- noise_magnitude * rnorm(n_obs)
    outcome <- expected_mean + noise
  } else {
    n_trial <- option$n_trial
    prob <- 1 / (1 + exp(-expected_mean))
    # Object type of `outcome` returned by this function is variable. One should 
    # in general be careful about introducing this type of inconsistency, but 
    # sometimes one might find it the most natural and/or reasonable thing to do.
    if (is.null(n_trial)) {
      outcome <- rbinom(n_obs, 1, prob)
    } else {
      n_success <- rbinom(n_obs, n_trial, prob)
      outcome <- list(n_success = n_success, n_trial = n_trial)
    }
  }
  return(list(design = design, outcome = outcome, coef_true = coef_true))
}



approx_grad <- function(func, x, dx = .Machine$double.eps^(1 / 3)) {
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

approx_hessian <- function(beta, X, y, eps = 1e-6) {
  H_numeric <- matrix(0, length(beta), length(beta))

  for (j in seq_along(beta)) {
    beta_up <- beta
    beta_down <- beta
    beta_up[j] <- beta[j] + eps
    beta_down[j] <- beta[j] - eps

    grad_up <- logit_neg_log_grad(beta_up, X, y)
    grad_down <- logit_neg_log_grad(beta_down, X, y)

    H_numeric[, j] <- (grad_up - grad_down) / (2 * eps)
  }
  return(H_numeric)
}