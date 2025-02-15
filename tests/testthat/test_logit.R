test_that("logistic gradient is consistent with numerical gradient", {
  set.seed(123)
  n_obs <- 10
  n_pred <- 3
  X <- matrix(rnorm(n_obs * n_pred), nrow = n_obs, ncol = n_pred)
  beta_true <- rnorm(n_pred)
  
  # Generate binary outcomes using logistic model
  probs <- 1 / (1 + exp(-X %*% beta_true))
  y <- rbinom(n_obs, 1, probs)

  beta_test <- rnorm(n_pred)

  # Compute analytical gradient using logistic log-likelihood gradient function
  g_analytical <- logistic_neg_log_grad(beta_test, X, y)

  # Compute numerical gradient using finite difference approximation
  test_func <- function(b) logistic_neg_log(b, X, y)
  g_numerical <- approx_grad(test_func, beta_test)

  # Compare analytical vs numerical gradients
  expect_true(
    are_all_close(g_analytical, g_numerical, abs_tol = 1e-5, rel_tol = 1e-5),
    info = sprintf(
      "Analytical = %s, Numerical = %s",
      toString(g_analytical),
      toString(g_numerical)
    )
  )
})