test_that("constructed gradient is consistent with numerical gradient", {
  set.seed(123)
  n_obs <- 10
  n_pred <- 3
  X <- matrix(rnorm(n_obs * n_pred), nrow = n_obs)
  beta_true <- rnorm(n_pred)
  y <- X %*% beta_true + rnorm(n_obs)

  beta_test <- rnorm(n_pred)

  # Gradient via previously constructed log likelihood function
  g_analytical <- lm_neg_log_grad(beta_test, X, y, noise_var = 1)

  # Numerical gradient
  test_func <- function(b) lm_neg_log(b, X, y, noise_var = 1)
  g_numerical <- approx_grad(test_func, beta_test)

  # Compare
  expect_true(
    are_all_close(g_analytical, g_numerical, abs_tol = 1e-5, rel_tol = 1e-5),
    info = sprintf(
      "Analytical = %s, Numerical = %s",
      toString(g_analytical),
      toString(g_numerical)
    )
  )
})