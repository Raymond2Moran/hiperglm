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
  g_analytical <- logit_neg_log_grad(beta_test, X, y)

  # Compute numerical gradient using finite difference approximation
  test_func <- function(b) logit_neg_log(b, X, y)
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

test_that("logistic Hessian is consistent with numerical Hessian", {
  set.seed(123)
  n_obs <- 10
  n_pred <- 3
  X <- matrix(rnorm(n_obs * n_pred), nrow = n_obs, ncol = n_pred)
  beta_test <- rnorm(n_pred)

  # Generate binary outcomes using logistic model
  probs <- 1 / (1 + exp(-X %*% beta_test))
  y <- rbinom(n_obs, 1, probs)

  H_analytical <- logit_hessian(beta_test, X, y)
  H_numerical <- approx_hessian(beta_test, X, y)

  # Compare analytical vs numerical Hessian
  expect_true(
    are_all_close(H_analytical, H_numerical, abs_tol = 1e-5, rel_tol = 1e-5),
    info = sprintf(
      "Analytical Hessian = %s, Numerical Hessian = %s",
      toString(H_analytical),
      toString(H_numerical)
    )
  )
})

test_that("newton and bfgs outputs coincide on logit model", {
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "logistic", seed = 1918)
  design <- data$design
  outcome <- data$outcome
  via_newton_out <- hiper_glm(design, outcome, model = "logistic",
                              option = list(mle_solver = "Newton"))
  via_bfgs_out <- hiper_glm(
    design,
    outcome,
    model = "logistic",
    option = list(mle_solver = "BFGS")
  )
  expect_true(are_all_close(
    coef(via_newton_out),
    coef(via_bfgs_out),
    abs_tol = 1e-2,
    rel_tol = 1e-2
  ))
})