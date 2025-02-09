test_that("linalg and optim least-sq coincides", {
  n_obs <- 32
  n_pred <- 4

  data <- simulate_data(n_obs, n_pred, model = "linear", seed = 1918)

  design <- data$design
  outcome <- data$outcome

  via_linalg_out <- hiper_glm(design, outcome, model = "linear")
  via_bfgs_out <- hiper_glm(
    design, outcome, model = "linear", option = list(mle_solver = "BFGS")
  )

  expect_true(are_all_close(
    coef(via_linalg_out),
    coef(via_bfgs_out),
    abs_tol = 1e-2,
    rel_tol = 1e-2
  ))
})