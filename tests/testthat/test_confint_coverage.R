test_that("Confidence intervals achieve nominal coverage with Monte Carlo", {
  skip_if_not(Sys.getenv("MANUAL_TEST") != "",
              "Skipping long-running test for confidence interval coverage.")

  set.seed(1918)
  n_sim <- 1000
  n_obs <- 100
  n_pred <- 3
  nominal <- 0.95

  covs <- c(
    "Linear (QR)"    = simulate_coverage(n_sim, n_obs, n_pred, model = "linear", solver_option = "linalg"),
    "Linear (BFGS)"  = simulate_coverage(n_sim, n_obs, n_pred, model = "linear", solver_option = "BFGS"),
    "Logit (Newton)" = simulate_coverage(n_sim, n_obs, n_pred, model = "logistic", solver_option = "Newton"),
    "Logit (BFGS)"   = simulate_coverage(n_sim, n_obs, n_pred, model = "logistic", solver_option = "BFGS")
  )

  mc_se <- sqrt(nominal * (1 - nominal) / n_sim)
  lower_bound <- nominal - 1.96 * mc_se
  upper_bound <- nominal + 1.96 * mc_se

  for (lab in names(covs)) {
    check_coverage(covs[lab], lab, nominal, lower_bound, upper_bound)
  }
})