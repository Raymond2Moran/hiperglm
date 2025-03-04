test_that("Confidence intervals achieve nominal coverage with Monte Carlo", {
  skip_if_not(Sys.getenv("MANUAL_TEST") != "",
              "Skipping long-running test for confidence interval coverage.")

  set.seed(1918)
  n_sim <- 1000
  n_obs <- 100
  n_pred <- 3
  significance <- 0.95 # nominal coverage

  covs <- c(
    "Linear (QR)"    = simulate_coverage(n_sim, n_obs, n_pred, model = "linear", solver_option = "linalg", significance = significance),
    "Linear (BFGS)"  = simulate_coverage(n_sim, n_obs, n_pred, model = "linear", solver_option = "BFGS", significance = significance),
    "Logit (Newton)" = simulate_coverage(n_sim, n_obs, n_pred, model = "logistic", solver_option = "Newton", significance = significance),
    "Logit (BFGS)"   = simulate_coverage(n_sim, n_obs, n_pred, model = "logistic", solver_option = "BFGS", significance = significance)
  )

  mc_se <- sqrt(significance * (1 - significance) / n_sim)
  lower_bound <- significance - 1.96 * mc_se
  upper_bound <- significance + 1.96 * mc_se

  for (lab in names(covs)) {
    check_coverage(covs[lab], lab, significance, lower_bound, upper_bound)
  }
})