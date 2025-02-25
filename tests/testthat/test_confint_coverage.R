simulate_coverage <- function(n_sim, n_obs, n_pred, model, solver_option = NULL) {

  if (is.null(solver_option)) {
    solver_option <- if (model == "linear") "linalg" else "BFGS"
  }

  coverage <- 0
  q_crit <- qchisq(0.95, df = n_pred)

  for (i in 1:n_sim) {
    data <- simulate_data(n_obs, n_pred, model = model, seed = i)
    design <- data$design
    outcome <- data$outcome
    beta_true <- data$coef_true

    fit <- hiper_glm(design, outcome, model = model, option = list(mle_solver = solver_option))
    beta_hat <- coef(fit)
    Sigma_hat <- vcov(fit)

    chol_Sigma <- chol(Sigma_hat)
    test_stat <- sum(solve(chol_Sigma, beta_true - beta_hat)^2)

    if (test_stat < q_crit) {
      coverage <- coverage + 1
    }
  }
  return(coverage / n_sim)
}

check_coverage <- function(coverage, label, nominal, lower, upper) {
  expect_true(
    coverage > lower && coverage < upper,
    info = sprintf("%s coverage = %.3f, Expected = %.3f, 95%% Monte Carlo Interval: [%.3f, %.3f]",
                   label, coverage, nominal, lower, upper)
  )
}

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