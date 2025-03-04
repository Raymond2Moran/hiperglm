mle_solver_linalg <- function(X, y) {
  beta_hat <- qr.solve(X, y)
  return(list(coefficients = as.vector(beta_hat)))
}