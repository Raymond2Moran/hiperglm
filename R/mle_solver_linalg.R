mle_solver_linalg <- function(X, y) {
  X_eigen <- as.matrix(X)
  y_eigen <- as.matrix(y)

  beta_hat <- rcpp_qr_solve(X_eigen, y_eigen)

  return(list(
    coefficients = as.vector(beta_hat),
    solver = "linalg"
  ))
}