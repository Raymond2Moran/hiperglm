mle_solver_linalg <- function(X, y) {
  beta_hat <- rcpp_qr_solve(X, y)
  
  return(list(
    coefficients = as.vector(beta_hat),
    solver = "linalg"
  ))
}