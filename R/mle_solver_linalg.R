mle_solver_linalg <- function(X, y) {
  # Solve MLE via pseudo-inverse
  # Uses QR factorization and avoids computing inverse
  
  beta_hat <- qr.solve(X, y)
  return(list(coefficients = as.numeric(beta_hat)))
}