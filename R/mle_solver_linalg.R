mle_solver_linalg <- function(X, y) {
  # Uses QR factorization and avoids computing inverse
  qr_decomp <- qr(X)
  R <- qr.R(qr_decomp)
  Qty <- qr.qty(qr_decomp, y)

  beta_hat <- backsolve(R, Qty[1:ncol(X)])

  return(list(coefficients = as.vector(beta_hat)))
}