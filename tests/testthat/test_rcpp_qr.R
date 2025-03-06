test_that("Rcpp QR matches base R QR", {
  set.seed(123)
  X <- matrix(rnorm(100), 10, 10)
  y <- rnorm(10)
  
  # Test QR solve
  beta_rcpp <- rcpp_qr_solve(X, y)
  beta_base <- qr.solve(X, y)
  expect_true(are_all_close(beta_rcpp, beta_base))
})