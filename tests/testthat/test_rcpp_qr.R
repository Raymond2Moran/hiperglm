test_that("Rcpp QR matches base R QR", {
  set.seed(123)
  X <- matrix(rnorm(100), 10, 10)
  y <- rnorm(10)
  
  # Test QR solve
  result_rcpp <- rcpp_qr_solve(X, y)
  result_base <- qr.solve(X, y)
  expect_true(are_all_close(result_rcpp, result_base))
})