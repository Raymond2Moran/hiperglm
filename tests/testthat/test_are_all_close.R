test_that("are_all_close() returns TRUE when differences are within both tolerances", {
  # Both the abs_diff and rel_diff are within the range (1e-6)
  # Expect TRUE

  v <- c(1, 2, 3)
  w <- c(1.0000001, 2.0000001, 3.0000001)
  expect_true(are_all_close(v, w))
})

test_that("returns FALSE if the relative error is too large", {
  # Here, abs_diff = 1e-7 < 1e-6 (passes),
  # but rel_diff = 1e-7 / 1e-7 = 1, which is greater than 1e-6 (fails).
  # Expect FALSE

  v <- c(0, 0)
  w <- c(1e-7, 1e-7)
  
  expect_false(are_all_close(v, w, abs_tol = 1e-6, rel_tol = 1e-6))
})

test_that("returns FALSE if the absolute error is too large", {
  # Here, abs_diff is 1e-4 > 1e-6 (fails)
  # but rel_diff = 0.0001 / 300 = 3.33e-7 < 1e-6 (passes).
  # Expect FALSE
  
  v <- c(200, 300)
  w <- c(200.0001, 300.0001)

  expect_false(are_all_close(v, w, abs_tol = 1e-6, rel_tol = 1e-6))
})