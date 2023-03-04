testthat::test_that(
  "Comparing if two vectors are close - test pass",
  {
    v1 <- rep(1,100)
    v2 <- v1 + 1e-10
    expect_true(are_all_close(
      v1, v2, abs_tol = 1e-5, rel_tol = 1e-5
    ))
  }
)

testthat::test_that(
  "Comparing if two vectors are close - relative error largar than rel_tol",
  {
    v1 <- rep(1,100)
    v2 <- v1 + 1e-5
    expect_false(are_all_close(
      v1, v2, abs_tol = 1e-4, rel_tol = 1e-6
    ))
  }
)

testthat::test_that(
  "Comparing if two vectors are close - absolute error is above abs_tol",
  {
    v1 <- rep(1,100)
    v2 <- v1 + 1e-5
    expect_false(are_all_close(
      v1, v2, abs_tol = 1e-6, rel_tol = 1e-4
    ))
  }
)
