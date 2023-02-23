testthat::test_that(
  "Comparing if two vectors are close",
  {
    v1 <- runif(100)
    v2 <- v1 + 1e-10
    expect_true(are_all_close(
      v1, v2, abs_tol = 1e-5, rel_tol = 1e-5
    ))
  }
)
# In this case, the absolute distance is about 1e-10, which is smaller than both the abs_tol
# and rel_tol * max(v1,v2).
# It returns TRUE since the two vectors are closer than the tolerant distances.


testthat::test_that(
  "Comparing if two vectors are close",
  {
    v1 <- runif(100)
    v2 <- v1 + 1e-5
    expect_false(are_all_close(
      v1, v2, abs_tol = 1e-4, rel_tol = 1e-6
    ))
  }
)
# In this case, the absolute distance is about 1e-5, which is smaller than abs_tol,
# however, the relative error might larger than the rel_tol * max(v1,v2).
# Therefore, it returns FALSE the relative error is above rel_tol.


testthat::test_that(
  "Comparing if two vectors are close",
  {
    v1 <- runif(100)
    v2 <- v1 + 1e-5
    expect_false(are_all_close(
      v1, v2, abs_tol = 1e-6, rel_tol = 1e-4
    ))
  }
)
# In this case, the absolute distance is about 1e-5, larger than the abs_tol.
# the relative error is still smaller than the rel_tol * max(v1,v2).
# Therefore, it returns FALSE the absolute error is above abs_tol.
