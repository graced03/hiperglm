testthat::test_that("Comparing the numerical gradients via finite difference with the analytical gradients", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 1918)
  design <- data$design; outcome <- data$outcome
  set.seed(42)
  coef <- rnorm(4) # randomly
  expect_true(are_all_close(
    lm_log_likelihood_grad(design, outcome, coef),
    lm_log_likelihood_approx_grad(design, outcome, coef),
    abs_tol = 1e-5, rel_tol = 1e-5
  ))
})
