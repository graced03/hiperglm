lm_log_likelihood_approx_grad <- function(X, y, betas, noise_var = 1){
  approx_grad(func = function(betas) lm_log_likelihood(X, y, betas, noise_var), x = betas)
}

testthat::test_that("Linear model - comparing the numerical gradients via finite difference with the analytical gradients", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "linear", seed = 1918)
  design <- data$design; outcome <- data$outcome
  set.seed(42)
  coef <- rnorm(n_pred)
  expect_true(are_all_close(
    lm_log_likelihood_grad(design, outcome, coef),
    lm_log_likelihood_approx_grad(design, outcome, coef),
    abs_tol = 1e-6, rel_tol = 1e-6
  ))
})


logistic_log_likelihood_approx_grad <- function(X, y, betas){
  approx_grad(func = function(betas) logistic_log_likelihood(X, y, betas), x = betas)
}

test_that("Logistic model - comparing the numerical gradients via finite difference with the analytical gradients", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "logit", seed = 1918)
  design <- data$design; outcome <- data$outcome
  set.seed(42)
  coef <- rnorm(n_pred)
  expect_true(are_all_close(
    logistic_log_likelihood_grad(design, outcome, coef),
    logistic_log_likelihood_approx_grad(design, outcome, coef),
    abs_tol = 1e-6, rel_tol = 1e-6
  ))
})
