#' Approximated gradient via finite difference method for multivariate input
#'
#' @param func function to approximate gradient
#' @param x variable in the function
#' @param dx delta x - referring to finite difference method
#'
#' @return numerical gradients of the function
#'
approx_grad <- function(func, x, dx = .Machine$double.eps^(1/3)) {
  numerical_grad <- sapply(1:length(x), function(i) approx_partial_grad(i, func, x, dx))
  return(numerical_grad)
}

#' Partial gradient used in the finite difference method for multivariate input
#'
#' @param func function to approximate gradient
#' @param x variable in the function
#' @param dx delta x
#'
#' @return gradient of x_i
#'
approx_partial_grad <- function(i, func, x, dx = .Machine$double.eps^(1/3)) {
  delta_x <- rep(0, length(x))
  delta_x[i] <- dx
  return ((func(x + delta_x) - func(x - delta_x)) / (2*dx))
}

#' Approximate gradients of the log-likelihood of linear regression
#' the approximate is achieved via finite difference
#'
#' @param X the design matrix
#' @param y the outcome vector/matrix of the model
#' @param betas coefficients of the linear model
#' @param noise_var an estimate of the residual variance
#'
#' @return numerical gradients of the log-likelihood of linear regression via finite difference
#'
lm_log_likelihood_approx_grad <- function(X, y, betas, noise_var = 1){
  approx_grad(func = function(betas) lm_log_likelihood(X, y, betas, noise_var), x = betas)
}

testthat::test_that("Comparing the numerical gradients via finite difference with the analytical gradients", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "linear", seed = 1918)
  design <- data$design; outcome <- data$outcome
  set.seed(42)
  coef <- rnorm(4)
  expect_true(are_all_close(
    lm_log_likelihood_grad(design, outcome, coef),
    lm_log_likelihood_approx_grad(design, outcome, coef),
    abs_tol = 1e-5, rel_tol = 1e-5
  ))
})
