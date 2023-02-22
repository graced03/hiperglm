#' Linear regression MLE estimation via ordinary least square
#'
#' @param X the design matrix
#' @param y the outcome vector/matrix of the model
#'
#' @return the ordinary least square estimation of betas coefficients (same as via pseudo-inverse)
#'
lm_ols <- function(X, y){
  return (solve(t(X) %*% X, t(X) %*% y))
}

#' Log-likelihood of linear regression
#'
#' @param X the design matrix
#' @param y the outcome vector/matrix of the model
#' @param betas coefficients of the linear model
#' @param noise_var an estimate of the residual variance
#'
#' @return S3 object of the model fitting output
#'
lm_log_likelihood <- function(X, y, betas, noise_var = 1) {
  return (- t(y - X %*% betas) %*% (y - X %*% betas)/noise_var/2)
}

#' Analytical gradients of the log-likelihood of linear regression
#'
#' @param X the design matrix
#' @param y the outcome vector/matrix of the model
#' @param betas coefficients of the linear model
#' @param noise_var an estimate of the residual variance
#'
#' @return analytical gradients of the log-likelihood of linear regression
#'
lm_log_likelihood_grad <- function(X, y, betas, noise_var = 1){
  return (t(X) %*% y - t(X) %*% X %*% betas)
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


#' Linear regression MLE estimation via BFGS
#' BFGS is a quasi-Newton method (also known as a variable metric algorithm),
#' specifically that published simultaneously in 1970 by Broyden, Fletcher, Goldfarb and Shanno.
#'
#' @param X the design matrix
#' @param y the outcome vector/matrix of the model
#'
#' @return linear regression MLE coefficient estimated via BFGS
#'
lm_bfgs <- function(X, y, noise_var = 1){
  # the number of coefficients is the same as the number of cols in the design matrix X
  init_coef <- rep(0, ncol(X))
  #  the BFGS method from optim
  bfgs_est <- stats::optim(par = init_coef, fn = lm_log_likelihood, gr = lm_log_likelihood_approx_grad,
                           X=X, y=y, method = "BFGS",
                           control=list(fnscale=-1))
  # return the best set of parameters found
  return (bfgs_est$par)
}

# set.seed(410)
# n_param <- 3
# X <- matrix(rnorm(2 * n_param^2), nrow = 2 * n_param, ncol = n_param)
# y <- c(33, 10, 39)
# betas <- c(1,2,3)
#
# numerical_grad <- lm_log_likelihood_grad(X, y, betas)
# analytical_grad <- lm_log_likelihood_approx_grad(X, y, betas)
#
# testthat::expect_true(are_all_close(
#   analytical_grad, numerical_grad, abs_tol = Inf, rel_tol = 1e-3
# ))
