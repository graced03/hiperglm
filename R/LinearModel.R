lm_ols <- function(X, y){
  return (solve(t(X) %*% X, t(X) %*% y))
}

lm_log_likelihood <- function(X, y, betas, noise_var = 1) {
  return (- t(y - X %*% betas) %*% (y - X %*% betas)/noise_var/2)
}

lm_log_likelihood_grad <- function(X, y, betas, noise_var = 1){
  return (t(X) %*% y - t(X) %*% X %*% betas)
}

lm_log_likelihood_approx_grad <- function(X, y, betas, noise_var = 1){
  approx_grad(func = function(betas) lm_log_likelihood(X, y, betas, noise_var), x = betas)
}

approx_grad <- function(func, x, dx = .Machine$double.eps^(1/3)) {
  # finite difference for multivariate
  # numerical_grad <- rep(0, length(x))
  numerical_grad <- sapply(1:length(x), function(i) approx_partial_grad(i, func, x, dx))
  return(numerical_grad)
}

approx_partial_grad <- function(i, func, x, dx = .Machine$double.eps^(1/3)) {
  delta_x <- rep(0, length(x))
  delta_x[i] <- dx
  return ((func(x + delta_x) - func(x - delta_x)) / (2*dx))
}

lm_bfgs <- function(X, y, noise_var = 1){
  #  the BFGS method from optim
  init_coef <- rep(0, ncol(X))
  # the number of coefficients is the same as the number of cols in the design matrix X
  bfgs_est <- stats::optim(init_coef, lm_log_likelihood, lm_log_likelihood_approx_grad,
                           design=X, outcome=y, method = "BFGS",
                           control=list(fnscale=-1))
  # return the best set of parameters found
  return (bfgs_est$par)
}

# set.seed(410)
# n_param <- 4
# X <- matrix(rnorm(2 * n_param^2), nrow = 2 * n_param, ncol = n_param)
# SIGMAINV <- t(X) %*% X
#
# # Calculate log-density of centered Gaussian up to an additive factor
# gaussian_logp <- function(x, Sigma_inv = SIGMAINV) {
#   logp <- - .5 * t(x) %*% Sigma_inv %*% x
#   return(logp)
# }
#
# gaussian_grad <- function(x, Sigma_inv  = SIGMAINV) {
#   grad <- - Sigma_inv %*% x
#   return(grad)
# }
#
# y <- c(33, 10, 39, 11)
# betas <- c(1,2,3)
#
# numerical_grad <- lm_log_likelihood_grad(X, y, betas)
# analytical_grad <- lm_log_likelihood_approx_grad(X, y, betas)
#
# testthat::expect_true(are_all_close(
#   analytical_grad, numerical_grad, abs_tol = Inf, rel_tol = 1e-3
# ))
