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
#' @return log-likelihood of linear regression
#'
lm_log_likelihood <- function(X, y, betas, noise_var = 1) {
  eta <- X %*% betas
  residual <- y - eta
  return (- sum(residual^2) / noise_var / 2)
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

#' Linear regression MLE estimation via BFGS
#' BFGS is a quasi-Newton method (also known as a variable metric algorithm),
#' specifically that published simultaneously in 1970 by Broyden, Fletcher, Goldfarb and Shanno.
#' Here we use the optim function for BFGS implementation
#'
#' @param X the design matrix
#' @param y the outcome vector/matrix of the model
#' @param noise_var an estimate of the residual variance
#'
#' @return linear regression MLE coefficient estimated via BFGS
#'
lm_bfgs <- function(X, y, noise_var = 1){
  init_coef <- rep(0, ncol(X))
  bfgs_est <- stats::optim(par = init_coef, fn = lm_log_likelihood, gr = lm_log_likelihood_grad,
                           X = X, y = y, noise_var = noise_var, method = "BFGS",
                           control=list(fnscale=-1))
  optim_converged <- (bfgs_est$convergence == 0L)
  if (!optim_converged) {
    warning("Optimization did not converge.")
  }
  return (bfgs_est$par)
}

