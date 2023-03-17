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
  fitted <- X %*% betas
  residual <- y - fitted
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


