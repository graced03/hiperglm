#' Logistic function
#'
#' @param x input variate for the function
#'
#' @return value of the logistic function
#'
logistic <- function(x){
  return ( exp(x) / (1+exp(x)) )
}

#' Log-likelihood of logistic regression
#'
#' @param X the design matrix
#' @param y the outcome vector/matrix of the model
#' @param betas coefficients of the linear model
#'
#' @return log-likelihood of logistic regression
#'
logistic_log_likelihood <- function(X, y, beta) {
  fitted <- X %*% beta
  return (sum((y * fitted) - log(1 + exp(fitted))))
}

#' Analytical gradients of the log-likelihood of logistic regression
#'
#' @param X the design matrix
#' @param y the outcome vector/matrix of the model
#' @param betas coefficients of the linear model
#'
#' @return analytical gradients of the log-likelihood of linear regression
#'
logistic_log_likelihood_grad <- function(X, y, beta) {
  prob <- logistic(X %*% beta)
  return (t(X) %*% (y - prob))
}

#' Logistic regression hessian matrix
#'
#' @param X the design matrix
#' @param betas coefficients of the linear model
#'
#' @return hessian of the log-likelihood of linear regression
#'
logistic_hession <- function(X, beta){
  prob <- as.vector(logistic(X %*% beta))
  W <- diag(prob * (1 - prob))
  return (-t(X) %*% W %*% X)
}

#' Newton-Raphson's method to find optimal coefficient estimations
#' for logistic regression Models
#' We declare convergence when the absolute and relative change in log-likelihood between the current and previous iterations
#' falls below pre-specified thresholds.
#' @param X the design matrix
#' @param y the outcome vector/matrix of the model
#' @param atol absolute tolerance for convergence check
#' @param rtol relative tolerance for convergence check
#' @param iter_max maximum iterations for running the algorithm
#'
#' @return Logistic regression MLE coefficient estimated via newton-raphson's method
#'
newton_raphson <- function(X, y, atol = 1e-8, rtol = 1e-8, iter_max = 25L) {
  beta <- rep(0, ncol(X))

  i <- 0L
  converged <- FALSE

  curr_log_likelihood <- logistic_log_likelihood(X, y, beta)

  while (i <= iter_max & converged == FALSE) {

    i <- i + 1L
    prev_log_likelihood <- curr_log_likelihood

    deltaBeta <- solve(logistic_hession(X, beta), logistic_log_likelihood_grad(X, y, beta))
    beta <- beta - deltaBeta
    curr_log_likelihood <- logistic_log_likelihood(X, y, beta)

    if (are_all_close(curr_log_likelihood, prev_log_likelihood, abs_tol = atol, rel_tol = rtol)) {
      converged <- TRUE
      break
    }
  }
  if (converged == FALSE) {
    warning(sprintf("Maximum iteration reached %d and the algorithm has not converged.", i))
  }
  return(beta)
}
