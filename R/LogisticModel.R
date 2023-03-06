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
  pi <- logistic(X %*% beta)
  return (t(y - pi) %*% X)
}

#' Logistic regression MLE estimation via BFGS
#' BFGS is a quasi-Newton method (also known as a variable metric algorithm),
#' specifically that published simultaneously in 1970 by Broyden, Fletcher, Goldfarb and Shanno.
#' Here we use the optim function for BFGS implementation
#'
#' @param X the design matrix
#' @param y the outcome vector/matrix of the model
#'
#' @return Logistic regression MLE coefficient estimated via BFGS
#'
logistic_bfgs <- function(X, y){
  init_coef <- rep(0, ncol(X))
  bfgs_est <- stats::optim(par = init_coef,
                           fn = logistic_log_likelihood,
                           gr = logistic_log_likelihood_grad,
                           X = X, y = y,
                           method = "BFGS",
                           control=list(fnscale=-1))
  optim_converged <- (bfgs_est$convergence == 0L)
  if (!optim_converged) {
    warning("Optimization did not converge.")
  }
  return (bfgs_est$par)
}

#' Newton-Rephson's Method to find optimal coefficient estimations
#' for logistic Regression Models
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
newton_raphson <- function(X, y, atol = 1e-8, rtol = 1e-8, iter_max = 100) {
  beta <- rep(0, ncol(X))

  i <- 0
  convergence <- FALSE
  while (i <= iter_max & convergence == FALSE) {

    i <- i + 1
    pi <- as.vector(logistic(X %*% beta))
    W <- diag(pi * (1 - pi))

    prev_log_likelihood <- logistic_log_likelihood(X, y, beta)

    deltaBeta <- solve(t(X) %*% W %*% X, t(X) %*% (y - pi))
    beta <- beta + deltaBeta
    new_log_likelihood <- logistic_log_likelihood(X, y, beta)

    abs_diff <- abs(new_log_likelihood - prev_log_likelihood)
    rel_diff <- abs_diff * max(new_log_likelihood, prev_log_likelihood)

    if ((abs_diff < atol) && (rel_diff < rtol)) {
      convergence <- TRUE
      break
    }
  }
  if (convergence == FALSE) {
    warning(sprintf("Maximum iteration reached %d and the algorithm has not converged.", i))
  }
  return(beta)
}

#' Logistic regression MLE estimation via Newton-Raphson's method
#' @param X the design matrix
#' @param y the outcome vector/matrix of the model
#'
#' @return Logistic regression MLE coefficient estimated via Newton-Raphson's method
#'
logistic_newton_raphson <- function(X, y){
  newtons_est <- newton_raphson(X, y)
  return (newtons_est)
}
