logistic <- function(x){
  return ( exp(x) / (1+exp(x)) )
}

logistic_log_likelihood <- function(X, y, beta) {
  fitted <- X %*% beta
  return (sum((y * fitted) - log(1 + exp(fitted))))
}

logistic_log_likelihood_grad <- function(X, y, beta) {
  pi <- logistic(X %*% beta)
  return (t(y - pi) %*% X)
}

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

newton_raphson <- function(X, y, tol = 1e-8, iter_max = 100) {
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

    if (abs(new_log_likelihood - prev_log_likelihood) < tol) {
      convergence <- TRUE
      break
    }
  }
  if (convergence == FALSE) {
    warning(sprintf("Maximum iteration reached %d and the algorithm has not converged.", i))
  }
  return(beta)
}

logistic_newton_raphson <- function(X, y){
  newtons_est <- newton_raphson(X, y)
  return (newtons_est)
}
